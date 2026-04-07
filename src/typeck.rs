mod closures;
mod err;
mod mthd;
mod normalize;
mod ops;
mod post_check;
mod ty_annots;
mod unify;

use std::collections::HashMap;

use crate::{
    ctxt::{
        self,
        fns::{self, FnInst},
        language_items,
        traits::{self, TraitInst},
        ty,
    },
    hlr,
};

pub use err::*;
pub use mthd::MthdResolution;

#[derive(Default)]
pub struct HlrTyping<'ty> {
    pub var_types: HashMap<hlr::VarId, ty::Ty<'ty>>,
    pub expr_types: HashMap<hlr::ExprId, ty::Ty<'ty>>,
    pub expr_extra: HashMap<hlr::ExprId, ExprExtra<'ty>>,
}

#[derive(Clone, Copy)]
enum MatchBinding {
    Direct,
    ByRef,
    ByRefMut,
}

pub enum DerefStep<'ty> {
    Builtin,
    Trait(MthdResolution<'ty>),
}

pub enum ExprExtra<'ty> {
    ValFn(fns::FnInst<'ty>),
    ValMthd(MthdResolution<'ty>),
    BinaryPrim(language_items::BinaryPrimOp),
    BinaryOpMthd(MthdResolution<'ty>),
    UnaryPrim(language_items::UnaryPrimOp),
    DerefMthd(MthdResolution<'ty>),
    MthdCall {
        resolution: MthdResolution<'ty>,
        steps: Vec<DerefStep<'ty>>,
    },
    FieldAccess {
        steps: Vec<DerefStep<'ty>>,
        index: usize,
    },
    Closure {
        captured_vars: Vec<hlr::VarId>,
    },
}

pub fn typeck<'a, 'ctxt: 'a>(
    ctxt: &'a mut ctxt::Ctxt<'ctxt>,
    fn_: &'a hlr::Fn<'ctxt>,
) -> TypeckResult<'ctxt, HlrTyping<'ctxt>> {
    let mut constraints: Vec<_> = fn_.fn_.all_constraints().cloned().collect();
    ctxt.expand_constraints_with_assoc_bounds(&mut constraints);
    let typeck = Typeck {
        ctxt,
        fn_,
        constraints,
        type_vars: HashMap::new(),
        typing: Default::default(),
        closure_counter: 0,
        return_ty_stack: vec![],
        var_uses: vec![],
        created_closure_structs: vec![],
        pending_obligations: vec![],
    };

    typeck.check()
}

struct Typeck<'a, 'ctxt: 'a> {
    ctxt: &'a mut ctxt::Ctxt<'ctxt>,
    fn_: &'a hlr::Fn<'ctxt>,
    constraints: Vec<ty::Constraint<'ctxt>>,

    type_vars: HashMap<ty::InfVar, ty::Ty<'ctxt>>,
    typing: HlrTyping<'ctxt>,
    closure_counter: usize,
    return_ty_stack: Vec<ty::Ty<'ctxt>>,
    var_uses: Vec<hlr::VarId>,

    created_closure_structs: Vec<(ty::Struct<'ctxt>, Vec<hlr::VarId>)>,

    pending_obligations: Vec<(ty::Ty<'ctxt>, ty::ConstraintRequirement<'ctxt>)>,
}

impl<'a, 'ctxt: 'a> Typeck<'a, 'ctxt> {
    fn check(mut self) -> TypeckResult<'ctxt, HlrTyping<'ctxt>> {
        let opaque_return = self.check_body()?;
        self.normalize_all();
        self.check_pending_obligations()?;
        self.normalize_all();

        if let Some((opaque, inf_var_ty)) = opaque_return {
            let concrete_ty = self.normalize(inf_var_ty);
            self.ctxt.tys.set_opaque_resolution(opaque, concrete_ty);
        }

        self.create_closure_fns();
        self.post_check();

        Ok(self.typing)
    }

    fn check_body(&mut self) -> TypeckResult<'ctxt, Option<(ty::Opaque<'ctxt>, ty::Ty<'ctxt>)>> {
        for (param, &param_var_id) in self.fn_.fn_.params.iter().zip(&self.fn_.param_var_ids) {
            self.typing.var_types.insert(param_var_id, param.ty);
        }

        let return_ty = self.fn_.fn_.return_ty;

        let (effective_return_ty, opaque_return) = if let ty::TyDef::Opaque { opaque, .. } = *return_ty.0 {
            let inf_var = self.ctxt.tys.inf_var();
            for req in opaque.constraints.iter().cloned() {
                self.pending_obligations.push((inf_var, req));
            }
            (inf_var, Some((opaque, inf_var)))
        } else {
            (return_ty, None)
        };

        self.return_ty_stack.push(effective_return_ty);
        let body_ty = self.check_expr(self.fn_.body, Some(effective_return_ty))?;

        if self.unify(body_ty, effective_return_ty) {
            Ok(opaque_return)
        } else {
            Err(TypeckError::ReturnTypeMismatch {
                expected: effective_return_ty,
                actual: body_ty,
            })
        }
    }

    fn check_expr(
        &mut self,
        expr: hlr::Expr<'ctxt>,
        hint: Option<ty::Ty<'ctxt>>,
    ) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        let ty = match expr.0 {
            hlr::ExprDef::Lit(lit) => self.check_lit(lit),
            hlr::ExprDef::Val(val) => self.check_val(expr.1, val),
            hlr::ExprDef::BinaryOp { left, right, operator } => self.check_binary_op(expr.1, *left, *right, *operator),
            hlr::ExprDef::UnaryOp { operand, operator } => self.check_unary_op(expr.1, *operand, *operator),
            hlr::ExprDef::Call { callee, args } => self.check_call(*callee, args),
            hlr::ExprDef::MthdCall {
                receiver,
                mthd_name,
                gen_args,
                args,
            } => self.check_mthd_call(expr.1, *receiver, mthd_name, *gen_args, args),
            hlr::ExprDef::Struct { constructor, fields } => self.check_struct_expr(constructor, fields),
            hlr::ExprDef::FieldAccess { base, field } => self.check_field_access(expr.1, *base, field),
            hlr::ExprDef::Tuple(exprs) => self.check_tuple_expr(exprs),
            hlr::ExprDef::Assign { target, value } => self.check_assignment(*target, *value),
            hlr::ExprDef::Deref(inner) => self.check_deref(expr.1, *inner),
            hlr::ExprDef::AddrOf(expr) => self.check_addr_of(*expr),
            hlr::ExprDef::AddrOfMut(expr) => self.check_addr_of_mut(*expr),
            hlr::ExprDef::As { expr, ty } => self.check_as(*expr, ty),
            hlr::ExprDef::Closure {
                params,
                return_ty,
                body,
            } => self.check_closure(expr.1, params, *return_ty, *body, hint),
            hlr::ExprDef::If { cond, then, else_ } => self.check_if(*cond, *then, *else_),
            hlr::ExprDef::Loop { body } => self.check_loop(*body),
            hlr::ExprDef::Match { scrutinee, arms } => self.check_match(*scrutinee, arms),
            hlr::ExprDef::Block { stmts, trailing } => self.check_block(stmts, *trailing, hint),
            hlr::ExprDef::QualifiedMthd {
                ty,
                trait_,
                trait_args,
                mthd_name,
                args,
            } => self.check_qualified_mthd(expr.1, ty, *trait_, *trait_args, mthd_name, *args),
        }?;

        self.typing.expr_types.insert(expr.1, ty);

        Ok(ty)
    }

    fn check_stmt(&mut self, stmt: hlr::Stmt<'ctxt>) -> TypeckResult<'ctxt, ()> {
        match stmt {
            hlr::StmtDef::Expr(expr) => self.check_expr(*expr, None).map(|_| ()),
            &hlr::StmtDef::Let { var, mutable, ty, init } => self.check_let_stmt(var, mutable, ty, init),
            hlr::StmtDef::Break => Ok(()),
            hlr::StmtDef::Return(expr) => self.check_return_stmt(*expr),
        }
    }

    fn check_lit(&mut self, lit: &hlr::Lit) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        let ty = match lit {
            hlr::Lit::Int(_) => self.ctxt.tys.primitive(ty::Primitive::Integer32),
            hlr::Lit::Bool(_) => self.ctxt.tys.primitive(ty::Primitive::Boolean),
            hlr::Lit::CChar(_) => self.ctxt.tys.primitive(ty::Primitive::CChar),
            hlr::Lit::CString(_) => {
                let c_char_ty = self.ctxt.tys.primitive(ty::Primitive::CChar);
                self.ctxt.tys.ptr(c_char_ty)
            }
        };

        Ok(ty)
    }

    fn check_val(&mut self, expr_id: hlr::ExprId, val: &hlr::Val<'ctxt>) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        match val {
            &hlr::Val::Var(var_id) => {
                self.var_uses.push(var_id);
                self.typing
                    .var_types
                    .get(&var_id)
                    .copied()
                    .ok_or(TypeckError::VarTypeNotSet { var_id, expr_id })
            }
            &hlr::Val::Fn(fn_, args) => self.check_fn(expr_id, fn_, args),
            hlr::Val::Struct(..) => unreachable!("raw struct values are not supported"),
            hlr::Val::Variant(..) => unreachable!("raw variant values are not supported"),
            hlr::Val::Mthd(base_ty, mthd_name, gen_args) => self.check_mthd(expr_id, base_ty, mthd_name, *gen_args),
        }
    }

    fn check_fn(
        &mut self,
        expr_id: hlr::ExprId,
        fn_: fns::Fn<'ctxt>,
        args: Option<hlr::TyAnnotSlice<'ctxt>>,
    ) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        let gen_arg_count = fn_.gen_params.len();

        let gen_args =
            self.resolve_optional_gen_args(args, gen_arg_count, |actual| TypeckError::FnGenArgCountMismatch {
                fn_,
                expected: gen_arg_count,
                actual,
            })?;

        let subst = ty::GenVarSubst::new(&fn_.gen_params, &gen_args).unwrap();
        let param_tys: Vec<_> = fn_.params.iter().map(|param| param.ty).collect();
        self.add_constraint_obligations(fn_, &subst);

        let fn_ty = self.ctxt.tys.fn_(&param_tys, fn_.return_ty, fn_.var_args);

        let gen_args = self.ctxt.tys.ty_slice(&gen_args);
        let empty = self.ctxt.tys.ty_slice(&[]);
        let fn_inst = FnInst::new(fn_, gen_args, empty)
            .unwrap()
            .with_self_ty(fn_.associated_ty);
        self.typing.expr_extra.insert(expr_id, ExprExtra::ValFn(fn_inst));

        Ok(self.ctxt.tys.substitute_gen_vars(fn_ty, &subst))
    }

    fn check_mthd(
        &mut self,
        expr_id: hlr::ExprId,
        base_ty: hlr::TyAnnot<'ctxt>,
        mthd_name: &str,
        gen_args: Option<hlr::TyAnnotSlice<'ctxt>>,
    ) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        let base_ty = self.resolve_ty_annot(base_ty)?;
        let found = self.resolve_mthd(base_ty, mthd_name, false)?;
        let resolution = self.instantiate_mthd(found, base_ty, mthd_name, gen_args)?;
        let fn_ty = self.fn_ty_of_mthd_resolution(&resolution);
        self.typing.expr_extra.insert(expr_id, ExprExtra::ValMthd(resolution));
        Ok(fn_ty)
    }

    fn check_qualified_mthd(
        &mut self,
        expr_id: hlr::ExprId,
        ty: hlr::TyAnnot<'ctxt>,
        trait_: Option<traits::Trait<'ctxt>>,
        trait_args: Option<hlr::TyAnnotSlice<'ctxt>>,
        mthd_name: &str,
        args: Option<hlr::TyAnnotSlice<'ctxt>>,
    ) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        let base_ty = self.resolve_ty_annot(ty)?;

        let found = match trait_ {
            None => self.resolve_mthd(base_ty, mthd_name, false)?,
            Some(trait_) => {
                let n_trait_gen_params = trait_.gen_params.len();
                let trait_gen_args = self.resolve_optional_gen_args(trait_args, n_trait_gen_params, |actual| {
                    TypeckError::TraitGenArgCountMismatch {
                        trait_,
                        expected: n_trait_gen_params,
                        actual,
                    }
                })?;
                let trait_gen_args = self.ctxt.tys.ty_slice(&trait_gen_args);
                let trait_inst = TraitInst::new(trait_, trait_gen_args).unwrap();

                let mthd = self
                    .ctxt
                    .traits
                    .resolve_trait_method(trait_, mthd_name)
                    .ok_or_else(|| TypeckError::MthdResolutionFailed {
                        base_ty,
                        mthd_name: mthd_name.to_string(),
                    })?;

                mthd::FoundMthd::Trait { trait_inst, mthd }
            }
        };

        let resolution = self.instantiate_mthd(found, base_ty, mthd_name, args)?;
        let fn_ty = self.fn_ty_of_mthd_resolution(&resolution);
        self.typing.expr_extra.insert(expr_id, ExprExtra::ValMthd(resolution));
        Ok(fn_ty)
    }

    fn check_unary_op(
        &mut self,
        expr_id: hlr::ExprId,
        operand: hlr::Expr<'ctxt>,
        operator: hlr::UnaryOperator,
    ) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        let operand_ty = self.check_expr(operand, None)?;

        let i32_ty = self.ctxt.tys.primitive(ty::Primitive::Integer32);
        let bool_ty = self.ctxt.tys.primitive(ty::Primitive::Boolean);

        use hlr::UnaryOperator::*;
        use language_items::UnaryPrimOp::*;

        let (prim, result_ty) = match operator {
            Negative if operand_ty == i32_ty => (NegI32, i32_ty),
            Not if operand_ty == bool_ty => (NotBool, bool_ty),
            _ => return Err(TypeckError::UnaryOpTypeMismatch { operator, operand_ty }),
        };

        self.typing.expr_extra.insert(expr_id, ExprExtra::UnaryPrim(prim));
        Ok(result_ty)
    }

    fn check_field_access(
        &mut self,
        expr_id: hlr::ExprId,
        base: hlr::Expr<'ctxt>,
        field: &hlr::FieldSpec,
    ) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        let base_ty = self.check_expr(base, None)?;
        let mut base_ty = self.normalize(base_ty);
        let mut steps: Vec<DerefStep<'ctxt>> = Vec::new();

        loop {
            let found_index = match field {
                hlr::FieldSpec::Name(name) => {
                    if let ty::TyDef::Struct { .. } = base_ty.0 {
                        base_ty.struct_field_index_by_name(name).ok()
                    } else {
                        None
                    }
                }
                hlr::FieldSpec::Index(index) => {
                    if matches!(base_ty.0, ty::TyDef::Tuple(_)) {
                        Some(*index)
                    } else {
                        None
                    }
                }
            };

            if let Some(index) = found_index {
                self.typing
                    .expr_extra
                    .insert(expr_id, ExprExtra::FieldAccess { steps, index });
                return match field {
                    hlr::FieldSpec::Name(_) => Ok(self.ctxt.tys.get_struct_field_ty(base_ty, index).unwrap()),
                    hlr::FieldSpec::Index(_) => Ok(base_ty.tuple_field_tys().unwrap()[index]),
                };
            }

            match self.try_deref_step(base_ty) {
                Some((inner_ty, step)) => {
                    steps.push(step);
                    base_ty = inner_ty;
                }
                None => {
                    return Err(match field {
                        hlr::FieldSpec::Name(name) => match base_ty.0 {
                            ty::TyDef::Struct { .. } => TypeckError::StructFieldNotFound {
                                struct_ty: base_ty,
                                field: name.clone(),
                            },
                            _ => TypeckError::NamedFieldAccessOnNonStruct { ty: base_ty },
                        },
                        hlr::FieldSpec::Index(_) => TypeckError::IndexedFieldAccessOnNonTuple { ty: base_ty },
                    });
                }
            }
        }
    }

    fn check_struct_expr(
        &mut self,
        constructor: &hlr::Val<'ctxt>,
        fields: hlr::StructFields<'ctxt>,
    ) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        let (return_ty, fields_ty) = match constructor {
            hlr::Val::Struct(struct_, gen_args) => {
                let n_gen_params = struct_.gen_params.len();
                let gen_args = self.resolve_optional_gen_args(*gen_args, n_gen_params, |actual| {
                    TypeckError::StructGenArgCountMismatch {
                        struct_,
                        expected: n_gen_params,
                        actual,
                    }
                })?;
                let ty = self.ctxt.tys.inst_struct(struct_, &gen_args).unwrap();
                (ty, ty)
            }
            hlr::Val::Variant(enum_, variant_idx, gen_args) => {
                let n_gen_params = enum_.gen_params.len();
                let gen_args = self.resolve_optional_gen_args(*gen_args, n_gen_params, |actual| {
                    TypeckError::EnumGenArgCountMismatch {
                        enum_,
                        expected: n_gen_params,
                        actual,
                    }
                })?;
                let enum_ty = self.ctxt.tys.inst_enum(enum_, &gen_args).unwrap();
                let variant_ty = self.ctxt.tys.get_enum_variant_ty(enum_ty, *variant_idx).unwrap();
                (enum_ty, variant_ty)
            }
            _ => unreachable!("struct expression constructor must be Val::Struct or Val::Variant"),
        };

        for (field_spec, field_expr) in fields {
            let field_idx = match field_spec {
                hlr::FieldSpec::Name(name) => {
                    fields_ty
                        .struct_field_index_by_name(name)
                        .map_err(|_| TypeckError::StructFieldNotFound {
                            struct_ty: fields_ty,
                            field: name.clone(),
                        })?
                }
                hlr::FieldSpec::Index(idx) => *idx,
            };
            let field_ty = self.ctxt.tys.get_struct_field_ty(fields_ty, field_idx).unwrap();
            let expr_ty = self.check_expr(*field_expr, Some(field_ty))?;
            if !self.unify(expr_ty, field_ty) {
                return Err(TypeckError::StructFieldTypeMismatch {
                    struct_ty: fields_ty,
                    field_idx,
                    expected: field_ty,
                    actual: expr_ty,
                });
            }
        }

        Ok(return_ty)
    }

    fn check_mthd_call(
        &mut self,
        expr_id: hlr::ExprId,
        receiver: hlr::Expr<'ctxt>,
        mthd_name: &str,
        gen_args: Option<hlr::TyAnnotSlice<'ctxt>>,
        args: hlr::ExprSlice<'ctxt>,
    ) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        let receiver_ty = self.check_expr(receiver, None)?;
        let receiver_ty = self.normalize(receiver_ty);

        let (found, steps, base_ty) = self.resolve_mthd_with_deref(receiver_ty, mthd_name)?;
        let resolution = self.instantiate_mthd(found, base_ty, mthd_name, gen_args)?;
        let fn_ty = self.fn_ty_of_mthd_resolution(&resolution);
        self.typing
            .expr_extra
            .insert(expr_id, ExprExtra::MthdCall { resolution, steps });

        let Some((param_tys, return_ty, var_args)) = self.ctxt.ty_is_callable(&self.constraints, fn_ty) else {
            unreachable!("method resolution always produces a callable type");
        };

        // param_tys[0] is self; user-supplied args map to param_tys[1..]
        let n_params = param_tys.len().saturating_sub(1);
        let n_args = args.len();
        if (var_args && n_args < n_params) || (!var_args && n_args != n_params) {
            return Err(TypeckError::CallArgCountMismatch {
                expected: n_params,
                actual: n_args,
                var_args,
            });
        }

        for (i, arg) in args.iter().enumerate() {
            let param_hint = param_tys.get(i + 1).copied();
            let arg_ty = self.check_expr(*arg, param_hint)?;
            if let Some(&param_ty) = param_tys.get(i + 1)
                && !self.unify(arg_ty, param_ty)
            {
                return Err(TypeckError::CallArgTypeMismatch {
                    index: i,
                    expected: param_ty,
                    actual: arg_ty,
                });
            }
        }

        Ok(return_ty)
    }

    fn check_call(
        &mut self,
        callee: hlr::Expr<'ctxt>,
        args: hlr::ExprSlice<'ctxt>,
    ) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        let callee_ty = self.check_expr(callee, None)?;
        let callee_ty = self.normalize(callee_ty);

        let Some((param_tys, return_ty, var_args)) = self.ctxt.ty_is_callable(&self.constraints, callee_ty) else {
            return Err(TypeckError::CalleeNotCallable { ty: callee_ty });
        };

        let n_args = args.len();
        let n_params = param_tys.len();
        if (var_args && n_args < n_params) || (!var_args && n_args != n_params) {
            return Err(TypeckError::CallArgCountMismatch {
                expected: n_params,
                actual: n_args,
                var_args,
            });
        }

        for (i, arg) in args.iter().enumerate() {
            let param_hint = param_tys.get(i).copied();
            let arg_ty = self.check_expr(*arg, param_hint)?;
            if let Some(&param_ty) = param_tys.get(i)
                && !self.unify(arg_ty, param_ty)
            {
                return Err(TypeckError::CallArgTypeMismatch {
                    index: i,
                    expected: param_ty,
                    actual: arg_ty,
                });
            }
        }

        Ok(return_ty)
    }

    fn check_tuple_expr(&mut self, exprs: hlr::ExprSlice<'ctxt>) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        let expr_tys: Vec<_> = exprs
            .iter()
            .map(|expr| self.check_expr(*expr, None))
            .collect::<TypeckResult<'ctxt, _>>()?;

        let tuple_ty = self.ctxt.tys.tuple(&expr_tys);
        Ok(tuple_ty)
    }

    fn check_expr_is_assignable_place(&self, expr: hlr::Expr<'ctxt>) -> TypeckResult<'ctxt, ()> {
        match expr.0 {
            hlr::ExprDef::Deref(_) | hlr::ExprDef::FieldAccess { .. } | hlr::ExprDef::Val(hlr::Val::Var(_)) => Ok(()),
            _ => Err(TypeckError::AssignmentTargetNotAPlace),
        }
    }

    fn check_assignment(
        &mut self,
        target: hlr::Expr<'ctxt>,
        value: hlr::Expr<'ctxt>,
    ) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        self.check_expr_is_assignable_place(target)?;

        let target_ty = self.check_expr(target, None)?;
        let value_ty = self.check_expr(value, None)?;
        if !self.unify(target_ty, value_ty) {
            return Err(TypeckError::AssignmentTypeMismatch {
                expected: target_ty,
                actual: value_ty,
            });
        }
        Ok(self.ctxt.tys.unit())
    }

    fn try_deref_step(&mut self, ty: ty::Ty<'ctxt>) -> Option<(ty::Ty<'ctxt>, DerefStep<'ctxt>)> {
        match ty.0 {
            &ty::TyDef::Ref(inner) | &ty::TyDef::RefMut(inner) | &ty::TyDef::Ptr(inner) => {
                Some((self.normalize(inner), DerefStep::Builtin))
            }
            _ => {
                let deref_trait = self.ctxt.language_items.deref_trait?;
                let gen_args = self.ctxt.tys.ty_slice(&[]);
                let trait_inst = TraitInst::new(deref_trait, gen_args).unwrap();
                if !self.ctxt.ty_implements_trait_inst(&self.constraints, ty, trait_inst) {
                    return None;
                }
                let target_assoc = self.ctxt.tys.assoc_ty(ty, trait_inst, 0);
                let target_ty = self.normalize(target_assoc);
                let mthd = self.ctxt.traits.resolve_trait_method(deref_trait, "deref").unwrap();
                let found = mthd::FoundMthd::Trait { trait_inst, mthd };
                let resolution = self.instantiate_mthd(found, ty, "deref", None).ok()?;
                Some((target_ty, DerefStep::Trait(resolution)))
            }
        }
    }

    fn check_deref(&mut self, expr_id: hlr::ExprId, inner: hlr::Expr<'ctxt>) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        let expr_ty = self.check_expr(inner, None)?;
        let expr_ty = self.normalize(expr_ty);

        match self.try_deref_step(expr_ty) {
            Some((target_ty, DerefStep::Builtin)) => {
                if target_ty.is_c_void() {
                    Err(TypeckError::DereferenceOfCVoid { ty: expr_ty })
                } else {
                    Ok(target_ty)
                }
            }
            Some((target_ty, DerefStep::Trait(resolution))) => {
                self.typing.expr_extra.insert(expr_id, ExprExtra::DerefMthd(resolution));
                Ok(target_ty)
            }
            None => Err(TypeckError::DereferenceOfNonRef { ty: expr_ty }),
        }
    }

    fn check_addr_of(&mut self, expr: hlr::Expr<'ctxt>) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        let expr_ty = self.check_expr(expr, None)?;
        Ok(self.ctxt.tys.ref_(expr_ty))
    }

    fn check_addr_of_mut(&mut self, expr: hlr::Expr<'ctxt>) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        let expr_ty = self.check_expr(expr, None)?;
        Ok(self.ctxt.tys.ref_mut(expr_ty))
    }

    fn check_as(
        &mut self,
        expr: hlr::Expr<'ctxt>,
        target_ty: hlr::TyAnnot<'ctxt>,
    ) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        let expr_ty = self.check_expr(expr, None)?;
        let expr_ty = self.normalize(expr_ty);

        let target_ty = self.resolve_ty_annot(target_ty)?;
        let target_ty = self.normalize(target_ty);

        match (expr_ty.0, target_ty.0) {
            (ty::TyDef::Ptr(_), ty::TyDef::Ptr(_)) => Ok(target_ty),
            (&ty::TyDef::Ref(op_base_ty), &ty::TyDef::Ptr(target_base_ty))
            | (&ty::TyDef::RefMut(op_base_ty), &ty::TyDef::Ptr(target_base_ty))
            | (&ty::TyDef::RefMut(op_base_ty), &ty::TyDef::Ref(target_base_ty)) => {
                if self.unify(op_base_ty, target_base_ty) {
                    Ok(target_ty)
                } else {
                    Err(TypeckError::InvalidAsConversion {
                        op_ty: expr_ty,
                        target_ty,
                    })
                }
            }
            _ => Err(TypeckError::InvalidAsConversion {
                op_ty: expr_ty,
                target_ty,
            }),
        }
    }

    fn check_if(
        &mut self,
        cond: hlr::Expr<'ctxt>,
        then: hlr::Expr<'ctxt>,
        else_: Option<hlr::Expr<'ctxt>>,
    ) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        let cond_ty = self.check_expr(cond, None)?;
        let bool_ty = self.ctxt.tys.primitive(ty::Primitive::Boolean);

        if !self.unify(cond_ty, bool_ty) {
            return Err(TypeckError::IfConditionNotBoolean);
        }

        let then_ty = self.check_expr(then, None)?;
        let else_ty = else_
            .map(|expr| self.check_expr(expr, None))
            .transpose()?
            .unwrap_or(self.ctxt.tys.unit());

        if !self.unify(then_ty, else_ty) {
            return Err(TypeckError::IfBranchesTypeMismatch { then_ty, else_ty });
        }

        Ok(then_ty)
    }

    fn check_loop(&mut self, body: hlr::Expr<'ctxt>) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        self.check_expr(body, None)?;
        Ok(self.ctxt.tys.unit())
    }

    fn check_match(
        &mut self,
        scrutinee: hlr::Expr<'ctxt>,
        arms: &[hlr::MatchArm<'ctxt>],
    ) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        let scrutinee_ty = self.check_expr(scrutinee, None)?;
        let scrutinee_ty = self.normalize(scrutinee_ty);

        let (enum_ty, binding) = match scrutinee_ty.0 {
            ty::TyDef::Enum { .. } => (scrutinee_ty, MatchBinding::Direct),
            &ty::TyDef::Ref(inner) => {
                let inner = self.normalize(inner);
                match inner.0 {
                    ty::TyDef::Enum { .. } => (inner, MatchBinding::ByRef),
                    _ => return Err(TypeckError::NonMatchableScrutinee { ty: scrutinee_ty }),
                }
            }
            &ty::TyDef::RefMut(inner) => {
                let inner = self.normalize(inner);
                match inner.0 {
                    ty::TyDef::Enum { .. } => (inner, MatchBinding::ByRefMut),
                    _ => return Err(TypeckError::NonMatchableScrutinee { ty: scrutinee_ty }),
                }
            }
            _ => return Err(TypeckError::NonMatchableScrutinee { ty: scrutinee_ty }),
        };

        let mut result_ty: Option<ty::Ty<'ctxt>> = None;

        for arm in arms {
            let hlr::PatternKind::Variant(pattern) = arm.pattern;

            let hlr::Val::Variant(arm_enum, variant_idx, gen_args) = &pattern.variant else {
                unreachable!("match arm pattern must be Val::Variant");
            };

            let n_gen_params = arm_enum.gen_params.len();
            let arm_gen_args = self.resolve_optional_gen_args(*gen_args, n_gen_params, |actual| {
                TypeckError::EnumGenArgCountMismatch {
                    enum_: arm_enum,
                    expected: n_gen_params,
                    actual,
                }
            })?;
            let arm_enum_ty = self.ctxt.tys.inst_enum(arm_enum, &arm_gen_args).unwrap();

            if !self.unify(arm_enum_ty, enum_ty) {
                return Err(TypeckError::MatchArmWrongEnum {
                    expected: enum_ty,
                    found: arm_enum_ty,
                });
            }

            let variant_ty = self.ctxt.tys.get_enum_variant_ty(enum_ty, *variant_idx).unwrap();

            for field in pattern.fields {
                let field_ty = self
                    .ctxt
                    .tys
                    .get_struct_field_ty(variant_ty, field.field_index)
                    .unwrap();
                let binding_ty = match binding {
                    MatchBinding::Direct => field_ty,
                    MatchBinding::ByRef => self.ctxt.tys.ref_(field_ty),
                    MatchBinding::ByRefMut => self.ctxt.tys.ref_mut(field_ty),
                };
                self.typing.var_types.insert(field.binding, binding_ty);
            }

            let arm_ty = self.check_expr(arm.body, result_ty)?;

            if let Some(ty) = result_ty {
                if !self.unify(arm_ty, ty) {
                    return Err(TypeckError::MatchArmTypeMismatch {
                        expected: ty,
                        actual: arm_ty,
                    });
                }
            } else {
                result_ty = Some(arm_ty);
            }
        }

        Ok(result_ty.unwrap_or_else(|| self.ctxt.tys.unit()))
    }

    fn check_block(
        &mut self,
        stmts: hlr::StmtSlice<'ctxt>,
        trailing: hlr::Expr<'ctxt>,
        hint: Option<ty::Ty<'ctxt>>,
    ) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        for stmt in stmts {
            self.check_stmt(stmt)?;
        }
        self.check_expr(trailing, hint)
    }

    fn check_let_stmt(
        &mut self,
        var: hlr::VarId,
        _mutable: bool,
        ty: Option<hlr::TyAnnot<'ctxt>>,
        init: hlr::Expr<'ctxt>,
    ) -> TypeckResult<'ctxt, ()> {
        let annot_ty = ty.map(|ty| self.resolve_ty_annot(ty)).transpose()?;
        let init_ty = self.check_expr(init, annot_ty)?;
        if let Some(annot_ty) = annot_ty {
            if !self.unify(init_ty, annot_ty) {
                return Err(TypeckError::LetTypeMismatch {
                    expected: annot_ty,
                    actual: init_ty,
                });
            }
            self.typing.var_types.insert(var, annot_ty);
        } else {
            self.typing.var_types.insert(var, init_ty);
        }
        Ok(())
    }

    fn check_return_stmt(&mut self, expr: Option<hlr::Expr<'ctxt>>) -> TypeckResult<'ctxt, ()> {
        let expected = *self.return_ty_stack.last().unwrap();

        let actual = expr
            .map(|expr| self.check_expr(expr, Some(expected)))
            .transpose()?
            .unwrap_or(self.ctxt.tys.unit());

        if self.unify(actual, expected) {
            Ok(())
        } else {
            Err(TypeckError::ReturnExprTypeMismatch { expected, actual })
        }
    }

    pub(super) fn add_constraint_obligations(&mut self, fn_: fns::Fn<'ctxt>, subst: &ty::GenVarSubst<'ctxt>) {
        self.push_constraint_obligations(&fn_.constraints, subst, None);
    }

    pub(super) fn add_trait_mthd_constraint_obligations(
        &mut self,
        constraints: &[ty::Constraint<'ctxt>],
        subst: &ty::GenVarSubst<'ctxt>,
        base_ty: ty::Ty<'ctxt>,
    ) {
        self.push_constraint_obligations(constraints, subst, Some(base_ty));
    }

    fn push_constraint_obligations(
        &mut self,
        constraints: &[ty::Constraint<'ctxt>],
        subst: &ty::GenVarSubst<'ctxt>,
        self_ty: Option<ty::Ty<'ctxt>>,
    ) {
        for constraint in constraints {
            let subject = self.ctxt.tys.substitute(constraint.subject, subst, self_ty);

            let req = match constraint.requirement {
                ty::ConstraintRequirement::Trait(trait_inst) => {
                    let new_gen_args = self.ctxt.tys.substitute_on_slice(trait_inst.gen_args, subst, self_ty);
                    let new_inst = trait_inst.with_gen_args(new_gen_args).unwrap();
                    ty::ConstraintRequirement::Trait(new_inst)
                }
                ty::ConstraintRequirement::Callable { param_tys, return_ty } => {
                    let param_tys = self.ctxt.tys.substitute_on_slice(param_tys, subst, self_ty);
                    let return_ty = self.ctxt.tys.substitute(return_ty, subst, self_ty);
                    ty::ConstraintRequirement::Callable { param_tys, return_ty }
                }
                ty::ConstraintRequirement::AssocTyEq(eq_ty) => {
                    ty::ConstraintRequirement::AssocTyEq(self.ctxt.tys.substitute(eq_ty, subst, self_ty))
                }
            };

            self.pending_obligations.push((subject, req));
        }
    }

    fn check_pending_obligations(&mut self) -> TypeckResult<'ctxt, ()> {
        let obligations = std::mem::take(&mut self.pending_obligations);
        for (subject, req) in obligations {
            let ty = self.normalize(subject);
            if matches!(ty.0, ty::TyDef::InfVar(_)) {
                continue;
            }
            match req {
                ty::ConstraintRequirement::Trait(trait_inst) => {
                    let gen_args = self.normalize_slice(trait_inst.gen_args);
                    let trait_inst = trait_inst.with_gen_args(gen_args).unwrap();
                    if !self.ctxt.ty_implements_trait_inst(&self.constraints, ty, trait_inst) {
                        return Err(TypeckError::ConstraintNotSatisfied { ty, trait_inst });
                    }
                }
                ty::ConstraintRequirement::AssocTyEq(eq_ty) => {
                    let subject = self.normalize(subject);
                    let eq_ty = self.normalize(eq_ty);
                    if subject != eq_ty {
                        return Err(TypeckError::AssocTyEqNotSatisfied {
                            subject,
                            expected: eq_ty,
                        });
                    }
                }
                ty::ConstraintRequirement::Callable { param_tys, return_ty } => {
                    let param_tys = self.normalize_slice(param_tys);
                    let return_ty = self.normalize(return_ty);

                    let Some((actual_params, actual_return, _)) = self.ctxt.ty_is_callable(&self.constraints, ty)
                    else {
                        return Err(TypeckError::CallableConstraintNotSatisfied {
                            ty,
                            expected_param_tys: param_tys,
                            expected_return_ty: return_ty,
                        });
                    };

                    let actual_params = self.normalize_slice(actual_params);
                    let actual_return = self.normalize(actual_return);

                    if param_tys.len() != actual_params.len()
                        || param_tys
                            .iter()
                            .zip(actual_params.iter())
                            .any(|(&ty1, &ty2)| !self.unify(ty1, ty2))
                        || !self.unify(return_ty, actual_return)
                    {
                        let param_tys = self.normalize_slice(param_tys);
                        let return_ty = self.normalize(return_ty);
                        return Err(TypeckError::CallableConstraintNotSatisfied {
                            ty,
                            expected_param_tys: param_tys,
                            expected_return_ty: return_ty,
                        });
                    }
                }
            }
        }
        Ok(())
    }
}
