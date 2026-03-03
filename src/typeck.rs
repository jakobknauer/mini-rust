mod closures;
mod err;
mod mthd;
mod normalize;
mod post_check;
mod ty_annots;
mod unify;

use std::collections::HashMap;

use crate::{
    ctxt::{self, fns, language_items, traits, ty},
    hlr,
};

pub use err::*;
pub use mthd::MthdResolution;

#[derive(Default)]
pub struct HlrTyping {
    pub var_types: HashMap<hlr::VarId, ty::Ty>,
    pub expr_types: HashMap<hlr::ExprId, ty::Ty>,
    pub expr_extra: HashMap<hlr::ExprId, ExprExtra>,
}

pub enum ExprExtra {
    ValFn(fns::FnInst),
    ValMthd(MthdResolution),
    BinaryOp(fns::FnInst),
    BinaryPrim(language_items::BinaryPrimOp),
    UnaryPrim(language_items::UnaryPrimOp),
    UnaryOp(fns::FnInst),
    FieldAccess {
        derefs: usize,
        index: usize,
    },
    Closure {
        fn_inst: fns::FnInst,
        captured_vars: Vec<hlr::VarId>,
    },
}

pub fn typeck<'hlr>(ctxt: &mut ctxt::Ctxt, fn_: &'hlr hlr::Fn<'hlr>) -> TypeckResult<HlrTyping> {
    let typeck = Typeck {
        ctxt,
        fn_,
        type_vars: HashMap::new(),
        typing: Default::default(),
        closure_counter: 0,
        return_ty_stack: vec![],
        var_uses: vec![],
        created_closure_fns: vec![],
        created_closure_structs: vec![],
        pending_obligations: vec![],
    };

    typeck.check()
}

struct Typeck<'ctxt, 'hlr> {
    ctxt: &'ctxt mut ctxt::Ctxt,
    fn_: &'hlr hlr::Fn<'hlr>,

    type_vars: HashMap<ty::InfVar, ty::Ty>,
    typing: HlrTyping,
    closure_counter: usize,
    return_ty_stack: Vec<ty::Ty>,
    var_uses: Vec<hlr::VarId>,

    created_closure_fns: Vec<fns::Fn>,
    created_closure_structs: Vec<ty::Struct>,

    pending_obligations: Vec<(ty::Ty, ty::ConstraintRequirement)>,
}

impl<'ctxt, 'hlr> Typeck<'ctxt, 'hlr> {
    fn check(mut self) -> TypeckResult<HlrTyping> {
        self.check_body()?;
        self.normalize_all();
        self.check_pending_obligations()?;
        self.post_check();

        Ok(self.typing)
    }

    fn check_body(&mut self) -> TypeckResult<()> {
        let sig = self.ctxt.fns.get_sig(self.fn_.fn_).unwrap();

        for (param, param_var_id) in sig.params.iter().zip(&self.fn_.param_var_ids) {
            self.typing.var_types.insert(*param_var_id, param.ty);
        }

        let return_ty = sig.return_ty;
        self.return_ty_stack.push(return_ty);
        let body_ty = self.infer_expr_ty(self.fn_.body, Some(return_ty))?;

        if self.unify(body_ty, return_ty) {
            Ok(())
        } else {
            Err(TypeckError::ReturnTypeMismatch {
                expected: return_ty,
                actual: body_ty,
            })
        }
    }

    fn infer_expr_ty(&mut self, expr: hlr::Expr<'hlr>, hint: Option<ty::Ty>) -> TypeckResult<ty::Ty> {
        let ty = match expr.0 {
            hlr::ExprDef::Lit(lit) => self.infer_lit_ty(lit),
            hlr::ExprDef::Val(val) => self.infer_val_ty(expr.1, val),
            hlr::ExprDef::BinaryOp { left, right, operator } => {
                self.infer_binary_op_ty(expr.1, *left, *right, *operator)
            }
            hlr::ExprDef::UnaryOp { operand, operator } => self.infer_unary_op_ty(expr.1, *operand, *operator),
            hlr::ExprDef::Call { callee, args } => self.infer_call_ty(*callee, args),
            hlr::ExprDef::MthdCall {
                receiver,
                mthd_name,
                gen_args,
                args,
            } => self.infer_mthd_call_ty(expr.1, *receiver, mthd_name, *gen_args, args),
            hlr::ExprDef::Struct { constructor, fields } => self.infer_struct_expr_ty(constructor, fields),
            hlr::ExprDef::FieldAccess { base, field } => self.infer_field_access_ty(expr.1, *base, field),
            hlr::ExprDef::Tuple(exprs) => self.infer_tuple_expr_ty(exprs),
            hlr::ExprDef::Assign { target, value } => self.infer_assignment_ty(*target, *value),
            hlr::ExprDef::Deref(expr) => self.infer_deref_ty(*expr),
            hlr::ExprDef::AddrOf(expr) => self.infer_addr_of_ty(*expr),
            hlr::ExprDef::As { expr, ty } => self.infer_as_ty(*expr, ty),
            hlr::ExprDef::Closure {
                params,
                return_ty,
                body,
            } => self.infer_closure_ty(expr.1, params, *return_ty, *body, hint),
            hlr::ExprDef::If { cond, then, else_ } => self.infer_if_ty(*cond, *then, *else_),
            hlr::ExprDef::Loop { body } => self.infer_loop_ty(*body),
            hlr::ExprDef::Match { scrutinee, arms } => self.infer_match_ty(*scrutinee, arms),
            hlr::ExprDef::Block { stmts, trailing } => self.infer_block_ty(stmts, *trailing, hint),
            hlr::ExprDef::QualifiedMthd {
                ty,
                trait_,
                trait_args,
                mthd_name,
                args,
            } => self.infer_qualified_mthd_ty(expr.1, ty, *trait_, *trait_args, mthd_name, *args),
        }?;

        self.typing.expr_types.insert(expr.1, ty);

        Ok(ty)
    }

    fn check_stmt(&mut self, stmt: hlr::Stmt<'hlr>) -> TypeckResult<()> {
        match stmt {
            hlr::StmtDef::Expr(expr) => self.infer_expr_ty(*expr, None).map(|_| ()),
            hlr::StmtDef::Let { var, ty, init } => self.check_let_stmt(*var, *ty, *init),
            hlr::StmtDef::Break => Ok(()),
            hlr::StmtDef::Return(expr) => self.check_return_stmt(*expr),
        }
    }

    fn infer_lit_ty(&mut self, lit: &hlr::Lit) -> TypeckResult<ty::Ty> {
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

    fn infer_val_ty(&mut self, expr_id: hlr::ExprId, val: &hlr::Val<'hlr>) -> TypeckResult<ty::Ty> {
        match val {
            &hlr::Val::Var(var_id) => {
                self.var_uses.push(var_id);
                self.typing
                    .var_types
                    .get(&var_id)
                    .copied()
                    .ok_or(TypeckError::VarTypeNotSet { var_id, expr_id })
            }
            &hlr::Val::Fn(fn_, args) => self.infer_fn_ty(expr_id, fn_, args),
            hlr::Val::Struct(..) => unreachable!("raw struct values are not supported"),
            hlr::Val::Variant(..) => unreachable!("raw variant values are not supported"),
            hlr::Val::Mthd(base_ty, mthd_name, gen_args) => self.infer_mthd_ty(expr_id, base_ty, mthd_name, *gen_args),
        }
    }

    fn infer_fn_ty(
        &mut self,
        expr_id: hlr::ExprId,
        fn_: fns::Fn,
        args: Option<hlr::TyAnnotSlice<'hlr>>,
    ) -> TypeckResult<ty::Ty> {
        let gen_arg_count = self.ctxt.fns.get_sig(fn_).unwrap().gen_params.len();

        let gen_args =
            self.resolve_optional_gen_args(args, gen_arg_count, |actual| TypeckError::FnGenArgCountMismatch {
                fn_,
                expected: gen_arg_count,
                actual,
            })?;

        let signature = self.ctxt.fns.get_sig(fn_).unwrap();
        let gen_params = signature.gen_params.clone();
        let subst = ty::GenVarSubst::new(&gen_params, gen_args.clone()).unwrap();
        let param_tys: Vec<_> = signature.params.iter().map(|param| param.ty).collect();
        let return_ty = signature.return_ty;
        let var_args = signature.var_args;
        let _ = signature;

        self.add_constraint_obligations(&gen_params, &gen_args, &subst);

        let fn_ty = self.ctxt.tys.fn_(&param_tys, return_ty, var_args);

        let gen_args = self.ctxt.tys.ty_slice(&gen_args);
        let empty = self.ctxt.tys.ty_slice(&[]);
        self.typing.expr_extra.insert(
            expr_id,
            ExprExtra::ValFn(fns::FnInst {
                fn_,
                gen_args,
                env_gen_args: empty,
            }),
        );

        Ok(self.ctxt.tys.substitute_gen_vars(fn_ty, &subst))
    }

    fn infer_mthd_ty(
        &mut self,
        expr_id: hlr::ExprId,
        base_ty: hlr::TyAnnot<'hlr>,
        mthd_name: &str,
        gen_args: Option<hlr::TyAnnotSlice<'hlr>>,
    ) -> TypeckResult<ty::Ty> {
        let base_ty = self.resolve_ty_annot(base_ty)?;
        let found = self.resolve_mthd(base_ty, mthd_name, false)?;
        let resolution = self.instantiate_mthd(found, base_ty, mthd_name, gen_args)?;
        let fn_ty = self.fn_ty_of_mthd_resolution(&resolution);
        self.typing.expr_extra.insert(expr_id, ExprExtra::ValMthd(resolution));
        Ok(fn_ty)
    }

    fn infer_qualified_mthd_ty(
        &mut self,
        expr_id: hlr::ExprId,
        ty: hlr::TyAnnot<'hlr>,
        trait_: Option<traits::Trait>,
        trait_args: Option<hlr::TyAnnotSlice<'hlr>>,
        mthd_name: &str,
        args: Option<hlr::TyAnnotSlice<'hlr>>,
    ) -> TypeckResult<ty::Ty> {
        let base_ty = self.resolve_ty_annot(ty)?;

        let found = match trait_ {
            None => self.resolve_mthd(base_ty, mthd_name, false)?,
            Some(trait_) => {
                let n_trait_gen_params = self.ctxt.traits.get_trait_def(trait_).gen_params.len();
                let trait_gen_args = self.resolve_optional_gen_args(trait_args, n_trait_gen_params, |actual| {
                    TypeckError::TraitGenArgCountMismatch {
                        trait_,
                        expected: n_trait_gen_params,
                        actual,
                    }
                })?;
                let trait_gen_args = self.ctxt.tys.ty_slice(&trait_gen_args);
                let trait_inst = traits::TraitInst {
                    trait_,
                    gen_args: trait_gen_args,
                };

                let mthd_idx = self
                    .ctxt
                    .traits
                    .resolve_trait_method(trait_, mthd_name)
                    .ok_or_else(|| TypeckError::MthdResolutionFailed {
                        base_ty,
                        mthd_name: mthd_name.to_string(),
                    })?;

                mthd::FoundMthd::Trait { trait_inst, mthd_idx }
            }
        };

        let resolution = self.instantiate_mthd(found, base_ty, mthd_name, args)?;
        let fn_ty = self.fn_ty_of_mthd_resolution(&resolution);
        self.typing.expr_extra.insert(expr_id, ExprExtra::ValMthd(resolution));
        Ok(fn_ty)
    }

    fn infer_binary_op_ty(
        &mut self,
        expr_id: hlr::ExprId,
        left: hlr::Expr<'hlr>,
        right: hlr::Expr<'hlr>,
        operator: hlr::BinaryOperator,
    ) -> TypeckResult<ty::Ty> {
        let left_ty = self.infer_expr_ty(left, None)?;
        let left_ty = self.normalize(left_ty);
        let right_ty = self.infer_expr_ty(right, None)?;
        let right_ty = self.normalize(right_ty);

        let i32_ty = self.ctxt.tys.primitive(ty::Primitive::Integer32);
        let bool_ty = self.ctxt.tys.primitive(ty::Primitive::Boolean);
        let unit_ty = self.ctxt.tys.unit();

        use hlr::BinaryOperator::*;

        // LogicalAnd/Or are lowered as short-circuit branches, not function calls — no ExprExtra needed
        if matches!(operator, LogicalAnd | LogicalOr) {
            if left_ty == bool_ty && right_ty == bool_ty {
                return Ok(bool_ty);
            } else {
                return Err(TypeckError::BinaryOpTypeMismatch {
                    operator,
                    left_ty,
                    right_ty,
                });
            }
        }

        use language_items::BinaryPrimOp::*;

        let i32 = left_ty == i32_ty && right_ty == i32_ty;
        let bool = left_ty == bool_ty && right_ty == bool_ty;
        let unit = left_ty == unit_ty && right_ty == unit_ty;

        let (prim, result_ty) = match operator {
            Add if i32 => (AddI32, i32_ty),
            Subtract if i32 => (SubI32, i32_ty),
            Multiply if i32 => (MulI32, i32_ty),
            Divide if i32 => (DivI32, i32_ty),
            Remainder if i32 => (RemI32, i32_ty),
            Equal if i32 => (EqI32, bool_ty),
            Equal if bool => (EqBool, bool_ty),
            Equal if unit => (EqUnit, bool_ty),
            NotEqual if i32 => (NeI32, bool_ty),
            NotEqual if bool => (NeBool, bool_ty),
            NotEqual if unit => (NeUnit, bool_ty),
            BitOr if bool => (BitOrBool, bool_ty),
            BitAnd if bool => (BitAndBool, bool_ty),
            LessThan if i32 => (LtI32, bool_ty),
            GreaterThan if i32 => (GtI32, bool_ty),
            LessThanOrEqual if i32 => (LeI32, bool_ty),
            GreaterThanOrEqual if i32 => (GeI32, bool_ty),
            _ => {
                return Err(TypeckError::BinaryOpTypeMismatch {
                    operator,
                    left_ty,
                    right_ty,
                });
            }
        };

        self.typing.expr_extra.insert(expr_id, ExprExtra::BinaryPrim(prim));
        Ok(result_ty)
    }

    fn infer_unary_op_ty(
        &mut self,
        expr_id: hlr::ExprId,
        operand: hlr::Expr<'hlr>,
        operator: hlr::UnaryOperator,
    ) -> TypeckResult<ty::Ty> {
        let operand_ty = self.infer_expr_ty(operand, None)?;

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

    fn infer_field_access_ty(
        &mut self,
        expr_id: hlr::ExprId,
        base: hlr::Expr<'hlr>,
        field: &hlr::FieldSpec,
    ) -> TypeckResult<ty::Ty> {
        let base_ty = self.infer_expr_ty(base, None)?;
        let mut base_ty = self.normalize(base_ty);

        let mut num_derefs = 0;
        while let &ty::TyDef::Ref(inner) | &ty::TyDef::Ptr(inner) = self.ctxt.tys.get_ty_def(base_ty) {
            base_ty = self.normalize(inner);
            num_derefs += 1;
        }

        match field {
            hlr::FieldSpec::Name(name) => {
                let ty::TyDef::Struct { .. } = self.ctxt.tys.get_ty_def(base_ty) else {
                    return Err(TypeckError::NamedFieldAccessOnNonStruct { ty: base_ty });
                };
                let index = self
                    .ctxt
                    .tys
                    .get_struct_field_index_by_name(base_ty, name)
                    .map_err(|_| TypeckError::StructFieldNotFound {
                        struct_ty: base_ty,
                        field: name.clone(),
                    })?;
                self.typing.expr_extra.insert(
                    expr_id,
                    ExprExtra::FieldAccess {
                        derefs: num_derefs,
                        index,
                    },
                );
                Ok(self.ctxt.tys.get_struct_field_ty(base_ty, index).unwrap())
            }
            hlr::FieldSpec::Index(index) => match self.ctxt.tys.get_ty_def(base_ty) {
                ty::TyDef::Tuple(_) => {
                    self.typing.expr_extra.insert(
                        expr_id,
                        ExprExtra::FieldAccess {
                            derefs: num_derefs,
                            index: *index,
                        },
                    );
                    Ok(self.ctxt.tys.get_tuple_field_tys(base_ty).unwrap()[*index])
                }
                _ => Err(TypeckError::IndexedFieldAccessOnNonTuple { ty: base_ty }),
            },
        }
    }

    fn infer_struct_expr_ty(
        &mut self,
        constructor: &hlr::Val<'hlr>,
        fields: hlr::StructFields<'hlr>,
    ) -> TypeckResult<ty::Ty> {
        let (return_ty, fields_ty) = match constructor {
            hlr::Val::Struct(struct_, gen_args) => {
                let n_gen_params = self.ctxt.tys.get_struct_def(*struct_).unwrap().gen_params.len();
                let gen_args = self.resolve_optional_gen_args(*gen_args, n_gen_params, |actual| {
                    TypeckError::StructGenArgCountMismatch {
                        struct_: *struct_,
                        expected: n_gen_params,
                        actual,
                    }
                })?;
                let ty = self.ctxt.tys.inst_struct(*struct_, &gen_args).unwrap();
                (ty, ty)
            }
            hlr::Val::Variant(enum_, variant_idx, gen_args) => {
                let n_gen_params = self.ctxt.tys.get_enum_def(*enum_).unwrap().gen_params.len();
                let gen_args = self.resolve_optional_gen_args(*gen_args, n_gen_params, |actual| {
                    TypeckError::EnumGenArgCountMismatch {
                        enum_: *enum_,
                        expected: n_gen_params,
                        actual,
                    }
                })?;
                let enum_ty = self.ctxt.tys.inst_enum(*enum_, &gen_args).unwrap();
                let variant_ty = self.ctxt.tys.get_enum_variant_ty(enum_ty, *variant_idx).unwrap();
                (enum_ty, variant_ty)
            }
            _ => unreachable!("struct expression constructor must be Val::Struct or Val::Variant"),
        };

        for (field_spec, field_expr) in fields {
            let field_idx = match field_spec {
                hlr::FieldSpec::Name(name) => {
                    self.ctxt
                        .tys
                        .get_struct_field_index_by_name(fields_ty, name)
                        .map_err(|_| TypeckError::StructFieldNotFound {
                            struct_ty: fields_ty,
                            field: name.clone(),
                        })?
                }
                hlr::FieldSpec::Index(idx) => *idx,
            };
            let field_ty = self.ctxt.tys.get_struct_field_ty(fields_ty, field_idx).unwrap();
            let expr_ty = self.infer_expr_ty(*field_expr, Some(field_ty))?;
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

    fn infer_mthd_call_ty(
        &mut self,
        expr_id: hlr::ExprId,
        receiver: hlr::Expr<'hlr>,
        mthd_name: &str,
        gen_args: Option<hlr::TyAnnotSlice<'hlr>>,
        args: hlr::ExprSlice<'hlr>,
    ) -> TypeckResult<ty::Ty> {
        let receiver_ty = self.infer_expr_ty(receiver, None)?;
        let receiver_ty = self.normalize(receiver_ty);

        let found = self.resolve_mthd(receiver_ty, mthd_name, true)?;
        let resolution = self.instantiate_mthd(found, receiver_ty, mthd_name, gen_args)?;
        let fn_ty = self.fn_ty_of_mthd_resolution(&resolution);
        self.typing.expr_extra.insert(expr_id, ExprExtra::ValMthd(resolution));

        let Some((param_tys, return_ty, var_args)) = self.ctxt.ty_is_callable(fn_ty) else {
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
            let arg_ty = self.infer_expr_ty(*arg, param_hint)?;
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

    fn infer_call_ty(&mut self, callee: hlr::Expr<'hlr>, args: hlr::ExprSlice<'hlr>) -> TypeckResult<ty::Ty> {
        let callee_ty = self.infer_expr_ty(callee, None)?;
        let callee_ty = self.normalize(callee_ty);

        let Some((param_tys, return_ty, var_args)) = self.ctxt.ty_is_callable(callee_ty) else {
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
            let arg_ty = self.infer_expr_ty(*arg, param_hint)?;
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

    fn infer_tuple_expr_ty(&mut self, exprs: hlr::ExprSlice<'hlr>) -> TypeckResult<ty::Ty> {
        let expr_tys: Vec<_> = exprs
            .iter()
            .map(|expr| self.infer_expr_ty(*expr, None))
            .collect::<TypeckResult<_>>()?;

        let tuple_ty = self.ctxt.tys.tuple(&expr_tys);
        Ok(tuple_ty)
    }

    fn infer_assignment_ty(&mut self, target: hlr::Expr<'hlr>, value: hlr::Expr<'hlr>) -> TypeckResult<ty::Ty> {
        let target_ty = self.infer_expr_ty(target, None)?;
        let value_ty = self.infer_expr_ty(value, None)?;
        if !self.unify(target_ty, value_ty) {
            return Err(TypeckError::AssignmentTypeMismatch {
                expected: target_ty,
                actual: value_ty,
            });
        }
        Ok(self.ctxt.tys.unit())
    }

    fn infer_deref_ty(&mut self, expr: hlr::Expr<'hlr>) -> TypeckResult<ty::Ty> {
        let expr_ty = self.infer_expr_ty(expr, None)?;
        let expr_ty = self.normalize(expr_ty);

        match self.ctxt.tys.get_ty_def(expr_ty) {
            &ty::TyDef::Ref(base_ty) | &ty::TyDef::Ptr(base_ty) => {
                if self.ctxt.tys.is_c_void_ty(base_ty) {
                    Err(TypeckError::DereferenceOfCVoid { ty: expr_ty })
                } else {
                    Ok(base_ty)
                }
            }
            _ => Err(TypeckError::DereferenceOfNonRef { ty: expr_ty }),
        }
    }

    fn infer_addr_of_ty(&mut self, expr: hlr::Expr<'hlr>) -> TypeckResult<ty::Ty> {
        let expr_ty = self.infer_expr_ty(expr, None)?;
        Ok(self.ctxt.tys.ref_(expr_ty))
    }

    fn infer_as_ty(&mut self, expr: hlr::Expr<'hlr>, target_ty: hlr::TyAnnot<'hlr>) -> TypeckResult<ty::Ty> {
        let expr_ty = self.infer_expr_ty(expr, None)?;
        let expr_ty = self.normalize(expr_ty);

        let target_ty = self.resolve_ty_annot(target_ty)?;
        let target_ty = self.normalize(target_ty);

        let expr_ty_def = self.ctxt.tys.get_ty_def(expr_ty);
        let target_ty_def = self.ctxt.tys.get_ty_def(target_ty);

        match (expr_ty_def, target_ty_def) {
            (ty::TyDef::Ptr(_), ty::TyDef::Ptr(_)) => Ok(target_ty),
            (&ty::TyDef::Ref(op_base_ty), &ty::TyDef::Ptr(target_base_ty)) => {
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

    fn infer_if_ty(
        &mut self,
        cond: hlr::Expr<'hlr>,
        then: hlr::Expr<'hlr>,
        else_: Option<hlr::Expr<'hlr>>,
    ) -> TypeckResult<ty::Ty> {
        let cond_ty = self.infer_expr_ty(cond, None)?;
        let bool_ty = self.ctxt.tys.primitive(ty::Primitive::Boolean);

        if !self.unify(cond_ty, bool_ty) {
            return Err(TypeckError::IfConditionNotBoolean);
        }

        let then_ty = self.infer_expr_ty(then, None)?;
        let else_ty = else_
            .map(|expr| self.infer_expr_ty(expr, None))
            .transpose()?
            .unwrap_or(self.ctxt.tys.unit());

        if !self.unify(then_ty, else_ty) {
            return Err(TypeckError::IfBranchesTypeMismatch { then_ty, else_ty });
        }

        Ok(then_ty)
    }

    fn infer_loop_ty(&mut self, body: hlr::Expr<'hlr>) -> TypeckResult<ty::Ty> {
        self.infer_expr_ty(body, None)?;
        Ok(self.ctxt.tys.unit())
    }

    fn infer_match_ty(&mut self, scrutinee: hlr::Expr<'hlr>, arms: &[hlr::MatchArm<'hlr>]) -> TypeckResult<ty::Ty> {
        let scrutinee_ty = self.infer_expr_ty(scrutinee, None)?;
        let scrutinee_ty = self.normalize(scrutinee_ty);

        let (enum_ty, by_ref) = match self.ctxt.tys.get_ty_def(scrutinee_ty) {
            ty::TyDef::Enum { .. } => (scrutinee_ty, false),
            &ty::TyDef::Ref(inner) => {
                let inner = self.normalize(inner);
                match self.ctxt.tys.get_ty_def(inner) {
                    ty::TyDef::Enum { .. } => (inner, true),
                    _ => return Err(TypeckError::NonMatchableScrutinee { ty: scrutinee_ty }),
                }
            }
            _ => return Err(TypeckError::NonMatchableScrutinee { ty: scrutinee_ty }),
        };

        let mut result_ty: Option<ty::Ty> = None;

        for arm in arms {
            let hlr::Val::Variant(arm_enum, variant_idx, gen_args) = &arm.pattern.variant else {
                unreachable!("match arm pattern must be Val::Variant");
            };

            let n_gen_params = self.ctxt.tys.get_enum_def(*arm_enum).unwrap().gen_params.len();
            let arm_gen_args = self.resolve_optional_gen_args(*gen_args, n_gen_params, |actual| {
                TypeckError::EnumGenArgCountMismatch {
                    enum_: *arm_enum,
                    expected: n_gen_params,
                    actual,
                }
            })?;
            let arm_enum_ty = self.ctxt.tys.inst_enum(*arm_enum, &arm_gen_args).unwrap();

            if !self.unify(arm_enum_ty, enum_ty) {
                return Err(TypeckError::MatchArmWrongEnum {
                    expected: enum_ty,
                    found: arm_enum_ty,
                });
            }

            let variant_ty = self.ctxt.tys.get_enum_variant_ty(enum_ty, *variant_idx).unwrap();

            for field in arm.pattern.fields {
                let field_ty = self
                    .ctxt
                    .tys
                    .get_struct_field_ty(variant_ty, field.field_index)
                    .unwrap();
                let binding_ty = if by_ref { self.ctxt.tys.ref_(field_ty) } else { field_ty };
                self.typing.var_types.insert(field.binding, binding_ty);
            }

            let arm_ty = self.infer_expr_ty(arm.body, result_ty)?;

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

    fn infer_block_ty(
        &mut self,
        stmts: hlr::StmtSlice<'hlr>,
        trailing: hlr::Expr<'hlr>,
        hint: Option<ty::Ty>,
    ) -> TypeckResult<ty::Ty> {
        for stmt in stmts {
            self.check_stmt(stmt)?;
        }
        self.infer_expr_ty(trailing, hint)
    }

    fn check_let_stmt(
        &mut self,
        var: hlr::VarId,
        ty: Option<hlr::TyAnnot<'hlr>>,
        init: hlr::Expr<'hlr>,
    ) -> TypeckResult<()> {
        let annot_ty = ty.map(|ty| self.resolve_ty_annot(ty)).transpose()?;
        let init_ty = self.infer_expr_ty(init, annot_ty)?;
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

    fn check_return_stmt(&mut self, expr: Option<hlr::Expr<'hlr>>) -> TypeckResult<()> {
        let expected = *self.return_ty_stack.last().unwrap();

        let actual = expr
            .map(|expr| self.infer_expr_ty(expr, Some(expected)))
            .transpose()?
            .unwrap_or(self.ctxt.tys.unit());

        if self.unify(actual, expected) {
            Ok(())
        } else {
            Err(TypeckError::ReturnExprTypeMismatch { expected, actual })
        }
    }

    pub(super) fn add_constraint_obligations(
        &mut self,
        gen_params: &[ty::GenVar],
        gen_args: &[ty::Ty],
        subst: &ty::GenVarSubst,
    ) {
        for (&gen_param, &gen_arg) in gen_params.iter().zip(gen_args) {
            let reqs: Vec<_> = self.ctxt.tys.get_requirements_for(gen_param).cloned().collect();
            for req in reqs {
                match &req {
                    ty::ConstraintRequirement::Trait(trait_inst) => {
                        let new_gen_args = self.ctxt.tys.substitute_gen_vars_on_slice(trait_inst.gen_args, subst);
                        let new_inst = traits::TraitInst {
                            trait_: trait_inst.trait_,
                            gen_args: new_gen_args,
                        };
                        self.pending_obligations
                            .push((gen_arg, ty::ConstraintRequirement::Trait(new_inst)));
                    }
                    ty::ConstraintRequirement::Callable { param_tys, return_ty } => {
                        let param_tys = param_tys
                            .iter()
                            .map(|&t| self.ctxt.tys.substitute_gen_vars(t, subst))
                            .collect();
                        let return_ty = self.ctxt.tys.substitute_gen_vars(*return_ty, subst);
                        self.pending_obligations
                            .push((gen_arg, ty::ConstraintRequirement::Callable { param_tys, return_ty }));
                    }
                }
            }
        }
    }

    fn check_pending_obligations(&mut self) -> TypeckResult<()> {
        let obligations = std::mem::take(&mut self.pending_obligations);
        for (ty, req) in obligations {
            let ty = self.normalize(ty);
            if matches!(self.ctxt.tys.get_ty_def(ty), ty::TyDef::InfVar(_)) {
                continue;
            }
            match req {
                ty::ConstraintRequirement::Trait(trait_inst) => {
                    let gen_args: Vec<_> = self.ctxt.tys.get_ty_slice(trait_inst.gen_args).to_vec();
                    let gen_args: Vec<_> = gen_args.iter().map(|&t| self.normalize(t)).collect();
                    let gen_args = self.ctxt.tys.ty_slice(&gen_args);
                    let trait_inst = traits::TraitInst {
                        trait_: trait_inst.trait_,
                        gen_args,
                    };
                    if !self.ctxt.ty_implements_trait_inst(ty, trait_inst) {
                        return Err(TypeckError::ConstraintNotSatisfied { ty, trait_inst });
                    }
                }
                ty::ConstraintRequirement::Callable { param_tys, return_ty } => {
                    let param_tys: Vec<_> = param_tys.iter().map(|&t| self.normalize(t)).collect();
                    let return_ty = self.normalize(return_ty);
                    let Some((actual_params, actual_return, _)) = self.ctxt.ty_is_callable(ty) else {
                        return Err(TypeckError::CallableConstraintNotSatisfied {
                            ty,
                            expected_param_tys: param_tys,
                            expected_return_ty: return_ty,
                        });
                    };
                    let actual_params: Vec<_> = actual_params.iter().map(|&t| self.normalize(t)).collect();
                    let actual_return = self.normalize(actual_return);
                    let types_match = param_tys.len() == actual_params.len()
                        && param_tys.iter().zip(&actual_params).all(|(&a, &b)| a == b)
                        && return_ty == actual_return;
                    if !types_match {
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
