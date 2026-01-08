pub mod opt;

mod closure_util;
mod err;
mod match_util;
mod ops;

#[macro_use]
mod macros;

use std::collections::HashMap;

use crate::{
    ctxt::{self, fns, mlr, ty},
    hlr,
    typechecker::{self, MethodResolution},
    util::mlr_builder::MlrBuilder,
};

pub use err::{H2MError, H2MResult};

pub fn hlr_to_mlr(ctxt: &mut ctxt::Ctxt, hlr_body: &hlr::Block, target_fn: fns::Fn) -> H2MResult<()> {
    hlr_to_mlr_with_external_scope(ctxt, hlr_body, target_fn, HashMap::new(), None)?;
    Ok(())
}

pub fn hlr_to_mlr_with_external_scope(
    ctxt: &mut ctxt::Ctxt,
    hlr_body: &hlr::Block,
    target_fn: fns::Fn,
    external_scope: HashMap<String, mlr::Loc>,
    captures_ty: Option<ty::Ty>,
) -> H2MResult<HashMap<mlr::Loc, usize>> {
    let (mlr, captured_values) = H2M::new(target_fn, ctxt, external_scope, captures_ty).build(hlr_body)?;
    ctxt.fns.add_fn_def(target_fn, mlr);
    Ok(captured_values)
}

struct H2M<'a> {
    builder: MlrBuilder<'a>,
    closure_counter: u32,

    /// The available local variables in scope surrounding this function (i.e. only relevant for closures)
    outer_scope: HashMap<String, mlr::Loc>,
    captures_struct_ty: Option<ty::Ty>,
    captures_place: Option<mlr::Place>,
    /// Map the original (outer) location of a captured variable to its index in the captures struct
    captured_values: HashMap<mlr::Loc, usize>,
}

impl<'a> H2M<'a> {
    pub fn new(
        target_fn: fns::Fn,
        ctxt: &'a mut ctxt::Ctxt,
        external_scope: HashMap<String, mlr::Loc>,
        captures_ty: Option<ty::Ty>,
    ) -> Self {
        Self {
            builder: MlrBuilder::new(target_fn, ctxt),

            closure_counter: 0,

            outer_scope: external_scope,
            captures_struct_ty: captures_ty,
            captures_place: None,
            captured_values: HashMap::new(),
        }
    }

    pub fn build(mut self, body: &'a hlr::Block) -> H2MResult<(fns::FnMlr, HashMap<mlr::Loc, usize>)> {
        let signature = self.builder.get_signature();
        if signature.var_args {
            return Err(H2MError::VarArgsNotSupported);
        }

        let params = signature.params.clone();
        let mut param_locs = Vec::new();

        self.builder.push_scope();
        for fns::FnParam { kind: name, ty } in params {
            let loc = self.builder.insert_typed_loc(ty)?;
            param_locs.push(loc);

            match name {
                fns::FnParamKind::Regular(name) => self.builder.add_binding(&name, loc),
                fns::FnParamKind::Self_ | fns::FnParamKind::SelfByRef => self.builder.register_receiver_loc(loc),
            }
        }

        if let Some(captures_ty) = self.captures_struct_ty {
            let first_param_loc = param_locs[0];
            let first_param_ty = self.mlr().get_loc_ty(first_param_loc);

            assert!(
                self.ctxt().tys.tys_eq(first_param_ty, captures_ty),
                "In a closure the first parameter must be the captures struct"
            );

            let first_param_place = self.builder.insert_loc_place(first_param_loc)?;
            self.captures_place = Some(first_param_place);
        }

        self.builder.start_new_block();

        let return_val = self.build_block(body, None)?;
        self.builder.insert_return_stmt(return_val)?;

        let body = self.builder.release_current_block();

        let mlr = fns::FnMlr { body, param_locs };
        Ok((mlr, self.captured_values))
    }

    fn typechecker(&mut self) -> typechecker::Typechecker<'_> {
        self.builder.typechecker()
    }

    fn ctxt(&mut self) -> &mut ctxt::Ctxt {
        self.builder.ctxt()
    }

    fn tys(&mut self) -> &mut ctxt::TyReg {
        self.builder.tys()
    }

    fn mlr(&mut self) -> &mut mlr::Mlr {
        self.builder.mlr()
    }

    fn fns(&mut self) -> &mut ctxt::FnReg {
        self.builder.fns()
    }

    fn traits(&mut self) -> &mut ctxt::TraitReg {
        self.builder.traits()
    }

    /// Build an HLR block by inserting the statements into the current MLR block
    /// and returning the value of the block's return expression,
    /// all while in a new scope.
    ///
    /// This method does not start or end a new MLR block; but it does push and pop a new scope.
    fn build_block(&mut self, block: &hlr::Block, expected: Option<ty::Ty>) -> H2MResult<mlr::Val> {
        self.builder.push_scope();

        for stmt in &block.stmts {
            self.build_stmt(stmt)?;
        }

        let output = match &block.return_expr {
            Some(expr) => self.lower_to_val(expr, expected)?,
            None => {
                let unit = self.builder.insert_unit_op()?;
                self.builder.insert_use_val(unit)?
            }
        };

        self.builder.pop_scope();

        Ok(output)
    }

    fn lower_to_val(&mut self, expr: &hlr::Expr, expected: Option<ty::Ty>) -> H2MResult<mlr::Val> {
        use hlr::Expr::*;

        match expr {
            Lit(..) | Ident { .. } | FieldAccess { .. } | Deref { .. } | Self_ => {
                let op = self.lower_to_op(expr, expected)?;
                self.builder.insert_use_val(op)
            }
            Tuple(exprs) => self.build_tuple_val(exprs),
            BinaryOp { left, operator, right } => self.build_binary_op(left, operator, right),
            Assign { target, value } => self.build_assignment(target, value),
            Call { callee, arguments } => self.build_call(callee, arguments),
            MethodCall { obj, method, arguments } => self.build_method_call(obj, method, arguments),
            Struct { name, fields } => self.build_struct_or_enum_val(name, fields),
            If {
                condition,
                then_block,
                else_block,
            } => self.build_if(condition, then_block, else_block.as_ref(), expected),
            Loop { body } => self.build_loop(body),
            Block(block) => self.build_block(block, expected),
            Match { scrutinee, arms } => self.build_match_expr(scrutinee, arms, expected),
            AddrOf { base } => self.build_addr_of_val(base),
            As { expr, target_ty } => self.build_as_expr(expr, target_ty),
            Closure {
                params,
                body,
                return_ty,
            } => self.build_closure(params, return_ty, body, expected),
        }
    }

    fn lower_to_place(&mut self, expr: &hlr::Expr) -> H2MResult<mlr::Place> {
        use hlr::Expr::*;

        match expr {
            Ident(ident) if ident.gen_args.is_empty() => self.lower_ident_to_place(&ident.ident),
            FieldAccess { obj, field } => self.lower_field_access_to_place(obj, field),
            Deref { base } => self.lower_deref_to_place(base),
            Self_ => {
                let loc = self.builder.get_receiver_loc().unwrap();
                self.builder.insert_loc_place(loc)
            }
            _ => Ok(assign_to_fresh_alloc!(self, self.lower_to_val(expr, None)?)),
        }
    }

    fn lower_to_op(&mut self, expr: &hlr::Expr, expected: Option<ty::Ty>) -> H2MResult<mlr::Op> {
        use hlr::Expr::*;

        match expr {
            Lit(lit) => self.build_literal(lit),
            Ident(hlr::Ident { ident, gen_args }) => self.lower_ident_to_op(ident, gen_args),
            FieldAccess { .. } | Deref { .. } | Self_ => {
                let place = self.lower_to_place(expr)?;
                self.builder.insert_copy_op(place)
            }
            _ => {
                let val_place = assign_to_fresh_alloc!(self, self.lower_to_val(expr, expected)?);
                self.builder.insert_copy_op(val_place)
            }
        }
    }

    fn build_literal(&mut self, lit: &hlr::Lit) -> H2MResult<mlr::Op> {
        use hlr::Lit::*;

        match *lit {
            Int(n) => self.builder.insert_int_op(n),
            Bool(b) => self.builder.insert_bool_op(b),
            CChar(c) => self.builder.insert_c_char_op(c),
            CString(ref s) => self.builder.insert_c_string_op(s),
        }
    }

    fn lower_ident_to_op(&mut self, ident: &str, gen_args: &[hlr::TyAnnot]) -> H2MResult<mlr::Op> {
        if gen_args.is_empty()
            && let Some(place) = self.resolve_name_to_place(ident)
        {
            self.builder.insert_copy_op(place)
        } else if let Some(fn_) = self.fns().get_fn_by_name(ident) {
            let n_gen_params = self.fns().get_sig(fn_).unwrap().gen_params.len();
            let gen_args = self.resolve_gen_args_or_insert_fresh_variables(gen_args, n_gen_params)?;

            self.builder.insert_gen_fn_op(fn_, gen_args, Vec::new())
        } else {
            Err(H2MError::UnresolvableSymbol {
                name: ident.to_string(),
            })
        }
    }

    fn build_binary_op(
        &mut self,
        left: &hlr::Expr,
        operator: &hlr::BinaryOperator,
        right: &hlr::Expr,
    ) -> H2MResult<mlr::Val> {
        let left_op = self.lower_to_op(left, None)?;
        let right_op = self.lower_to_op(right, None)?;

        let op = {
            let left_ty = self.mlr().get_op_ty(left_op);
            let right_ty = self.mlr().get_op_ty(right_op);
            let fn_ = self.resolve_operator(operator, (left_ty, right_ty))?;
            self.builder.insert_fn_op(fn_)?
        };

        self.builder.insert_call_val(op, vec![left_op, right_op])
    }

    fn build_assignment(&mut self, target: &hlr::Expr, value: &hlr::Expr) -> H2MResult<mlr::Val> {
        let loc = self.lower_to_place(target)?;
        let value = self.lower_to_val(value, None)?;
        self.builder.insert_assign_stmt(loc, value)?;

        let output = self.builder.insert_unit_op()?;
        self.builder.insert_use_val(output)
    }

    fn build_call(&mut self, callee: &hlr::Expr, args: &[hlr::Expr]) -> H2MResult<mlr::Val> {
        let callee = self.lower_to_op(callee, None)?;
        let callee_ty = self.mlr().get_op_ty(callee);

        let param_tys = self
            .ctxt()
            .ty_is_callable(callee_ty)
            .map(|(param_tys, ..)| param_tys)
            .unwrap_or_default();

        let args: Vec<_> = args
            .iter()
            .enumerate()
            .map(|(idx, arg)| {
                let expected = param_tys.get(idx).cloned(); // possibly None
                self.lower_to_op(arg, expected)
            })
            .collect::<H2MResult<_>>()?;

        self.builder.insert_call_val(callee, args)
    }

    fn build_method_call(&mut self, obj: &hlr::Expr, method: &hlr::Ident, args: &[hlr::Expr]) -> H2MResult<mlr::Val> {
        let base_place = self.lower_to_place(obj)?;
        let base_ty = self.mlr().get_place_ty(base_place);

        let (callee, by_ref) = match self.typechecker().resolve_method(base_ty, &method.ident)? {
            MethodResolution::Inherent { fn_, env_gen_args } => {
                let sig = self.fns().get_sig(fn_).unwrap();
                let by_ref = sig.params[0].kind == fns::FnParamKind::SelfByRef;

                let n_gen_params = sig.gen_params.len();
                let gen_args = self.resolve_gen_args_or_insert_fresh_variables(&method.gen_args, n_gen_params)?;

                let fn_inst = fns::FnInst {
                    fn_,
                    gen_args,
                    env_gen_args,
                };

                (self.builder.insert_fn_inst_op(fn_inst)?, by_ref)
            }
            MethodResolution::Trait { trait_inst, method_idx } => {
                let sig = self.traits().get_trait_method_sig(trait_inst.trait_, method_idx);
                let by_ref = sig.params[0].kind == fns::FnParamKind::SelfByRef;

                let n_gen_params = sig.gen_params.len();
                let gen_args = self.resolve_gen_args_or_insert_fresh_variables(&method.gen_args, n_gen_params)?;

                let trait_method = fns::TraitMethod {
                    trait_inst,
                    method_idx,
                    impl_ty: base_ty,
                    gen_args,
                };

                (self.builder.insert_trait_method_op(trait_method)?, by_ref)
            }
        };

        let base = if by_ref {
            let base_addr = self.builder.insert_addr_of_val(base_place)?;
            let base_addr_place = assign_to_fresh_alloc!(self, base_addr);
            self.builder.insert_copy_op(base_addr_place)?
        } else {
            self.builder.insert_copy_op(base_place)?
        };

        let callee_ty = self.mlr().get_op_ty(callee);

        let param_tys = self
            .ctxt()
            .ty_is_callable(callee_ty)
            .map(|(param_tys, ..)| param_tys)
            .unwrap_or_default();

        let args = std::iter::once(Ok(base))
            .chain(args.iter().enumerate().map(|(idx, arg)| {
                let expected = param_tys.get(idx + 1).cloned(); // possibly None, skip self param
                self.lower_to_op(arg, expected)
            }))
            .collect::<H2MResult<_>>()?;

        self.builder.insert_call_val(callee, args)
    }

    fn build_if(
        &mut self,
        condition: &hlr::Expr,
        then_block: &hlr::Block,
        else_block: Option<&hlr::Block>,
        expected: Option<ty::Ty>,
    ) -> H2MResult<mlr::Val> {
        let cond = self.lower_to_op(condition, None)?;

        let result_place = self.builder.insert_fresh_alloc()?;

        self.builder.start_new_block();
        let then_result = self.build_block(then_block, expected)?;
        self.builder.insert_assign_stmt(result_place, then_result)?;
        let then_block = self.builder.release_current_block();

        self.builder.start_new_block();
        let else_result = match else_block {
            Some(block) => self.build_block(block, expected),
            None => {
                let unit = self.builder.insert_unit_op()?;
                self.builder.insert_use_val(unit)
            }
        }?;
        self.builder.insert_assign_stmt(result_place, else_result)?;
        let else_block = self.builder.release_current_block();

        self.builder.insert_if_stmt(cond, then_block, else_block)?;
        let result_op = self.builder.insert_copy_op(result_place)?;
        self.builder.insert_use_val(result_op)
    }

    fn build_loop(&mut self, body: &hlr::Block) -> H2MResult<mlr::Val> {
        self.builder.start_new_block();
        self.build_block(body, None)?;
        let body = self.builder.release_current_block();
        self.builder.insert_loop_stmt(body)?;

        let unit = self.builder.insert_unit_op()?;
        self.builder.insert_use_val(unit)
    }

    fn build_struct_or_enum_val(&mut self, ident: &hlr::Ident, fields: &[(String, hlr::Expr)]) -> H2MResult<mlr::Val> {
        if let Some(struct_) = self.tys().get_struct_by_name(&ident.ident) {
            let n_gen_params = self.tys().get_struct_def(struct_).unwrap().gen_params.len();
            let gen_args = self.resolve_gen_args_or_insert_fresh_variables(&ident.gen_args, n_gen_params)?;

            let ty = self.tys().instantiate_struct(struct_, gen_args)?;
            self.build_struct_val(ty, fields)
        } else if let Some((enum_, variant_index)) = self.builder.try_resolve_enum_variant(&ident.ident) {
            let n_gen_params = self.tys().get_enum_def(enum_).unwrap().gen_params.len();
            let gen_args = self.resolve_gen_args_or_insert_fresh_variables(&ident.gen_args, n_gen_params)?;

            let ty = self.tys().instantiate_enum(enum_, gen_args)?;
            self.build_enum_val(ty, &variant_index, fields)
        } else {
            H2MError::UnresolvableStructOrEnum {
                ty_name: ident.ident.to_string(),
            }
            .into()
        }
    }

    fn build_struct_val(&mut self, ty: ty::Ty, fields: &[(String, hlr::Expr)]) -> H2MResult<mlr::Val> {
        let struct_val_place = self.builder.insert_alloc_with_ty(ty)?;
        self.build_struct_field_init_stmts(ty, fields, struct_val_place)?;
        self.builder.insert_use_place_val(struct_val_place)
    }

    fn build_enum_val(
        &mut self,
        ty: ty::Ty,
        variant_index: &usize,
        fields: &[(String, hlr::Expr)],
    ) -> H2MResult<mlr::Val> {
        // Create empty enum value
        let base_place = self.builder.insert_alloc_with_ty(ty)?;

        // Fill discriminant
        let discriminant_place = self.builder.insert_enum_discriminant_place(base_place)?;
        let discriminant_op = self.builder.insert_int_op(*variant_index as i64)?;
        let discriminant_value = self.builder.insert_use_val(discriminant_op)?;
        self.builder
            .insert_assign_stmt(discriminant_place, discriminant_value)?;

        // Fill fields
        let variant_place = self
            .builder
            .insert_project_to_variant_place(base_place, *variant_index)?;
        let variant_ty = self.builder.typechecker().get_enum_variant_ty(ty, *variant_index)?;
        self.build_struct_field_init_stmts(variant_ty, fields, variant_place)?;

        self.builder.insert_use_place_val(base_place)
    }

    fn build_struct_field_init_stmts(
        &mut self,
        ty: ty::Ty,
        fields: &[(String, hlr::Expr)],
        base_place: mlr::Place,
    ) -> H2MResult<()> {
        let field_indices = self
            .builder
            .typechecker()
            .resolve_struct_fields(ty, fields.iter().map(|(name, _)| name.as_str()))?;

        for ((_, expr), field_index) in fields.iter().zip(field_indices) {
            let field_place = self.builder.insert_field_access_place(base_place, field_index)?;

            let expected_ty = self.tys().get_struct_field_ty(ty, field_index)?;
            let field_value = self.lower_to_val(expr, Some(expected_ty))?;

            self.builder.insert_assign_stmt(field_place, field_value)?;
        }
        Ok(())
    }

    fn build_tuple_val(&mut self, exprs: &[hlr::Expr]) -> H2MResult<mlr::Val> {
        let exprs = exprs
            .iter()
            .map(|expr| self.lower_to_val(expr, None))
            .collect::<H2MResult<Vec<_>>>()?;

        let expr_tys = exprs
            .iter()
            .map(|&expr| self.mlr().get_val_ty(expr))
            .collect::<Vec<_>>();

        let tuple_ty = self.tys().register_tuple_ty(expr_tys);

        let tuple_place = self.builder.insert_alloc_with_ty(tuple_ty)?;
        for (field_index, expr) in exprs.into_iter().enumerate() {
            let field_place = self.builder.insert_field_access_place(tuple_place, field_index)?;
            self.builder.insert_assign_stmt(field_place, expr)?;
        }
        self.builder.insert_use_place_val(tuple_place)
    }

    fn build_match_expr(
        &mut self,
        scrutinee: &hlr::Expr,
        arms: &[hlr::MatchArm],
        expected: Option<ty::Ty>,
    ) -> H2MResult<mlr::Val> {
        let scrutinee_place = self.lower_to_place(scrutinee)?;
        let scrutinee_ty = self.mlr().get_place_ty(scrutinee_place);
        let scrutinee_ty_def = self.tys().get_ty_def(scrutinee_ty).unwrap();

        let (enum_ty, by_ref, scrutinee_place) = match scrutinee_ty_def {
            ty::TyDef::Enum { .. } => Ok((scrutinee_ty, false, scrutinee_place)),
            &ty::TyDef::Ref(inner) => {
                let inner_ty_def = self.tys().get_ty_def(inner).unwrap();
                match inner_ty_def {
                    ty::TyDef::Enum { .. } => {
                        let scrutinee_addr_op = self.builder.insert_copy_op(scrutinee_place)?;
                        let scrutinee_place = self.builder.insert_deref_place(scrutinee_addr_op)?;
                        Ok((inner, true, scrutinee_place))
                    }
                    _ => Err(H2MError::NonMatchableScrutinee { ty: scrutinee_ty }),
                }
            }
            _ => Err(H2MError::NonMatchableScrutinee { ty: scrutinee_ty }),
        }?;

        let discriminant_place = self.builder.insert_enum_discriminant_place(scrutinee_place)?;
        let discriminant = self.builder.insert_copy_op(discriminant_place)?;

        let arm_indices = self
            .typechecker()
            .resolve_enum_variants(enum_ty, arms.iter().map(|arm| arm.pattern.variant.as_str()))?;

        // resolve equality function for discriminant comparisons once
        let eq_fn = {
            let i32 = self.tys().get_primitive_ty(ty::Primitive::Integer32);
            let eq_fn = self.resolve_operator(&hlr::BinaryOperator::Equal, (i32, i32))?;
            self.builder.insert_fn_op(eq_fn)?
        };

        // now build the match statement
        let result_place = self.builder.insert_fresh_alloc()?;
        self.build_match_arms(
            enum_ty,
            by_ref,
            arms,
            &arm_indices,
            eq_fn,
            discriminant,
            scrutinee_place,
            result_place,
            expected,
        )?;

        let result_op = self.builder.insert_copy_op(result_place)?;
        self.builder.insert_use_val(result_op)
    }

    fn build_addr_of_val(&mut self, base: &hlr::Expr) -> H2MResult<mlr::Val> {
        let base = self.lower_to_place(base)?;
        self.builder.insert_addr_of_val(base)
    }

    fn build_as_expr(&mut self, expr: &hlr::Expr, target_ty: &hlr::TyAnnot) -> H2MResult<mlr::Val> {
        let expr_op = self.lower_to_op(expr, None)?;
        let target_ty = self.builder.resolve_hlr_ty_annot(target_ty)?;
        self.builder.insert_as_val(expr_op, target_ty)
    }

    fn build_closure(
        &mut self,
        params: &[hlr::ClosureParam],
        return_ty: &Option<hlr::TyAnnot>,
        body: &hlr::Block,
        expected: Option<ty::Ty>,
    ) -> H2MResult<mlr::Val> {
        let param_tys = params
            .iter()
            .map(|param| self.builder.resolve_hlr_ty_annot_or_insert_new_type(param.ty.as_ref()))
            .collect::<H2MResult<Vec<_>>>()?;
        let return_ty = self
            .builder
            .resolve_hlr_ty_annot_or_insert_new_type(return_ty.as_ref())?;

        self.match_param_and_return_ty(&param_tys, return_ty, expected)?;

        let captures_ty = self.generate_captures_ty()?;
        let param_names = params.iter().map(|param| param.name.clone()).collect::<Vec<_>>();
        let fn_sig = self.generate_closure_fn_sig(&param_names, &param_tys, return_ty, captures_ty);
        let fn_inst = self.generate_closure_fn_inst(fn_sig)?;
        let fn_ = fn_inst.fn_;

        let closure_ty = self.generate_closure_ty(fn_inst, captures_ty);

        let current_bindings = self.builder.get_flattened_scope();
        let captured_values =
            hlr_to_mlr_with_external_scope(self.ctxt(), body, fn_, current_bindings, Some(captures_ty))?;

        let closure_place = self.builder.insert_alloc_with_ty(closure_ty)?;
        self.fill_captures_fields(closure_place, captured_values)?;
        self.builder.insert_use_place_val(closure_place)
    }

    fn build_stmt(&mut self, stmt: &hlr::Stmt) -> H2MResult<()> {
        use hlr::Stmt::*;

        match stmt {
            Let { name, value, ty_annot } => self.build_let_stmt(name, ty_annot.as_ref(), value),
            Expr(expr) => self.build_expr_stmt(expr),
            Return(expr) => self.build_return_stmt(expr.as_ref()),
            Break => self.build_break_stmt(),
        }
    }

    fn build_let_stmt(&mut self, name: &str, ty_annot: Option<&hlr::TyAnnot>, value: &hlr::Expr) -> H2MResult<()> {
        let annot_ty = match ty_annot {
            Some(ty_annot) => self.builder.resolve_hlr_ty_annot(ty_annot)?,
            None => self.tys().new_undefined_ty(),
        };
        let loc = self.mlr().insert_typed_loc(annot_ty);
        self.builder.insert_alloc_stmt(loc)?;

        self.builder.start_new_block();

        let val = self.lower_to_val(value, Some(annot_ty))?;
        self.builder.insert_assign_to_loc_stmt(loc, val)?;

        self.builder.end_and_insert_current_block();

        self.builder.add_binding(name, loc);
        Ok(())
    }

    fn build_expr_stmt(&mut self, expr: &hlr::Expr) -> H2MResult<()> {
        self.builder.start_new_block();
        let _ = assign_to_fresh_alloc!(self, self.lower_to_val(expr, None)?);
        self.builder.end_and_insert_current_block();
        Ok(())
    }

    fn build_return_stmt(&mut self, expr: Option<&hlr::Expr>) -> H2MResult<()> {
        self.builder.start_new_block();

        let return_ty = self.builder.get_signature().return_ty;

        let return_val = match expr {
            Some(expr) => self.lower_to_val(expr, Some(return_ty))?,
            None => {
                let unit = self.builder.insert_unit_op()?;
                self.builder.insert_use_val(unit)?
            }
        };
        self.builder.insert_return_stmt(return_val)?;

        self.builder.end_and_insert_current_block();
        Ok(())
    }

    fn build_break_stmt(&mut self) -> H2MResult<()> {
        self.builder.insert_break_stmt()?;
        Ok(())
    }

    fn lower_ident_to_place(&mut self, name: &str) -> H2MResult<mlr::Place> {
        self.resolve_name_to_place(name)
            .ok_or_else(|| H2MError::UnresolvableSymbol { name: name.to_string() })
    }

    fn lower_field_access_to_place(
        &mut self,
        obj: &hlr::Expr,
        field_desc: &hlr::FieldDescriptor,
    ) -> H2MResult<mlr::Place> {
        // TODO allow general expressions as base (by lowering to val and then creating a
        // temporary place). This requires some attention to different expressions (temporaries vs.
        // places).
        let obj = self.lower_to_place(obj)?;
        let obj_ty = self.mlr().get_place_ty(obj);

        let field_index = match field_desc {
            hlr::FieldDescriptor::Named(ident) => {
                if !ident.gen_args.is_empty() {
                    return Err(H2MError::NotAPlace);
                }
                let field_name = ident.ident.as_str();

                self.typechecker().resolve_struct_field(obj_ty, field_name)?
            }
            &hlr::FieldDescriptor::Indexed(index) => index,
        };
        self.builder.insert_field_access_place(obj, field_index)
    }

    fn lower_deref_to_place(&mut self, base: &hlr::Expr) -> H2MResult<mlr::Place> {
        let base_op = self.lower_to_op(base, None)?;
        self.builder.insert_deref_place(base_op)
    }

    fn resolve_gen_args_or_insert_fresh_variables(
        &mut self,
        gen_args: &[hlr::TyAnnot],
        n_expected: usize,
    ) -> H2MResult<Vec<ty::Ty>> {
        if gen_args.is_empty() {
            Ok((0..n_expected).map(|_| self.tys().new_undefined_ty()).collect())
        } else {
            gen_args
                .iter()
                .map(|annot| self.builder.resolve_hlr_ty_annot(annot))
                .collect::<H2MResult<_>>()
        }
    }

    fn resolve_name_to_place(&mut self, name: &str) -> Option<mlr::Place> {
        self.builder
            .resolve_name_to_place(name)
            .or_else(|| self.try_resolve_name_to_captured_value(name))
    }

    fn try_resolve_name_to_captured_value(&mut self, name: &str) -> Option<mlr::Place> {
        let captures_struct_ty = self.captures_struct_ty?;

        let outer_loc_of_captured_value = self.outer_scope.get(name).cloned()?;

        if !self.captured_values.contains_key(&outer_loc_of_captured_value) {
            let captured_value_ty = self.mlr().get_loc_ty(outer_loc_of_captured_value);

            let captures_struct = self.tys().get_ty_def(captures_struct_ty).unwrap();
            let ty::TyDef::Struct { struct_, .. } = *captures_struct else {
                return None;
            };

            let captures_struct_def = self.tys().get_mut_struct_def(struct_).unwrap();
            let new_field_index = captures_struct_def.fields.len();

            captures_struct_def.fields.push(ty::StructField {
                name: name.to_string(),
                ty: captured_value_ty,
            });

            self.captured_values
                .insert(outer_loc_of_captured_value, new_field_index);
        };

        let index = *self.captured_values.get(&outer_loc_of_captured_value).unwrap();

        self.builder.insert_field_access_place(self.captures_place?, index).ok()
    }
}
