pub mod opt;

mod err;
mod match_util;
mod ops;
#[macro_use]
mod macros;

use crate::{
    ctxt::{self, fns, mlr, ty},
    hlr,
    typechecker::{self, MethodResolution},
    util::mlr_builder::MlrBuilder,
};

pub use err::{H2MError, H2MResult};

pub fn hlr_to_mlr(ctxt: &mut ctxt::Ctxt, hlr_fn: &hlr::Fn, target_fn: fns::Fn) -> H2MResult<fns::FnMlr> {
    let h2m = H2M::new(target_fn, ctxt);
    h2m.build(hlr_fn)
}

struct H2M<'a> {
    builder: MlrBuilder<'a>,
}

impl<'a> H2M<'a> {
    pub fn new(target_fn: fns::Fn, ctxt: &'a mut ctxt::Ctxt) -> Self {
        Self {
            builder: MlrBuilder::new(target_fn, ctxt),
        }
    }

    pub fn build(mut self, input: &'a hlr::Fn) -> H2MResult<fns::FnMlr> {
        let signature = self.builder.get_signature();
        if signature.var_args {
            return Err(H2MError::VarArgsNotSupported);
        }

        let params = signature.params.clone();
        let has_receiver = signature.has_receiver;
        let mut param_locs = Vec::new();

        self.builder.push_scope();
        for fns::FnParam { name, ty } in params {
            let loc = self.builder.insert_typed_loc(ty)?;
            self.builder.add_binding(&name, loc);
            param_locs.push(loc);
        }

        if has_receiver {
            self.builder.register_receiver_loc(param_locs[0]);
        }

        self.builder.start_new_block();

        let return_val = self.build_block(input.body.as_ref().unwrap(), None)?;
        self.builder.insert_return_stmt(return_val)?;

        let body = self.builder.release_current_block();

        Ok(fns::FnMlr { body, param_locs })
    }

    fn typechecker(&mut self) -> typechecker::Typechecker<'_> {
        self.builder.typechecker()
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
            Closure { .. } => todo!(),
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
            _ => Err(H2MError::NotAPlace),
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
            && let Some(loc) = self.builder.resolve_name_to_location(ident)
        {
            let place = self.builder.insert_loc_place(loc)?;
            self.builder.insert_copy_op(place)
        } else if let Some(fn_) = self.fns().get_fn_by_name(ident) {
            let gen_arg_tys = if gen_args.is_empty() {
                let n_gen_params = self.fns().get_sig(fn_).unwrap().gen_params.len();
                (0..n_gen_params).map(|_| self.tys().new_undefined_ty()).collect()
            } else {
                gen_args
                    .iter()
                    .map(|annot| self.builder.resolve_hlr_ty_annot(annot))
                    .collect::<H2MResult<_>>()?
            };

            self.builder.insert_gen_fn_op(fn_, gen_arg_tys, Vec::new())
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

        // TODO get callee type, check that it is a function
        // then pass the argument types as expected to lower_to_op

        let args = args
            .iter()
            .map(|arg| self.lower_to_op(arg, None))
            .collect::<H2MResult<Vec<_>>>()?;

        self.builder.insert_call_val(callee, args)
    }

    fn build_method_call(
        &mut self,
        obj: &hlr::Expr,
        method: &hlr::Ident,
        args: &[hlr::Expr],
    ) -> Result<mlr::Val, H2MError> {
        let base = self.lower_to_op(obj, None)?;
        let base_ty = self.mlr().get_op_ty(base);

        let gen_args: Vec<_> = method
            .gen_args
            .iter()
            .map(|annot| self.builder.resolve_hlr_ty_annot(annot))
            .collect::<Result<_, _>>()?;

        let method = match self.typechecker().resolve_method(base_ty, &method.ident)? {
            MethodResolution::Inherent { fn_, env_gen_args } => {
                let gen_args = if gen_args.is_empty() {
                    let n_gen_params = self.fns().get_sig(fn_).unwrap().gen_params.len();
                    (0..n_gen_params).map(|_| self.tys().new_undefined_ty()).collect()
                } else {
                    gen_args
                };
                let fn_spec = fns::FnSpecialization {
                    fn_,
                    gen_args,
                    env_gen_args,
                };
                self.builder.insert_fn_spec_op(fn_spec)
            }
            MethodResolution::Trait { trait_, method_idx } => {
                let gen_args = if gen_args.is_empty() {
                    let n_gen_params = self.traits().get_trait_method_sig(trait_, method_idx).gen_params.len();
                    (0..n_gen_params).map(|_| self.tys().new_undefined_ty()).collect()
                } else {
                    gen_args
                };
                let trait_method = fns::TraitMethod {
                    trait_,
                    method_idx,
                    impl_ty: base_ty,
                    gen_args,
                };
                self.builder.insert_trait_method_op(trait_method)
            }
        }?;

        // TODO similar to build_call
        let args = std::iter::once(Ok(base))
            .chain(args.iter().map(|arg| self.lower_to_op(arg, None)))
            .collect::<H2MResult<Vec<_>>>()?;
        self.builder.insert_call_val(method, args)
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
            let gen_arg_tys = if ident.gen_args.is_empty() {
                let n_gen_params = self.tys().get_struct_def(struct_).unwrap().gen_params.len();
                (0..n_gen_params).map(|_| self.tys().new_undefined_ty()).collect()
            } else {
                ident
                    .gen_args
                    .iter()
                    .map(|annot| self.builder.resolve_hlr_ty_annot(annot))
                    .collect::<H2MResult<Vec<_>>>()?
            };

            let ty = self.tys().instantiate_struct(struct_, gen_arg_tys)?;
            self.build_struct_val(ty, fields)
        } else if let Some((enum_, variant_index)) = self.builder.try_resolve_enum_variant(&ident.ident) {
            let gen_arg_tys = if ident.gen_args.is_empty() {
                let n_gen_params = self.tys().get_enum_def(enum_).unwrap().gen_params.len();
                (0..n_gen_params).map(|_| self.tys().new_undefined_ty()).collect()
            } else {
                ident
                    .gen_args
                    .iter()
                    .map(|annot| self.builder.resolve_hlr_ty_annot(annot))
                    .collect::<H2MResult<Vec<_>>>()?
            };
            let ty = self.tys().instantiate_enum(enum_, gen_arg_tys)?;
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
            let field_value = self.lower_to_val(expr, None)?; // TODO pass expected type
            self.builder.insert_assign_stmt(field_place, field_value)?;
        }
        Ok(())
    }

    fn build_match_expr(
        &mut self,
        scrutinee: &hlr::Expr,
        arms: &[hlr::MatchArm],
        expected: Option<ty::Ty>,
    ) -> H2MResult<mlr::Val> {
        let scrutinee = self.lower_to_op(scrutinee, None)?;
        let scrutinee_place = assign_to_fresh_alloc!(self, self.builder.insert_use_val(scrutinee)?);

        let discriminant_place = self.builder.insert_enum_discriminant_place(scrutinee_place)?;
        let discriminant = self.builder.insert_copy_op(discriminant_place)?;

        // resolve equality function for discriminant comparisons once
        let eq_fn = {
            let i32 = self.tys().get_primitive_ty(ty::Primitive::Integer32);
            let eq_fn = self.resolve_operator(&hlr::BinaryOperator::Equal, (i32, i32))?;
            self.builder.insert_fn_op(eq_fn)?
        };

        let scrutinee_ty = self.mlr().get_place_ty(scrutinee_place);
        let arm_indices = self
            .typechecker()
            .resolve_enum_variants(scrutinee_ty, arms.iter().map(|arm| arm.pattern.variant.as_str()))?;

        // now build the match statement
        let result_place = self.builder.insert_fresh_alloc()?;
        self.build_match_arms(
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

    fn build_as_expr(&mut self, expr: &hlr::Expr, target_ty: &hlr::TyAnnot) -> Result<mlr::Val, H2MError> {
        let expr_op = self.lower_to_op(expr, None)?;
        let target_ty = self.builder.resolve_hlr_ty_annot(target_ty)?;
        self.builder.insert_as_val(expr_op, target_ty)
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
        let loc = self
            .builder
            .resolve_name_to_location(name)
            .ok_or_else(|| H2MError::UnresolvableSymbol { name: name.to_string() })?;

        self.builder.insert_loc_place(loc)
    }

    fn lower_field_access_to_place(&mut self, obj: &hlr::Expr, field: &hlr::Ident) -> H2MResult<mlr::Place> {
        // TODO allow general expressions as base (by lowering to val and then creating a
        // temporary place). This requires some attention to different expressions (temporaries vs.
        // places).
        let obj = self.lower_to_place(obj)?;
        let obj_ty = self.mlr().get_place_ty(obj);

        if !field.gen_args.is_empty() {
            return Err(H2MError::NotAPlace);
        }
        let field_name = field.ident.as_str();

        let field_index = self.typechecker().resolve_struct_field(obj_ty, field_name)?;
        self.builder.insert_field_access_place(obj, field_index)
    }

    fn lower_deref_to_place(&mut self, base: &hlr::Expr) -> H2MResult<mlr::Place> {
        let base_op = self.lower_to_op(base, None)?;
        self.builder.insert_deref_place(base_op)
    }
}
