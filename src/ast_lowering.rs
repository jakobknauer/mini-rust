pub mod opt;

mod closure_util;
mod err;
mod lowered;
mod match_util;
mod ops;

#[macro_use]
mod macros;

use std::collections::HashMap;

use crate::{
    ast,
    ctxt::{self, fns, mlr, traits, ty},
    typechecker::{self, MthdResolution},
    util::mlr_builder::MlrBuilder,
};

pub use err::{AstLoweringError, AstLoweringResult};
use lowered::Lowered;

pub fn ast_to_mlr(
    ctxt: &mut ctxt::Ctxt,
    ast: &ast::Ast,
    ast_body: &ast::Block,
    target_fn: fns::Fn,
) -> AstLoweringResult<()> {
    ast_to_mlr_with_external_scope(ctxt, ast, ast_body, target_fn, HashMap::new(), None)?;
    Ok(())
}

pub fn ast_to_mlr_with_external_scope(
    ctxt: &mut ctxt::Ctxt,
    ast: &ast::Ast,
    ast_body: &ast::Block,
    target_fn: fns::Fn,
    external_scope: HashMap<String, mlr::Loc>,
    captures_ty: Option<ty::Ty>,
) -> AstLoweringResult<HashMap<mlr::Loc, usize>> {
    let (mlr, captured_values) =
        AstLowerer::new(target_fn, ctxt, ast, external_scope, captures_ty).lower_block(ast_body)?;
    ctxt.fns.add_fn_def(target_fn, mlr);
    Ok(captured_values)
}

struct AstLowerer<'a> {
    builder: MlrBuilder<'a>,
    closure_counter: u32,
    ast: &'a ast::Ast,

    /// The available local variables in scope surrounding this function (i.e. only relevant for closures)
    outer_scope: HashMap<String, mlr::Loc>,
    captures_struct_ty: Option<ty::Ty>,
    captures_place: Option<mlr::Place>,
    /// Map the original (outer) location of a captured variable to its index in the captures struct
    captured_values: HashMap<mlr::Loc, usize>,
}

impl<'a> AstLowerer<'a> {
    pub fn new(
        target_fn: fns::Fn,
        ctxt: &'a mut ctxt::Ctxt,
        ast: &'a ast::Ast,
        external_scope: HashMap<String, mlr::Loc>,
        captures_ty: Option<ty::Ty>,
    ) -> Self {
        Self {
            builder: MlrBuilder::new(target_fn, ctxt),
            ast,

            closure_counter: 0,

            outer_scope: external_scope,
            captures_struct_ty: captures_ty,
            captures_place: None,
            captured_values: HashMap::new(),
        }
    }

    pub fn lower_block(mut self, body: &'a ast::Block) -> AstLoweringResult<(fns::FnMlr, HashMap<mlr::Loc, usize>)> {
        let signature = self.builder.get_signature();
        if signature.var_args {
            return Err(AstLoweringError::VarArgsNotSupported);
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

    /// Build an ast block by inserting the statements into the current MLR block
    /// and returning the value of the block's return expression,
    /// all while in a new scope.
    ///
    /// This method does not start or end a new MLR block; but it does push and pop a new scope.
    fn build_block(&mut self, block: &ast::Block, expected: Option<ty::Ty>) -> AstLoweringResult<mlr::Val> {
        self.builder.push_scope();

        for &stmt in self.ast.stmt_slice(block.stmts) {
            self.build_stmt(stmt)?;
        }

        let output = match block.return_expr {
            Some(expr) => self.lower_to_val(expr, expected)?,
            None => {
                let unit = self.builder.insert_unit_op()?;
                self.builder.insert_use_val(unit)?
            }
        };

        self.builder.pop_scope();

        Ok(output)
    }

    fn lower(&mut self, expr: ast::Expr, expected: Option<ty::Ty>) -> AstLoweringResult<Lowered> {
        use ast::ExprKind::*;

        let expr = self.ast.expr(expr);

        let lowered = match expr {
            Lit(lit) => self.build_literal(lit)?.into(),
            Path(path) => self.lower_path(path)?,
            QualifiedPath(qual_path) => self.lower_qualified_path(qual_path)?,
            &Tuple(exprs) => self.build_tuple_val(exprs)?.into(),
            &BinaryOp { left, operator, right } => self.build_binary_op(left, operator, right)?.into(),
            &UnaryOp { operator, operand } => self.build_unary_op(operator, operand)?.into(),
            &Assign { target, value } => self.build_assignment(target, value)?.into(),
            &Call { callee, args } => self.build_call(callee, args)?.into(),
            MthdCall { obj, mthd, args } => self.build_mthd_call(*obj, mthd, *args)?.into(),
            Struct { ty_path, fields } => self.build_struct_or_enum_val(ty_path, fields)?.into(),
            FieldAccess { obj, field } => self.lower_field_access_to_place(*obj, field)?.into(),
            Block(block) => self.build_block(block, expected)?.into(),
            If { cond, then, else_ } => self.build_if(*cond, then, else_.as_ref(), expected)?.into(),
            Loop { body } => self.build_loop(body)?.into(),
            While { cond, body } => self.build_while(*cond, body)?.into(),
            Match { scrutinee, arms } => self.build_match_expr(*scrutinee, arms, expected)?.into(),
            &Deref { base } => self.lower_deref(base)?.into(),
            &AddrOf { base } => self.build_addr_of_val(base)?.into(),
            &As { expr, target_ty } => self.build_as_expr(expr, target_ty)?.into(),
            Self_ => self.build_self_place()?.into(),
            Closure {
                params,
                body,
                return_ty,
            } => self.build_closure(params, *return_ty, body, expected)?.into(),
        };

        Ok(lowered)
    }

    fn lower_to_val(&mut self, expr: ast::Expr, expected: Option<ty::Ty>) -> AstLoweringResult<mlr::Val> {
        self.lower(expr, expected)?.into_val(&mut self.builder)
    }

    fn lower_to_place(&mut self, expr: ast::Expr) -> AstLoweringResult<mlr::Place> {
        self.lower(expr, None)?.into_place(&mut self.builder)
    }

    fn lower_to_op(&mut self, expr: ast::Expr, expected: Option<ty::Ty>) -> AstLoweringResult<mlr::Op> {
        self.lower(expr, expected)?.into_op(&mut self.builder)
    }

    fn build_literal(&mut self, lit: &ast::Lit) -> AstLoweringResult<mlr::Op> {
        use ast::Lit::*;

        match *lit {
            Int(n) => self.builder.insert_int_op(n),
            Bool(b) => self.builder.insert_bool_op(b),
            CChar(c) => self.builder.insert_c_char_op(c),
            CString(ref s) => self.builder.insert_c_string_op(s),
        }
    }

    fn lower_path(&mut self, path: &ast::Path) -> AstLoweringResult<Lowered> {
        let lowered: Lowered = match path.segments.as_slice() {
            [single_segment] => match single_segment {
                ast::PathSegment {
                    ident,
                    args: None,
                    is_self: false,
                } => {
                    if let Some(place) = self.resolve_name_to_place(ident) {
                        place.into()
                    } else if let Some(fn_) = self.fns().get_fn_by_name(ident) {
                        let n_gen_params = self.fns().get_sig(fn_).unwrap().gen_params.len();
                        let gen_args = (0..n_gen_params).map(|_| self.tys().undef_ty()).collect();
                        self.builder.insert_gen_fn_op(fn_, gen_args, Vec::new())?.into()
                    } else {
                        return Err(AstLoweringError::UnresolvablePath { path: path.clone() });
                    }
                }
                &ast::PathSegment {
                    ref ident,
                    args: Some(args),
                    is_self: false,
                } => {
                    if let Some(fn_) = self.fns().get_fn_by_name(ident) {
                        let args = self
                            .ast
                            .ty_annot_slice(args)
                            .iter()
                            .map(|&annot| self.builder.resolve_ast_ty_annot(self.ast, annot))
                            .collect::<AstLoweringResult<_>>()?;
                        self.builder.insert_gen_fn_op(fn_, args, Vec::new())?.into()
                    } else {
                        return Err(AstLoweringError::UnresolvablePath { path: path.clone() });
                    }
                }
                _ => return Err(AstLoweringError::UnresolvablePath { path: path.clone() }),
            },

            [ty_path, mthd_name] if !mthd_name.is_self => {
                let ty = self.resolve_path_segment_to_ty(ty_path)?;
                let mthd_resolution = self.typechecker().resolve_mthd(ty, &mthd_name.ident, false)?;
                let (mthd, _) = self.mthd_resolution_to_op(mthd_resolution, ty, mthd_name.args)?;
                Lowered::Op(mthd)
            }
            _ => return Err(AstLoweringError::UnresolvablePath { path: path.clone() }),
        };

        Ok(lowered)
    }

    fn build_binary_op(
        &mut self,
        left: ast::Expr,
        operator: ast::BinaryOperator,
        right: ast::Expr,
    ) -> AstLoweringResult<mlr::Val> {
        match operator {
            ast::BinaryOperator::LogicalAnd => self.build_logical_and(left, right),
            ast::BinaryOperator::LogicalOr => self.build_logical_or(left, right),
            _ => {
                let left_op = self.lower_to_op(left, None)?;
                let right_op = self.lower_to_op(right, None)?;

                let op = {
                    let left_ty = self.mlr().get_op_ty(left_op);
                    let right_ty = self.mlr().get_op_ty(right_op);
                    let fn_ = self.resolve_binary_operator(operator, (left_ty, right_ty))?;
                    self.builder.insert_fn_op(fn_)?
                };

                self.builder.insert_call_val(op, vec![left_op, right_op])
            }
        }
    }

    fn build_unary_op(
        &mut self,
        operator: ast::UnaryOperator,
        operand: ast::Expr,
    ) -> Result<mlr::Val, AstLoweringError> {
        let operand = self.lower_to_op(operand, None)?;
        let operand_ty = self.mlr().get_op_ty(operand);

        let fn_ = self.resolve_unary_operator(operator, operand_ty)?;
        let op = self.builder.insert_fn_op(fn_)?;
        self.builder.insert_call_val(op, vec![operand])
    }

    fn build_logical_and(&mut self, left: ast::Expr, right: ast::Expr) -> AstLoweringResult<mlr::Val> {
        let bool_ty = self.tys().primitive(ty::Primitive::Boolean);
        let result_place = self.builder.insert_alloc_with_ty(bool_ty)?;

        let left_op = self.lower_to_op(left, None)?;

        self.builder.start_new_block();
        {
            let right_op = self.lower_to_op(right, None)?;
            let right_val = self.builder.insert_use_val(right_op)?;
            self.builder.insert_assign_stmt(result_place, right_val)?;
        }
        let then_block = self.builder.release_current_block();

        self.builder.start_new_block();
        {
            let false_op = self.builder.insert_bool_op(false)?;
            let false_val = self.builder.insert_use_val(false_op)?;
            self.builder.insert_assign_stmt(result_place, false_val)?;
        }
        let else_block = self.builder.release_current_block();

        self.builder.insert_if_stmt(left_op, then_block, else_block)?;
        self.builder.insert_use_place_val(result_place)
    }

    fn build_logical_or(&mut self, left: ast::Expr, right: ast::Expr) -> AstLoweringResult<mlr::Val> {
        let bool_ty = self.tys().primitive(ty::Primitive::Boolean);
        let result_place = self.builder.insert_alloc_with_ty(bool_ty)?;

        let left_op = self.lower_to_op(left, None)?;

        self.builder.start_new_block();
        {
            let true_op = self.builder.insert_bool_op(true)?;
            let true_val = self.builder.insert_use_val(true_op)?;
            self.builder.insert_assign_stmt(result_place, true_val)?;
        }
        let then_block = self.builder.release_current_block();

        self.builder.start_new_block();
        {
            let right_op = self.lower_to_op(right, None)?;
            let right_val = self.builder.insert_use_val(right_op)?;
            self.builder.insert_assign_stmt(result_place, right_val)?;
        }
        let else_block = self.builder.release_current_block();

        self.builder.insert_if_stmt(left_op, then_block, else_block)?;
        self.builder.insert_use_place_val(result_place)
    }

    fn build_assignment(&mut self, target: ast::Expr, value: ast::Expr) -> AstLoweringResult<mlr::Val> {
        let loc = self.lower_to_place(target)?;
        let value = self.lower_to_val(value, None)?;
        self.builder.insert_assign_stmt(loc, value)?;

        let output = self.builder.insert_unit_op()?;
        self.builder.insert_use_val(output)
    }

    fn build_call(&mut self, callee: ast::Expr, args: ast::ExprSlice) -> AstLoweringResult<mlr::Val> {
        let callee = self.lower_to_op(callee, None)?;
        let callee_ty = self.mlr().get_op_ty(callee);

        let param_tys = self
            .ctxt()
            .ty_is_callable(callee_ty)
            .map(|(param_tys, ..)| param_tys)
            .unwrap_or_default();

        let args: Vec<_> = self
            .ast
            .expr_slice(args)
            .iter()
            .enumerate()
            .map(|(idx, &arg)| {
                let expected = param_tys.get(idx).cloned(); // possibly None
                self.lower_to_op(arg, expected)
            })
            .collect::<AstLoweringResult<_>>()?;

        self.builder.insert_call_val(callee, args)
    }

    fn build_mthd_call(
        &mut self,
        obj: ast::Expr,
        mthd: &ast::PathSegment,
        args: ast::ExprSlice,
    ) -> AstLoweringResult<mlr::Val> {
        let base_place = self.lower_to_place(obj)?;
        let base_ty = self.mlr().get_place_ty(base_place);

        if mthd.is_self {
            unreachable!("Self is not a valid method name");
        }

        let mthd_resolution = self.typechecker().resolve_mthd(base_ty, &mthd.ident, true)?;
        let (callee, by_ref) = self.mthd_resolution_to_op(mthd_resolution, base_ty, mthd.args)?;

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
            .chain(self.ast.expr_slice(args).iter().enumerate().map(|(idx, &arg)| {
                let expected = param_tys.get(idx + 1).cloned(); // possibly None, skip self param
                self.lower_to_op(arg, expected)
            }))
            .collect::<AstLoweringResult<_>>()?;

        self.builder.insert_call_val(callee, args)
    }

    fn build_if(
        &mut self,
        condition: ast::Expr,
        then_block: &ast::Block,
        else_block: Option<&ast::Block>,
        expected: Option<ty::Ty>,
    ) -> AstLoweringResult<mlr::Val> {
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
        self.builder.insert_use_place_val(result_place)
    }

    fn build_loop(&mut self, body: &ast::Block) -> AstLoweringResult<mlr::Val> {
        self.builder.start_new_block();
        self.build_block(body, None)?;
        let body = self.builder.release_current_block();
        self.builder.insert_loop_stmt(body)?;

        let unit = self.builder.insert_unit_op()?;
        self.builder.insert_use_val(unit)
    }

    fn build_while(&mut self, condition: ast::Expr, body: &ast::Block) -> AstLoweringResult<mlr::Val> {
        self.builder.start_new_block();

        // build 'if condition, then break'
        let cond = self.lower_to_op(condition, None)?;
        let cond_ty = self.mlr().get_op_ty(cond);
        self.typechecker().assert_while_condition_ty(cond_ty)?;

        {
            self.builder.start_new_block();
        }
        let then_block = self.builder.release_current_block();

        {
            self.builder.start_new_block();
            self.builder.insert_break_stmt()?;
        }
        let else_block = self.builder.release_current_block();

        self.builder.insert_if_stmt(cond, then_block, else_block)?;

        // build actual loop body
        self.builder.push_scope();
        self.build_block(body, None)?;
        self.builder.pop_scope();

        let body = self.builder.release_current_block();
        self.builder.insert_loop_stmt(body)?;

        let unit = self.builder.insert_unit_op()?;
        self.builder.insert_use_val(unit)
    }

    fn build_struct_or_enum_val(
        &mut self,
        ty_path: &ast::Path,
        fields: &[(String, ast::Expr)],
    ) -> AstLoweringResult<mlr::Val> {
        match self.resolve_path_to_struct_or_enum_variant(ty_path)? {
            StructOrEnumResolution::Struct(ty) => self.build_struct_val(ty, fields),
            StructOrEnumResolution::EnumVariant(ty, variant_index) => self.build_enum_val(ty, variant_index, fields),
        }
    }

    fn build_struct_val(&mut self, ty: ty::Ty, fields: &[(String, ast::Expr)]) -> AstLoweringResult<mlr::Val> {
        let struct_val_place = self.builder.insert_alloc_with_ty(ty)?;
        self.build_struct_field_init_stmts(ty, fields, struct_val_place)?;
        self.builder.insert_use_place_val(struct_val_place)
    }

    fn build_enum_val(
        &mut self,
        ty: ty::Ty,
        variant_index: usize,
        fields: &[(String, ast::Expr)],
    ) -> AstLoweringResult<mlr::Val> {
        // Create empty enum value
        let base_place = self.builder.insert_alloc_with_ty(ty)?;

        // Fill discriminant
        let discriminant_place = self.builder.insert_enum_discriminant_place(base_place)?;
        let discriminant_op = self.builder.insert_int_op(variant_index as i64)?;
        let discriminant_value = self.builder.insert_use_val(discriminant_op)?;
        self.builder
            .insert_assign_stmt(discriminant_place, discriminant_value)?;

        // Fill fields
        let variant_place = self
            .builder
            .insert_project_to_variant_place(base_place, variant_index)?;
        let variant_ty = self.builder.typechecker().get_enum_variant_ty(ty, variant_index)?;
        self.build_struct_field_init_stmts(variant_ty, fields, variant_place)?;

        self.builder.insert_use_place_val(base_place)
    }

    fn build_struct_field_init_stmts(
        &mut self,
        ty: ty::Ty,
        fields: &[(String, ast::Expr)],
        base_place: mlr::Place,
    ) -> AstLoweringResult<()> {
        let field_indices = self
            .builder
            .typechecker()
            .resolve_struct_fields(ty, fields.iter().map(|(name, _)| name.as_str()))?;

        for (&(_, expr), field_index) in fields.iter().zip(field_indices) {
            let field_place = self.builder.insert_field_access_place(base_place, field_index)?;

            let expected_ty = self.tys().get_struct_field_ty(ty, field_index)?;
            let field_value = self.lower_to_val(expr, Some(expected_ty))?;

            self.builder.insert_assign_stmt(field_place, field_value)?;
        }
        Ok(())
    }

    fn build_tuple_val(&mut self, exprs: ast::ExprSlice) -> AstLoweringResult<mlr::Val> {
        let exprs = self
            .ast
            .expr_slice(exprs)
            .iter()
            .map(|&expr| self.lower_to_val(expr, None))
            .collect::<AstLoweringResult<Vec<_>>>()?;

        let expr_tys = exprs
            .iter()
            .map(|&expr| self.mlr().get_val_ty(expr))
            .collect::<Vec<_>>();

        let tuple_ty = self.tys().tuple(&expr_tys);

        let tuple_place = self.builder.insert_alloc_with_ty(tuple_ty)?;
        for (field_index, expr) in exprs.into_iter().enumerate() {
            let field_place = self.builder.insert_field_access_place(tuple_place, field_index)?;
            self.builder.insert_assign_stmt(field_place, expr)?;
        }
        self.builder.insert_use_place_val(tuple_place)
    }

    fn build_match_expr(
        &mut self,
        scrutinee: ast::Expr,
        arms: &[ast::MatchArm],
        expected: Option<ty::Ty>,
    ) -> AstLoweringResult<mlr::Val> {
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
                    _ => Err(AstLoweringError::NonMatchableScrutinee { ty: scrutinee_ty }),
                }
            }
            _ => Err(AstLoweringError::NonMatchableScrutinee { ty: scrutinee_ty }),
        }?;

        let discriminant_place = self.builder.insert_enum_discriminant_place(scrutinee_place)?;
        let discriminant = self.builder.insert_copy_op(discriminant_place)?;

        // now check if all arms are for the same enum type, namely enum_ty, and extract the
        // variant indices
        let arm_indices = arms
            .iter()
            .map(|arm| self.resolve_path_to_struct_or_enum_variant(&arm.pattern.variant))
            .collect::<AstLoweringResult<Vec<_>>>()?
            .iter()
            .map(|variant| match variant {
                StructOrEnumResolution::Struct(_) => Err(AstLoweringError::MatchArmPatternNotEnumVariant),
                StructOrEnumResolution::EnumVariant(variant_enum_ty, variant_index) => {
                    if self.tys().unify(*variant_enum_ty, enum_ty).is_ok() {
                        Ok(*variant_index)
                    } else {
                        Err(AstLoweringError::MatchArmPatternWrongEnum {
                            expected: enum_ty,
                            found: *variant_enum_ty,
                        })
                    }
                }
            })
            .collect::<AstLoweringResult<Vec<_>>>()?;

        // resolve equality function for discriminant comparisons once
        let eq_fn = {
            let i32 = self.tys().primitive(ty::Primitive::Integer32);
            let eq_fn = self.resolve_binary_operator(ast::BinaryOperator::Equal, (i32, i32))?;
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

    fn build_addr_of_val(&mut self, base: ast::Expr) -> AstLoweringResult<mlr::Val> {
        let base = self.lower_to_place(base)?;
        self.builder.insert_addr_of_val(base)
    }

    fn build_as_expr(&mut self, expr: ast::Expr, target_ty: ast::TyAnnot) -> AstLoweringResult<mlr::Val> {
        let expr_op = self.lower_to_op(expr, None)?;
        let target_ty = self.builder.resolve_ast_ty_annot(self.ast, target_ty)?;
        self.builder.insert_as_val(expr_op, target_ty)
    }

    fn build_self_place(&mut self) -> AstLoweringResult<mlr::Place> {
        let loc = self
            .builder
            .get_receiver_loc()
            .ok_or(AstLoweringError::NoSelfOutsideOfMethod)?;
        self.builder.insert_loc_place(loc)
    }

    fn build_closure(
        &mut self,
        params: &[ast::ClosureParam],
        return_ty: Option<ast::TyAnnot>,
        body: &ast::Block,
        expected: Option<ty::Ty>,
    ) -> AstLoweringResult<mlr::Val> {
        let param_tys = params
            .iter()
            .map(|param| self.builder.resolve_ast_ty_annot_or_insert_new_type(self.ast, param.ty))
            .collect::<AstLoweringResult<Vec<_>>>()?;
        let return_ty = self
            .builder
            .resolve_ast_ty_annot_or_insert_new_type(self.ast, return_ty)?;

        self.match_param_and_return_ty(&param_tys, return_ty, expected)?;

        let captures_ty = self.generate_captures_ty()?;
        let param_names = params.iter().map(|param| param.name.clone()).collect::<Vec<_>>();
        let fn_sig = self.generate_closure_fn_sig(&param_names, &param_tys, return_ty, captures_ty);
        let fn_inst = self.generate_closure_fn_inst(fn_sig)?;
        let fn_ = fn_inst.fn_;

        let closure_ty = self.generate_closure_ty(fn_inst, captures_ty);

        let current_bindings = self.builder.get_flattened_scope();
        let captured_values = ast_to_mlr_with_external_scope(
            self.builder.ctxt(),
            self.ast,
            body,
            fn_,
            current_bindings,
            Some(captures_ty),
        )?;

        let closure_place = self.builder.insert_alloc_with_ty(closure_ty)?;
        self.fill_captures_fields(closure_place, captured_values)?;
        self.builder.insert_use_place_val(closure_place)
    }

    fn build_stmt(&mut self, stmt: ast::Stmt) -> AstLoweringResult<()> {
        use ast::StmtKind::*;

        let stmt = self.ast.stmt(stmt);

        match stmt {
            Let { name, value, ty_annot } => self.build_let_stmt(name, *ty_annot, *value),
            &Expr(expr) => self.build_expr_stmt(expr),
            &Return(expr) => self.build_return_stmt(expr),
            Break => self.build_break_stmt(),
        }
    }

    fn build_let_stmt(
        &mut self,
        name: &str,
        ty_annot: Option<ast::TyAnnot>,
        value: ast::Expr,
    ) -> AstLoweringResult<()> {
        let annot_ty = match ty_annot {
            Some(ty_annot) => self.builder.resolve_ast_ty_annot(self.ast, ty_annot)?,
            None => self.tys().undef_ty(),
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

    fn build_expr_stmt(&mut self, expr: ast::Expr) -> AstLoweringResult<()> {
        self.builder.start_new_block();
        let _ = assign_to_fresh_alloc!(self, self.lower_to_val(expr, None)?);
        self.builder.end_and_insert_current_block();
        Ok(())
    }

    fn build_return_stmt(&mut self, expr: Option<ast::Expr>) -> AstLoweringResult<()> {
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

    fn build_break_stmt(&mut self) -> AstLoweringResult<()> {
        self.builder.insert_break_stmt()?;
        Ok(())
    }

    fn lower_field_access_to_place(
        &mut self,
        obj: ast::Expr,
        field_desc: &ast::FieldDescriptor,
    ) -> AstLoweringResult<mlr::Place> {
        let mut obj = self.lower_to_place(obj)?;
        let obj_ty = self.mlr().get_place_ty(obj);

        let field_resolution = match field_desc {
            ast::FieldDescriptor::Named(path) => {
                if path.is_self || path.args.is_some() {
                    return Err(AstLoweringError::NotAPlace);
                };

                self.typechecker().resolve_struct_field(obj_ty, &path.ident)?
            }
            &ast::FieldDescriptor::Indexed(index) => self.typechecker().resolve_tuple_field(obj_ty, index)?,
        };

        for _ in 0..field_resolution.num_derefs {
            let obj_op = self.builder.insert_copy_op(obj)?;
            obj = self.builder.insert_deref_place(obj_op)?;
        }

        self.builder
            .insert_field_access_place(obj, field_resolution.field_index)
    }

    fn lower_deref(&mut self, base: ast::Expr) -> AstLoweringResult<mlr::Place> {
        let base_op = self.lower_to_op(base, None)?;
        self.builder.insert_deref_place(base_op)
    }

    fn lower_qualified_path(&mut self, qual_path: &ast::QualifiedPath) -> AstLoweringResult<Lowered> {
        let ty = self.builder.resolve_ast_ty_annot(self.ast, qual_path.ty)?;

        if let Some(trait_) = &qual_path.trait_ {
            let trait_inst = self.resolve_trait_annot(trait_)?;

            let [segment] = qual_path.path.segments.as_slice() else {
                return Err(AstLoweringError::UnresolvablePath {
                    path: qual_path.path.clone(),
                });
            };

            if segment.is_self {
                return Err(AstLoweringError::UnresolvablePath {
                    path: qual_path.path.clone(),
                });
            }

            let mthd_idx = self
                .traits()
                .resolve_trait_method(trait_inst.trait_, &segment.ident)
                .unwrap();

            let gen_args: Vec<_> = match &segment.args {
                &Some(args) => self
                    .ast
                    .ty_annot_slice(args)
                    .iter()
                    .map(|&annot| self.builder.resolve_ast_ty_annot(self.ast, annot))
                    .collect::<AstLoweringResult<_>>()?,
                None => Vec::new(),
            };

            let trait_mthd_inst = fns::TraitMthdInst {
                trait_inst,
                mthd_idx,
                impl_ty: ty,
                gen_args: self.tys().ty_slice(&gen_args),
            };

            let op = self.builder.insert_trait_mthd_op(trait_mthd_inst)?;

            Ok(Lowered::Op(op))
        } else {
            let [segment] = qual_path.path.segments.as_slice() else {
                return Err(AstLoweringError::UnresolvablePath {
                    path: qual_path.path.clone(),
                });
            };

            if segment.is_self {
                return Err(AstLoweringError::UnresolvablePath {
                    path: qual_path.path.clone(),
                });
            }

            let mthd_resolution = self.typechecker().resolve_mthd(ty, &segment.ident, false)?;
            let (mthd, _) = self.mthd_resolution_to_op(mthd_resolution, ty, segment.args)?;
            Ok(Lowered::Op(mthd))
        }
    }

    fn resolve_gen_args_or_insert_fresh_variables(
        &mut self,
        gen_args: Option<ast::TyAnnotSlice>,
        n_expected: usize,
    ) -> AstLoweringResult<Vec<ty::Ty>> {
        match gen_args {
            Some(gen_args) => self
                .ast
                .ty_annot_slice(gen_args)
                .iter()
                .map(|&annot| self.builder.resolve_ast_ty_annot(self.ast, annot))
                .collect::<AstLoweringResult<_>>(),
            None => Ok((0..n_expected).map(|_| self.tys().undef_ty()).collect()),
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

    fn resolve_path_to_struct_or_enum_variant(
        &mut self,
        path: &ast::Path,
    ) -> AstLoweringResult<StructOrEnumResolution> {
        match path.segments.as_slice() {
            [segment] => {
                if segment.is_self {
                    return AstLoweringError::UnresolvableStructOrEnum { path: path.clone() }.into();
                }

                let struct_ = self
                    .tys()
                    .get_struct_by_name(&segment.ident)
                    .ok_or(AstLoweringError::UnresolvableStructOrEnum { path: path.clone() })?;

                let args: Vec<_> = match segment.args {
                    None => {
                        let n_gen_params = self.tys().get_struct_def(struct_).unwrap().gen_params.len();
                        (0..n_gen_params).map(|_| self.tys().undef_ty()).collect()
                    }
                    Some(args) => self
                        .ast
                        .ty_annot_slice(args)
                        .iter()
                        .map(|&annot| self.builder.resolve_ast_ty_annot(self.ast, annot))
                        .collect::<AstLoweringResult<_>>()?,
                };

                let ty = self.tys().inst_struct(struct_, &args)?;
                Ok(StructOrEnumResolution::Struct(ty))
            }
            [enum_seg, variant_name] => {
                if enum_seg.is_self {
                    return AstLoweringError::UnresolvableStructOrEnum { path: path.clone() }.into();
                }

                let enum_ = self
                    .tys()
                    .get_enum_by_name(&enum_seg.ident)
                    .ok_or(AstLoweringError::UnresolvableStructOrEnum { path: path.clone() })?;

                let args: Vec<_> = match enum_seg.args {
                    None => {
                        let n_gen_params = self.tys().get_enum_def(enum_).unwrap().gen_params.len();
                        (0..n_gen_params).map(|_| self.tys().undef_ty()).collect()
                    }
                    Some(args) => self
                        .ast
                        .ty_annot_slice(args)
                        .iter()
                        .map(|&annot| self.builder.resolve_ast_ty_annot(self.ast, annot))
                        .collect::<AstLoweringResult<_>>()?,
                };
                let ty = self.tys().inst_enum(enum_, &args)?;

                if variant_name.is_self || variant_name.args.is_some() {
                    return AstLoweringError::UnresolvableStructOrEnum { path: path.clone() }.into();
                }

                let variant_index = self
                    .tys()
                    .get_enum_def(enum_)
                    .unwrap()
                    .variants
                    .iter()
                    .position(|variant| variant.name == variant_name.ident)
                    .ok_or(AstLoweringError::UnresolvableStructOrEnum { path: path.clone() })?;

                Ok(StructOrEnumResolution::EnumVariant(ty, variant_index))
            }
            _ => AstLoweringError::UnresolvableStructOrEnum { path: path.clone() }.into(),
        }
    }

    fn resolve_trait_annot(&mut self, trait_annot: &ast::TraitAnnot) -> AstLoweringResult<traits::TraitInst> {
        let trait_ =
            self.traits()
                .resolve_trait_name(&trait_annot.name)
                .ok_or(AstLoweringError::UnresolvableTraitAnnot {
                    trait_name: trait_annot.name.clone(),
                })?;

        let trait_args: Vec<_> = match trait_annot.args {
            Some(args) => self
                .ast
                .ty_annot_slice(args)
                .iter()
                .map(|&arg| self.builder.resolve_ast_ty_annot(self.ast, arg))
                .collect::<Result<_, _>>()?,
            None => Vec::new(),
        };

        let trait_inst = traits::TraitInst {
            trait_,
            gen_args: self.tys().ty_slice(&trait_args),
        };
        Ok(trait_inst)
    }

    fn resolve_path_segment_to_ty(&mut self, ty_path: &ast::PathSegment) -> AstLoweringResult<ty::Ty> {
        use ctxt::Named::*;

        if ty_path.is_self {
            unreachable!("Self is not a valid type name");
        }

        // TODO allow generic variables
        let named_ty = *self.tys().get_ty_by_name(&ty_path.ident)?;

        let ty = match (named_ty, ty_path.args) {
            (Ty(ty), None) => ty,
            (Ty(ty), Some(_)) => return AstLoweringError::NotAGenericType(ty).into(),
            (Struct(struct_), args) => {
                let n_gen_args = self.tys().get_struct_def(struct_).unwrap().gen_params.len();
                let gen_args = self.resolve_gen_args_or_insert_fresh_variables(args, n_gen_args)?;
                self.tys().inst_struct(struct_, &gen_args)?
            }
            (Enum(enum_), gen_args) => {
                let n_gen_args = self.tys().get_enum_def(enum_).unwrap().gen_params.len();
                let gen_args = self.resolve_gen_args_or_insert_fresh_variables(gen_args, n_gen_args)?;
                self.tys().inst_enum(enum_, &gen_args)?
            }
        };

        Ok(ty)
    }

    fn mthd_resolution_to_op(
        &mut self,
        mthd_resolution: MthdResolution,
        obj_ty: ty::Ty,
        gen_args: Option<ast::TyAnnotSlice>,
    ) -> AstLoweringResult<(mlr::Op, bool)> {
        match mthd_resolution {
            MthdResolution::Inherent { fn_, env_gen_args } => {
                let sig = self.fns().get_sig(fn_).unwrap();
                let by_ref = sig
                    .params
                    .first()
                    .map(|param| param.kind == fns::FnParamKind::SelfByRef)
                    .unwrap_or(false);

                let n_gen_params = sig.gen_params.len();
                let gen_args = self.resolve_gen_args_or_insert_fresh_variables(gen_args, n_gen_params)?;

                let fn_inst = fns::FnInst {
                    fn_,
                    gen_args: self.tys().ty_slice(&gen_args),
                    env_gen_args,
                };

                Ok((self.builder.insert_fn_inst_op(fn_inst)?, by_ref))
            }
            MthdResolution::Trait { trait_inst, mthd_idx } => {
                let sig = self.traits().get_trait_mthd_sig(trait_inst.trait_, mthd_idx);
                let by_ref = sig
                    .params
                    .first()
                    .map(|param| param.kind == fns::FnParamKind::SelfByRef)
                    .unwrap_or(false);

                let n_gen_params = sig.gen_params.len();
                let gen_args = self.resolve_gen_args_or_insert_fresh_variables(gen_args, n_gen_params)?;

                let trait_mthd_inst = fns::TraitMthdInst {
                    trait_inst,
                    mthd_idx,
                    impl_ty: obj_ty,
                    gen_args: self.tys().ty_slice(&gen_args),
                };

                Ok((self.builder.insert_trait_mthd_op(trait_mthd_inst)?, by_ref))
            }
        }
    }
}

enum StructOrEnumResolution {
    Struct(ty::Ty),
    EnumVariant(ty::Ty, usize),
}
