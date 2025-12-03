pub mod opt;

mod err;
mod match_util;
mod ops;
mod util;
#[macro_use]
mod macros;

use std::collections::{HashMap, VecDeque};

use crate::{
    ctxt::{self, fns, mlr, ty},
    hlr,
    typechecker::Typechecker,
};

pub use err::{H2MError, H2MResult};

pub fn hlr_to_mlr(ctxt: &mut ctxt::Ctxt, hlr_fn: &hlr::Fn, target_fn: fns::Fn) -> H2MResult<fns::FnMlr> {
    let h2m = H2M::new(hlr_fn, target_fn, ctxt);
    h2m.build()
}

struct H2M<'a> {
    input: &'a hlr::Fn,

    target_fn: fns::Fn,
    ctxt: &'a mut ctxt::Ctxt,

    scopes: VecDeque<Scope>,
    blocks: VecDeque<Vec<mlr::Stmt>>,
}

struct Scope {
    vars: HashMap<String, mlr::Loc>,
}

impl Scope {
    fn new() -> Self {
        Self { vars: HashMap::new() }
    }
}

impl<'a> H2M<'a> {
    pub fn new(input: &'a hlr::Fn, target_fn: fns::Fn, ctxt: &'a mut ctxt::Ctxt) -> Self {
        Self {
            input,
            target_fn,
            ctxt,

            scopes: VecDeque::new(),
            blocks: VecDeque::new(),
        }
    }

    fn get_signature(&self) -> &fns::FnSig {
        self.ctxt
            .fns
            .get_sig(&self.target_fn)
            .expect("function signature should be registered")
    }

    fn current_scope(&mut self) -> &mut Scope {
        self.scopes.back_mut().expect("self.scopes should never be empty")
    }

    fn push_scope(&mut self) {
        self.scopes.push_back(Scope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop_back();
    }

    fn add_to_scope(&mut self, name: &str, loc: mlr::Loc) {
        self.current_scope().vars.insert(name.to_string(), loc);
    }

    fn start_new_block(&mut self) {
        self.blocks.push_back(Vec::new());
    }

    fn release_current_block(&mut self) -> mlr::Stmt {
        let stmts = self.blocks.pop_back().expect("self.blocks should never be empty");
        let block = mlr::StmtDef::Block(stmts);
        self.ctxt.mlr.insert_stmt(block)
    }

    fn end_and_insert_current_block(&mut self) {
        let block_stmt = self.release_current_block();
        self.blocks
            .back_mut()
            .expect("self.blocks should never be empty")
            .push(block_stmt);
    }

    fn typechecker(&mut self) -> Typechecker<'_> {
        Typechecker::new(self.ctxt, self.target_fn)
    }

    pub fn build(mut self) -> H2MResult<fns::FnMlr> {
        self.push_scope();

        let mut param_locs = Vec::new();
        let params = self.get_signature().params.clone();
        for fns::FnParam { name, ty } in params {
            let loc = self.ctxt.mlr.insert_typed_loc(ty);
            self.add_to_scope(&name, loc);
            param_locs.push(loc);
        }

        self.start_new_block();

        let return_val = self.build_block(&self.input.body)?;
        self.insert_return_stmt(return_val)?;

        let body = self.release_current_block();

        Ok(fns::FnMlr { body, param_locs })
    }

    /// Build an HLR block by inserting the statements into the current MLR block
    /// and returning the value of the block's return expression,
    /// all while in a new scope.
    ///
    /// This method does not start or end a new MLR block; but it does push and pop a new scope.
    pub fn build_block(&mut self, block: &hlr::Block) -> H2MResult<mlr::Val> {
        self.push_scope();

        for stmt in &block.stmts {
            self.build_stmt(stmt)?;
        }

        let output = match &block.return_expr {
            Some(expr) => self.lower_to_val(expr)?,
            None => {
                let unit = self.insert_unit_op()?;
                self.insert_use_val(unit)?
            }
        };

        self.pop_scope();

        Ok(output)
    }

    fn lower_to_val(&mut self, expr: &hlr::Expr) -> H2MResult<mlr::Val> {
        use hlr::Expr::*;

        match expr {
            Lit(..) | Ident(..) | GenQualIdent { .. } | FieldAccess { .. } | Deref { .. } => {
                let op = self.lower_to_op(expr)?;
                self.insert_use_val(op)
            }
            BinaryOp { left, operator, right } => self.build_binary_op(left, operator, right),
            Assign { target, value } => self.build_assignment(target, value),
            Call { callee, arguments } => self.build_call(callee, arguments),
            Struct { name, fields } => self.build_struct_or_enum_val(name, fields),
            If {
                condition,
                then_block,
                else_block,
            } => self.build_if(condition, then_block, else_block.as_ref()),
            Loop { body } => self.build_loop(body),
            Block(block) => self.build_block(block),
            Match { scrutinee, arms } => self.build_match_expr(scrutinee, arms),
            AddrOf { base } => self.build_addr_of_val(base),
        }
    }

    fn lower_to_place(&mut self, expr: &hlr::Expr) -> H2MResult<mlr::Place> {
        use hlr::Expr::*;

        match expr {
            Ident(name) => self.lower_ident_to_place(name),
            FieldAccess { base, name } => self.lower_field_access_to_place(base, name),
            Deref { base } => self.lower_deref_to_place(base),
            _ => Err(H2MError::NotAPlace),
        }
    }

    fn lower_to_op(&mut self, expr: &hlr::Expr) -> H2MResult<mlr::Op> {
        use hlr::Expr::*;

        match expr {
            Lit(lit) => self.build_literal(lit),
            Ident(name) => self.build_ident(name),
            GenQualIdent(hlr::GenQualIdent { ident, gen_args }) => self.build_gen_qual_ident(ident, gen_args),
            FieldAccess { .. } | Deref { .. } => {
                let place = self.lower_to_place(expr)?;
                self.insert_copy_op(place)
            }
            _ => {
                let val_place = assign_to_fresh_alloc!(self, self.lower_to_val(expr)?);
                self.insert_copy_op(val_place)
            }
        }
    }

    fn build_literal(&mut self, lit: &hlr::Lit) -> H2MResult<mlr::Op> {
        use hlr::Lit::*;

        match lit {
            Int(n) => self.insert_int_op(*n),
            Bool(b) => self.insert_bool_op(*b),
        }
    }

    fn build_ident(&mut self, name: &str) -> H2MResult<mlr::Op> {
        self.resolve_name(name)
    }

    fn build_gen_qual_ident(&mut self, ident: &str, gen_args: &[hlr::TyAnnot]) -> H2MResult<mlr::Op> {
        let fn_ = self
            .ctxt
            .fns
            .get_fn_by_name(ident)
            .ok_or_else(|| H2MError::UnresolvableSymbol {
                name: ident.to_string(),
            })?;

        let gen_arg_tys = gen_args
            .iter()
            .map(|annot| self.resolve_hlr_ty_annot(annot))
            .collect::<H2MResult<_>>()?;

        self.insert_gen_fn_op(fn_, gen_arg_tys)
    }

    fn build_binary_op(
        &mut self,
        left: &hlr::Expr,
        operator: &hlr::BinaryOperator,
        right: &hlr::Expr,
    ) -> H2MResult<mlr::Val> {
        let left_op = self.lower_to_op(left)?;
        let right_op = self.lower_to_op(right)?;

        let op = {
            let left_ty = self.ctxt.mlr.get_op_ty(&left_op);
            let right_ty = self.ctxt.mlr.get_op_ty(&right_op);
            let fn_ = self.resolve_operator(operator, (left_ty, right_ty))?;
            self.insert_fn_op(fn_)?
        };

        self.insert_call_val(op, vec![left_op, right_op])
    }

    fn build_assignment(&mut self, target: &hlr::Expr, value: &hlr::Expr) -> H2MResult<mlr::Val> {
        let loc = self.lower_to_place(target)?;
        let value = self.lower_to_val(value)?;
        self.insert_assign_stmt(loc, value)?;

        let output = self.insert_unit_op()?;
        self.insert_use_val(output)
    }

    fn build_call(&mut self, callee: &hlr::Expr, arguments: &[hlr::Expr]) -> H2MResult<mlr::Val> {
        let callee = self.lower_to_op(callee)?;

        let args = arguments
            .iter()
            .map(|arg| self.lower_to_op(arg))
            .collect::<H2MResult<Vec<_>>>()?;

        self.insert_call_val(callee, args)
    }

    fn build_if(
        &mut self,
        condition: &hlr::Expr,
        then_block: &hlr::Block,
        else_block: Option<&hlr::Block>,
    ) -> H2MResult<mlr::Val> {
        let cond = self.lower_to_op(condition)?;

        let result_place = self.insert_fresh_alloc()?;

        self.start_new_block();
        let then_result = self.build_block(then_block)?;
        self.insert_assign_stmt(result_place, then_result)?;
        let then_block = self.release_current_block();

        self.start_new_block();
        let else_result = match else_block {
            Some(block) => self.build_block(block),
            None => {
                let unit = self.insert_unit_op()?;
                self.insert_use_val(unit)
            }
        }?;
        self.insert_assign_stmt(result_place, else_result)?;
        let else_block = self.release_current_block();

        self.insert_if_stmt(cond, then_block, else_block)?;
        let result_op = self.insert_copy_op(result_place)?;
        self.insert_use_val(result_op)
    }

    fn build_loop(&mut self, body: &hlr::Block) -> H2MResult<mlr::Val> {
        self.start_new_block();
        self.build_block(body)?;
        let body = self.release_current_block();
        self.insert_loop_stmt(body)?;

        let unit = self.insert_unit_op()?;
        self.insert_use_val(unit)
    }

    fn build_struct_or_enum_val(&mut self, struct_name: &str, fields: &[(String, hlr::Expr)]) -> H2MResult<mlr::Val> {
        if let Some(ty) = self.ctxt.tys.get_ty_by_name(struct_name) {
            self.build_struct_val(&ty, fields)
        } else if let Some((ty, variant_index)) = self.try_resolve_enum_variant(struct_name) {
            self.build_enum_val(&ty, &variant_index, fields)
        } else {
            H2MError::UnresolvableStructOrEnum {
                ty_name: struct_name.to_string(),
            }
            .into()
        }
    }

    fn build_struct_val(&mut self, ty: &ty::Ty, fields: &[(String, hlr::Expr)]) -> H2MResult<mlr::Val> {
        let struct_val_place = self.insert_alloc_with_ty(*ty)?;
        self.build_struct_field_init_stmts(ty, fields, &struct_val_place)?;
        self.insert_use_place_val(struct_val_place)
    }

    fn build_enum_val(
        &mut self,
        ty: &ty::Ty,
        variant_index: &usize,
        fields: &[(String, hlr::Expr)],
    ) -> H2MResult<mlr::Val> {
        // Create empty enum value
        let base_place = self.insert_alloc_with_ty(*ty)?;

        // Fill discriminant
        let discriminant_place = self.insert_enum_discriminant_place(base_place)?;
        let discriminant_op = self.insert_int_op(*variant_index as i64)?;
        let discriminant_value = self.insert_use_val(discriminant_op)?;
        self.insert_assign_stmt(discriminant_place, discriminant_value)?;

        // Fill fields
        let variant_place = self.insert_project_to_variant_place(base_place, *variant_index)?;
        let enum_def = self.ctxt.tys.get_enum_def_by_ty(ty)?;
        let variant_ty = enum_def
            .variants
            .get(*variant_index)
            .expect("variant index should be valid")
            .ty;
        self.build_struct_field_init_stmts(&variant_ty, fields, &variant_place)?;

        self.insert_use_place_val(base_place)
    }

    fn build_struct_field_init_stmts(
        &mut self,
        ty: &ty::Ty,
        fields: &[(String, hlr::Expr)],
        base_place: &mlr::Place,
    ) -> H2MResult<()> {
        let field_indices = self
            .typechecker()
            .resolve_struct_fields(*ty, fields.iter().map(|(name, _)| name.as_str()))?;

        for ((_, expr), field_index) in fields.iter().zip(field_indices) {
            let field_place = self.insert_field_access_place(*base_place, field_index)?;
            let field_value = self.lower_to_val(expr)?;
            self.insert_assign_stmt(field_place, field_value)?;
        }
        Ok(())
    }

    fn build_match_expr(&mut self, scrutinee: &hlr::Expr, arms: &[hlr::MatchArm]) -> H2MResult<mlr::Val> {
        let scrutinee = self.lower_to_op(scrutinee)?;
        let scrutinee_place = assign_to_fresh_alloc!(self, self.insert_use_val(scrutinee)?);

        let discriminant_place = self.insert_enum_discriminant_place(scrutinee_place)?;
        let discriminant = self.insert_copy_op(discriminant_place)?;

        // resolve equality function for discriminant comparisons once
        let eq_fn = {
            let i32 = self.ctxt.tys.get_primitive_ty(ty::Primitive::Integer32);
            let eq_fn = self.resolve_operator(&hlr::BinaryOperator::Equal, (i32, i32))?;
            self.insert_fn_op(eq_fn)?
        };

        let scrutinee_ty = self.ctxt.mlr.get_place_ty(&scrutinee_place);
        let arm_indices = self
            .typechecker()
            .resolve_enum_variants(scrutinee_ty, arms.iter().map(|arm| arm.pattern.variant.as_str()))?;

        // now build the match statement
        let result_loc = self.insert_fresh_alloc()?;
        self.build_match_arms(arms, &arm_indices, eq_fn, discriminant, scrutinee_place, result_loc)?;

        let result_op = self.insert_copy_op(result_loc)?;
        self.insert_use_val(result_op)
    }

    fn build_addr_of_val(&mut self, base: &hlr::Expr) -> H2MResult<mlr::Val> {
        let base = self.lower_to_place(base)?;
        self.insert_addr_of_val(base)
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
            Some(ty_annot) => self.resolve_hlr_ty_annot(ty_annot)?,
            None => self.ctxt.tys.new_undefined_ty(),
        };
        let loc = self.ctxt.mlr.insert_typed_loc(annot_ty);
        self.insert_alloc_stmt(loc)?;

        self.start_new_block();

        let val = self.lower_to_val(value)?;
        self.insert_assign_to_loc_stmt(loc, val)?;

        self.end_and_insert_current_block();

        self.add_to_scope(name, loc);
        Ok(())
    }

    fn build_expr_stmt(&mut self, expr: &hlr::Expr) -> H2MResult<()> {
        self.start_new_block();
        let _ = assign_to_fresh_alloc!(self, self.lower_to_val(expr)?);
        self.end_and_insert_current_block();
        Ok(())
    }

    fn build_return_stmt(&mut self, expr: Option<&hlr::Expr>) -> H2MResult<()> {
        self.start_new_block();

        let return_val = match expr {
            Some(expr) => self.lower_to_val(expr)?,
            None => {
                let unit = self.insert_unit_op()?;
                self.insert_use_val(unit)?
            }
        };
        self.insert_return_stmt(return_val)?;

        self.end_and_insert_current_block();
        Ok(())
    }

    fn build_break_stmt(&mut self) -> H2MResult<()> {
        self.insert_break_stmt().map(|_| ())
    }

    fn lower_ident_to_place(&mut self, name: &str) -> H2MResult<mlr::Place> {
        let loc = self
            .resolve_name_to_location(name)
            .ok_or_else(|| H2MError::UnresolvableSymbol { name: name.to_string() })?;

        self.insert_loc_place(loc)
    }

    fn lower_field_access_to_place(&mut self, base: &hlr::Expr, field_name: &str) -> H2MResult<mlr::Place> {
        // TODO allow general expressions as base (by lowering to val and then creating a
        // temporary place). This requires some attention to different expressions (temporaries vs.
        // places).
        let base = self.lower_to_place(base)?;
        let base_ty = self.ctxt.mlr.get_place_ty(&base);
        let field_index = self.typechecker().resolve_struct_field(base_ty, field_name)?;
        self.insert_field_access_place(base, field_index)
    }

    fn lower_deref_to_place(&mut self, base: &hlr::Expr) -> H2MResult<mlr::Place> {
        let base_op = self.lower_to_op(base)?;
        self.insert_deref_place(base_op)
    }
}
