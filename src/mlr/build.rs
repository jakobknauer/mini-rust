use std::collections::{HashMap, VecDeque};

mod err;
mod ops;
mod pattern_util;
mod type_util;
mod util;

#[macro_use]
mod macros;

use crate::{
    ctxt::{
        self,
        fns::{Fn, FnParam, FnSig},
        types,
    },
    hlr, mlr,
};

pub use err::{MlrBuilderError, Result, TypeError};

pub struct MlrBuilder<'a> {
    input: &'a hlr::Fn,

    mlr_fn: Fn,
    ctxt: &'a mut ctxt::Ctxt,

    output: mlr::Mlr,

    scopes: VecDeque<Scope>,
    blocks: VecDeque<Vec<mlr::StmtId>>,

    next_val_id: mlr::ValId,
    next_place_id: mlr::PlaceId,
    next_stmt_id: mlr::StmtId,
    next_loc_id: mlr::LocId,
    next_op_id: mlr::OpId,
}

struct Scope {
    vars: HashMap<String, mlr::LocId>,
}

impl Scope {
    fn new() -> Self {
        Self { vars: HashMap::new() }
    }
}

impl<'a> MlrBuilder<'a> {
    pub fn new(input: &'a hlr::Fn, fn_: Fn, ctxt: &'a mut ctxt::Ctxt) -> Self {
        Self {
            input,
            mlr_fn: fn_,
            ctxt,

            output: mlr::Mlr::new(),

            scopes: VecDeque::new(),
            blocks: VecDeque::new(),

            next_val_id: mlr::ValId(0),
            next_place_id: mlr::PlaceId(0),
            next_stmt_id: mlr::StmtId(0),
            next_loc_id: mlr::LocId(0),
            next_op_id: mlr::OpId(0),
        }
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

    fn add_to_scope(&mut self, name: &str, loc: mlr::LocId) {
        self.current_scope().vars.insert(name.to_string(), loc);
    }

    fn start_new_block(&mut self) {
        self.blocks.push_back(Vec::new());
    }

    fn release_current_block(&mut self) -> mlr::StmtId {
        let stmts = self.blocks.pop_back().expect("self.blocks should never be empty");
        let block = mlr::Stmt::Block(stmts);
        let stmt_id = self.get_next_stmt_id();
        self.output.stmts.insert(stmt_id, block);
        stmt_id
    }

    fn end_and_insert_current_block(&mut self) {
        let block_stmt_id = self.release_current_block();
        self.blocks
            .back_mut()
            .expect("self.blocks should never be empty")
            .push(block_stmt_id);
    }

    pub fn build(mut self) -> Result<mlr::Mlr> {
        let FnSig { parameters, .. } = self.ctxt.fns.get_signature_by_id(&self.mlr_fn).cloned().unwrap();

        self.push_scope();

        for FnParam { name, type_ } in parameters {
            let loc = self.get_next_loc_id();
            self.output.allocated_locs.insert(loc);
            self.add_to_scope(&name, loc);
            self.output.loc_types.insert(loc, type_);
            self.output.param_locs.push(loc);
        }

        self.start_new_block();

        let return_val = self.build_block(&self.input.body)?;
        self.insert_return_stmt(return_val)?;

        self.output.body = self.release_current_block();

        Ok(self.output)
    }

    /// Build an HLR block by inserting the statements into the current MLR block
    /// and returning the value of the block's return expression,
    /// all while in a new scope.
    ///
    /// This method does not start or end a new MLR block; but it does push and pop a new scope.
    pub fn build_block(&mut self, block: &hlr::Block) -> Result<mlr::ValId> {
        self.push_scope();

        for stmt in &block.statements {
            self.build_statement(stmt)?;
        }

        let output = match &block.return_expression {
            Some(expr) => self.lower_to_val(expr)?,
            None => {
                let unit = self.insert_unit_op()?;
                self.insert_use_val(unit)?
            }
        };

        self.pop_scope();

        Ok(output)
    }

    fn lower_to_val(&mut self, expr: &hlr::Expression) -> Result<mlr::ValId> {
        use hlr::Expression::*;

        match expr {
            Literal(..) | Variable(..) | FieldAccess { .. } => {
                let op = self.lower_to_op(expr)?;
                self.insert_use_val(op)
            }
            BinaryOp { left, operator, right } => self.build_binary_op(left, operator, right),
            Assignment { target, value } => self.build_assignment(target, value),
            Call { callee, arguments } => self.build_call(callee, arguments),
            StructExpr { struct_name, fields } => self.build_struct_or_enum_val(struct_name, fields),
            If {
                condition,
                then_block,
                else_block,
            } => self.build_if(condition, then_block, else_block.as_ref()),
            Loop { body } => self.build_loop(body),
            Block(block) => self.build_block(block),
            Match { scrutinee, arms } => self.build_match_expression(scrutinee, arms),
        }
    }

    fn lower_to_place(&mut self, expr: &hlr::Expression) -> Result<mlr::PlaceId> {
        use hlr::Expression::*;

        match expr {
            Variable(name) => self.lower_var_to_place(name),
            FieldAccess { base, field_name } => self.lower_field_access_to_place(base, field_name),
            _ => panic!("Only variables and field access expressions are supported as places."),
        }
    }

    fn lower_to_op(&mut self, expr: &hlr::Expression) -> Result<mlr::OpId> {
        use hlr::Expression::*;

        match expr {
            Literal(literal) => self.build_literal(literal),
            Variable(name) => self.build_variable(name),
            FieldAccess { .. } => {
                let place = self.lower_to_place(expr)?;
                self.insert_copy_op(place)
            }
            _ => {
                let val_loc = assign_to_new_loc!(self, self.lower_to_val(expr)?);
                self.insert_copy_loc_op(val_loc)
            }
        }
    }

    fn build_literal(&mut self, literal: &hlr::Literal) -> Result<mlr::OpId> {
        use hlr::Literal::*;

        match literal {
            Int(n) => self.insert_int_op(*n),
            Bool(b) => self.insert_bool_op(*b),
        }
    }

    fn build_variable(&mut self, name: &str) -> Result<mlr::OpId> {
        self.resolve_name(name)
    }

    fn build_binary_op(
        &mut self,
        left: &hlr::Expression,
        operator: &hlr::BinaryOperator,
        right: &hlr::Expression,
    ) -> Result<mlr::ValId> {
        let left_op = self.lower_to_op(left)?;
        let right_op = self.lower_to_op(right)?;

        let op = {
            let left_type = self.get_op_type(&left_op);
            let right_type = self.get_op_type(&right_op);
            let fn_ = self.resolve_operator(operator, (left_type, right_type))?;
            self.insert_fn_op(fn_)?
        };

        self.insert_call_val(op, vec![left_op, right_op])
    }

    fn build_assignment(&mut self, target: &hlr::Expression, value: &hlr::Expression) -> Result<mlr::ValId> {
        let loc = self.lower_to_place(target)?;
        let value = self.lower_to_val(value)?;
        self.insert_assign_stmt(loc, value)?;

        let output = self.insert_unit_op()?;
        self.insert_use_val(output)
    }

    fn build_call(&mut self, callee: &hlr::Expression, arguments: &[hlr::Expression]) -> Result<mlr::ValId> {
        let callee = self.lower_to_op(callee)?;

        let args = arguments
            .iter()
            .map(|arg| self.lower_to_op(arg))
            .collect::<Result<Vec<_>>>()?;

        self.insert_call_val(callee, args)
    }

    fn build_if(
        &mut self,
        condition: &hlr::Expression,
        then_block: &hlr::Block,
        else_block: Option<&hlr::Block>,
    ) -> Result<mlr::ValId> {
        let cond = self.lower_to_op(condition)?;

        let result_loc = self.insert_fresh_alloc()?;

        self.start_new_block();
        let then_result = self.build_block(then_block)?;
        self.insert_assign_to_loc_stmt(result_loc, then_result)?;
        let then_block = self.release_current_block();

        self.start_new_block();
        let else_result = match else_block {
            Some(block) => self.build_block(block),
            None => {
                let unit = self.insert_unit_op()?;
                self.insert_use_val(unit)
            }
        }?;
        self.insert_assign_to_loc_stmt(result_loc, else_result)?;
        let else_block = self.release_current_block();

        self.insert_if_stmt(cond, then_block, else_block)?;
        let result_op = self.insert_copy_loc_op(result_loc)?;
        self.insert_use_val(result_op)
    }

    fn build_loop(&mut self, body: &hlr::Block) -> Result<mlr::ValId> {
        self.start_new_block();
        self.build_block(body)?;
        let body = self.release_current_block();
        self.insert_loop_stmt(body)?;

        let unit = self.insert_unit_op()?;
        self.insert_use_val(unit)
    }

    fn build_struct_or_enum_val(
        &mut self,
        struct_name: &str,
        fields: &[(String, hlr::Expression)],
    ) -> Result<mlr::ValId> {
        if let Some(type_id) = self.ctxt.types.get_type_id_by_name(struct_name) {
            self.build_struct_val(&type_id, fields)
        } else if let Some((type_id, variant_index)) = self.try_resolve_enum_variant(struct_name) {
            self.build_enum_val(&type_id, &variant_index, fields)
        } else {
            TypeError::UnresolvableTypeName {
                type_name: struct_name.to_string(),
            }
            .into()
        }
    }

    fn build_struct_val(
        &mut self,
        type_id: &types::TypeId,
        fields: &[(String, hlr::Expression)],
    ) -> Result<mlr::ValId> {
        let struct_val_loc = assign_to_new_loc!(self, self.insert_empty_val(*type_id)?);
        let struct_val_place = self.insert_loc_place(struct_val_loc)?;
        self.build_struct_field_init_stmts(type_id, fields, &struct_val_place)?;
        self.insert_use_place_val(struct_val_place)
    }

    fn build_enum_val(
        &mut self,
        type_id: &types::TypeId,
        variant_index: &usize,
        fields: &[(String, hlr::Expression)],
    ) -> Result<mlr::ValId> {
        // Create empty enum value
        let enum_val_loc = assign_to_new_loc!(self, self.insert_empty_val(*type_id)?);
        let base_place = self.insert_loc_place(enum_val_loc)?;

        // Fill discriminant
        let discriminant_place = self.insert_enum_discriminant_place(base_place)?;
        let discriminant_op = self.insert_int_op(*variant_index as i64)?;
        let discriminant_value = self.insert_use_val(discriminant_op)?;
        self.insert_assign_stmt(discriminant_place, discriminant_value)?;

        // Fill fields
        let variant_place = self.insert_project_to_variant_place(base_place, *variant_index)?;
        let enum_def = self.get_enum_def(type_id)?;
        let variant_type_id = enum_def
            .variants
            .get(*variant_index)
            .expect("variant index should be valid")
            .type_id;
        self.build_struct_field_init_stmts(&variant_type_id, fields, &variant_place)?;

        self.insert_use_place_val(base_place)
    }

    fn build_match_expression(&mut self, scrutinee: &hlr::Expression, arms: &[hlr::MatchArm]) -> Result<mlr::ValId> {
        let scrutinee = self.lower_to_op(scrutinee)?;
        let scrutinee_loc = assign_to_new_loc!(self, self.insert_use_val(scrutinee)?);
        let scrutinee_place = self.insert_loc_place(scrutinee_loc)?;

        let discriminant_place = self.insert_enum_discriminant_place(scrutinee_place)?;
        let discriminant = self.insert_copy_op(discriminant_place)?;

        // resolve equality function for discriminant comparisons once
        let eq_fn = {
            let i32 = self
                .ctxt
                .types
                .get_primitive_type_id(types::PrimitiveType::Integer32)
                .unwrap();
            let eq_fn = self.resolve_operator(&hlr::BinaryOperator::Equal, (i32, i32))?;
            self.insert_fn_op(eq_fn)?
        };

        let scrutinee_type_id = self.get_loc_type(&scrutinee_loc);
        let enum_def = self.get_enum_def(&scrutinee_type_id)?;
        let arm_indices = self.get_arm_indices(arms, enum_def, &scrutinee_type_id)?;

        // now build the match statement
        let result_loc = self.insert_fresh_alloc()?;
        self.build_match_arms(arms, &arm_indices, eq_fn, discriminant, scrutinee_place, result_loc)?;

        let result_op = self.insert_copy_loc_op(result_loc)?;
        self.insert_use_val(result_op)
    }

    fn build_statement(&mut self, stmt: &hlr::Statement) -> Result<()> {
        use hlr::Statement::*;

        match stmt {
            Let { name, value, .. } => self.build_let_statement(name, value),
            Expression(expression) => self.build_expression_statement(expression),
            Return(expression) => self.build_return_statement(expression.as_ref()),
            Break => self.build_break_statement(),
        }
    }

    fn build_let_statement(&mut self, name: &str, value: &hlr::Expression) -> Result<()> {
        let loc = self.insert_fresh_alloc()?;

        self.start_new_block();

        let val = self.lower_to_val(value)?;
        self.insert_assign_to_loc_stmt(loc, val)?;

        self.end_and_insert_current_block();

        self.add_to_scope(name, loc);
        Ok(())
    }

    fn build_expression_statement(&mut self, expression: &hlr::Expression) -> Result<()> {
        self.start_new_block();
        let _ = assign_to_new_loc!(self, self.lower_to_val(expression)?);
        self.end_and_insert_current_block();
        Ok(())
    }

    fn build_return_statement(&mut self, expression: Option<&hlr::Expression>) -> Result<()> {
        self.start_new_block();

        let return_val = match expression {
            Some(expression) => self.lower_to_val(expression)?,
            None => {
                let unit = self.insert_unit_op()?;
                self.insert_use_val(unit)?
            }
        };
        self.insert_return_stmt(return_val)?;

        self.end_and_insert_current_block();
        Ok(())
    }

    fn build_break_statement(&mut self) -> Result<()> {
        self.insert_break_stmt().map(|_| ())
    }

    fn lower_var_to_place(&mut self, name: &str) -> Result<mlr::PlaceId> {
        let loc = self
            .resolve_name_to_location(name)
            .ok_or_else(|| MlrBuilderError::UnresolvableSymbol { name: name.to_string() })?;

        self.insert_loc_place(loc)
    }

    fn lower_field_access_to_place(&mut self, base: &hlr::Expression, field_name: &str) -> Result<mlr::PlaceId> {
        // TODO allow general expressions as base (by lowering to val and then creating a
        // temporary place). This however requires a better builder infrastructure, so we
        // can here at this point create temporary variables/places in the current scope.
        let base = self.lower_to_place(base)?;

        let base_type_id = self.get_place_type(&base);
        let struct_def = self.get_struct_def(&base_type_id)?;

        let field_index = struct_def
            .fields
            .iter()
            .position(|struct_field| struct_field.name == field_name)
            .ok_or(MlrBuilderError::TypeError(TypeError::NotAStructField {
                type_id: base_type_id,
                field_name: field_name.to_string(),
            }))?;

        self.insert_field_access_place(base, field_index)
    }

    fn build_match_arms(
        &mut self,
        arms: &[hlr::MatchArm],
        arm_indices: &[usize],
        eq_fn: mlr::OpId,
        discriminant: mlr::OpId,
        scrutinee_place: mlr::PlaceId,
        result_loc: mlr::LocId,
    ) -> Result<()> {
        if arms.is_empty() {
            panic!("Match expressions must have at least one arm.");
        }

        if arms.len() == 1 {
            let arm = &arms[0];
            let variant_index = arm_indices[0];
            let arm_result = self.build_arm_block(
                arm,
                &self.get_place_type(&scrutinee_place),
                &variant_index,
                &scrutinee_place,
            )?;
            self.insert_assign_to_loc_stmt(result_loc, arm_result)?;
            return Ok(());
        } else {
            let first_arm = &arms[0];
            let first_variant_index = arm_indices[0];

            let condition = self.build_arm_condition(&first_variant_index, &eq_fn, &discriminant)?;

            self.start_new_block();
            let first_arm_result = self.build_arm_block(
                first_arm,
                &self.get_place_type(&scrutinee_place),
                &first_variant_index,
                &scrutinee_place,
            )?;
            self.insert_assign_to_loc_stmt(result_loc, first_arm_result)?;
            let then_block = self.release_current_block();

            self.start_new_block();
            self.build_match_arms(
                &arms[1..],
                &arm_indices[1..],
                eq_fn,
                discriminant,
                scrutinee_place,
                result_loc,
            )?;
            let else_block = self.release_current_block();

            self.insert_if_stmt(condition, then_block, else_block)?;
        }

        Ok(())
    }
}
