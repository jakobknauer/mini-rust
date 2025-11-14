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
        functions::{FnId, FunctionParameter, FunctionSignature},
        types,
    },
    hlr, mlr,
};

pub use err::{MlrBuilderError, Result, TypeError};

pub struct MlrBuilder<'a> {
    function: &'a hlr::Function,
    fn_id: FnId,
    ctxt: &'a mut ctxt::Ctxt,
    output: mlr::Mlr,
    scopes: VecDeque<Scope>,
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
    pub fn new(function: &'a hlr::Function, fn_id: FnId, ctxt: &'a mut ctxt::Ctxt) -> Self {
        Self {
            function,
            fn_id,
            ctxt,
            output: mlr::Mlr::new(),
            scopes: VecDeque::new(),
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

    pub fn build(mut self) -> Result<mlr::Mlr> {
        let FunctionSignature {
            parameters,
            return_type,
            ..
        } = self
            .ctxt
            .function_registry
            .get_signature_by_id(&self.fn_id)
            .cloned()
            .unwrap();

        self.push_scope();
        for FunctionParameter { name, type_ } in parameters {
            let loc = self.get_next_loc_id();
            self.add_to_scope(&name, loc);
            self.output.loc_types.insert(loc, type_);
            self.output.param_locs.push(loc);
        }

        let body = self.build_block(&self.function.body)?;

        let output_type = self.get_val_type(&body);
        if !self.ctxt.type_registry.types_equal(&return_type, &output_type) {
            return TypeError::ReturnTypeMismatch {
                expected: return_type,
                actual: output_type,
            }
            .into();
        };

        self.output.body = body;
        Ok(self.output)
    }

    pub fn build_block(&mut self, block: &hlr::Block) -> Result<mlr::ValId> {
        self.push_scope();

        let statements: Vec<_> = block
            .statements
            .iter()
            .map(|stmt| self.build_statement(stmt))
            .collect::<Result<_>>()?;

        let output = match &block.return_expression {
            Some(expr) => self.lower_to_val(expr)?,
            None => {
                let unit = self.insert_unit_op()?;
                self.insert_use_val(unit)?
            }
        };

        self.pop_scope();

        self.insert_new_block_val(statements, output)
    }

    fn lower_to_val(&mut self, expr: &hlr::Expression) -> Result<mlr::ValId> {
        use hlr::Expression::*;

        match expr {
            Literal(literal) => {
                let literal = self.build_literal(literal)?;
                self.insert_use_val(literal)
            }
            Variable(name) => {
                let op = self.build_variable(name)?;
                self.insert_use_val(op)
            }
            BinaryOp { left, operator, right } => self.build_binary_op(left, operator, right),
            Assignment { target, value } => self.build_assignment(target, value),
            FunctionCall { function, arguments } => self.build_function_call(function, arguments),
            StructExpr { struct_name, fields } => self.build_struct_or_enum_val(struct_name, fields),
            If {
                condition,
                then_block,
                else_block,
            } => self.build_if(condition, then_block, else_block.as_ref()),
            Loop { body } => self.build_loop(body),
            Block(block) => self.build_block(block),
            FieldAccess { .. } => {
                let place = self.lower_to_place(expr)?;
                self.insert_use_place_val(place)
            }
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
            _ => {
                let val = self.lower_to_val(expr)?;
                // Store the value in a temporary location and return a copy op
                let (temp_loc, temp_stmt) = assign_to_new_loc!(self, val);
                let temp_place = self.insert_loc_place(temp_loc)?;
                let temp_op = self.insert_copy_op(temp_place)?;
                // Insert the statement to the current block
                self.insert_stmt_in_current_block(temp_stmt)?;
                Ok(temp_op)
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
            let fn_id = self.resolve_operator(operator, (left_type, right_type))?;
            self.insert_function_op(fn_id)?
        };

        self.insert_call_val(op, vec![left_op, right_op])
    }

    fn build_assignment(&mut self, target: &hlr::Expression, value: &hlr::Expression) -> Result<mlr::ValId> {
        let assign_stmt = {
            let loc = self.lower_to_place(target)?;
            let value = self.lower_to_val(value)?;
            self.insert_assign_stmt(loc, value)?
        };

        let output = self.insert_unit_op()?;
        let output = self.insert_use_val(output)?;
        self.insert_new_block_val(vec![assign_stmt], output)
    }

    fn build_function_call(&mut self, function: &hlr::Expression, arguments: &[hlr::Expression]) -> Result<mlr::ValId> {
        let function = self.lower_to_op(function)?;

        let args = arguments
            .iter()
            .map(|arg| self.lower_to_op(arg))
            .collect::<Result<Vec<_>>>()?;

        self.insert_call_val(function, args)
    }

    fn build_if(
        &mut self,
        condition: &hlr::Expression,
        then_block: &hlr::Block,
        else_block: Option<&hlr::Block>,
    ) -> Result<mlr::ValId> {
        let cond = self.lower_to_op(condition)?;

        let then_block = self.build_block(then_block)?;
        let else_block = match else_block {
            Some(block) => self.build_block(block),
            None => {
                let unit = self.insert_unit_op()?;
                self.insert_use_val(unit)
            }
        }?;

        self.insert_if_val(cond, then_block, else_block)
    }

    fn build_loop(&mut self, body: &hlr::Block) -> Result<mlr::ValId> {
        let body = self.build_block(body)?;
        self.insert_loop_val(body)
    }

    fn build_struct_or_enum_val(
        &mut self,
        struct_name: &str,
        fields: &[(String, hlr::Expression)],
    ) -> Result<mlr::ValId> {
        if let Some(type_id) = self.ctxt.type_registry.get_type_id_by_name(struct_name) {
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
        let (struct_val_loc, struct_val_stmt) = assign_to_new_loc!(self, self.insert_empty_val(*type_id)?);
        let struct_val_place = self.insert_loc_place(struct_val_loc)?;

        let field_init_stmts = self.build_struct_field_init_stmts(type_id, fields, &struct_val_place)?;

        let struct_val = self.insert_use_place_val(struct_val_place)?;

        let statements = std::iter::once(struct_val_stmt).chain(field_init_stmts).collect();
        self.insert_new_block_val(statements, struct_val)
    }

    fn build_enum_val(
        &mut self,
        type_id: &types::TypeId,
        variant_index: &usize,
        fields: &[(String, hlr::Expression)],
    ) -> Result<mlr::ValId> {
        // Create empty enum value
        let (enum_val_loc, enum_val_stmt) = assign_to_new_loc!(self, self.insert_empty_val(*type_id)?);
        let base_place = self.insert_loc_place(enum_val_loc)?;

        // Fill discriminant
        let discriminant_place = self.insert_enum_discriminant_place(base_place)?;
        let discriminant_op = self.insert_int_op(*variant_index as i64)?;
        let discriminant_value = self.insert_use_val(discriminant_op)?;
        let discriminant_stmt = self.insert_assign_stmt(discriminant_place, discriminant_value)?;

        // Fill fields
        let variant_place = self.insert_project_to_variant_place(base_place, *variant_index)?;
        let enum_def = self.get_enum_def(type_id)?;
        let variant_type_id = enum_def
            .variants
            .get(*variant_index)
            .expect("variant index should be valid")
            .type_id;
        let field_init_stmts = self.build_struct_field_init_stmts(&variant_type_id, fields, &variant_place)?;

        let enum_val = self.insert_use_place_val(base_place)?;

        // Build final block
        let statements = std::iter::once(enum_val_stmt)
            .chain(std::iter::once(discriminant_stmt))
            .chain(field_init_stmts)
            .collect();
        self.insert_new_block_val(statements, enum_val)
    }

    fn build_match_expression(&mut self, scrutinee: &hlr::Expression, arms: &[hlr::MatchArm]) -> Result<mlr::ValId> {
        // build scrutinee (TODO lower to op instead of val? - match on a constant etc. does not really
        // make sense though)
        let (scrutinee_loc, scrutinee_stmt) = assign_to_new_loc!(self, self.lower_to_val(scrutinee)?);
        let scrutinee_place = self.insert_loc_place(scrutinee_loc)?;

        // store discriminant value in a temporary location
        let discriminant_place = self.insert_enum_discriminant_place(scrutinee_place)?;
        let discriminant = self.insert_copy_op(discriminant_place)?;

        // resolve equality function for discriminant comparisons once
        let eq_fn = {
            let i32 = self
                .ctxt
                .type_registry
                .get_primitive_type_id(types::PrimitiveType::Integer32)
                .unwrap();
            let eq_fn_id = self.resolve_operator(&hlr::BinaryOperator::Equal, (i32, i32))?;
            self.insert_function_op(eq_fn_id)?
        };

        let scrutinee_type_id = self.get_loc_type(&scrutinee_loc);
        let enum_def = self.get_enum_def(&scrutinee_type_id)?;
        let arm_indices = self.get_arm_indices(arms, enum_def, &scrutinee_type_id)?;

        // Now build nested mlr::Ifs from arm conditions and arm blocks
        // The iterative approach is used to avoid deep recursion for match expressions with many
        // arms at the cost of LocIds being unsorted in the resulting MLR

        // Start with the else block being the last arm's block (this fails if no arms are given;
        // we ignore that case for now)
        let mut else_block = self.build_arm_block(
            arms.last().unwrap(),
            &scrutinee_type_id,
            arm_indices.last().unwrap(),
            &scrutinee_place,
        )?;

        // Iterate over the remaining arms in reverse order, nesting Ifs
        for arm_index in arm_indices.into_iter().rev().skip(1) {
            let (arm_condition_loc, arm_condition_stmt) =
                assign_to_new_loc!(self, self.build_arm_condition(&arm_index, &eq_fn, &discriminant)?);
            let arm_condition = self.insert_copy_loc_op(arm_condition_loc)?;

            let then_block =
                self.build_arm_block(&arms[arm_index], &scrutinee_type_id, &arm_index, &scrutinee_place)?;
            let output = self.insert_if_val(arm_condition, then_block, else_block)?;

            else_block = self.insert_new_block_val(vec![arm_condition_stmt], output)?;
        }

        let output = else_block;
        let statements = vec![scrutinee_stmt];
        self.insert_new_block_val(statements, output)
    }

    fn build_statement(&mut self, stmt: &hlr::Statement) -> Result<mlr::StmtId> {
        use hlr::Statement::*;

        let stmt = match stmt {
            Let { name, value, .. } => self.build_let_statement(name, value)?,
            Expression(expression) => self.build_expression_statement(expression)?,
            Return(expression) => self.build_return_statement(expression.as_ref())?,
            Break => self.build_break_statement()?,
        };

        Ok(stmt)
    }

    fn build_let_statement(&mut self, name: &str, value: &hlr::Expression) -> Result<mlr::StmtId> {
        let (loc, stmt) = assign_to_new_loc!(self, self.lower_to_val(value)?);
        self.add_to_scope(name, loc);
        Ok(stmt)
    }

    fn build_expression_statement(&mut self, expression: &hlr::Expression) -> Result<mlr::StmtId> {
        let (_, stmt) = assign_to_new_loc!(self, self.lower_to_val(expression)?);
        Ok(stmt)
    }

    fn build_return_statement(&mut self, expression: Option<&hlr::Expression>) -> Result<mlr::StmtId> {
        let (return_val_loc, return_val_stmt) = match expression {
            Some(expression) => assign_to_new_loc!(self, self.lower_to_val(expression)?),
            None => assign_to_new_loc!(self, {
                let unit = self.insert_unit_op()?;
                self.insert_use_val(unit)?
            }),
        };
        let return_val_place = self.insert_loc_place(return_val_loc)?;
        let return_val = self.insert_use_place_val(return_val_place)?;

        let return_stmt = self.insert_return_stmt(return_val)?;
        let output = self.insert_unit_op()?;
        let output = self.insert_use_val(output)?;
        let block = self.insert_new_block_val(vec![return_val_stmt, return_stmt], output)?;

        let (_, block_stmt) = assign_to_new_loc!(self, block);

        Ok(block_stmt)
    }

    fn build_break_statement(&mut self) -> Result<mlr::StmtId> {
        self.insert_break_stmt()
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
}
