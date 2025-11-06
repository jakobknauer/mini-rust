use std::collections::{HashMap, VecDeque};

mod err;
mod ops;
mod pattern_util;
mod type_util;
mod util;

#[macro_use]
mod macros;

use itertools::Itertools;

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

        let output_type = self.output.loc_types.get(&body.output).unwrap();
        if !self.ctxt.type_registry.types_equal(&return_type, output_type) {
            return TypeError::ReturnTypeMismatch {
                expected: return_type,
                actual: *output_type,
            }
            .into();
        };

        self.output.body = body;
        Ok(self.output)
    }

    pub fn build_block(&mut self, block: &hlr::Block) -> Result<mlr::Block> {
        self.push_scope();

        let mut statements: Vec<_> = block
            .statements
            .iter()
            .map(|stmt| self.build_statement(stmt))
            .collect::<Result<_>>()?;

        let (output_loc, output_stmt) = assign_to_new_loc!(
            self,
            match &block.return_expression {
                Some(expr) => self.lower_to_val(expr)?,
                None => self.create_unit_value()?,
            }
        );

        statements.push(output_stmt);

        self.pop_scope();

        Ok(mlr::Block {
            statements,
            output: output_loc,
        })
    }

    fn lower_to_val(&mut self, expr: &hlr::Expression) -> Result<mlr::ValId> {
        use hlr::Expression::*;

        match expr {
            Literal(literal) => self.build_literal(literal),
            Variable(name) => self.build_variable(name),
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
            Block(block) => {
                let block = mlr::Value::Block(self.build_block(block)?);
                self.insert_val(block)
            }
            FieldAccess { .. } => {
                let place = self.lower_to_place(expr)?;
                self.insert_use_val(place)
            }
            Match { scrutinee, arms } => self.build_match_expression(scrutinee, arms),
        }
    }

    fn lower_to_place(&mut self, expr: &hlr::Expression) -> Result<mlr::PlaceId> {
        use hlr::Expression::*;

        let place = match expr {
            Variable(name) => self.lower_var_to_place(name)?,
            FieldAccess { base, field_name } => self.lower_field_access_to_place(base, field_name)?,
            _ => panic!("Only variables and field access expressions are supported as places."),
        };

        self.insert_place(place)
    }

    fn build_literal(&mut self, literal: &hlr::Literal) -> Result<mlr::ValId> {
        use hlr::Literal::*;

        let val = match literal {
            Int(n) => mlr::Value::Constant(mlr::Constant::Int(*n)),
            Bool(b) => mlr::Value::Constant(mlr::Constant::Bool(*b)),
        };

        self.insert_val(val)
    }

    fn build_variable(&mut self, name: &str) -> Result<mlr::ValId> {
        self.resolve_name(name).and_then(|val| self.insert_val(val))
    }

    fn build_binary_op(
        &mut self,
        left: &hlr::Expression,
        operator: &hlr::BinaryOperator,
        right: &hlr::Expression,
    ) -> Result<mlr::ValId> {
        let (left_loc, left_stmt) = assign_to_new_loc!(self, self.lower_to_val(left)?);

        let (right_loc, right_stmt) = assign_to_new_loc!(self, self.lower_to_val(right)?);

        let (op_loc, op_stmt) = assign_to_new_loc!(self, {
            let left_type = self.get_loc_type(&left_loc);
            let right_type = self.get_loc_type(&right_loc);
            let fn_id = self.resolve_operator(operator, (left_type, right_type))?;
            self.insert_function_val(fn_id)?
        });

        let (call_loc, call_stmt) = assign_to_new_loc!(self, self.insert_call_val(op_loc, vec![left_loc, right_loc])?);

        self.insert_block_val(vec![left_stmt, right_stmt, op_stmt, call_stmt], call_loc)
    }

    fn build_assignment(&mut self, target: &hlr::Expression, value: &hlr::Expression) -> Result<mlr::ValId> {
        let assign_stmt = {
            let loc = self.lower_to_place(target)?;
            let value = self.lower_to_val(value)?;
            self.insert_assign_stmt(loc, value)?
        };

        let (unit_loc, unit_stmt) = assign_to_new_loc!(self, self.create_unit_value()?);

        self.insert_block_val(vec![assign_stmt, unit_stmt], unit_loc)
    }

    fn build_function_call(&mut self, function: &hlr::Expression, arguments: &[hlr::Expression]) -> Result<mlr::ValId> {
        let (function_loc, function_stmt) = assign_to_new_loc!(self, self.lower_to_val(function)?);

        let (arg_locs, arg_stmts): (Vec<_>, Vec<_>) = arguments
            .iter()
            .map(|arg| Ok(assign_to_new_loc!(self, self.lower_to_val(arg)?)))
            .process_results(|it| it.unzip())?;

        let (call_loc, call_stmt) = assign_to_new_loc!(self, self.insert_call_val(function_loc, arg_locs)?);

        let statements: Vec<_> = std::iter::once(function_stmt)
            .chain(arg_stmts)
            .chain(std::iter::once(call_stmt))
            .collect();

        self.insert_block_val(statements, call_loc)
    }

    fn build_if(
        &mut self,
        condition: &hlr::Expression,
        then_block: &hlr::Block,
        else_block: Option<&hlr::Block>,
    ) -> Result<mlr::ValId> {
        let (cond_loc, cond_stmt) = assign_to_new_loc!(self, self.lower_to_val(condition)?);

        let (if_loc, if_stmt) = assign_to_new_loc!(self, {
            let then_block = self.build_block(then_block)?;
            let else_block = match else_block {
                Some(block) => self.build_block(block),
                None => self.create_unit_block(),
            }?;
            self.insert_if_val(cond_loc, then_block, else_block)?
        });

        self.insert_block_val(vec![cond_stmt, if_stmt], if_loc)
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
        let struct_val_place = self.insert_place(mlr::Place::Local(struct_val_loc))?;

        let field_init_stmts = self.build_struct_field_init_stmts(type_id, fields, &struct_val_place)?;

        let statements = std::iter::once(struct_val_stmt).chain(field_init_stmts).collect();

        self.insert_block_val(statements, struct_val_loc)
    }

    fn build_enum_val(
        &mut self,
        type_id: &types::TypeId,
        variant_index: &usize,
        fields: &[(String, hlr::Expression)],
    ) -> Result<mlr::ValId> {
        // Create empty enum value
        let (enum_val_loc, enum_val_stmt) = assign_to_new_loc!(self, self.insert_empty_val(*type_id)?);
        let base_place = self.insert_place(mlr::Place::Local(enum_val_loc))?;

        // Fill discriminant
        let discriminant_place = self.insert_place(mlr::Place::EnumDiscriminant { base: base_place })?;
        let discriminant_value = self.insert_int_val(*variant_index as i64)?;
        let discriminant_stmt = self.insert_assign_stmt(discriminant_place, discriminant_value)?;

        // Fill fields
        let variant_place = self.insert_place(mlr::Place::ProjectToVariant {
            base: base_place,
            variant_index: *variant_index,
        })?;
        let enum_def = self.get_enum_def(type_id)?;
        let variant_type_id = enum_def
            .variants
            .get(*variant_index)
            .expect("variant index should be valid")
            .type_id;
        let field_init_stmts = self.build_struct_field_init_stmts(&variant_type_id, fields, &variant_place)?;

        // Build final block
        let statements = std::iter::once(enum_val_stmt)
            .chain(std::iter::once(discriminant_stmt))
            .chain(field_init_stmts)
            .collect();

        self.insert_block_val(statements, enum_val_loc)
    }

    fn build_match_expression(&mut self, scrutinee: &hlr::Expression, arms: &[hlr::MatchArm]) -> Result<mlr::ValId> {
        // build scrutinee value
        let (scrutinee_loc, scrutinee_stmt) = assign_to_new_loc!(self, self.lower_to_val(scrutinee)?);
        let scrutinee_place = self.insert_place(mlr::Place::Local(scrutinee_loc))?;

        // get scrutinee type
        let scrutinee_type_id = self.get_loc_type(&scrutinee_loc);
        let enum_def = self.get_enum_def(&scrutinee_type_id)?;

        // map match arms to enum variant indices
        let arm_indices = self.get_arm_indices(arms, enum_def, &scrutinee_type_id)?;

        // store discriminant value in a temporary location
        let (discriminant_loc, discriminant_stmt) = assign_to_new_loc!(self, {
            let discriminant_place = self.insert_place(mlr::Place::EnumDiscriminant { base: scrutinee_place })?;
            self.insert_use_val(discriminant_place)?
        });

        // resolve equality function for discriminant comparisons once
        let (eq_fn_loc, eq_fn_statement) = assign_to_new_loc!(self, {
            let i32 = self
                .ctxt
                .type_registry
                .get_primitive_type_id(types::PrimitiveType::Integer32)
                .unwrap();
            let eq_fn_id = self.resolve_operator(&hlr::BinaryOperator::Equal, (i32, i32))?;
            self.insert_function_val(eq_fn_id)?
        });

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
            let (arm_condition_loc, arm_condition_stmt) = assign_to_new_loc!(
                self,
                self.build_arm_condition(&arm_index, &eq_fn_loc, &discriminant_loc)?
            );

            let (new_block_loc, new_block_stmt) = assign_to_new_loc!(self, {
                let then_block =
                    self.build_arm_block(&arms[arm_index], &scrutinee_type_id, &arm_index, &scrutinee_place)?;
                self.insert_if_val(arm_condition_loc, then_block, else_block)?
            });

            else_block = mlr::Block {
                statements: vec![arm_condition_stmt, new_block_stmt],
                output: new_block_loc,
            };
        }

        // Turn the final else_block into a value
        let (final_block_loc, final_block_stmt) = assign_to_new_loc!(self, {
            let val = mlr::Value::Block(else_block);
            self.insert_val(val)?
        });

        let statements = vec![scrutinee_stmt, discriminant_stmt, eq_fn_statement, final_block_stmt];
        self.insert_block_val(statements, final_block_loc)
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
            None => assign_to_new_loc!(self, self.create_unit_value()?),
        };

        let return_stmt = self.insert_return_stmt(return_val_loc)?;
        let block = self.insert_block_val(vec![return_val_stmt, return_stmt], return_val_loc)?;
        let (_, block_stmt) = assign_to_new_loc!(self, block);

        Ok(block_stmt)
    }

    fn build_break_statement(&mut self) -> Result<mlr::StmtId> {
        self.insert_break_stmt()
    }

    fn lower_var_to_place(&self, name: &str) -> Result<mlr::Place> {
        let loc = self
            .resolve_name_to_location(name)
            .ok_or_else(|| MlrBuilderError::UnresolvableSymbol { name: name.to_string() })?;

        Ok(mlr::Place::Local(loc))
    }

    fn lower_field_access_to_place(&mut self, base: &hlr::Expression, field_name: &str) -> Result<mlr::Place> {
        // TODO allow general expressions as base (by lowering to val and then creating a
        // temporary place). This however requires a better builder infrastructure, so we
        // can here at this point create temporary variables/places in the current scope.
        let base = self.lower_to_place(base)?;

        let base_type_id = self
            .output
            .place_types
            .get(&base)
            .expect("type of base place should be registered");

        let struct_def = self.get_struct_def(base_type_id)?;

        let field_index = struct_def
            .fields
            .iter()
            .position(|struct_field| struct_field.name == field_name)
            .ok_or(MlrBuilderError::TypeError(TypeError::NotAStructField {
                type_id: *base_type_id,
                field_name: field_name.to_string(),
            }))?;

        Ok(mlr::Place::FieldAccess { base, field_index })
    }
}
