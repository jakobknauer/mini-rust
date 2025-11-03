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

        let mut scope = Scope::new();
        for FunctionParameter { name, type_ } in parameters {
            let loc = self.get_next_loc_id();
            scope.vars.insert(name, loc);
            self.output.loc_types.insert(loc, type_);
            self.output.param_locs.push(loc);
        }
        self.scopes.push_back(scope);

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
        self.scopes.push_back(Scope::new());

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

        self.scopes.pop_back();

        Ok(mlr::Block {
            statements,
            output: output_loc,
        })
    }

    fn lower_to_val(&mut self, expr: &hlr::Expression) -> Result<mlr::ValId> {
        use hlr::Expression::*;

        let val = match expr {
            Literal(literal) => self.build_literal(literal)?,
            Variable(name) => self.build_variable(name)?,
            BinaryOp { left, operator, right } => self.build_binary_op(left, operator, right)?,
            Assignment { target, value } => self.build_assignment(target, value)?,
            FunctionCall { function, arguments } => self.build_function_call(function, arguments)?,
            StructExpr { struct_name, fields } => self.build_struct_or_enum_val(struct_name, fields)?,
            If {
                condition,
                then_block,
                else_block,
            } => self.build_if(condition, then_block, else_block.as_ref())?,
            Loop { body } => self.build_loop(body)?,
            Block(block) => mlr::Value::Block(self.build_block(block)?),
            FieldAccess { .. } => {
                let place = self.lower_to_place(expr)?;
                mlr::Value::Use(place)
            }
            Match { scrutinee, arms } => self.build_match_expression(scrutinee, arms)?,
        };

        self.insert_val(val)
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

    fn build_literal(&mut self, literal: &hlr::Literal) -> Result<mlr::Value> {
        use hlr::Literal::*;

        let val = match literal {
            Int(n) => mlr::Value::Constant(mlr::Constant::Int(*n)),
            Bool(b) => mlr::Value::Constant(mlr::Constant::Bool(*b)),
        };

        Ok(val)
    }

    fn build_variable(&mut self, name: &str) -> Result<mlr::Value> {
        self.resolve_name(name)
    }

    fn build_binary_op(
        &mut self,
        left: &hlr::Expression,
        operator: &hlr::BinaryOperator,
        right: &hlr::Expression,
    ) -> Result<mlr::Value> {
        let (left_loc, left_stmt) = assign_to_new_loc!(self, self.lower_to_val(left)?);

        let (right_loc, right_stmt) = assign_to_new_loc!(self, self.lower_to_val(right)?);

        let (op_loc, op_stmt) = assign_to_new_loc!(self, {
            let left_type = self
                .output
                .loc_types
                .get(&left_loc)
                .expect("type of left_loc should be registered");
            let right_type = self
                .output
                .loc_types
                .get(&right_loc)
                .expect("type of right_loc should be registered");
            let fn_id = self.resolve_operator(operator, (*left_type, *right_type))?;
            let init = mlr::Value::Function(fn_id);
            self.insert_val(init)?
        });

        let (call_loc, call_stmt) = assign_to_new_loc!(self, {
            let init = mlr::Value::Call {
                callable: op_loc,
                args: vec![left_loc, right_loc],
            };
            self.insert_val(init)?
        });

        Ok(mlr::Value::Block(mlr::Block {
            statements: vec![left_stmt, right_stmt, op_stmt, call_stmt],
            output: call_loc,
        }))
    }

    fn build_assignment(&mut self, target: &hlr::Expression, value: &hlr::Expression) -> Result<mlr::Value> {
        let assign_stmt = {
            let loc = self.lower_to_place(target)?;
            let value = self.lower_to_val(value)?;
            self.insert_assign_stmt(loc, value)?
        };

        let (unit_loc, unit_stmt) = assign_to_new_loc!(self, self.create_unit_value()?);

        Ok(mlr::Value::Block(mlr::Block {
            statements: vec![assign_stmt, unit_stmt],
            output: unit_loc,
        }))
    }

    fn build_function_call(&mut self, function: &hlr::Expression, arguments: &[hlr::Expression]) -> Result<mlr::Value> {
        let (function_loc, function_stmt) = assign_to_new_loc!(self, self.lower_to_val(function)?);

        let (arg_locs, arg_stmts): (Vec<_>, Vec<_>) = arguments
            .iter()
            .map(|arg| Ok(assign_to_new_loc!(self, self.lower_to_val(arg)?)))
            .process_results(|it| it.unzip())?;

        let (call_loc, call_stmt) = assign_to_new_loc!(self, {
            let init = mlr::Value::Call {
                callable: function_loc,
                args: arg_locs,
            };
            self.insert_val(init)?
        });

        let statements: Vec<_> = std::iter::once(function_stmt)
            .chain(arg_stmts)
            .chain(std::iter::once(call_stmt))
            .collect();

        Ok(mlr::Value::Block(mlr::Block {
            statements,
            output: call_loc,
        }))
    }

    fn build_if(
        &mut self,
        condition: &hlr::Expression,
        then_block: &hlr::Block,
        else_block: Option<&hlr::Block>,
    ) -> Result<mlr::Value> {
        let (cond_loc, cond_stmt) = assign_to_new_loc!(self, self.lower_to_val(condition)?);

        let (if_loc, if_stmt) = assign_to_new_loc!(self, {
            let init = mlr::Value::If(mlr::If {
                condition: cond_loc,
                then_block: self.build_block(then_block)?,
                else_block: match else_block {
                    Some(block) => self.build_block(block)?,
                    None => self.create_unit_block()?,
                },
            });
            self.insert_val(init)?
        });

        Ok(mlr::Value::Block(mlr::Block {
            statements: vec![cond_stmt, if_stmt],
            output: if_loc,
        }))
    }

    fn build_loop(&mut self, body: &hlr::Block) -> Result<mlr::Value> {
        let body = self.build_block(body)?;
        Ok(mlr::Value::Loop { body })
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
        self.current_scope().vars.insert(name.to_owned(), loc);
        Ok(stmt)
    }

    fn build_expression_statement(&mut self, expression: &hlr::Expression) -> Result<mlr::StmtId> {
        let (_, stmt) = assign_to_new_loc!(self, self.lower_to_val(expression)?);
        Ok(stmt)
    }

    fn build_return_statement(&mut self, expression: Option<&hlr::Expression>) -> Result<mlr::StmtId> {
        let (val_loc, val_stmt) = match expression {
            Some(expression) => assign_to_new_loc!(self, self.lower_to_val(expression)?),
            None => assign_to_new_loc!(self, self.create_unit_value()?),
        };

        let return_stmt = self.insert_return_stmt(val_loc)?;

        let block = mlr::Value::Block(mlr::Block {
            statements: vec![val_stmt, return_stmt],
            output: val_loc,
        });
        let block = self.insert_val(block)?;

        let (_, block_stmt) = assign_to_new_loc!(self, block);
        Ok(block_stmt)
    }

    fn build_break_statement(&mut self) -> Result<mlr::StmtId> {
        self.insert_stmt(mlr::Statement::Break)
    }

    fn build_struct_or_enum_val(
        &mut self,
        struct_name: &str,
        fields: &[(String, hlr::Expression)],
    ) -> Result<mlr::Value> {
        if let Some(type_id) = self.ctxt.type_registry.get_type_id_by_name(struct_name) {
            self.build_struct_val(&type_id, fields)
        } else if let Some((type_id, enum_id, variant_index)) = self.try_resolve_enum_variant(struct_name) {
            self.build_enum_val(&type_id, &enum_id, &variant_index, fields)
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
    ) -> Result<mlr::Value> {
        let (struct_val_loc, struct_val_stmt) = assign_to_new_loc!(self, {
            let struct_val = mlr::Value::Empty { type_id: *type_id };
            self.insert_val(struct_val)?
        });
        let struct_val_place = self.insert_place(mlr::Place::Local(struct_val_loc))?;

        let field_init_stmts = self.build_struct_field_init_stmts(type_id, fields, &struct_val_place)?;

        let statements = std::iter::once(struct_val_stmt).chain(field_init_stmts).collect();

        Ok(mlr::Value::Block(mlr::Block {
            statements,
            output: struct_val_loc,
        }))
    }

    fn build_enum_val(
        &mut self,
        type_id: &types::TypeId,
        enum_id: &types::EnumId,
        variant_index: &usize,
        fields: &[(String, hlr::Expression)],
    ) -> std::result::Result<mlr::Value, MlrBuilderError> {
        // Create empty enum value
        let (enum_val_loc, enum_val_stmt) = assign_to_new_loc!(self, {
            let enum_val = mlr::Value::Empty { type_id: *type_id };
            self.insert_val(enum_val)?
        });
        let base_place = self.insert_place(mlr::Place::Local(enum_val_loc))?;

        // Fill discriminant
        let discriminant_place = self.insert_place(mlr::Place::EnumDiscriminant {
            base: base_place,
            enum_id: *enum_id,
        })?;
        let discriminant_value = {
            let val = mlr::Value::Constant(mlr::Constant::Int(*variant_index as i64));
            self.insert_val(val)?
        };
        let discriminant_stmt = self.insert_assign_stmt(discriminant_place, discriminant_value)?;

        // Fill fields
        let variant_place = self.insert_place(mlr::Place::ProjectToVariant {
            base: base_place,
            enum_id: *enum_id,
            variant_index: *variant_index,
        })?;
        let variant_type_id = self
            .ctxt
            .type_registry
            .get_enum_variant(enum_id, variant_index)
            .expect("variant index should be valid")
            .type_id;
        let field_init_stmts = self.build_struct_field_init_stmts(&variant_type_id, fields, &variant_place)?;

        // Build final block
        let statements = std::iter::once(enum_val_stmt)
            .chain(std::iter::once(discriminant_stmt))
            .chain(field_init_stmts)
            .collect();

        Ok(mlr::Value::Block(mlr::Block {
            statements,
            output: enum_val_loc,
        }))
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

        let base_type = self
            .ctxt
            .type_registry
            .get_type_by_id(base_type_id)
            .expect("base type should be registered");

        let types::Type::NamedType(_, types::NamedType::Struct(struct_id)) = base_type else {
            return Err(MlrBuilderError::TypeError(TypeError::NotAStruct {
                type_id: *base_type_id,
            }));
        };

        let struct_def = self
            .ctxt
            .type_registry
            .get_struct_definition(struct_id)
            .expect("struct definition should be registered");

        let field_index = struct_def
            .fields
            .iter()
            .position(|struct_field| struct_field.name == field_name)
            .ok_or(MlrBuilderError::TypeError(TypeError::NotAStructField {
                type_id: *base_type_id,
                field_name: field_name.to_string(),
            }))?;

        Ok(mlr::Place::FieldAccess {
            base,
            struct_id: *struct_id,
            field_index,
        })
    }

    fn build_match_expression(&mut self, scrutinee: &hlr::Expression, arms: &[hlr::MatchArm]) -> Result<mlr::Value> {
        let mut statements: Vec<mlr::StmtId> = Vec::new();

        let (scrutinee_loc, scrutinee_stmt) = assign_to_new_loc!(self, self.lower_to_val(scrutinee)?);
        statements.push(scrutinee_stmt);
        let scrutinee_place = self.insert_place(mlr::Place::Local(scrutinee_loc))?;

        // get scrutinee type
        let scrutinee_type_id = self
            .output
            .loc_types
            .get(&scrutinee_loc)
            .expect("type of scrutinee_loc should be registered");
        let scrutinee_type = self
            .ctxt
            .type_registry
            .get_type_by_id(scrutinee_type_id)
            .expect("scrutinee type should be registered");
        let &types::Type::NamedType(_, types::NamedType::Enum(enum_id)) = scrutinee_type else {
            return Err(MlrBuilderError::TypeError(TypeError::NotAnEnum {
                type_id: *scrutinee_type_id,
            }));
        };
        let enum_def = self
            .ctxt
            .type_registry
            .get_enum_definition(&enum_id)
            .expect("enum definition should be registered");

        // map arms to indices
        let arm_indices = self.get_arm_indices(arms, enum_def, scrutinee_type_id)?;

        // Build condition for each arm
        let (discriminant_loc, discriminant_stmt) = assign_to_new_loc!(self, {
            let discriminant_place = self.insert_place(mlr::Place::EnumDiscriminant {
                base: scrutinee_place,
                enum_id,
            })?;
            let val = mlr::Value::Use(discriminant_place);
            self.insert_val(val)?
        });
        statements.push(discriminant_stmt);

        let (eq_fn_loc, eq_fn_statement) = assign_to_new_loc!(self, {
            let i32 = self
                .ctxt
                .type_registry
                .get_primitive_type_id(types::PrimitiveType::Integer32)
                .unwrap();
            let eq_fn_id = self.resolve_operator(&hlr::BinaryOperator::Equal, (i32, i32))?;
            let val = mlr::Value::Function(eq_fn_id);
            self.insert_val(val)?
        });
        statements.push(eq_fn_statement);

        let mut arm_conditions = self.build_arm_conditions(&arm_indices, &eq_fn_loc, &discriminant_loc)?;
        arm_conditions.pop();

        let (arm_condition_locs, arm_condition_stmts): (Vec<_>, Vec<_>) = arm_conditions
            .into_iter()
            .map(|val_id| {
                let (loc, stmt) = assign_to_new_loc!(self, val_id);
                Ok((loc, stmt))
            })
            .process_results(|it| it.unzip())?;
        statements.extend(arm_condition_stmts);

        // Build match arms
        let mut arm_blocks: Vec<mlr::Block> = arms
            .iter()
            .zip(arm_indices.iter())
            .map(|(arm, variant_index)| self.build_arm_block(arm, &enum_id, variant_index, &scrutinee_place))
            .collect::<Result<_>>()?;

        // Now build nested mlr::Ifs from arm conditions and arm blocks
        let mut current_else_block: Option<mlr::Block> = arm_blocks.pop();
        for (arm_block, arm_condition_loc) in arm_blocks.into_iter().zip(arm_condition_locs.into_iter()).rev() {
            let if_value = mlr::Value::If(mlr::If {
                condition: arm_condition_loc,
                then_block: arm_block,
                else_block: current_else_block.unwrap(),
            });
            let if_loc = self.insert_val(if_value)?;
            let (new_block_loc, new_block_stmt) = assign_to_new_loc!(self, if_loc);
            current_else_block = Some(mlr::Block {
                statements: vec![new_block_stmt],
                output: new_block_loc,
            });
        }

        let final_block = current_else_block.expect("there should be at least one arm");
        let (final_block_loc, final_block_stmt) = assign_to_new_loc!(self, {
            let val = mlr::Value::Block(final_block);
            self.insert_val(val)?
        });
        statements.push(final_block_stmt);

        Ok(mlr::Value::Block(mlr::Block {
            statements,
            output: final_block_loc,
        }))
    }
}
