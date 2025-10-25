use std::collections::{HashMap, VecDeque};

mod err;
mod ops;
mod type_util;
mod util;

#[macro_use]
mod macros;

use itertools::Itertools;

use crate::{
    ctxt::{
        self,
        functions::{FnId, FunctionParameter, FunctionSignature},
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
            StructExpr { struct_name, fields } => self.build_struct_val(struct_name, fields)?,
            If {
                condition,
                then_block,
                else_block,
            } => self.build_if(condition, then_block, else_block.as_ref())?,
            Loop { body } => self.build_loop(body)?,
            Block(block) => mlr::Value::Block(self.build_block(block)?),
        };

        self.insert_val(val)
    }

    fn lower_to_place(&mut self, expr: &hlr::Expression) -> Result<mlr::Place> {
        use hlr::Expression::*;

        let place = match expr {
            Variable(name) => {
                let loc = self
                    .resolve_name_to_location(name)
                    .ok_or_else(|| MlrBuilderError::UnresolvableSymbol { name: name.to_string() })?;

                mlr::Place::Local(loc)
            }
            _ => panic!("Only variables are supported as places."),
        };

        Ok(place)
    }

    fn build_literal(&mut self, literal: &hlr::Literal) -> Result<mlr::Value> {
        let val = match literal {
            hlr::Literal::Int(n) => mlr::Value::Constant(mlr::Constant::Int(*n)),
            hlr::Literal::Bool(b) => mlr::Value::Constant(mlr::Constant::Bool(*b)),
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
        let stmt = match stmt {
            hlr::Statement::Let { name, value, .. } => self.build_let_statement(name, value)?,
            hlr::Statement::Expression(expression) => self.build_expression_statement(expression)?,
            hlr::Statement::Return(expression) => self.build_return_statement(expression.as_ref())?,
            hlr::Statement::Break => self.build_break_statement()?,
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

    fn build_struct_val(&mut self, struct_name: &str, fields: &[(String, hlr::Expression)]) -> Result<mlr::Value> {
        let type_id = self
            .ctxt
            .type_registry
            .get_type_id_by_name(struct_name)
            .ok_or(MlrBuilderError::TypeError(TypeError::UnresolvableTypeName {
                struct_name: struct_name.to_string(),
            }))?;

        let (field_init_locs, field_init_stmts): (Vec<_>, Vec<_>) = fields
            .iter()
            .map(|(field_name, expr)| {
                let (field_loc, field_stmt) = assign_to_new_loc!(self, self.lower_to_val(expr)?);
                Ok(((field_name.clone(), field_loc), field_stmt))
            })
            .process_results(|it| it.unzip())?;

        let (struct_val_loc, struct_val_stmt) = assign_to_new_loc!(self, {
            let struct_val = mlr::Value::Struct {
                type_id,
                field_initializers: field_init_locs,
            };
            self.insert_val(struct_val)?
        });

        let statements: Vec<_> = field_init_stmts
            .into_iter()
            .chain(std::iter::once(struct_val_stmt))
            .collect();

        Ok(mlr::Value::Block(mlr::Block {
            statements,
            output: struct_val_loc,
        }))
    }
}
