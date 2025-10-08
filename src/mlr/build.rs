use std::collections::{HashMap, VecDeque};

mod err;
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
    next_expr_id: mlr::ExprId,
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
            next_expr_id: mlr::ExprId(0),
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
                Some(expr) => self.build_expression(expr)?,
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

    fn build_expression(&mut self, expr: &hlr::Expression) -> Result<mlr::ExprId> {
        let expr = match expr {
            hlr::Expression::Literal(literal) => self.build_literal(literal)?,
            hlr::Expression::Variable(name) => self.build_variable(name)?,
            hlr::Expression::BinaryOp { left, operator, right } => self.build_binary_op(left, operator, right)?,
            hlr::Expression::Assignment { target, value } => self.build_assignment(target, value)?,
            hlr::Expression::FunctionCall { function, arguments } => self.build_function_call(function, arguments)?,
            hlr::Expression::StructInit { .. } => todo!("lowering of struct initializers"),
            hlr::Expression::If {
                condition,
                then_block,
                else_block,
            } => self.build_if(condition, then_block, else_block.as_ref())?,
            hlr::Expression::Loop { body } => self.build_loop(body)?,
            hlr::Expression::Block(block) => mlr::Expression::Block(self.build_block(block)?),
        };

        self.insert_expr(expr)
    }

    fn build_literal(&mut self, literal: &hlr::Literal) -> Result<mlr::Expression> {
        let expr = match literal {
            hlr::Literal::Int(n) => mlr::Expression::Constant(mlr::Constant::Int(*n)),
            hlr::Literal::Bool(b) => mlr::Expression::Constant(mlr::Constant::Bool(*b)),
        };
        Ok(expr)
    }

    fn build_variable(&mut self, name: &str) -> Result<mlr::Expression> {
        self.resolve_name(name)
    }

    fn build_binary_op(
        &mut self,
        left: &hlr::Expression,
        operator: &hlr::BinaryOperator,
        right: &hlr::Expression,
    ) -> Result<mlr::Expression> {
        let (left_loc, left_stmt) = assign_to_new_loc!(self, self.build_expression(left)?);

        let (right_loc, right_stmt) = assign_to_new_loc!(self, self.build_expression(right)?);

        let (op_loc, op_stmt) = assign_to_new_loc!(self, {
            // TODO resolve function based on operator (and argument types...)
            let fn_id = match operator {
                hlr::BinaryOperator::Add => self.ctxt.function_registry.get_function_by_name("add::<i32>").ok_or(
                    MlrBuilderError::MissingOperatorImpl {
                        name: "add::<i32>".to_string(),
                    },
                )?,
                hlr::BinaryOperator::Multiply => self.ctxt.function_registry.get_function_by_name("mul::<i32>").ok_or(
                    MlrBuilderError::MissingOperatorImpl {
                        name: "mul::<i32>".to_string(),
                    },
                )?,
                _ => todo!("implement builtin operators"),
            };

            let init = mlr::Expression::Function(fn_id);
            self.insert_expr(init)?
        });

        let (call_loc, call_stmt) = assign_to_new_loc!(self, {
            let init = mlr::Expression::Call {
                callable: op_loc,
                args: vec![left_loc, right_loc],
            };
            self.insert_expr(init)?
        });

        Ok(mlr::Expression::Block(mlr::Block {
            statements: vec![left_stmt, right_stmt, op_stmt, call_stmt],
            output: call_loc,
        }))
    }

    fn build_assignment(&mut self, target: &hlr::Expression, value: &hlr::Expression) -> Result<mlr::Expression> {
        let assign_stmt = {
            let assign_loc = match target {
                hlr::Expression::Variable(name) => self
                    .resolve_name_to_location(name)
                    .ok_or_else(|| MlrBuilderError::UnresolvableSymbol { name: name.to_string() })?,
                _ => unimplemented!("Only variables are supported as assignment targets."),
            };
            let value = self.build_expression(value)?;
            self.insert_assign_stmt(assign_loc, value)?
        };

        let (unit_loc, unit_stmt) = assign_to_new_loc!(self, self.create_unit_value()?);

        Ok(mlr::Expression::Block(mlr::Block {
            statements: vec![assign_stmt, unit_stmt],
            output: unit_loc,
        }))
    }

    fn build_function_call(
        &mut self,
        function: &hlr::Expression,
        arguments: &[hlr::Expression],
    ) -> Result<mlr::Expression> {
        let (function_loc, function_stmt) = assign_to_new_loc!(self, self.build_expression(function)?);

        let (arg_locs, arg_stmts): (Vec<_>, Vec<_>) = arguments
            .iter()
            .map(|arg| Ok(assign_to_new_loc!(self, self.build_expression(arg)?)))
            .process_results(|it| it.unzip())?;

        let (call_loc, call_stmt) = assign_to_new_loc!(self, {
            let init = mlr::Expression::Call {
                callable: function_loc,
                args: arg_locs,
            };
            self.insert_expr(init)?
        });

        let statements: Vec<_> = std::iter::once(function_stmt)
            .chain(arg_stmts)
            .chain(std::iter::once(call_stmt))
            .collect();

        Ok(mlr::Expression::Block(mlr::Block {
            statements,
            output: call_loc,
        }))
    }

    fn build_if(
        &mut self,
        condition: &hlr::Expression,
        then_block: &hlr::Block,
        else_block: Option<&hlr::Block>,
    ) -> Result<mlr::Expression> {
        let (cond_loc, cond_stmt) = assign_to_new_loc!(self, self.build_expression(condition)?);

        let (if_loc, if_stmt) = assign_to_new_loc!(self, {
            let init = mlr::Expression::If(mlr::If {
                condition: cond_loc,
                then_block: self.build_block(then_block)?,
                else_block: match else_block {
                    Some(block) => self.build_block(block)?,
                    None => self.create_unit_block()?,
                },
            });
            self.insert_expr(init)?
        });

        Ok(mlr::Expression::Block(mlr::Block {
            statements: vec![cond_stmt, if_stmt],
            output: if_loc,
        }))
    }

    fn build_loop(&mut self, body: &hlr::Block) -> Result<mlr::Expression> {
        let body = mlr::Expression::Block(self.build_block(body)?);
        let body = self.insert_expr(body)?;
        Ok(mlr::Expression::Loop { body })
    }

    fn build_statement(&mut self, stmt: &hlr::Statement) -> Result<mlr::StmtId> {
        let stmt = match stmt {
            hlr::Statement::Let { name, value, .. } => self.build_let_statement(name, value)?,
            hlr::Statement::Expression(expression) => self.build_expression_statement(expression)?,
            hlr::Statement::Return(expression) => self.build_return_statement(expression.as_ref())?,
            hlr::Statement::Break => todo!("break statements"),
        };
        Ok(stmt)
    }

    fn build_let_statement(&mut self, name: &str, value: &hlr::Expression) -> Result<mlr::StmtId> {
        let loc = self.get_next_loc_id();
        self.current_scope().vars.insert(name.to_string(), loc);
        let value = self.build_expression(value)?;
        self.insert_assign_stmt(loc, value)
    }

    fn build_expression_statement(&mut self, expression: &hlr::Expression) -> Result<mlr::StmtId> {
        let (_, stmt) = assign_to_new_loc!(self, self.build_expression(expression)?);
        Ok(stmt)
    }

    fn build_return_statement(&mut self, expression: Option<&hlr::Expression>) -> Result<mlr::StmtId> {
        let (expr_loc, expr_stmt) = match expression {
            Some(expression) => assign_to_new_loc!(self, self.build_expression(expression)?),
            None => assign_to_new_loc!(self, self.create_unit_value()?),
        };

        let return_stmt = self.insert_return_stmt(expr_loc)?;

        let block = mlr::Expression::Block(mlr::Block {
            statements: vec![expr_stmt, return_stmt],
            output: expr_loc,
        });
        let block = self.insert_expr(block)?;

        let (_, block_stmt) = assign_to_new_loc!(self, block);
        Ok(block_stmt)
    }
}
