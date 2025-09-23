use std::io::Write;

use crate::{
    context::{function_registry::FunctionRegistry, type_registry::TypeRegistry},
    mlr::*,
};

pub fn print_mlr<W: Write>(
    mlr: &Mlr,
    type_registry: &TypeRegistry,
    function_registry: &FunctionRegistry,
    writer: &mut W,
) -> Result<(), std::io::Error> {
    let mut printer = MlrPrinter {
        mlr,
        type_registry,
        function_registry,
        indent_level: 0,
        writer,
    };
    printer.print_mlr()
}

struct MlrPrinter<'a, W: Write> {
    mlr: &'a Mlr,
    type_registry: &'a TypeRegistry,
    function_registry: &'a FunctionRegistry,
    indent_level: usize,
    writer: &'a mut W,
}

const INDENT: &str = "    ";

impl<'a, W: Write> MlrPrinter<'a, W> {
    fn print_mlr(&mut self) -> Result<(), std::io::Error> {
        self.print_block(&self.mlr.body)
    }

    fn print_block(&mut self, block: &Block) -> Result<(), std::io::Error> {
        writeln!(self.writer, "{{")?;
        self.indent_level += 1;

        for stmt_id in &block.statements {
            self.print_statement(*stmt_id)?;
        }

        self.indent()?;
        writeln!(self.writer, "{}", block.output)?;

        self.indent_level -= 1;
        self.indent()?;
        write!(self.writer, "}}")
    }

    fn print_statement(&mut self, stmt_id: StmtId) -> Result<(), std::io::Error> {
        let stmt = &self.mlr.statements.get(&stmt_id);
        match stmt {
            Some(stmt) => match stmt {
                Statement::Assign { loc, value } => {
                    self.indent()?;
                    write!(self.writer, "let {} = ", loc)?;
                    self.print_expression(*value)?;
                    writeln!(self.writer, ";")
                }
                Statement::Return { value } => {
                    self.indent()?;
                    writeln!(self.writer, "return {};", value)
                }
            },
            None => writeln!(self.writer, "<unknown stmt id {}>", stmt_id.0),
        }
    }

    fn print_expression(&mut self, expr_id: ExprId) -> Result<(), std::io::Error> {
        let expr = &self.mlr.expressions.get(&expr_id);
        match expr {
            Some(expr) => match expr {
                Expression::Block(block) => self.print_block(block),
                Expression::Constant(constant) => match constant {
                    Constant::Int(i) => write!(self.writer, "const {}", i),
                    Constant::Bool(b) => write!(self.writer, "const {}", b),
                    Constant::Unit => write!(self.writer, "const ()"),
                },
                Expression::Var(loc) => {
                    write!(self.writer, "copy {}", loc)
                }
                Expression::AddressOf(loc) => {
                    write!(self.writer, "&{}", loc)
                }
                Expression::Call { callable, args } => {
                    write!(self.writer, "call {}(", callable)?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(self.writer, ", ")?;
                        }
                        write!(self.writer, "{}", arg)?;
                    }
                    write!(self.writer, ")")
                }
                Expression::Function(fn_id) => {
                    if let Some(func) = self.function_registry.get_signature_by_id(*fn_id) {
                        write!(self.writer, "fn {}()", func.name)
                    } else {
                        write!(self.writer, "<unknown fn id {}>", fn_id.0)
                    }
                }
                Expression::If {
                    condition,
                    then_block,
                    else_block,
                } => {
                    write!(self.writer, "if {} ", condition)?;
                    self.print_block(then_block)?;
                    self.indent()?;
                    write!(self.writer, "else ")?;
                    self.print_block(else_block)
                }
                Expression::Loop { body } => {
                    write!(self.writer, "loop ")?;
                    self.print_expression(*body)
                }
            },
            None => write!(self.writer, "<unknown expr id {}>", expr_id.0),
        }
    }

    fn indent(&mut self) -> Result<(), std::io::Error> {
        for _ in 0..self.indent_level {
            write!(self.writer, "{}", INDENT)?;
        }
        Ok(())
    }
}
