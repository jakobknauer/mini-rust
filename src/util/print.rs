use std::io::Write;

use crate::{
    ctxt::{
        self,
        functions::{FnId, FunctionSignature},
    },
    mlr::*,
};

pub fn print_mlr<W: Write>(fn_id: FnId, ctxt: &ctxt::Ctxt, writer: &mut W) -> Result<(), std::io::Error> {
    let mut printer = MlrPrinter {
        fn_id,
        mlr: ctxt.function_registry.get_function_mlr(fn_id),
        signature: ctxt.function_registry.get_signature_by_id(fn_id),
        ctxt,
        indent_level: 0,
        writer,
    };
    printer.print_mlr()
}

struct MlrPrinter<'a, W: Write> {
    fn_id: FnId,
    mlr: Option<&'a Mlr>,
    signature: Option<&'a FunctionSignature>,
    ctxt: &'a ctxt::Ctxt,
    indent_level: usize,
    writer: &'a mut W,
}

const INDENT: &str = "    ";

impl<'a, W: Write> MlrPrinter<'a, W> {
    fn print_mlr(&mut self) -> Result<(), std::io::Error> {
        self.print_signature()?;
        write!(self.writer, " ")?;

        if let Some(mlr) = self.mlr {
            self.print_block(&mlr.body)
        } else {
            write!(self.writer, "<undefined definition for fn id {}>", self.fn_id.0)
        }
    }

    fn print_signature(&mut self) -> Result<(), std::io::Error> {
        let Some(signature) = self.signature else {
            return write!(self.writer, "<undefined signature for fn id {}>", self.fn_id.0);
        };

        write!(self.writer, "fn {}(", signature.name)?;
        for (i, param) in signature.parameters.iter().enumerate() {
            if i > 0 {
                write!(self.writer, ", ")?;
            }
            let type_name = self.ctxt.type_registry.get_type_name_by_id(param.type_);
            match type_name {
                Some(name) => write!(self.writer, "{}: {}", param.name, name)?,
                None => write!(self.writer, "<unknown type id {}>", param.type_.0)?,
            };
        }
        write!(self.writer, ") -> ")?;
        let return_type_name = self.ctxt.type_registry.get_type_name_by_id(signature.return_type);
        match return_type_name {
            Some(name) => write!(self.writer, "{}", name),
            None => write!(self.writer, "<unknown type id {}>", signature.return_type.0),
        }
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
        let stmt = &self.mlr.expect("self.mlr should not be empty").statements.get(&stmt_id);

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
        let expr = &self
            .mlr
            .expect("self.mlr should not be empty")
            .expressions
            .get(&expr_id);

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
                    if let Some(func) = self.ctxt.function_registry.get_signature_by_id(*fn_id) {
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
