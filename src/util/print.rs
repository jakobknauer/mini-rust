use std::io::Write;

use crate::{
    ctxt::{
        self,
        functions::{FnId, FunctionSignature},
    },
    mlr,
};

pub fn print_mlr<W: Write>(fn_id: &FnId, ctxt: &ctxt::Ctxt, writer: &mut W) -> Result<(), std::io::Error> {
    let mut printer = MlrPrinter {
        fn_id: *fn_id,
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
    mlr: Option<&'a mlr::Mlr>,
    signature: Option<&'a FunctionSignature>,
    ctxt: &'a ctxt::Ctxt,
    indent_level: usize,
    writer: &'a mut W,
}

const INDENT: &str = "    ";

impl<'a, W: Write> MlrPrinter<'a, W> {
    fn print_mlr(&mut self) -> Result<(), std::io::Error> {
        self.print_signature()?;

        if let Some(mlr) = self.mlr {
            write!(self.writer, " ")?;
            self.print_val(&mlr.body)
        } else {
            write!(self.writer, ";")
        }
    }

    fn print_signature(&mut self) -> Result<(), std::io::Error> {
        let Some(signature) = self.signature else {
            return write!(self.writer, "<signature for fn id {}>", self.fn_id.0);
        };

        write!(self.writer, "fn {}(", signature.name)?;
        for (i, param) in signature.parameters.iter().enumerate() {
            if i > 0 {
                write!(self.writer, ", ")?;
            }
            let param_type = self.ctxt.type_registry.get_string_rep(&param.type_);
            write!(self.writer, "{}: {}", param.name, param_type)?;
        }
        write!(self.writer, ") -> ")?;

        let return_type = self.ctxt.type_registry.get_string_rep(&signature.return_type);
        write!(self.writer, "{}", return_type)
    }

    fn print_block(&mut self, statements: &[mlr::StmtId], output: &mlr::ValId) -> Result<(), std::io::Error> {
        writeln!(self.writer, "{{")?;
        self.indent_level += 1;

        for stmt_id in statements {
            self.print_statement(stmt_id)?;
        }

        self.indent()?;
        self.print_val(output)?;
        writeln!(self.writer)?;

        self.indent_level -= 1;
        self.indent()?;
        write!(self.writer, "}}")
    }

    fn print_statement(&mut self, stmt_id: &mlr::StmtId) -> Result<(), std::io::Error> {
        use mlr::Statement::*;

        let stmt = &self.mlr.expect("self.mlr should not be empty").stmts.get(stmt_id);

        match stmt {
            Some(stmt) => match stmt {
                Assign { place, value } => {
                    self.indent()?;
                    let place_type = self
                        .mlr
                        .expect("self.mlr should not be empty")
                        .place_types
                        .get(place)
                        .expect("type of place should be known");
                    let type_name = self.ctxt.type_registry.get_string_rep(place_type);

                    self.print_place(place)?;
                    write!(self.writer, ": {} = ", type_name)?;
                    self.print_val(value)?;
                    writeln!(self.writer, ";")
                }
                Return { value } => {
                    self.indent()?;
                    writeln!(self.writer, "return {};", value)
                }
                Break => {
                    self.indent()?;
                    writeln!(self.writer, "break;")
                }
            },
            None => writeln!(self.writer, "<stmt id {}>", stmt_id.0),
        }
    }

    fn print_val(&mut self, val_id: &mlr::ValId) -> Result<(), std::io::Error> {
        use mlr::{Constant::*, Value::*};

        let val = &self.mlr.expect("self.mlr should not be empty").vals.get(val_id);

        match val {
            Some(val) => match val {
                Block { statements, output } => self.print_block(statements, output),
                Constant(constant) => match constant {
                    Int(i) => write!(self.writer, "const {}", i),
                    Bool(b) => write!(self.writer, "const {}", b),
                    Unit => write!(self.writer, "const ()"),
                },
                Use(place) => {
                    write!(self.writer, "copy ")?;
                    self.print_place(place)
                }
                Call { callable, args } => {
                    write!(self.writer, "call {}(", callable)?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(self.writer, ", ")?;
                        }
                        write!(self.writer, "{}", arg)?;
                    }
                    write!(self.writer, ")")
                }
                Function(fn_id) => {
                    if let Some(func) = self.ctxt.function_registry.get_signature_by_id(fn_id) {
                        write!(self.writer, "fn {}", func.name)
                    } else {
                        write!(self.writer, "<fn id {}>", fn_id.0)
                    }
                }
                If(mlr::If {
                    condition,
                    then_block,
                    else_block,
                }) => {
                    write!(self.writer, "if {} ", condition)?;
                    self.print_val(then_block)?;
                    write!(self.writer, " else ")?;
                    self.print_val(else_block)
                }
                Loop { body } => {
                    write!(self.writer, "loop ")?;
                    self.print_val(body)
                }
                Empty { type_id } => {
                    let type_name = self.ctxt.type_registry.get_string_rep(type_id);
                    write!(self.writer, "empty {}", type_name)
                }
            },
            None => write!(self.writer, "<val id {}>", val_id.0),
        }
    }

    fn print_place(&mut self, place_id: &mlr::PlaceId) -> Result<(), std::io::Error> {
        use mlr::Place::*;

        let place = &self.mlr.expect("self.mlr should not be empty").places.get(place_id);

        match place {
            Some(place) => match place {
                Local(loc_id) => write!(self.writer, "{}", loc_id),
                FieldAccess { base, field_index, .. } => {
                    self.print_place(base)?;
                    write!(self.writer, ".{}", field_index)
                }
                EnumDiscriminant { base, .. } => {
                    write!(self.writer, "Disc(")?;
                    self.print_place(base)?;
                    write!(self.writer, ")")
                }
                ProjectToVariant { base, variant_index } => {
                    let type_id = self
                        .mlr
                        .expect("self.mlr should not be empty")
                        .place_types
                        .get(base)
                        .expect("type of base place should be known");
                    let enum_name = self.ctxt.type_registry.get_string_rep(type_id);
                    write!(self.writer, "(")?;
                    self.print_place(base)?;
                    write!(self.writer, " as {}::{})", enum_name, variant_index)
                }
            },
            None => write!(self.writer, "<place id {}>", place_id.0),
        }
    }

    fn indent(&mut self) -> Result<(), std::io::Error> {
        for _ in 0..self.indent_level {
            write!(self.writer, "{}", INDENT)?;
        }
        Ok(())
    }
}
