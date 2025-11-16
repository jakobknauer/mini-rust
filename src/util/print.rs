use std::io::Write;

use crate::{
    ctxt::{
        self,
        fns::{Fn, FnSig},
    },
    mlr,
};

pub fn print_mlr<W: Write>(fn_: &Fn, ctxt: &ctxt::Ctxt, writer: &mut W) -> Result<(), std::io::Error> {
    let mut printer = MlrPrinter {
        fn_: *fn_,
        mlr: ctxt.fns.get_fn_def(fn_),
        signature: ctxt.fns.get_signature_by_id(fn_),
        ctxt,
        indent_level: 0,
        writer,
    };
    printer.print_mlr()
}

struct MlrPrinter<'a, W: Write> {
    fn_: Fn,
    mlr: Option<&'a mlr::Mlr>,
    signature: Option<&'a FnSig>,
    ctxt: &'a ctxt::Ctxt,
    indent_level: usize,
    writer: &'a mut W,
}

const INDENT: &str = "    ";

impl<'a, W: Write> MlrPrinter<'a, W> {
    fn print_mlr(&mut self) -> Result<(), std::io::Error> {
        self.print_signature()?;

        if let Some(mlr) = self.mlr {
            writeln!(self.writer)?;
            self.print_statement(&mlr.body)
        } else {
            write!(self.writer, ";")
        }
    }

    fn print_signature(&mut self) -> Result<(), std::io::Error> {
        let Some(signature) = self.signature else {
            return write!(self.writer, "<signature for fn id {}>", self.fn_.0);
        };

        write!(self.writer, "fn {}(", signature.name)?;
        for (i, param) in signature.parameters.iter().enumerate() {
            if i > 0 {
                write!(self.writer, ", ")?;
            }
            let param_ty = self.ctxt.tys.get_string_rep(&param.ty);
            write!(self.writer, "{}: {}", param.name, param_ty)?;
        }
        write!(self.writer, ") -> ")?;

        let return_ty = self.ctxt.tys.get_string_rep(&signature.return_ty);
        write!(self.writer, "{}", return_ty)
    }

    fn print_block(&mut self, statements: &[mlr::StmtId]) -> Result<(), std::io::Error> {
        writeln!(self.writer, "{{")?;
        self.indent_level += 1;

        for stmt_id in statements {
            self.print_statement(stmt_id)?;
        }

        self.indent_level -= 1;
        self.indent()?;
        write!(self.writer, "}}")
    }

    fn print_statement(&mut self, stmt_id: &mlr::StmtId) -> Result<(), std::io::Error> {
        use mlr::Stmt::*;

        let stmt = &self.mlr.expect("self.mlr should not be empty").stmts.get(stmt_id);

        match stmt {
            Some(stmt) => match stmt {
                Alloc { loc } => {
                    let loc_ty = self
                        .mlr
                        .expect("self.mlr should not be empty")
                        .loc_tys
                        .get(loc)
                        .expect("type of place should be known");
                    let ty_name = self.ctxt.tys.get_string_rep(loc_ty);
                    self.indent()?;
                    writeln!(self.writer, "alloc {}: {};", loc, ty_name)
                }
                Assign { place, value } => {
                    self.indent()?;
                    write!(self.writer, "assign ")?;
                    self.print_place(place)?;
                    write!(self.writer, " = ")?;
                    self.print_val(value)?;
                    writeln!(self.writer, ";")
                }
                Return { value } => {
                    self.indent()?;
                    write!(self.writer, "return ")?;
                    self.print_val(value)?;
                    writeln!(self.writer, ";")
                }
                Break => {
                    self.indent()?;
                    writeln!(self.writer, "break;")
                }
                Block(stmt_ids) => {
                    self.indent()?;
                    self.print_block(stmt_ids)?;
                    writeln!(self.writer)
                }
                If(if_) => {
                    self.indent()?;
                    write!(self.writer, "if ")?;
                    self.print_op(&if_.condition)?;
                    writeln!(self.writer)?;
                    self.print_statement(&if_.then_block)?;
                    self.indent()?;
                    writeln!(self.writer, "else")?;
                    self.print_statement(&if_.else_block)
                    // writeln!(self.writer)
                }
                Loop { body } => {
                    self.indent()?;
                    writeln!(self.writer, "loop")?;
                    self.print_statement(body)
                    // writeln!(self.writer)
                }
            },
            None => writeln!(self.writer, "<stmt id {}>", stmt_id.0),
        }
    }

    fn print_val(&mut self, val_id: &mlr::ValId) -> Result<(), std::io::Error> {
        use mlr::Val::*;

        let val = &self.mlr.expect("self.mlr should not be empty").vals.get(val_id);

        match val {
            Some(val) => match val {
                Use(op) => {
                    // write!(self.writer, "use ")?;
                    self.print_op(op)
                }
                Call { callable, args } => {
                    write!(self.writer, "call ")?;
                    self.print_op(callable)?;
                    write!(self.writer, "(")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(self.writer, ", ")?;
                        }
                        self.print_op(arg)?;
                    }
                    write!(self.writer, ")")
                }
                Empty { ty } => {
                    let ty_name = self.ctxt.tys.get_string_rep(ty);
                    write!(self.writer, "empty {}", ty_name)
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
                    let ty = self
                        .mlr
                        .expect("self.mlr should not be empty")
                        .place_tys
                        .get(base)
                        .expect("type of base place should be known");
                    let enum_name = self.ctxt.tys.get_string_rep(ty);
                    write!(self.writer, "(")?;
                    self.print_place(base)?;
                    write!(self.writer, " as {}::{})", enum_name, variant_index)
                }
            },
            None => write!(self.writer, "<place id {}>", place_id.0),
        }
    }

    fn print_op(&mut self, op_id: &mlr::OpId) -> Result<(), std::io::Error> {
        use mlr::Constant::*;
        use mlr::Operand::*;

        let operand = &self.mlr.expect("self.mlr should not be empty").ops.get(op_id);

        match operand {
            Some(operand) => match operand {
                Fn(fn_) => {
                    if let Some(func) = self.ctxt.fns.get_signature_by_id(fn_) {
                        write!(self.writer, "fn {}", func.name)
                    } else {
                        write!(self.writer, "<fn id {}>", fn_.0)
                    }
                }
                Constant(constant) => match constant {
                    Int(i) => write!(self.writer, "const {}", i),
                    Bool(b) => write!(self.writer, "const {}", b),
                    Unit => write!(self.writer, "const ()"),
                },
                Copy(place) => {
                    write!(self.writer, "copy ")?;
                    self.print_place(place)
                }
            },
            None => write!(self.writer, "<op id {}>", op_id.0),
        }
    }

    fn indent(&mut self) -> Result<(), std::io::Error> {
        for _ in 0..self.indent_level {
            write!(self.writer, "{}", INDENT)?;
        }
        Ok(())
    }
}
