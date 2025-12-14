use std::io::Write;

use crate::ctxt::{
    self,
    fns::{Fn, FnMlr, FnSig},
    mlr,
};

pub fn print_mlr<W: Write>(fn_: &Fn, ctxt: &ctxt::Ctxt, writer: &mut W) -> Result<(), std::io::Error> {
    let mut printer = MlrPrinter {
        fn_: *fn_,
        mlr: ctxt.fns.get_fn_def(fn_),
        signature: ctxt.fns.get_sig(fn_),
        ctxt,
        indent_level: 0,
        writer,
    };
    printer.print_mlr()
}

struct MlrPrinter<'a, W: Write> {
    fn_: Fn,
    mlr: Option<&'a FnMlr>,
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
            self.print_stmt(&mlr.body)
        } else {
            write!(self.writer, ";")
        }
    }

    fn print_signature(&mut self) -> Result<(), std::io::Error> {
        let Some(signature) = self.signature else {
            return write!(self.writer, "<signature for fn id {}>", self.fn_.0);
        };

        let mlr = self.mlr.expect("self.mlr should not be empty");

        write!(self.writer, "fn {}", signature.name)?;

        for (i, &gen_param) in signature.gen_params.iter().enumerate() {
            if i == 0 {
                write!(self.writer, "<")?;
            } else {
                write!(self.writer, ", ")?;
            }
            let gen_param_name = self.ctxt.tys.get_gen_var_name(gen_param);
            write!(self.writer, "{}", gen_param_name)?;
        }
        if !signature.gen_params.is_empty() {
            write!(self.writer, ">")?;
        }

        write!(self.writer, "(")?;
        for (i, (param, param_loc)) in signature.params.iter().zip(&mlr.param_locs).enumerate() {
            if i > 0 {
                write!(self.writer, ", ")?;
            }
            let param_ty = self.ctxt.tys.get_string_rep(param.ty);
            write!(self.writer, "{}: {}", param_loc, param_ty)?;
        }
        write!(self.writer, ") -> ")?;

        let return_ty = self.ctxt.tys.get_string_rep(signature.return_ty);
        write!(self.writer, "{}", return_ty)
    }

    fn print_block(&mut self, stmts: &[mlr::Stmt]) -> Result<(), std::io::Error> {
        writeln!(self.writer, "{{")?;
        self.indent_level += 1;

        for stmt in stmts {
            self.print_stmt(stmt)?;
        }

        self.indent_level -= 1;
        self.indent()?;
        write!(self.writer, "}}")
    }

    fn print_stmt(&mut self, stmt: &mlr::Stmt) -> Result<(), std::io::Error> {
        use mlr::StmtDef::*;

        let stmt_def = &self.ctxt.mlr.try_get_stmt_def(*stmt);

        match stmt_def {
            Some(stmt) => match stmt {
                Alloc { loc } => {
                    let loc_ty = self.ctxt.mlr.get_loc_ty(loc);
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
                Block(stmts) => {
                    self.indent()?;
                    self.print_block(stmts)?;
                    writeln!(self.writer)
                }
                If(if_) => {
                    self.indent()?;
                    write!(self.writer, "if ")?;
                    self.print_op(&if_.cond)?;
                    writeln!(self.writer)?;
                    self.print_stmt(&if_.then)?;
                    self.indent()?;
                    writeln!(self.writer, "else")?;
                    self.print_stmt(&if_.else_)
                    // writeln!(self.writer)
                }
                Loop { body } => {
                    self.indent()?;
                    writeln!(self.writer, "loop")?;
                    self.print_stmt(body)
                    // writeln!(self.writer)
                }
            },
            None => writeln!(self.writer, "<stmt id {}>", stmt.0),
        }
    }

    fn print_val(&mut self, val: &mlr::Val) -> Result<(), std::io::Error> {
        use mlr::ValDef::*;

        let val_def = &self.ctxt.mlr.try_get_val_def(val);

        match val_def {
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
                AddrOf(place) => {
                    write!(self.writer, "AddrOf(")?;
                    self.print_place(place)?;
                    write!(self.writer, ")")
                }
                As { op, target_ty } => {
                    write!(self.writer, "(")?;
                    self.print_op(op)?;
                    let ty_name = self.ctxt.tys.get_string_rep(*target_ty);
                    write!(self.writer, " as {})", ty_name)
                }
            },
            None => write!(self.writer, "<val id {}>", val.0),
        }
    }

    fn print_place(&mut self, place: &mlr::Place) -> Result<(), std::io::Error> {
        use mlr::PlaceDef::*;

        let place_def = &self.ctxt.mlr.try_get_place_def(place);

        match place_def {
            Some(place) => match place {
                Loc(loc) => write!(self.writer, "{}", loc),
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
                    let ty = self.ctxt.mlr.get_place_ty(base);
                    let enum_name = self.ctxt.tys.get_string_rep(ty);
                    write!(self.writer, "(")?;
                    self.print_place(base)?;
                    write!(self.writer, " as {}::{})", enum_name, variant_index)
                }
                Deref(op) => {
                    write!(self.writer, "Deref(")?;
                    self.print_op(op)?;
                    write!(self.writer, ")")
                }
            },
            None => write!(self.writer, "<place id {}>", place.0),
        }
    }

    fn print_op(&mut self, op: &mlr::Op) -> Result<(), std::io::Error> {
        use mlr::Const::*;
        use mlr::OpDef::*;

        let op_def = &self.ctxt.mlr.try_get_op_def(op);

        match op_def {
            Some(operand) => match operand {
                Fn(fn_spec) => {
                    let fn_name = self.ctxt.get_fn_spec_name(fn_spec);
                    write!(self.writer, "fn {}", fn_name)
                }
                Const(constant) => match constant {
                    Int(i) => write!(self.writer, "const {}", i),
                    Bool(b) => write!(self.writer, "const {}", b),
                    Unit => write!(self.writer, "const ()"),
                },
                Copy(place) => {
                    write!(self.writer, "copy ")?;
                    self.print_place(place)
                }
            },
            None => write!(self.writer, "<op id {}>", op.0),
        }
    }

    fn indent(&mut self) -> Result<(), std::io::Error> {
        for _ in 0..self.indent_level {
            write!(self.writer, "{}", INDENT)?;
        }
        Ok(())
    }
}
