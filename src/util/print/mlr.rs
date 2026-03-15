use std::io::Write;

use crate::{
    ctxt::{
        self,
        fns::{Fn, FnSig},
        language_items,
    },
    mlr,
};

pub fn print_mlr<'mlr, W: Write>(
    fn_: Fn,
    mlr_fn: Option<&mlr::Fn<'mlr>>,
    ctxt: &ctxt::Ctxt<'mlr>,
    writer: &mut W,
) -> Result<(), std::io::Error> {
    let mut printer = MlrPrinter {
        fn_,
        mlr_fn,
        signature: ctxt.fns.get_sig(fn_),
        ctxt,
        indent_level: 0,
        writer,
    };
    printer.print_mlr()
}

struct MlrPrinter<'a, 'mlr, W: Write> {
    fn_: Fn,
    mlr_fn: Option<&'a mlr::Fn<'mlr>>,
    signature: Option<&'a FnSig<'mlr>>,
    ctxt: &'a ctxt::Ctxt<'mlr>,
    indent_level: usize,
    writer: &'a mut W,
}

const INDENT: &str = "    ";

impl<'a, 'mlr, W: Write> MlrPrinter<'a, 'mlr, W> {
    fn print_mlr(&mut self) -> Result<(), std::io::Error> {
        self.print_signature()?;

        if let Some(mlr) = self.mlr_fn {
            writeln!(self.writer)?;
            self.print_stmt(mlr.body)
        } else {
            writeln!(self.writer, ";")
        }
    }

    fn print_signature(&mut self) -> Result<(), std::io::Error> {
        let Some(signature) = self.signature else {
            return write!(self.writer, "<signature for fn id {}>", self.fn_);
        };

        // Print signature similar to printing of fn_inst in src/ctxt.rs

        let assoc_ty = if let Some(assoc_ty) = signature.associated_ty {
            let assoc_ty_name = self.ctxt.tys.get_string_rep(assoc_ty);
            if let Some(assoc_trait_inst) = &signature.associated_trait_inst {
                let assoc_trait_name = self.ctxt.traits.get_trait_name(assoc_trait_inst.trait_);
                let assoc_trait_gen_params = if assoc_trait_inst.gen_args.is_empty() {
                    "".to_string()
                } else {
                    format!(
                        "<{}>",
                        assoc_trait_inst
                            .gen_args
                            .iter()
                            .map(|&ty| self.ctxt.tys.get_string_rep(ty))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                };

                format!(
                    "<{} as {}{}>::",
                    assoc_ty_name, assoc_trait_name, assoc_trait_gen_params
                )
            } else {
                format!("{}::", assoc_ty_name)
            }
        } else {
            "".to_string()
        };

        let env_gen_args = if signature.env_gen_params.is_empty() {
            "".to_string()
        } else {
            format!(
                "{{{}}}",
                signature
                    .env_gen_params
                    .iter()
                    .map(|&gv| gv.name())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };

        let gen_args = if signature.gen_params.is_empty() {
            "".to_string()
        } else {
            format!(
                "<{}>",
                signature
                    .gen_params
                    .iter()
                    .map(|&gv| gv.name())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };

        write!(
            self.writer,
            "fn {}{}{}{}",
            assoc_ty, signature.name, env_gen_args, gen_args
        )?;

        write!(self.writer, "(")?;
        if let Some(mlr_fn) = self.mlr_fn {
            for (i, (param, param_loc)) in signature.params.iter().zip(&mlr_fn.param_locs).enumerate() {
                if i > 0 {
                    write!(self.writer, ", ")?;
                }
                let param_ty = self.ctxt.tys.get_string_rep(param.ty);
                write!(self.writer, "{}: {}", param_loc, param_ty)?;
            }
        } else {
            for (i, param) in signature.params.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ")?;
                }
                let param_ty = self.ctxt.tys.get_string_rep(param.ty);
                write!(self.writer, "_{}: {}", i, param_ty)?;
            }
        }
        write!(self.writer, ") -> ")?;

        let return_ty = self.ctxt.tys.get_string_rep(signature.return_ty);
        write!(self.writer, "{}", return_ty)
    }

    fn print_block(&mut self, stmts: &[mlr::Stmt<'mlr>]) -> Result<(), std::io::Error> {
        writeln!(self.writer, "{{")?;
        self.indent_level += 1;

        for &stmt in stmts {
            self.print_stmt(stmt)?;
        }

        self.indent_level -= 1;
        self.indent()?;
        write!(self.writer, "}}")
    }

    fn print_stmt(&mut self, stmt: mlr::Stmt<'mlr>) -> Result<(), std::io::Error> {
        use mlr::StmtDef::*;

        match *stmt {
            Alloc { loc } => {
                let ty_name = self.ctxt.tys.get_string_rep(loc.1);
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
                self.print_op(if_.cond)?;
                writeln!(self.writer)?;
                self.print_stmt(if_.then)?;
                self.indent()?;
                writeln!(self.writer, "else")?;
                self.print_stmt(if_.else_)
            }
            Loop { body } => {
                self.indent()?;
                writeln!(self.writer, "loop")?;
                self.print_stmt(body)
            }
        }
    }

    fn print_val(&mut self, val: mlr::Val<'mlr>) -> Result<(), std::io::Error> {
        use mlr::ValDef::*;

        match *val {
            Use(op) => {
                // write!(self.writer, "use ")?;
                self.print_op(op)
            }
            Call { callable, ref args } => {
                write!(self.writer, "call ")?;
                self.print_op(callable)?;
                write!(self.writer, "(")?;
                for (i, &arg) in args.iter().enumerate() {
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
                let ty_name = self.ctxt.tys.get_string_rep(target_ty);
                write!(self.writer, " as {})", ty_name)
            }
            UnaryPrim { op, operand } => {
                use language_items::UnaryPrimOp::*;
                let op_str = match op {
                    NegI32 => "-",
                    NotBool => "!",
                };
                write!(self.writer, "{op_str}")?;
                self.print_op(operand)
            }
            BinaryPrim { op, lhs, rhs } => {
                use language_items::BinaryPrimOp::*;
                let op_str = match op {
                    AddI32 => "+",
                    SubI32 => "-",
                    MulI32 => "*",
                    DivI32 => "/",
                    RemI32 => "%",
                    EqI32 | EqBool | EqUnit => "==",
                    NeI32 | NeBool | NeUnit => "!=",
                    BitOrBool | BitOrI32 => "|",
                    BitAndBool | BitAndI32 => "&",
                    LtI32 => "<",
                    GtI32 => ">",
                    LeI32 => "<=",
                    GeI32 => ">=",
                };
                self.print_op(lhs)?;
                write!(self.writer, " {op_str} ")?;
                self.print_op(rhs)
            }
        }
    }

    fn print_place(&mut self, place: mlr::Place<'mlr>) -> Result<(), std::io::Error> {
        use mlr::PlaceDef::*;

        match *place {
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
                let enum_name = self.ctxt.tys.get_string_rep(base.1);
                write!(self.writer, "(")?;
                self.print_place(base)?;
                write!(self.writer, " as {}::{})", enum_name, variant_index)
            }
            Deref(op) => {
                write!(self.writer, "Deref(")?;
                self.print_op(op)?;
                write!(self.writer, ")")
            }
            ClosureCaptures(place) => {
                write!(self.writer, "ClosureCaptures(")?;
                self.print_place(place)?;
                write!(self.writer, ")")
            }
        }
    }

    fn print_op(&mut self, op: mlr::Op<'mlr>) -> Result<(), std::io::Error> {
        use mlr::Const::*;
        use mlr::OpDef::*;

        match *op {
            Fn(fn_inst) => {
                let fn_name = self.ctxt.get_fn_inst_name(fn_inst);
                write!(self.writer, "fn {}", fn_name)
            }
            TraitMthd(trait_mthd) => {
                let base_ty_name = self.ctxt.tys.get_string_rep(trait_mthd.impl_ty);
                let trait_name = self.ctxt.traits.get_trait_name(trait_mthd.trait_inst.trait_);

                let trait_gen_args = if trait_mthd.trait_inst.gen_args.is_empty() {
                    "".to_string()
                } else {
                    format!(
                        "<{}>",
                        trait_mthd
                            .trait_inst
                            .gen_args
                            .iter()
                            .map(|&ty| self.ctxt.tys.get_string_rep(ty))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                };

                let mthd_name = self
                    .ctxt
                    .traits
                    .get_trait_mthd_name(trait_mthd.trait_inst.trait_, trait_mthd.mthd_idx);
                write!(
                    self.writer,
                    "<{} as {}{}>::{}",
                    base_ty_name, trait_name, trait_gen_args, mthd_name
                )
            }
            Const(ref constant) => match *constant {
                Int(i) => write!(self.writer, "const {}", i),
                Bool(b) => write!(self.writer, "const {}", b),
                CChar(c) => {
                    write!(
                        self.writer,
                        "const '{}'",
                        super::reinsert_escape_sequences(&(c as char).to_string())
                    )
                }
                CString(ref s) => {
                    let s = std::str::from_utf8(s).unwrap();
                    let s = super::reinsert_escape_sequences(s);
                    write!(self.writer, "const \"{}\"", s)
                }
            },
            Copy(place) => {
                write!(self.writer, "copy ")?;
                self.print_place(place)
            }
        }
    }

    fn indent(&mut self) -> Result<(), std::io::Error> {
        for _ in 0..self.indent_level {
            write!(self.writer, "{}", INDENT)?;
        }
        Ok(())
    }
}
