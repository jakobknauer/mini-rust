use std::io::Write;

use crate::{hlr, typeck::HlrTyping};

pub fn print_hlr<'ctxt, W: Write>(
    hlr_fn: &hlr::Fn<'ctxt>,
    typing: Option<&HlrTyping<'ctxt>>,
    writer: &mut W,
) -> Result<(), std::io::Error> {
    let mut printer = HlrPrinter {
        typing,
        indent_level: 0,
        writer,
    };
    printer.print_fn(hlr_fn)
}

struct HlrPrinter<'ctxt, 'a, W: Write> {
    typing: Option<&'a HlrTyping<'ctxt>>,
    indent_level: usize,
    writer: &'a mut W,
}

const INDENT: &str = "    ";

impl<'ctxt, 'a, W: Write> HlrPrinter<'ctxt, 'a, W> {
    fn print_fn(&mut self, hlr_fn: &hlr::Fn<'ctxt>) -> Result<(), std::io::Error> {
        let assoc = if let Some(assoc_ty) = hlr_fn.fn_.associated_ty {
            if let Some(trait_inst) = &hlr_fn.fn_.associated_trait_inst {
                format!("<{} as {}>::", assoc_ty, trait_inst)
            } else {
                format!("{}::", assoc_ty)
            }
        } else {
            String::new()
        };

        let gen_params = if hlr_fn.fn_.gen_params.is_empty() {
            String::new()
        } else {
            let names: Vec<_> = hlr_fn.fn_.gen_params.iter().map(|&gv| gv.name().to_string()).collect();
            format!("<{}>", names.join(", "))
        };

        write!(self.writer, "{}{}{}(", assoc, hlr_fn.fn_.name, gen_params)?;
        for (i, (param, &var_id)) in hlr_fn.fn_.params.iter().zip(&hlr_fn.param_var_ids).enumerate() {
            if i > 0 {
                write!(self.writer, ", ")?;
            }
            write!(self.writer, "{}: {}", var_id, param.ty)?;
        }
        write!(self.writer, ") -> {} ", hlr_fn.fn_.return_ty)?;
        self.print_expr(hlr_fn.body)?;
        writeln!(self.writer)
    }

    fn print_expr_indented(&mut self, expr: hlr::Expr<'ctxt>) -> Result<(), std::io::Error> {
        self.indent()?;
        self.print_expr(expr)
    }

    fn print_expr(&mut self, expr: hlr::Expr<'ctxt>) -> Result<(), std::io::Error> {
        use hlr::ExprDef::*;
        match expr.0 {
            Lit(lit) => self.print_lit(lit),
            Val(val) => self.print_val(val),
            BinaryOp { left, right, operator } => {
                self.print_expr(*left)?;
                write!(self.writer, " {} ", operator)?;
                self.print_expr(*right)
            }
            UnaryOp { operand, operator } => {
                write!(self.writer, "{}", operator)?;
                self.print_expr(*operand)
            }
            Call { callee, args } => {
                self.print_expr(*callee)?;
                write!(self.writer, "(")?;
                for (i, &arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.print_expr(arg)?;
                }
                write!(self.writer, ")")
            }
            MthdCall {
                receiver,
                mthd_name,
                gen_args,
                args,
            } => {
                self.print_expr(*receiver)?;
                write!(self.writer, ".{}", mthd_name)?;
                if let Some(gen_args) = gen_args {
                    write!(self.writer, "::<")?;
                    for (i, &arg) in gen_args.iter().enumerate() {
                        if i > 0 {
                            write!(self.writer, ", ")?;
                        }
                        self.print_ty_annot(arg)?;
                    }
                    write!(self.writer, ">")?;
                }
                write!(self.writer, "(")?;
                for (i, &arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.print_expr(arg)?;
                }
                write!(self.writer, ")")
            }
            Struct { constructor, fields } => {
                self.print_val(constructor)?;
                write!(self.writer, " {{")?;
                for (i, (field, expr)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ",")?;
                    }
                    write!(self.writer, " ")?;
                    match field {
                        hlr::FieldSpec::Name(name) => write!(self.writer, "{}", name)?,
                        hlr::FieldSpec::Index(idx) => write!(self.writer, "{}", idx)?,
                    }
                    write!(self.writer, ": ")?;
                    self.print_expr(*expr)?;
                }
                write!(self.writer, " }}")
            }
            FieldAccess { base, field } => {
                self.print_expr(*base)?;
                match field {
                    hlr::FieldSpec::Name(name) => write!(self.writer, ".{}", name),
                    hlr::FieldSpec::Index(idx) => write!(self.writer, ".{}", idx),
                }
            }
            Tuple(exprs) => {
                write!(self.writer, "(")?;
                for (i, &e) in exprs.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.print_expr(e)?;
                }
                if exprs.len() == 1 {
                    write!(self.writer, ",")?;
                }
                write!(self.writer, ")")
            }
            Assign { target, value } => {
                self.print_expr(*target)?;
                write!(self.writer, " = ")?;
                self.print_expr(*value)
            }
            Deref(inner) => {
                write!(self.writer, "*")?;
                self.print_expr(*inner)
            }
            AddrOf(inner) => {
                write!(self.writer, "&")?;
                self.print_expr(*inner)
            }
            AddrOfMut(inner) => {
                write!(self.writer, "&mut ")?;
                self.print_expr(*inner)
            }
            As { expr, ty } => {
                self.print_expr(*expr)?;
                write!(self.writer, " as ")?;
                self.print_ty_annot(ty)
            }
            Closure {
                params,
                return_ty,
                body,
            } => {
                write!(self.writer, "|")?;
                for (i, hlr::ClosureParam(var_id, annot)) in params.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    if let Some(typing) = self.typing
                        && let Some(&ty) = typing.var_types.get(var_id)
                    {
                        write!(self.writer, "{}: {}", var_id, ty)?;
                    } else if let Some(annot) = annot {
                        write!(self.writer, "{}: ", var_id)?;
                        self.print_ty_annot(annot)?;
                    } else {
                        write!(self.writer, "{}", var_id)?;
                    }
                }
                write!(self.writer, "|")?;
                if let Some(ret) = return_ty {
                    write!(self.writer, " -> ")?;
                    self.print_ty_annot(ret)?;
                }
                write!(self.writer, " ")?;
                self.print_expr(*body)
            }
            If { cond, then, else_ } => {
                write!(self.writer, "if ")?;
                self.print_expr(*cond)?;
                write!(self.writer, " ")?;
                self.print_block_expr(*then)?;
                if let Some(else_) = else_ {
                    write!(self.writer, " else ")?;
                    self.print_block_expr(*else_)?;
                }
                Ok(())
            }
            Loop { body } => {
                write!(self.writer, "loop ")?;
                self.print_block_expr(*body)
            }
            Match { scrutinee, arms } => {
                write!(self.writer, "match ")?;
                self.print_expr(*scrutinee)?;
                writeln!(self.writer, " {{")?;
                self.indent_level += 1;
                for arm in arms.iter() {
                    self.indent()?;
                    self.print_pattern(arm.pattern)?;
                    write!(self.writer, " => ")?;
                    self.print_expr(arm.body)?;
                    writeln!(self.writer, ",")?;
                }
                self.indent_level -= 1;
                self.indent()?;
                write!(self.writer, "}}")
            }
            Block { stmts, trailing } => {
                writeln!(self.writer, "{{")?;
                self.indent_level += 1;
                for stmt in stmts.iter() {
                    self.print_stmt(stmt)?;
                }
                self.print_expr_indented(*trailing)?;
                writeln!(self.writer)?;
                self.indent_level -= 1;
                self.indent()?;
                write!(self.writer, "}}")
            }
            QualifiedMthd {
                ty,
                trait_,
                trait_args,
                mthd_name,
                args,
            } => {
                write!(self.writer, "<")?;
                self.print_ty_annot(ty)?;
                if let Some(trait_) = trait_ {
                    write!(self.writer, " as {}", trait_.name)?;
                    if let Some(args) = trait_args {
                        write!(self.writer, "<")?;
                        for (i, &arg) in args.iter().enumerate() {
                            if i > 0 {
                                write!(self.writer, ", ")?;
                            }
                            self.print_ty_annot(arg)?;
                        }
                        write!(self.writer, ">")?;
                    }
                }
                write!(self.writer, ">::{}", mthd_name)?;
                if let Some(args) = args {
                    write!(self.writer, "::<")?;
                    for (i, &arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(self.writer, ", ")?;
                        }
                        self.print_ty_annot(arg)?;
                    }
                    write!(self.writer, ">")?;
                }
                Ok(())
            }
        }
    }

    fn print_pattern(&mut self, pattern: hlr::Pattern<'ctxt>) -> Result<(), std::io::Error> {
        match pattern {
            hlr::PatternKind::Wildcard => write!(self.writer, "_"),
            hlr::PatternKind::Lit(lit) => self.print_lit(lit),
            hlr::PatternKind::Identifier { var_id, mutable } => {
                if *mutable {
                    write!(self.writer, "mut ")?;
                }
                write!(self.writer, "{var_id}")
            }
            hlr::PatternKind::Variant(pattern) => {
                self.print_val(&pattern.variant)?;
                if !pattern.fields.is_empty() {
                    write!(self.writer, "(")?;
                    for (i, field) in pattern.fields.iter().enumerate() {
                        if i > 0 {
                            write!(self.writer, ", ")?;
                        }
                        self.print_pattern(field.pattern)?;
                    }
                    write!(self.writer, ")")?;
                }
                Ok(())
            }
            hlr::PatternKind::Struct(pattern) => {
                self.print_val(&pattern.constructor)?;
                write!(self.writer, " {{")?;
                for (i, field) in pattern.fields.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    write!(self.writer, "{}: ", field.field_index)?;
                    self.print_pattern(field.pattern)?;
                }
                write!(self.writer, "}}")
            }
            hlr::PatternKind::Tuple(sub_patterns) => {
                write!(self.writer, "(")?;
                for (i, &sub_pattern) in sub_patterns.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.print_pattern(sub_pattern)?;
                }
                write!(self.writer, ")")
            }
            hlr::PatternKind::Ref(inner) => {
                write!(self.writer, "&")?;
                self.print_pattern(inner)
            }
            hlr::PatternKind::RefMut(inner) => {
                write!(self.writer, "&mut ")?;
                self.print_pattern(inner)
            }
        }
    }

    // Print an expression that should be a block (for if/loop bodies)
    fn print_block_expr(&mut self, expr: hlr::Expr<'ctxt>) -> Result<(), std::io::Error> {
        if matches!(expr.0, hlr::ExprDef::Block { .. }) {
            self.print_expr(expr)
        } else {
            writeln!(self.writer, "{{")?;
            self.indent_level += 1;
            self.print_expr_indented(expr)?;
            writeln!(self.writer)?;
            self.indent_level -= 1;
            self.indent()?;
            write!(self.writer, "}}")
        }
    }

    fn print_stmt(&mut self, stmt: hlr::Stmt<'ctxt>) -> Result<(), std::io::Error> {
        use hlr::StmtDef::*;
        match stmt {
            Expr(expr) => {
                self.print_expr_indented(*expr)?;
                writeln!(self.writer, ";")
            }
            &Let { var, mutable, ty, init } => {
                self.indent()?;
                write!(self.writer, "let ")?;
                if mutable {
                    write!(self.writer, "mut ")?;
                }
                if let Some(typing) = self.typing
                    && let Some(&inferred) = typing.var_types.get(&var)
                {
                    write!(self.writer, "{}", inferred)?;
                } else if let Some(annot) = ty {
                    self.print_ty_annot(annot)?;
                } else {
                    write!(self.writer, "_")?;
                }
                write!(self.writer, " = ")?;
                self.print_expr(init)?;
                writeln!(self.writer, ";")
            }
            Break => {
                self.indent()?;
                writeln!(self.writer, "break;")
            }
            Return(expr) => {
                self.indent()?;
                write!(self.writer, "return")?;
                if let Some(expr) = expr {
                    write!(self.writer, " ")?;
                    self.print_expr(*expr)?;
                }
                writeln!(self.writer, ";")
            }
        }
    }

    fn print_val(&mut self, val: &hlr::Val<'ctxt>) -> Result<(), std::io::Error> {
        match val {
            hlr::Val::Var(var_id) => write!(self.writer, "{}", var_id),
            hlr::Val::Fn(fn_, gen_args) => {
                write!(self.writer, "{}", fn_.name)?;
                self.print_optional_gen_args(*gen_args)
            }
            hlr::Val::Struct(struct_, gen_args) => {
                let name = &struct_.name;
                write!(self.writer, "{}", name)?;
                self.print_optional_gen_args(*gen_args)
            }
            hlr::Val::Variant(enum_, variant_idx, gen_args) => {
                let variant_name = &enum_.get_variant(*variant_idx).name;
                write!(self.writer, "{}::{}", enum_.name, variant_name)?;
                self.print_optional_gen_args(*gen_args)
            }
            hlr::Val::Mthd(ty_annot, name, gen_args) => {
                self.print_ty_annot(ty_annot)?;
                write!(self.writer, "::{}", name)?;
                self.print_optional_gen_args(*gen_args)
            }
        }
    }

    fn print_optional_gen_args(&mut self, gen_args: Option<hlr::TyAnnotSlice<'ctxt>>) -> Result<(), std::io::Error> {
        if let Some(args) = gen_args {
            write!(self.writer, "::<")?;
            for (i, &arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ")?;
                }
                self.print_ty_annot(arg)?;
            }
            write!(self.writer, ">")?;
        }
        Ok(())
    }

    fn print_ty_annot(&mut self, annot: hlr::TyAnnot<'ctxt>) -> Result<(), std::io::Error> {
        use hlr::TyAnnotDef::*;
        match annot {
            Struct(struct_, gen_args) => {
                let name = &struct_.name;
                write!(self.writer, "{}", name)?;
                if let Some(args) = gen_args {
                    write!(self.writer, "<")?;
                    for (i, &arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(self.writer, ", ")?;
                        }
                        self.print_ty_annot(arg)?;
                    }
                    write!(self.writer, ">")?;
                }
                Ok(())
            }
            Enum(enum_, gen_args) => {
                let name = &enum_.name;
                write!(self.writer, "{}", name)?;
                if let Some(args) = gen_args {
                    write!(self.writer, "<")?;
                    for (i, &arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(self.writer, ", ")?;
                        }
                        self.print_ty_annot(arg)?;
                    }
                    write!(self.writer, ">")?;
                }
                Ok(())
            }
            Ty(ty) => write!(self.writer, "{}", ty),
            GenVar(gv) => write!(self.writer, "{}", gv.name()),
            AssocTy { base, trait_, name } => {
                write!(self.writer, "<")?;
                self.print_ty_annot(base)?;
                if let Some((trait_, gen_args)) = trait_ {
                    write!(self.writer, " as {}", trait_.name)?;
                    if let Some(args) = gen_args {
                        write!(self.writer, "<")?;
                        for (i, arg) in args.iter().enumerate() {
                            if i > 0 {
                                write!(self.writer, ", ")?;
                            }
                            self.print_ty_annot(arg)?;
                        }
                        write!(self.writer, ">")?;
                    }
                }
                write!(self.writer, ">::{}", name)
            }
            Ref(inner) => {
                write!(self.writer, "&")?;
                self.print_ty_annot(inner)
            }
            RefMut(inner) => {
                write!(self.writer, "&mut ")?;
                self.print_ty_annot(inner)
            }
            Ptr(inner) => {
                write!(self.writer, "*")?;
                self.print_ty_annot(inner)
            }
            Fn { params, ret } => {
                write!(self.writer, "fn(")?;
                for (i, &p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.print_ty_annot(p)?;
                }
                write!(self.writer, ")")?;
                if let Some(ret) = ret {
                    write!(self.writer, " -> ")?;
                    self.print_ty_annot(ret)?;
                }
                Ok(())
            }
            Tuple(elems) => {
                write!(self.writer, "(")?;
                for (i, &e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.print_ty_annot(e)?;
                }
                if elems.len() == 1 {
                    write!(self.writer, ",")?;
                }
                write!(self.writer, ")")
            }
            Infer => write!(self.writer, "_"),
            Self_ => write!(self.writer, "Self"),
            Never => write!(self.writer, "!"),
        }
    }

    fn print_lit(&mut self, lit: &hlr::Lit) -> Result<(), std::io::Error> {
        match lit {
            hlr::Lit::Int(i) => write!(self.writer, "{}", i),
            hlr::Lit::Bool(b) => write!(self.writer, "{}", b),
            hlr::Lit::CChar(c) => {
                let s = super::reinsert_escape_sequences(&(*c as char).to_string());
                write!(self.writer, "'{}'", s)
            }
            hlr::Lit::CString(bytes) => {
                let s = String::from_utf8_lossy(bytes);
                let s = super::reinsert_escape_sequences(&s);
                write!(self.writer, "\"{}\"", s)
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
