#![allow(unused)]

mod err;
mod ty_annots;

use std::collections::HashMap;

use crate::{
    ctxt::{self, fns, ty},
    hlr,
};

pub use err::*;

#[derive(Default)]
pub struct HlrTyping {
    pub var_types: HashMap<hlr::VarId, ty::Ty>,
    pub expr_types: HashMap<hlr::ExprId, ty::Ty>,
}

pub fn typeck<'hlr>(
    ctxt: &mut ctxt::Ctxt,
    hlr: &'hlr hlr::Hlr<'hlr>,
    fn_: &'hlr hlr::FnHlr<'hlr>,
) -> TypeckResult<HlrTyping> {
    let typeck = Typeck {
        ctxt,
        hlr,
        fn_,
        type_vars: HashMap::new(),
        typing: Default::default(),
    };

    typeck.check()
}

struct Typeck<'ctxt, 'hlr> {
    ctxt: &'ctxt mut ctxt::Ctxt,
    hlr: &'hlr hlr::Hlr<'hlr>,
    fn_: &'hlr hlr::FnHlr<'hlr>,

    type_vars: HashMap<ty::InfVar, ty::Ty>,
    typing: HlrTyping,
}

impl<'ctxt, 'hlr> Typeck<'ctxt, 'hlr> {
    fn check(mut self) -> TypeckResult<HlrTyping> {
        let sig = self.ctxt.fns.get_sig(self.fn_.fn_).unwrap();

        for (param, param_var_id) in sig.params.iter().zip(&self.fn_.param_var_ids) {
            self.typing.var_types.insert(*param_var_id, param.ty);
        }

        let return_ty = sig.return_ty;
        let body_ty = self.infer_expr_ty(self.fn_.body, Some(return_ty))?;

        if !self.ctxt.tys.tys_eq(body_ty, return_ty) {
            return Err(TypeckError::ReturnTypeMismatch {
                expected: return_ty,
                actual: body_ty,
            });
        }

        Ok(self.typing)
    }

    fn infer_expr_ty(&mut self, expr: hlr::Expr<'hlr>, hint: Option<ty::Ty>) -> TypeckResult<ty::Ty> {
        let ty = match expr.0 {
            hlr::ExprDef::Lit(lit) => self.infer_lit_ty(lit),
            hlr::ExprDef::Val(val) => self.infer_val_ty(val, hint),
            hlr::ExprDef::BinaryOp { left, right, operator } => todo!(),
            hlr::ExprDef::UnaryOp { operand, operator } => todo!(),
            hlr::ExprDef::Call { callee, args } => todo!(),
            hlr::ExprDef::MthdCall {
                receiver,
                mthd_name,
                gen_args,
                args,
            } => todo!(),
            hlr::ExprDef::Struct { constructor, fields } => todo!(),
            hlr::ExprDef::FieldAccess { base, field } => todo!(),
            hlr::ExprDef::Tuple(exprs) => todo!(),
            hlr::ExprDef::Assign { target, value } => todo!(),
            hlr::ExprDef::Deref(expr) => todo!(),
            hlr::ExprDef::AddrOf(expr) => todo!(),
            hlr::ExprDef::As { expr, ty } => todo!(),
            hlr::ExprDef::Closure {
                params,
                return_ty,
                body,
            } => todo!(),
            hlr::ExprDef::If { cond, then, else_ } => todo!(),
            hlr::ExprDef::Loop { body } => todo!(),
            hlr::ExprDef::Match { scrutinee, arms } => todo!(),
            hlr::ExprDef::Block { stmts, trailing } => todo!(),
            hlr::ExprDef::QualifiedMthd {
                ty,
                trait_,
                trait_args,
                mthd_name,
                args,
            } => todo!(),
        }?;

        self.typing.expr_types.insert(expr.1, ty);

        Ok(ty)
    }

    fn infer_lit_ty(&mut self, lit: &hlr::Lit) -> TypeckResult<ty::Ty> {
        let ty = match lit {
            hlr::Lit::Int(_) => self.ctxt.tys.primitive(ty::Primitive::Integer32),
            hlr::Lit::Bool(_) => self.ctxt.tys.primitive(ty::Primitive::Boolean),
            hlr::Lit::CChar(_) => self.ctxt.tys.primitive(ty::Primitive::CChar),
            hlr::Lit::CString(_) => {
                let c_char_ty = self.ctxt.tys.primitive(ty::Primitive::CChar);
                self.ctxt.tys.ptr(c_char_ty)
            }
        };

        Ok(ty)
    }

    fn infer_val_ty(&mut self, val: &hlr::Val<'hlr>, hint: Option<ty::Ty>) -> TypeckResult<ty::Ty> {
        let ty = match val {
            hlr::Val::Var(var_id) => self.typing.var_types.get(var_id).copied().unwrap(),
            &hlr::Val::Fn(fn_, args) => self.infer_fn_ty(fn_, args)?,
            hlr::Val::Struct(struct_, args) => todo!(),
            hlr::Val::Variant(_, _, ty_annot_defs) => todo!(),
            hlr::Val::Mthd(ty_annot_def, _, ty_annot_defs) => todo!(),
        };

        Ok(ty)
    }

    fn infer_fn_ty(&mut self, fn_: fns::Fn, args: Option<hlr::TyAnnotSlice<'hlr>>) -> TypeckResult<ty::Ty> {
        let gen_arg_count = self.ctxt.fns.get_sig(fn_).unwrap().gen_params.len();

        let args =
            self.resolve_optional_gen_args(args, gen_arg_count, |actual| TypeckError::FnGenArgCountMismatch {
                fn_,
                expected: gen_arg_count,
                actual,
            })?;

        let signature = self.ctxt.fns.get_sig(fn_).unwrap();

        let subst = ty::GenVarSubst::new(&signature.gen_params, args).unwrap();

        // TODO check generic constraints

        let param_tys: Vec<_> = signature.params.iter().map(|param| param.ty).collect();
        let fn_ty = self.ctxt.tys.fn_(&param_tys, signature.return_ty, signature.var_args);
        let fn_inst_ty = self.ctxt.tys.substitute_gen_vars(fn_ty, &subst);

        Ok(fn_inst_ty)
    }
}
