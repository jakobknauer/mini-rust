#![allow(unused)]

mod err;

use std::collections::HashMap;

use crate::{
    ctxt::{self, ty},
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
        let body_ty = self.check_expr(self.fn_.body, Some(return_ty))?;

        if !self.ctxt.tys.tys_eq(body_ty, return_ty) {
            return Err(TypeckError::ReturnTypeMismatch {
                expected: return_ty,
                actual: body_ty,
            });
        }

        Ok(self.typing)
    }

    fn check_expr(&mut self, expr: hlr::Expr<'hlr>, expected: Option<ty::Ty>) -> TypeckResult<ty::Ty> {
        let ty = match expr.0 {
            hlr::ExprDef::Lit(lit) => self.check_lit(lit),
            hlr::ExprDef::Val(val) => todo!(),
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

    fn check_lit(&mut self, lit: &hlr::Lit) -> TypeckResult<ty::Ty> {
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
}
