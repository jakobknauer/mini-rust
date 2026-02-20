#![allow(unused)]

mod err;
mod mthd;
mod ty_annots;
mod unify;

use std::collections::HashMap;

use crate::{
    ctxt::{self, fns, ty},
    hlr,
};

pub use err::*;
pub use mthd::MthdResolution;

#[derive(Default)]
pub struct HlrTyping {
    pub var_types: HashMap<hlr::VarId, ty::Ty>,
    pub expr_types: HashMap<hlr::ExprId, ty::Ty>,
    pub val_mthd_resolutions: HashMap<hlr::ExprId, MthdResolution>,
    pub binary_op_resolutions: HashMap<hlr::ExprId, fns::FnInst>,
    pub unary_op_resolutions: HashMap<hlr::ExprId, fns::FnInst>,
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

        if !self.unify(body_ty, return_ty) {
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
            hlr::ExprDef::Val(val) => self.infer_val_ty(expr.1, val, hint),
            hlr::ExprDef::BinaryOp { left, right, operator } => {
                self.infer_binary_op_ty(expr.1, *left, *right, *operator)
            }
            hlr::ExprDef::UnaryOp { operand, operator } => self.infer_unary_op_ty(expr.1, *operand, *operator),
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

    fn infer_val_ty(
        &mut self,
        expr_id: hlr::ExprId,
        val: &hlr::Val<'hlr>,
        hint: Option<ty::Ty>,
    ) -> TypeckResult<ty::Ty> {
        match val {
            hlr::Val::Var(var_id) => Ok(self.typing.var_types.get(var_id).copied().unwrap()),
            &hlr::Val::Fn(fn_, args) => self.infer_fn_ty(fn_, args),
            hlr::Val::Struct(..) => unreachable!("raw struct values are not supported"),
            hlr::Val::Variant(..) => unreachable!("raw variant values are not supported"),
            hlr::Val::Mthd(base_ty, mthd_name, gen_args) => self.infer_mthd_ty(expr_id, base_ty, mthd_name, *gen_args),
        }
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
        Ok(self.ctxt.tys.substitute_gen_vars(fn_ty, &subst))
    }

    fn infer_mthd_ty(
        &mut self,
        expr_id: hlr::ExprId,
        base_ty: hlr::TyAnnot<'hlr>,
        mthd_name: &str,
        gen_args: Option<hlr::TyAnnotSlice<'hlr>>,
    ) -> TypeckResult<ty::Ty> {
        let base_ty = self.resolve_ty_annot(base_ty)?;
        let found = self.resolve_mthd(base_ty, mthd_name, false)?;
        let resolution = self.instantiate_mthd(found, base_ty, mthd_name, gen_args)?;
        let fn_ty = self.fn_ty_of_mthd_resolution(&resolution);
        self.typing.val_mthd_resolutions.insert(expr_id, resolution);
        Ok(fn_ty)
    }

    fn infer_binary_op_ty(
        &mut self,
        expr_id: hlr::ExprId,
        left: hlr::Expr<'hlr>,
        right: hlr::Expr<'hlr>,
        operator: hlr::BinaryOperator,
    ) -> TypeckResult<ty::Ty> {
        let left_ty = self.infer_expr_ty(left, None)?;
        let right_ty = self.infer_expr_ty(right, None)?;

        let left_ty = self.ctxt.tys.canonicalize(left_ty);
        let right_ty = self.ctxt.tys.canonicalize(right_ty);

        let i32_ty = self.ctxt.tys.primitive(ty::Primitive::Integer32);
        let bool_ty = self.ctxt.tys.primitive(ty::Primitive::Boolean);
        let unit_ty = self.ctxt.tys.unit();

        use hlr::BinaryOperator::*;
        let (fn_name, result_ty) = match operator {
            Add if left_ty == i32_ty && right_ty == i32_ty => ("add::<i32>", i32_ty),
            Subtract if left_ty == i32_ty && right_ty == i32_ty => ("sub::<i32>", i32_ty),
            Multiply if left_ty == i32_ty && right_ty == i32_ty => ("mul::<i32>", i32_ty),
            Divide if left_ty == i32_ty && right_ty == i32_ty => ("div::<i32>", i32_ty),
            Remainder if left_ty == i32_ty && right_ty == i32_ty => ("rem::<i32>", i32_ty),
            Equal if left_ty == i32_ty && right_ty == i32_ty => ("eq::<i32>", bool_ty),
            Equal if left_ty == bool_ty && right_ty == bool_ty => ("eq::<bool>", bool_ty),
            Equal if left_ty == unit_ty && right_ty == unit_ty => ("eq::<()>", bool_ty),
            NotEqual if left_ty == i32_ty && right_ty == i32_ty => ("ne::<i32>", bool_ty),
            NotEqual if left_ty == bool_ty && right_ty == bool_ty => ("ne::<bool>", bool_ty),
            NotEqual if left_ty == unit_ty && right_ty == unit_ty => ("ne::<()>", bool_ty),
            BitOr if left_ty == bool_ty && right_ty == bool_ty => ("bitor::<bool>", bool_ty),
            BitAnd if left_ty == bool_ty && right_ty == bool_ty => ("bitand::<bool>", bool_ty),
            LessThan if left_ty == i32_ty && right_ty == i32_ty => ("lt::<i32>", bool_ty),
            GreaterThan if left_ty == i32_ty && right_ty == i32_ty => ("gt::<i32>", bool_ty),
            LessThanOrEqual if left_ty == i32_ty && right_ty == i32_ty => ("le::<i32>", bool_ty),
            GreaterThanOrEqual if left_ty == i32_ty && right_ty == i32_ty => ("ge::<i32>", bool_ty),
            _ => {
                return Err(TypeckError::BinaryOpTypeMismatch {
                    operator,
                    left_ty,
                    right_ty,
                });
            }
        };

        let fn_ = self
            .ctxt
            .fns
            .get_fn_by_name(fn_name)
            .expect("operator impl should be registered");
        let empty = self.ctxt.tys.ty_slice(&[]);
        self.typing.binary_op_resolutions.insert(
            expr_id,
            fns::FnInst {
                fn_,
                gen_args: empty,
                env_gen_args: empty,
            },
        );
        Ok(result_ty)
    }

    fn infer_unary_op_ty(
        &mut self,
        expr_id: hlr::ExprId,
        operand: hlr::Expr<'hlr>,
        operator: hlr::UnaryOperator,
    ) -> TypeckResult<ty::Ty> {
        let operand_ty = self.infer_expr_ty(operand, None)?;
        let operand_c = self.ctxt.tys.canonicalize(operand_ty);

        let i32_ty = self.ctxt.tys.primitive(ty::Primitive::Integer32);
        let bool_ty = self.ctxt.tys.primitive(ty::Primitive::Boolean);

        use hlr::UnaryOperator::*;
        let (fn_name, result_ty) = match operator {
            Negative if operand_c == i32_ty => ("neg::<i32>", i32_ty),
            Not if operand_c == bool_ty => ("not::<bool>", bool_ty),
            _ => return Err(TypeckError::UnaryOpTypeMismatch { operator, operand_ty }),
        };

        let fn_ = self
            .ctxt
            .fns
            .get_fn_by_name(fn_name)
            .expect("operator impl should be registered");
        let empty = self.ctxt.tys.ty_slice(&[]);
        self.typing.unary_op_resolutions.insert(
            expr_id,
            fns::FnInst {
                fn_,
                gen_args: empty,
                env_gen_args: empty,
            },
        );
        Ok(result_ty)
    }
}
