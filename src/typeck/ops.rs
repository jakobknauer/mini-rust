use crate::ctxt::{language_items, traits::TraitInst, ty};
use crate::hlr;

use super::{ExprExtra, TypeckError, TypeckResult};

impl<'a, 'ctxt: 'a> super::Typeck<'a, 'ctxt> {
    pub(super) fn check_binary_op(
        &mut self,
        expr_id: hlr::ExprId,
        left: hlr::Expr<'ctxt>,
        right: hlr::Expr<'ctxt>,
        operator: hlr::BinaryOperator,
    ) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        let left_ty = self.check_expr(left, None)?;
        let left_ty = self.normalize(left_ty);
        let right_ty = self.check_expr(right, None)?;
        let right_ty = self.normalize(right_ty);

        if let Some(result) = self.try_check_short_circuit_op(left_ty, right_ty, operator) {
            result
        } else if let Some((prim, result_ty)) = self.try_check_builtin_binary_prim_op(left_ty, right_ty, operator) {
            self.typing.expr_extra.insert(expr_id, ExprExtra::BinaryPrim(prim));
            Ok(result_ty)
        } else {
            self.check_overloaded_op(expr_id, left_ty, right_ty, operator)
        }
    }

    fn try_check_short_circuit_op(
        &mut self,
        left_ty: ty::Ty<'ctxt>,
        right_ty: ty::Ty<'ctxt>,
        operator: hlr::BinaryOperator,
    ) -> Option<TypeckResult<'ctxt, ty::Ty<'ctxt>>> {
        use hlr::BinaryOperator::*;
        if !matches!(operator, LogicalAnd | LogicalOr) {
            return None;
        }
        let bool_ty = self.ctxt.tys.primitive(ty::Primitive::Boolean);
        if left_ty == bool_ty && right_ty == bool_ty {
            Some(Ok(bool_ty))
        } else {
            Some(Err(TypeckError::BinaryOpTypeMismatch {
                operator,
                left_ty,
                right_ty,
            }))
        }
    }

    fn try_check_builtin_binary_prim_op(
        &mut self,
        left_ty: ty::Ty<'ctxt>,
        right_ty: ty::Ty<'ctxt>,
        operator: hlr::BinaryOperator,
    ) -> Option<(language_items::BinaryPrimOp, ty::Ty<'ctxt>)> {
        use hlr::BinaryOperator::*;
        use language_items::BinaryPrimOp::*;

        let i32_ty = self.ctxt.tys.primitive(ty::Primitive::Integer32);
        let bool_ty = self.ctxt.tys.primitive(ty::Primitive::Boolean);
        let unit_ty = self.ctxt.tys.unit();

        let c_char_ty = self.ctxt.tys.primitive(ty::Primitive::CChar);

        let i32 = left_ty == i32_ty && right_ty == i32_ty;
        let bool = left_ty == bool_ty && right_ty == bool_ty;
        let unit = left_ty == unit_ty && right_ty == unit_ty;
        let c_char = left_ty == c_char_ty && right_ty == c_char_ty;

        match operator {
            Add if i32 => Some((AddI32, i32_ty)),
            Subtract if i32 => Some((SubI32, i32_ty)),
            Multiply if i32 => Some((MulI32, i32_ty)),
            Divide if i32 => Some((DivI32, i32_ty)),
            Remainder if i32 => Some((RemI32, i32_ty)),
            Equal if i32 => Some((EqI32, bool_ty)),
            Equal if bool => Some((EqBool, bool_ty)),
            Equal if unit => Some((EqUnit, bool_ty)),
            Equal if c_char => Some((EqCChar, bool_ty)),
            NotEqual if i32 => Some((NeI32, bool_ty)),
            NotEqual if bool => Some((NeBool, bool_ty)),
            NotEqual if unit => Some((NeUnit, bool_ty)),
            NotEqual if c_char => Some((NeCChar, bool_ty)),
            BitOr if bool => Some((BitOrBool, bool_ty)),
            BitAnd if bool => Some((BitAndBool, bool_ty)),
            BitOr if i32 => Some((BitOrI32, i32_ty)),
            BitAnd if i32 => Some((BitAndI32, i32_ty)),
            LessThan if i32 => Some((LtI32, bool_ty)),
            GreaterThan if i32 => Some((GtI32, bool_ty)),
            LessThanOrEqual if i32 => Some((LeI32, bool_ty)),
            GreaterThanOrEqual if i32 => Some((GeI32, bool_ty)),
            _ => None,
        }
    }

    fn check_overloaded_op(
        &mut self,
        expr_id: hlr::ExprId,
        left_ty: ty::Ty<'ctxt>,
        right_ty: ty::Ty<'ctxt>,
        operator: hlr::BinaryOperator,
    ) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        use hlr::BinaryOperator::*;

        let (trait_, mthd_name) = match operator {
            Add => (self.ctxt.language_items.add_trait, "add"),
            Subtract => (self.ctxt.language_items.sub_trait, "sub"),
            Multiply => (self.ctxt.language_items.mul_trait, "mul"),
            Divide => (self.ctxt.language_items.div_trait, "div"),
            BitOr => (self.ctxt.language_items.bit_or_trait, "bitor"),
            BitAnd => (self.ctxt.language_items.bit_and_trait, "bitand"),
            Remainder => (self.ctxt.language_items.rem_trait, "rem"),
            Equal => (self.ctxt.language_items.eq_trait, "eq"),
            NotEqual => (self.ctxt.language_items.eq_trait, "ne"),
            _ => {
                return Err(TypeckError::BinaryOpTypeMismatch {
                    operator,
                    left_ty,
                    right_ty,
                });
            }
        };
        let trait_ = trait_.ok_or(TypeckError::ArithTraitNotImplemented {
            operator,
            left_ty,
            right_ty,
        })?;

        let gen_args = self.ctxt.tys.ty_slice(&[right_ty]);
        let trait_inst = TraitInst::new(trait_, gen_args).unwrap();
        self.pending_obligations
            .push((left_ty, ty::ConstraintRequirement::Trait(trait_inst)));

        let mthd = self.ctxt.traits.resolve_trait_method(trait_, mthd_name).unwrap();
        let found = super::mthd::FoundMthd::Trait { trait_inst, mthd };
        let resolution = self.instantiate_mthd(found, left_ty, mthd_name, None)?;
        self.typing
            .expr_extra
            .insert(expr_id, ExprExtra::BinaryOpMthd(resolution));

        let result_ty = match operator {
            Equal | NotEqual => self.ctxt.tys.primitive(ty::Primitive::Boolean),
            _ => self.ctxt.tys.assoc_ty(left_ty, trait_inst, 0),
        };
        Ok(result_ty)
    }
}
