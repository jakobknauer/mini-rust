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
        let left_ty = self.check_expr(left, None, false)?;
        let left_ty = self.normalize(left_ty);
        let right_ty = self.check_expr(right, None, false)?;
        let right_ty = self.normalize(right_ty);

        if let Some(result) = self.try_check_short_circuit_op(left_ty, right_ty, operator) {
            result
        } else if let Some((prim, result_ty)) = self.try_check_builtin_binary_prim_op(left_ty, right_ty, operator) {
            self.typing.expr_extra.insert(expr_id, ExprExtra::BinaryPrim(prim));
            Ok(result_ty)
        } else {
            self.check_overloaded_binary_op(expr_id, left_ty, right_ty, operator)
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

        let bool_ty = self.ctxt.tys.primitive(ty::Primitive::Boolean);
        let unit_ty = self.ctxt.tys.unit();
        let c_char_ty = self.ctxt.tys.primitive(ty::Primitive::CChar);

        let sint_ty = match (left_ty.0, right_ty.0) {
            (
                ty::TyDef::Primitive(ty::Primitive::SignedInt(lw)),
                ty::TyDef::Primitive(ty::Primitive::SignedInt(rw)),
            ) if lw == rw => Some(left_ty),
            _ => None,
        };
        let float_ty = match (left_ty.0, right_ty.0) {
            (ty::TyDef::Primitive(ty::Primitive::Float(lw)), ty::TyDef::Primitive(ty::Primitive::Float(rw)))
                if lw == rw =>
            {
                Some(left_ty)
            }
            _ => None,
        };
        let sint = sint_ty.is_some();
        let float = float_ty.is_some();
        let bool = left_ty == bool_ty && right_ty == bool_ty;
        let unit = left_ty == unit_ty && right_ty == unit_ty;
        let c_char = left_ty == c_char_ty && right_ty == c_char_ty;

        match operator {
            Add if sint => Some((AddInt, sint_ty.unwrap())),
            Add if float => Some((AddFloat, float_ty.unwrap())),
            Subtract if sint => Some((SubInt, sint_ty.unwrap())),
            Subtract if float => Some((SubFloat, float_ty.unwrap())),
            Multiply if sint => Some((MulInt, sint_ty.unwrap())),
            Multiply if float => Some((MulFloat, float_ty.unwrap())),
            Divide if sint => Some((DivInt, sint_ty.unwrap())),
            Divide if float => Some((DivFloat, float_ty.unwrap())),
            Remainder if sint => Some((RemInt, sint_ty.unwrap())),
            Remainder if float => Some((RemFloat, float_ty.unwrap())),
            Equal if sint => Some((EqInt, bool_ty)),
            Equal if float => Some((EqFloat, bool_ty)),
            Equal if bool => Some((EqBool, bool_ty)),
            Equal if unit => Some((EqUnit, bool_ty)),
            Equal if c_char => Some((EqCChar, bool_ty)),
            NotEqual if sint => Some((NeInt, bool_ty)),
            NotEqual if float => Some((NeFloat, bool_ty)),
            NotEqual if bool => Some((NeBool, bool_ty)),
            NotEqual if unit => Some((NeUnit, bool_ty)),
            NotEqual if c_char => Some((NeCChar, bool_ty)),
            BitOr if bool => Some((BitOrBool, bool_ty)),
            BitAnd if bool => Some((BitAndBool, bool_ty)),
            BitXor if bool => Some((BitXorBool, bool_ty)),
            BitOr if sint => Some((BitOrInt, sint_ty.unwrap())),
            BitAnd if sint => Some((BitAndInt, sint_ty.unwrap())),
            BitXor if sint => Some((BitXorInt, sint_ty.unwrap())),
            LessThan if sint => Some((LtInt, bool_ty)),
            LessThan if float => Some((LtFloat, bool_ty)),
            GreaterThan if sint => Some((GtInt, bool_ty)),
            GreaterThan if float => Some((GtFloat, bool_ty)),
            LessThanOrEqual if sint => Some((LeInt, bool_ty)),
            LessThanOrEqual if float => Some((LeFloat, bool_ty)),
            GreaterThanOrEqual if sint => Some((GeInt, bool_ty)),
            GreaterThanOrEqual if float => Some((GeFloat, bool_ty)),
            _ => None,
        }
    }

    fn check_overloaded_binary_op(
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
            BitXor => (self.ctxt.language_items.bit_xor_trait, "bitxor"),
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

    pub(super) fn check_overloaded_unary_op(
        &mut self,
        expr_id: hlr::ExprId,
        operand_ty: ty::Ty<'ctxt>,
        operator: hlr::UnaryOperator,
    ) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        use hlr::UnaryOperator::*;

        let (trait_, mthd_name) = match operator {
            Negative => (self.ctxt.language_items.neg_trait, "neg"),
            Not => (self.ctxt.language_items.not_trait, "not"),
        };
        let trait_ = trait_.ok_or(TypeckError::UnaryOpTypeMismatch { operator, operand_ty })?;

        let gen_args = self.ctxt.tys.ty_slice(&[]);
        let trait_inst = TraitInst::new(trait_, gen_args).unwrap();
        self.pending_obligations
            .push((operand_ty, ty::ConstraintRequirement::Trait(trait_inst)));

        let mthd = self.ctxt.traits.resolve_trait_method(trait_, mthd_name).unwrap();
        let found = super::mthd::FoundMthd::Trait { trait_inst, mthd };
        let resolution = self.instantiate_mthd(found, operand_ty, mthd_name, None)?;
        self.typing
            .expr_extra
            .insert(expr_id, ExprExtra::UnaryOpMthd(resolution));

        Ok(self.ctxt.tys.assoc_ty(operand_ty, trait_inst, 0))
    }
}
