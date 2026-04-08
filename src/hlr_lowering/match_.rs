use crate::ctxt::{language_items, ty};
use crate::{hlr, mlr};

use super::lowered_expr::LoweredExpr;

#[derive(Clone, Copy)]
enum MatchBinding {
    Direct,
    ByRef,
    ByRefMut,
}

impl<'a, 'ctxt: 'a> super::HlrLowerer<'a, 'ctxt> {
    pub(super) fn lower_match(
        &mut self,
        expr_id: hlr::ExprId,
        scrutinee: hlr::Expr<'ctxt>,
        arms: &'ctxt [hlr::MatchArm<'ctxt>],
    ) -> LoweredExpr<'ctxt> {
        let result_ty = self.typing.expr_types[&expr_id];
        let scrutinee_ty = self.typing.expr_types[&scrutinee.1];
        let scrutinee_place = self.lower_to_place(scrutinee);
        let result_place = self.builder.alloc_place(result_ty);

        self.lower_match_arms(scrutinee_ty, arms, scrutinee_place, result_place);

        self.builder.copy_val(result_place).into()
    }

    fn lower_match_arms(
        &mut self,
        scrutinee_ty: ty::Ty<'ctxt>,
        arms: &'ctxt [hlr::MatchArm<'ctxt>],
        scrutinee_place: mlr::Place<'ctxt>,
        result_place: mlr::Place<'ctxt>,
    ) {
        match arms {
            [] => panic!("match must have at least one arm"),
            [arm] => {
                // Last arm — emit unconditionally (no exhaustiveness check)
                // TODO check condition + exhaustiveness
                let arm_val = self.lower_match_arm(scrutinee_ty, arm, scrutinee_place);
                self.builder.insert_assign_stmt(result_place, arm_val);
            }
            [first_arm, remaining_arms @ ..] => {
                let cond = self.lower_pattern_condition(first_arm.pattern, scrutinee_ty, scrutinee_place);

                if let Some(cond_op) = cond {
                    self.builder.start_block();
                    let arm_val = self.lower_match_arm(scrutinee_ty, first_arm, scrutinee_place);
                    self.builder.insert_assign_stmt(result_place, arm_val);
                    let then_block = self.builder.end_block();

                    self.builder.start_block();
                    self.lower_match_arms(scrutinee_ty, remaining_arms, scrutinee_place, result_place);
                    let else_block = self.builder.end_block();

                    self.builder.insert_if_stmt(cond_op, then_block, else_block);
                } else {
                    // Pattern is irrefutable — emit directly, remaining arms are unreachable
                    let arm_val = self.lower_match_arm(scrutinee_ty, first_arm, scrutinee_place);
                    self.builder.insert_assign_stmt(result_place, arm_val);
                }
            }
        }
    }

    fn lower_pattern_condition(
        &mut self,
        pattern: hlr::Pattern<'ctxt>,
        scrutinee_ty: ty::Ty<'ctxt>,
        scrutinee_place: mlr::Place<'ctxt>,
    ) -> Option<mlr::Op<'ctxt>> {
        match pattern {
            hlr::PatternKind::Identifier { .. } => None,
            hlr::PatternKind::Variant(pattern) => {
                let hlr::Val::Variant(_, variant_idx, _) = &pattern.variant else {
                    panic!("variant pattern must have Val::Variant")
                };
                let (_, _, enum_place) = self.resolve_enum_place(scrutinee_ty, scrutinee_place);
                let disc_place = self.builder.insert_enum_discriminant_place(enum_place);
                let discriminant_op = self.builder.insert_copy_op(disc_place);
                let i32_ty = self.builder.ctxt.tys.primitive(ty::Primitive::Integer32);
                let variant_idx_op = self
                    .builder
                    .insert_const_op(mlr::Const::Int(*variant_idx as i64), i32_ty);
                let bool_ty = self.builder.ctxt.tys.primitive(ty::Primitive::Boolean);
                let cond_val = self.builder.insert_binary_prim_val(
                    language_items::BinaryPrimOp::EqI32,
                    discriminant_op,
                    variant_idx_op,
                    bool_ty,
                );
                let cond_place = self.builder.alloc_place(bool_ty);
                self.builder.insert_assign_stmt(cond_place, cond_val);
                Some(self.builder.insert_copy_op(cond_place))
            }
        }
    }

    fn lower_match_arm(
        &mut self,
        scrutinee_ty: ty::Ty<'ctxt>,
        arm: &'ctxt hlr::MatchArm<'ctxt>,
        scrutinee_place: mlr::Place<'ctxt>,
    ) -> mlr::Val<'ctxt> {
        match arm.pattern {
            hlr::PatternKind::Identifier { var_id, mutable } => {
                self.lower_identifier_pattern_arm(*var_id, *mutable, scrutinee_place);
            }
            hlr::PatternKind::Variant(pattern) => {
                self.lower_variant_pattern_arm(pattern, scrutinee_ty, scrutinee_place);
            }
        }
        self.lower_to_val(arm.body)
    }

    fn lower_identifier_pattern_arm(&mut self, var_id: hlr::VarId, mutable: bool, scrutinee_place: mlr::Place<'ctxt>) {
        let binding_ty = self.typing.var_types[&var_id];
        let loc = if mutable {
            self.builder.alloc_mut_loc(binding_ty)
        } else {
            self.builder.alloc_loc(binding_ty)
        };
        let val = self.builder.copy_val(scrutinee_place);
        self.builder.insert_assign_to_loc_stmt(loc, val);
        self.var_locs.insert(var_id, loc);
    }

    fn lower_variant_pattern_arm(
        &mut self,
        pattern: &hlr::VariantPattern<'ctxt>,
        scrutinee_ty: ty::Ty<'ctxt>,
        scrutinee_place: mlr::Place<'ctxt>,
    ) {
        let hlr::Val::Variant(_, variant_idx, _) = &pattern.variant else {
            panic!("match arm pattern must be Val::Variant")
        };
        let variant_idx = *variant_idx;

        let (enum_ty, binding, enum_place) = self.resolve_enum_place(scrutinee_ty, scrutinee_place);
        let variant_ty = self.builder.ctxt.tys.get_enum_variant_ty(enum_ty, variant_idx).unwrap();
        let variant_place = self
            .builder
            .insert_project_to_variant_place(enum_place, variant_idx, variant_ty);

        for field in pattern.fields {
            let field_ty = self
                .builder
                .ctxt
                .tys
                .get_struct_field_ty(variant_ty, field.field_index)
                .unwrap();
            let field_place = self
                .builder
                .insert_field_access_place(variant_place, field.field_index, field_ty);

            let binding_ty = self.typing.var_types[&field.binding];
            let binding_loc = if field.mutable {
                self.builder.alloc_mut_loc(binding_ty)
            } else {
                self.builder.alloc_loc(binding_ty)
            };

            match binding {
                MatchBinding::Direct => {
                    let field_val = self.builder.copy_val(field_place);
                    self.builder.insert_assign_to_loc_stmt(binding_loc, field_val);
                }
                MatchBinding::ByRef => {
                    let addr_val = self.builder.insert_addr_of_val(field_place);
                    self.builder.insert_assign_to_loc_stmt(binding_loc, addr_val);
                }
                MatchBinding::ByRefMut => {
                    let addr_val = self.builder.insert_addr_of_mut_val(field_place);
                    self.builder.insert_assign_to_loc_stmt(binding_loc, addr_val);
                }
            }

            self.var_locs.insert(field.binding, binding_loc);
        }
    }

    // Returns (enum_ty, binding, enum_place) — dereferencing the scrutinee if it is a reference.
    fn resolve_enum_place(
        &mut self,
        scrutinee_ty: ty::Ty<'ctxt>,
        scrutinee_place: mlr::Place<'ctxt>,
    ) -> (ty::Ty<'ctxt>, MatchBinding, mlr::Place<'ctxt>) {
        match scrutinee_ty.0 {
            ty::TyDef::Enum { .. } => (scrutinee_ty, MatchBinding::Direct, scrutinee_place),
            &ty::TyDef::Ref(inner) => {
                let copy_op = self.builder.insert_copy_op(scrutinee_place);
                let deref_place = self.builder.insert_deref_place(copy_op);
                (inner, MatchBinding::ByRef, deref_place)
            }
            &ty::TyDef::RefMut(inner) => {
                let copy_op = self.builder.insert_copy_op(scrutinee_place);
                let deref_place = self.builder.insert_deref_place(copy_op);
                (inner, MatchBinding::ByRefMut, deref_place)
            }
            _ => panic!("variant pattern requires enum scrutinee"),
        }
    }
}
