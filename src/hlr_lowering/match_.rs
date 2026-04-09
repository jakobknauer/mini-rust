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
                let cond_op = self.lower_pattern_condition(first_arm.pattern, scrutinee_ty, scrutinee_place);

                self.builder.start_block();
                let arm_val = self.lower_match_arm(scrutinee_ty, first_arm, scrutinee_place);
                self.builder.insert_assign_stmt(result_place, arm_val);
                let then_block = self.builder.end_block();

                self.builder.start_block();
                self.lower_match_arms(scrutinee_ty, remaining_arms, scrutinee_place, result_place);
                let else_block = self.builder.end_block();

                self.builder.insert_if_stmt(cond_op, then_block, else_block);
            }
        }
    }

    fn lower_pattern_condition(
        &mut self,
        pattern: hlr::Pattern<'ctxt>,
        scrutinee_ty: ty::Ty<'ctxt>,
        scrutinee_place: mlr::Place<'ctxt>,
    ) -> mlr::Op<'ctxt> {
        match pattern {
            hlr::PatternKind::Identifier { .. } => self.builder.insert_bool_const(true),
            hlr::PatternKind::Variant(pattern) => {
                self.lower_variant_pattern_condition(pattern, scrutinee_ty, scrutinee_place)
            }
        }
    }

    fn lower_variant_pattern_condition(
        &mut self,
        pattern: &hlr::VariantPattern<'ctxt>,
        scrutinee_ty: ty::Ty<'ctxt>,
        scrutinee_place: mlr::Place<'ctxt>,
    ) -> mlr::Op<'ctxt> {
        let hlr::Val::Variant(_, variant_idx, _) = &pattern.variant else {
            panic!("variant pattern must have Val::Variant")
        };
        let variant_idx = *variant_idx;

        let (enum_ty, _, enum_place) = self.resolve_enum_place(scrutinee_ty, scrutinee_place);
        let disc_place = self.builder.insert_enum_discriminant_place(enum_place);
        let discriminant_op = self.builder.insert_copy_op(disc_place);
        let i32_ty = self.builder.ctxt.tys.primitive(ty::Primitive::Integer32);
        let variant_idx_op = self
            .builder
            .insert_const_op(mlr::Const::Int(variant_idx as i64), i32_ty);
        let bool_ty = self.builder.ctxt.tys.primitive(ty::Primitive::Boolean);
        let disc_cond_val = self.builder.insert_binary_prim_val(
            language_items::BinaryPrimOp::EqI32,
            discriminant_op,
            variant_idx_op,
            bool_ty,
        );
        let disc_cond_place = self.builder.alloc_place(bool_ty);
        self.builder.insert_assign_stmt(disc_cond_place, disc_cond_val);
        let disc_cond_op = self.builder.insert_copy_op(disc_cond_place);

        // Project to variant so we can check field sub-pattern conditions.
        // This is emitted unconditionally; reading wrong-variant data before the
        // discriminant check is harmless at the IR level (just an address computation).
        let variant_ty = self.builder.ctxt.tys.get_enum_variant_ty(enum_ty, variant_idx).unwrap();
        let variant_place = self
            .builder
            .insert_project_to_variant_place(enum_place, variant_idx, variant_ty);

        self.lower_field_conditions(disc_cond_op, pattern.fields, variant_ty, variant_place)
    }

    fn lower_field_conditions(
        &mut self,
        guard: mlr::Op<'ctxt>,
        fields: &'ctxt [hlr::VariantPatternField<'ctxt>],
        variant_ty: ty::Ty<'ctxt>,
        variant_place: mlr::Place<'ctxt>,
    ) -> mlr::Op<'ctxt> {
        let [field, remaining_fields @ ..] = fields else {
            return guard;
        };

        let bool_ty = self.builder.ctxt.tys.primitive(ty::Primitive::Boolean);
        let field_ty = self
            .builder
            .ctxt
            .tys
            .get_struct_field_ty(variant_ty, field.field_index)
            .unwrap();
        let field_place = self
            .builder
            .insert_field_access_place(variant_place, field.field_index, field_ty);
        let combined_place = self.builder.alloc_place(bool_ty);

        self.builder.start_block();
        // Inside this block guard holds — use field condition as the new guard for remaining.
        let field_cond = self.lower_pattern_condition(field.pattern, field_ty, field_place);
        let remaining_cond = self.lower_field_conditions(field_cond, remaining_fields, variant_ty, variant_place);
        self.builder
            .insert_assign_stmt(combined_place, self.builder.insert_use_val(remaining_cond));
        let then_block = self.builder.end_block();

        self.builder.start_block();
        let false_op = self.builder.insert_bool_const(false);
        self.builder
            .insert_assign_stmt(combined_place, self.builder.insert_use_val(false_op));
        let else_block = self.builder.end_block();

        self.builder.insert_if_stmt(guard, then_block, else_block);
        self.builder.insert_copy_op(combined_place)
    }

    fn lower_match_arm(
        &mut self,
        scrutinee_ty: ty::Ty<'ctxt>,
        arm: &'ctxt hlr::MatchArm<'ctxt>,
        scrutinee_place: mlr::Place<'ctxt>,
    ) -> mlr::Val<'ctxt> {
        match arm.pattern {
            hlr::PatternKind::Identifier { .. } => {
                self.lower_pattern_bindings(arm.pattern, scrutinee_place, scrutinee_ty, MatchBinding::Direct);
            }
            hlr::PatternKind::Variant(pattern) => {
                self.lower_variant_pattern_arm(pattern, scrutinee_ty, scrutinee_place);
            }
        }
        self.lower_to_val(arm.body)
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
            self.lower_pattern_bindings(field.pattern, field_place, field_ty, binding);
        }
    }

    fn lower_pattern_bindings(
        &mut self,
        pattern: hlr::Pattern<'ctxt>,
        place: mlr::Place<'ctxt>,
        place_ty: ty::Ty<'ctxt>,
        binding: MatchBinding,
    ) {
        match pattern {
            hlr::PatternKind::Identifier { var_id, mutable } => {
                let binding_ty = self.typing.var_types[var_id];
                let loc = if *mutable {
                    self.builder.alloc_mut_loc(binding_ty)
                } else {
                    self.builder.alloc_loc(binding_ty)
                };
                let val = match binding {
                    MatchBinding::Direct => self.builder.copy_val(place),
                    MatchBinding::ByRef => self.builder.insert_addr_of_val(place),
                    MatchBinding::ByRefMut => self.builder.insert_addr_of_mut_val(place),
                };
                self.builder.insert_assign_to_loc_stmt(loc, val);
                self.var_locs.insert(*var_id, loc);
            }
            hlr::PatternKind::Variant(nested_pattern) => {
                let hlr::Val::Variant(_, variant_idx, _) = &nested_pattern.variant else {
                    panic!("nested variant pattern must have Val::Variant")
                };
                let variant_idx = *variant_idx;

                let (enum_ty, _, enum_place) = self.resolve_enum_place(place_ty, place);
                let variant_ty = self.builder.ctxt.tys.get_enum_variant_ty(enum_ty, variant_idx).unwrap();
                let variant_place = self
                    .builder
                    .insert_project_to_variant_place(enum_place, variant_idx, variant_ty);

                for field in nested_pattern.fields {
                    let field_ty = self
                        .builder
                        .ctxt
                        .tys
                        .get_struct_field_ty(variant_ty, field.field_index)
                        .unwrap();
                    let field_place =
                        self.builder
                            .insert_field_access_place(variant_place, field.field_index, field_ty);
                    // binding propagates from the top-level arm unchanged
                    self.lower_pattern_bindings(field.pattern, field_place, field_ty, binding);
                }
            }
        }
    }

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
