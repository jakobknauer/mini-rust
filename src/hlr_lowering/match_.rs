use crate::ctxt::{language_items, ty};
use crate::typeck::MatchBinding;
use crate::{hlr, mlr};

use super::lowered_expr::LoweredExpr;

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

        result_place.into()
    }

    fn lower_match_arms(
        &mut self,
        scrutinee_ty: ty::Ty<'ctxt>,
        arms: &'ctxt [hlr::MatchArm<'ctxt>],
        scrutinee_place: mlr::Place<'ctxt>,
        result_place: mlr::Place<'ctxt>,
    ) {
        match arms {
            [] => self.builder.insert_unreachable_stmt(),
            [first_arm, remaining_arms @ ..] => {
                let cond_op = self.lower_pattern_condition(first_arm.pattern, scrutinee_ty, scrutinee_place);

                self.builder.start_block();
                self.lower_pattern_bindings(first_arm.pattern, scrutinee_place, scrutinee_ty, MatchBinding::Direct);
                let arm_val = self.lower_to_val(first_arm.body);
                self.builder.insert_assign_stmt(result_place, arm_val);
                let then_block = self.builder.end_block();

                self.builder.start_block();
                self.lower_match_arms(scrutinee_ty, remaining_arms, scrutinee_place, result_place);
                let else_block = self.builder.end_block();

                self.builder.insert_if_stmt(cond_op, then_block, else_block);
            }
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
            hlr::PatternKind::Or(alternatives) => {
                self.lower_or_pattern_bindings(alternatives, place, place_ty, binding);
            }
            hlr::PatternKind::Wildcard | hlr::PatternKind::Lit(_) => {}
            hlr::PatternKind::Identifier { var_id, mutable } => {
                self.lower_identifier_pattern_bindings(*var_id, *mutable, place, binding);
            }
            hlr::PatternKind::Struct(nested_pattern) => {
                self.lower_struct_pattern_bindings(pattern, nested_pattern, place, place_ty);
            }
            hlr::PatternKind::Variant(nested_pattern) => {
                self.lower_variant_pattern_bindings(pattern, nested_pattern, place, place_ty);
            }
            hlr::PatternKind::Tuple(sub_patterns) => {
                self.lower_tuple_pattern_bindings(pattern, sub_patterns, place, place_ty);
            }
            hlr::PatternKind::Ref(inner) | hlr::PatternKind::RefMut(inner) => {
                let (inner_ty, inner_place) = self.deref_scrutinee_place(place_ty, place);
                self.lower_pattern_bindings(inner, inner_place, inner_ty, MatchBinding::Direct);
            }
        }
    }

    fn lower_identifier_pattern_bindings(
        &mut self,
        var_id: hlr::VarId,
        mutable: bool,
        place: mlr::Place<'ctxt>,
        binding: MatchBinding,
    ) {
        let binding_ty = self.typing.var_types[&var_id];
        let loc = if let Some(&existing) = self.var_locs.get(&var_id) {
            existing
        } else {
            let loc = if mutable {
                self.builder.alloc_mut_loc(binding_ty)
            } else {
                self.builder.alloc_loc(binding_ty)
            };
            self.var_locs.insert(var_id, loc);
            loc
        };
        let val = match binding {
            MatchBinding::Direct => self.builder.copy_val(place),
            MatchBinding::ByRef => self.builder.insert_addr_of_val(place),
            MatchBinding::ByRefMut => self.builder.insert_addr_of_mut_val(place),
        };
        self.builder.insert_assign_to_loc_stmt(loc, val);
    }

    fn lower_struct_pattern_bindings(
        &mut self,
        pattern: hlr::Pattern<'ctxt>,
        nested_pattern: &'ctxt hlr::StructPattern<'ctxt>,
        place: mlr::Place<'ctxt>,
        place_ty: ty::Ty<'ctxt>,
    ) {
        let binding = self.pattern_binding(pattern);
        let (struct_ty, struct_place) = self.deref_scrutinee_place(place_ty, place);
        for field in nested_pattern.fields {
            let field_ty = self
                .builder
                .ctxt
                .tys
                .get_struct_field_ty(struct_ty, field.field_index)
                .unwrap();
            let field_place = self
                .builder
                .insert_field_access_place(struct_place, field.field_index, field_ty);
            self.lower_pattern_bindings(field.pattern, field_place, field_ty, binding);
        }
    }

    fn lower_variant_pattern_bindings(
        &mut self,
        pattern: hlr::Pattern<'ctxt>,
        nested_pattern: &'ctxt hlr::VariantPattern<'ctxt>,
        place: mlr::Place<'ctxt>,
        place_ty: ty::Ty<'ctxt>,
    ) {
        let binding = self.pattern_binding(pattern);
        let hlr::Val::Variant(_, variant_idx, _) = &nested_pattern.variant else {
            panic!("nested variant pattern must have Val::Variant")
        };
        let variant_idx = *variant_idx;

        let (enum_ty, enum_place) = self.deref_scrutinee_place(place_ty, place);
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
            let field_place = self
                .builder
                .insert_field_access_place(variant_place, field.field_index, field_ty);
            self.lower_pattern_bindings(field.pattern, field_place, field_ty, binding);
        }
    }

    fn lower_tuple_pattern_bindings(
        &mut self,
        pattern: hlr::Pattern<'ctxt>,
        sub_patterns: &'ctxt [hlr::Pattern<'ctxt>],
        place: mlr::Place<'ctxt>,
        place_ty: ty::Ty<'ctxt>,
    ) {
        let binding = self.pattern_binding(pattern);
        let (tuple_ty, tuple_place) = self.deref_scrutinee_place(place_ty, place);
        let field_tys = tuple_ty.tuple_field_tys().unwrap();
        for (i, (&sub_pattern, &field_ty)) in sub_patterns.iter().zip(field_tys).enumerate() {
            let field_place = self.builder.insert_field_access_place(tuple_place, i, field_ty);
            self.lower_pattern_bindings(sub_pattern, field_place, field_ty, binding);
        }
    }

    fn lower_or_pattern_bindings(
        &mut self,
        alternatives: &'ctxt [hlr::Pattern<'ctxt>],
        place: mlr::Place<'ctxt>,
        place_ty: ty::Ty<'ctxt>,
        binding: MatchBinding,
    ) {
        match alternatives {
            [] => self.builder.insert_unreachable_stmt(),
            [first, rest @ ..] => {
                let cond = self.lower_pattern_condition(first, place_ty, place);

                // Pre-allocate binding locs in the current (outer) block so all
                // branches share the same locs. lower_identifier_pattern_bindings
                // will reuse them rather than allocating new ones per branch.
                self.pre_alloc_or_bindings(first);

                self.builder.start_block();
                self.lower_pattern_bindings(first, place, place_ty, binding);
                let then_block = self.builder.end_block();

                self.builder.start_block();
                self.lower_or_pattern_bindings(rest, place, place_ty, binding);
                let else_block = self.builder.end_block();

                self.builder.insert_if_stmt(cond, then_block, else_block);
            }
        }
    }

    fn pre_alloc_or_bindings(&mut self, pattern: hlr::Pattern<'ctxt>) {
        match pattern {
            hlr::PatternKind::Identifier { var_id, mutable } => {
                if !self.var_locs.contains_key(var_id) {
                    let binding_ty = self.typing.var_types[var_id];
                    let loc = if *mutable {
                        self.builder.alloc_mut_loc(binding_ty)
                    } else {
                        self.builder.alloc_loc(binding_ty)
                    };
                    self.var_locs.insert(*var_id, loc);
                }
            }
            hlr::PatternKind::Tuple(sub_patterns) => {
                for sub in *sub_patterns {
                    self.pre_alloc_or_bindings(sub);
                }
            }
            hlr::PatternKind::Struct(nested) => {
                for field in nested.fields {
                    self.pre_alloc_or_bindings(field.pattern);
                }
            }
            hlr::PatternKind::Variant(nested) => {
                for field in nested.fields {
                    self.pre_alloc_or_bindings(field.pattern);
                }
            }
            hlr::PatternKind::Ref(inner) | hlr::PatternKind::RefMut(inner) => {
                self.pre_alloc_or_bindings(inner);
            }
            hlr::PatternKind::Or(alternatives) => {
                if let Some(&first) = alternatives.first() {
                    self.pre_alloc_or_bindings(first);
                }
            }
            hlr::PatternKind::Wildcard | hlr::PatternKind::Lit(_) => {}
        }
    }

    fn lower_pattern_condition(
        &mut self,
        pattern: hlr::Pattern<'ctxt>,
        scrutinee_ty: ty::Ty<'ctxt>,
        scrutinee_place: mlr::Place<'ctxt>,
    ) -> mlr::Op<'ctxt> {
        match pattern {
            hlr::PatternKind::Or(alternatives) => {
                let bool_ty = self.builder.ctxt.tys.primitive(ty::Primitive::Boolean);
                alternatives
                    .iter()
                    .fold(self.builder.insert_bool_const(false), |acc, &alt| {
                        let alt_cond = self.lower_pattern_condition(alt, scrutinee_ty, scrutinee_place);
                        let or_val = self.builder.insert_binary_prim_val(
                            language_items::BinaryPrimOp::BitOrBool,
                            acc,
                            alt_cond,
                            bool_ty,
                        );
                        let or_place = self.builder.alloc_place(bool_ty);
                        self.builder.insert_assign_stmt(or_place, or_val);
                        self.builder.insert_copy_op(or_place)
                    })
            }
            hlr::PatternKind::Wildcard | hlr::PatternKind::Identifier { .. } => self.builder.insert_bool_const(true),
            hlr::PatternKind::Variant(pattern) => {
                self.lower_variant_pattern_condition(pattern, scrutinee_ty, scrutinee_place)
            }
            hlr::PatternKind::Struct(pattern) => {
                let (struct_ty, struct_place) = self.deref_scrutinee_place(scrutinee_ty, scrutinee_place);
                let guard = self.builder.insert_bool_const(true);
                self.lower_field_conditions(guard, pattern.fields, struct_ty, struct_place)
            }
            hlr::PatternKind::Tuple(sub_patterns) => {
                self.lower_tuple_pattern_condition(sub_patterns, scrutinee_ty, scrutinee_place)
            }
            hlr::PatternKind::Lit(lit) => self.lower_lit_pattern_condition(lit, scrutinee_ty, scrutinee_place),
            hlr::PatternKind::Ref(inner) | hlr::PatternKind::RefMut(inner) => {
                let (inner_ty, inner_place) = self.deref_scrutinee_place(scrutinee_ty, scrutinee_place);
                self.lower_pattern_condition(inner, inner_ty, inner_place)
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

        let (enum_ty, enum_place) = self.deref_scrutinee_place(scrutinee_ty, scrutinee_place);
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
        fields: &'ctxt [hlr::PatternField<'ctxt>],
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

    fn lower_lit_pattern_condition(
        &mut self,
        lit: &hlr::Lit,
        scrutinee_ty: ty::Ty<'ctxt>,
        scrutinee_place: mlr::Place<'ctxt>,
    ) -> mlr::Op<'ctxt> {
        let (const_, eq_op) = match lit {
            hlr::Lit::Int(n) => (mlr::Const::Int(*n), language_items::BinaryPrimOp::EqI32),
            hlr::Lit::Bool(b) => (mlr::Const::Bool(*b), language_items::BinaryPrimOp::EqBool),
            hlr::Lit::CChar(c) => (mlr::Const::CChar(*c), language_items::BinaryPrimOp::EqCChar),
            hlr::Lit::CString(_) => unreachable!("CString not supported in patterns"),
        };
        let bool_ty = self.builder.ctxt.tys.primitive(ty::Primitive::Boolean);
        let scrutinee_op = self.builder.insert_copy_op(scrutinee_place);
        let lit_op = self.builder.insert_const_op(const_, scrutinee_ty);
        let cond_val = self
            .builder
            .insert_binary_prim_val(eq_op, scrutinee_op, lit_op, bool_ty);
        let cond_place = self.builder.alloc_place(bool_ty);
        self.builder.insert_assign_stmt(cond_place, cond_val);
        self.builder.insert_copy_op(cond_place)
    }

    fn lower_tuple_pattern_condition(
        &mut self,
        sub_patterns: &'ctxt [hlr::Pattern<'ctxt>],
        scrutinee_ty: ty::Ty<'ctxt>,
        scrutinee_place: mlr::Place<'ctxt>,
    ) -> mlr::Op<'ctxt> {
        let (tuple_ty, tuple_place) = self.deref_scrutinee_place(scrutinee_ty, scrutinee_place);
        let field_tys = tuple_ty.tuple_field_tys().unwrap();
        let guard = self.builder.insert_bool_const(true);
        self.lower_tuple_field_conditions(guard, sub_patterns, field_tys, tuple_place, 0)
    }

    fn lower_tuple_field_conditions(
        &mut self,
        guard: mlr::Op<'ctxt>,
        sub_patterns: &'ctxt [hlr::Pattern<'ctxt>],
        field_tys: &'ctxt [ty::Ty<'ctxt>],
        tuple_place: mlr::Place<'ctxt>,
        offset: usize,
    ) -> mlr::Op<'ctxt> {
        let ([pattern, remaining_patterns @ ..], [field_ty, remaining_tys @ ..]) = (sub_patterns, field_tys) else {
            return guard;
        };

        let bool_ty = self.builder.ctxt.tys.primitive(ty::Primitive::Boolean);
        let field_place = self.builder.insert_field_access_place(tuple_place, offset, *field_ty);
        let combined_place = self.builder.alloc_place(bool_ty);

        self.builder.start_block();
        let field_cond = self.lower_pattern_condition(pattern, *field_ty, field_place);
        let remaining_cond =
            self.lower_tuple_field_conditions(field_cond, remaining_patterns, remaining_tys, tuple_place, offset + 1);
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

    fn pattern_binding(&self, pattern: hlr::Pattern<'ctxt>) -> MatchBinding {
        self.typing.match_bindings[&(pattern as *const _)]
    }

    fn deref_scrutinee_place(
        &mut self,
        scrutinee_ty: ty::Ty<'ctxt>,
        scrutinee_place: mlr::Place<'ctxt>,
    ) -> (ty::Ty<'ctxt>, mlr::Place<'ctxt>) {
        match *scrutinee_ty.0 {
            ty::TyDef::Ref(inner) | ty::TyDef::RefMut(inner) => {
                let copy_op = self.builder.insert_copy_op(scrutinee_place);
                let deref_place = self.builder.insert_deref_place(copy_op);
                (inner, deref_place)
            }
            _ => (scrutinee_ty, scrutinee_place),
        }
    }
}
