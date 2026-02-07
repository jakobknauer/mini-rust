use crate::{
    ast,
    ast_lowering::{AstLoweringResult, macros::assign_to_fresh_alloc},
    ctxt::{mlr, ty},
};

impl<'a> super::AstLowerer<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn build_match_arms(
        &mut self,
        enum_ty: ty::Ty,
        by_ref: bool,
        arms: &[ast::MatchArm],
        variant_indices: &[usize],
        eq_fn: mlr::Op,
        discriminant_op: mlr::Op,
        scrutinee_place: mlr::Place,
        result_place: mlr::Place,
        expected: Option<ty::Ty>,
    ) -> AstLoweringResult<()> {
        assert!(
            arms.len() == variant_indices.len(),
            "arm_variant_indices length should match arms length"
        );

        match (arms, variant_indices) {
            ([], []) => panic!("Match expressions must have at least one arm."),

            ([arm], [variant_index]) => {
                let arm_result =
                    self.build_arm_block(enum_ty, by_ref, arm, *variant_index, scrutinee_place, expected)?;
                self.builder.insert_assign_stmt(result_place, arm_result)?;
                Ok(())
            }

            ([first_arm, arms @ ..], [first_variant_index, variant_indices @ ..]) => {
                let condition = self.build_arm_condition(*first_variant_index, eq_fn, discriminant_op)?;

                self.builder.start_new_block();
                let first_arm_result = self.build_arm_block(
                    enum_ty,
                    by_ref,
                    first_arm,
                    *first_variant_index,
                    scrutinee_place,
                    expected,
                )?;
                self.builder.insert_assign_stmt(result_place, first_arm_result)?;
                let then_block = self.builder.release_current_block();

                self.builder.start_new_block();
                self.build_match_arms(
                    enum_ty,
                    by_ref,
                    arms,
                    variant_indices,
                    eq_fn,
                    discriminant_op,
                    scrutinee_place,
                    result_place,
                    expected,
                )?;
                let else_block = self.builder.release_current_block();

                self.builder.insert_if_stmt(condition, then_block, else_block)?;
                Ok(())
            }

            _ => unreachable!("unreachable match arm pattern in build_match_arms"),
        }
    }

    fn build_arm_condition(
        &mut self,
        variant_index: usize,
        eq_fn: mlr::Op,
        discriminant: mlr::Op,
    ) -> AstLoweringResult<mlr::Op> {
        let variant_index = self.builder.insert_int_op(variant_index as i64)?;
        let condition_place = assign_to_fresh_alloc!(
            self,
            self.builder.insert_call_val(eq_fn, vec![discriminant, variant_index])?
        );
        self.builder.insert_copy_op(condition_place)
    }

    fn build_arm_block(
        &mut self,
        enum_ty: ty::Ty,
        by_ref: bool,
        arm: &ast::MatchArm,
        variant_index: usize,
        base_place: mlr::Place,
        expected: Option<ty::Ty>,
    ) -> AstLoweringResult<mlr::Val> {
        let enum_variant_ty = self.tys().get_enum_variant_ty(enum_ty, variant_index)?;
        let field_indices = self.typechecker().resolve_struct_fields(
            enum_variant_ty,
            arm.pattern.fields.iter().map(|f| f.field_name.as_str()),
        )?;

        self.builder.push_scope();

        // bind pattern variables to fresh locations
        let variant_place = self
            .builder
            .insert_project_to_variant_place(base_place, variant_index)?;
        for (ast::VariantPatternField { binding_name, .. }, field_index) in arm.pattern.fields.iter().zip(field_indices)
        {
            let field_place = self.builder.insert_field_access_place(variant_place, field_index)?;
            let field_ty = self.mlr().get_place_ty(field_place);
            if by_ref {
                let field_ref_ty = self.tys().ref_(field_ty);

                let field_addr = self.builder.insert_addr_of_val(field_place)?;
                let assign_loc = self.mlr().insert_typed_loc(field_ref_ty);
                self.builder.insert_alloc_stmt(assign_loc)?;
                self.builder.insert_assign_to_loc_stmt(assign_loc, field_addr)?;

                self.builder.add_binding(binding_name, assign_loc);
            } else {
                let field_ty = self.mlr().get_place_ty(field_place);

                let field_val = self.builder.insert_use_place_val(field_place)?;
                let assign_loc = self.mlr().insert_typed_loc(field_ty);
                self.builder.insert_alloc_stmt(assign_loc)?;
                self.builder.insert_assign_to_loc_stmt(assign_loc, field_val)?;

                self.builder.add_binding(binding_name, assign_loc);
            }
        }

        // build actual arm block
        let output = self.lower_to_val(arm.value, expected)?;

        self.builder.pop_scope();

        Ok(output)
    }
}
