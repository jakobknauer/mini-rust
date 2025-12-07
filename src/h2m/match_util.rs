use crate::{
    ctxt::{mlr, ty},
    h2m::{H2MResult, macros::assign_to_fresh_alloc},
    hlr,
};

impl<'a> super::H2M<'a> {
    pub fn build_match_arms(
        &mut self,
        arms: &[hlr::MatchArm],
        variant_indices: &[usize],
        eq_fn: mlr::Op,
        discriminant: mlr::Op,
        scrutinee_place: mlr::Place,
        result_place: mlr::Place,
    ) -> H2MResult<()> {
        assert!(
            arms.len() == variant_indices.len(),
            "arm_variant_indices length should match arms length"
        );

        match (arms, variant_indices) {
            ([], []) => panic!("Match expressions must have at least one arm."),

            ([arm], [variant_index]) => {
                let arm_result = self.build_arm_block(
                    arm,
                    self.ctxt.mlr.get_place_ty(&scrutinee_place),
                    *variant_index,
                    scrutinee_place,
                )?;
                self.insert_assign_stmt(result_place, arm_result)?;
                Ok(())
            }

            ([first_arm, arms @ ..], [first_variant_index, variant_indices @ ..]) => {
                let condition = self.build_arm_condition(first_variant_index, &eq_fn, &discriminant)?;

                self.start_new_block();
                let first_arm_result = self.build_arm_block(
                    first_arm,
                    self.ctxt.mlr.get_place_ty(&scrutinee_place),
                    *first_variant_index,
                    scrutinee_place,
                )?;
                self.insert_assign_stmt(result_place, first_arm_result)?;
                let then_block = self.release_current_block();

                self.start_new_block();
                self.build_match_arms(
                    arms,
                    variant_indices,
                    eq_fn,
                    discriminant,
                    scrutinee_place,
                    result_place,
                )?;
                let else_block = self.release_current_block();

                self.insert_if_stmt(condition, then_block, else_block)?;
                Ok(())
            }

            _ => unreachable!("unreachable match arm pattern in build_match_arms"),
        }
    }

    fn build_arm_condition(
        &mut self,
        variant_index: &usize,
        eq_fn: &mlr::Op,
        discriminant: &mlr::Op,
    ) -> H2MResult<mlr::Op> {
        let variant_index = self.insert_int_op(*variant_index as i64)?;
        let condition_place =
            assign_to_fresh_alloc!(self, self.insert_call_val(*eq_fn, vec![*discriminant, variant_index])?);
        self.insert_copy_op(condition_place)
    }

    fn build_arm_block(
        &mut self,
        arm: &hlr::MatchArm,
        enum_ty: ty::Ty,
        variant_index: usize,
        base_place: mlr::Place,
    ) -> H2MResult<mlr::Val> {
        let enum_variant_ty = self.ctxt.tys.get_enum_def_by_ty(enum_ty)?.variants[variant_index].ty;
        let field_indices = self.typechecker().resolve_struct_fields(
            enum_variant_ty,
            arm.pattern.fields.iter().map(|f| f.field_name.as_str()),
        )?;

        self.push_scope();

        // bind pattern variables to fresh locations
        let variant_place = self.insert_project_to_variant_place(base_place, variant_index)?;
        for (hlr::StructPatternField { binding_name, .. }, field_index) in arm.pattern.fields.iter().zip(field_indices)
        {
            let field_place = self.insert_field_access_place(variant_place, field_index)?;
            let field_ty = self.ctxt.mlr.get_place_ty(&field_place);
            let field_val = self.insert_use_place_val(field_place)?;

            let assign_loc = self.ctxt.mlr.insert_typed_loc(field_ty);
            self.insert_alloc_stmt(assign_loc)?;

            self.insert_assign_to_loc_stmt(assign_loc, field_val)?;
            self.add_to_scope(binding_name, assign_loc);
        }

        // build actual arm block
        let output = self.lower_to_val(&arm.value)?;

        self.pop_scope();

        Ok(output)
    }
}
