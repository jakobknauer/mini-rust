use crate::{
    ctxt::types::{EnumDefinition, TypeId},
    hlr,
    mlr::{
        self,
        build::{MlrBuilderError, Result, TypeError, macros::assign_to_new_loc},
    },
};

impl<'a> super::MlrBuilder<'a> {
    pub fn get_arm_indices(
        &self,
        arms: &[hlr::MatchArm],
        enum_def: &EnumDefinition,
        type_id: &TypeId,
    ) -> Result<Vec<usize>> {
        arms.iter()
            .map(|arm| {
                enum_def
                    .variants
                    .iter()
                    .position(|variant| variant.name == arm.pattern.variant)
                    .ok_or(MlrBuilderError::TypeError(TypeError::NotAnEnumVariant {
                        type_id: *type_id,
                        variant_name: arm.pattern.variant.clone(),
                    }))
            })
            .collect::<Result<_>>()
    }

    pub fn build_arm_condition(
        &mut self,
        variant_index: &usize,
        eq_fn: &mlr::OpId,
        discriminant: &mlr::OpId,
    ) -> Result<mlr::OpId> {
        let variant_index = self.insert_int_op(*variant_index as i64)?;
        let condition_loc = assign_to_new_loc!(self, self.insert_call_val(*eq_fn, vec![*discriminant, variant_index])?);
        self.insert_copy_loc_op(condition_loc)
    }

    pub fn build_arm_block(
        &mut self,
        arm: &hlr::MatchArm,
        enum_type_id: &TypeId,
        variant_index: &usize,
        base_place: &mlr::PlaceId,
    ) -> Result<mlr::ValId> {
        let variant_place = self.insert_project_to_variant_place(*base_place, *variant_index)?;

        let enum_def = self.get_enum_def(enum_type_id)?;
        let enum_variant = enum_def
            .variants
            .get(*variant_index)
            .expect("variant index should be valid");
        let enum_variant_struct_def = self.get_struct_def(&enum_variant.type_id)?;

        let field_indices: Vec<usize> = arm
            .pattern
            .fields
            .iter()
            .map(|hlr::StructPatternField { field_name, .. }| {
                enum_variant_struct_def
                    .fields
                    .iter()
                    .position(|f| f.name == *field_name)
                    .ok_or(MlrBuilderError::TypeError(TypeError::NotAStructField {
                        type_id: enum_variant.type_id,
                        field_name: field_name.clone(),
                    }))
            })
            .collect::<Result<_>>()?;

        self.push_scope();

        // bind pattern variables
        for (hlr::StructPatternField { binding_name, .. }, field_index) in arm.pattern.fields.iter().zip(field_indices)
        {
            let field_place = self.insert_field_access_place(variant_place, field_index)?;
            let assign_loc = assign_to_new_loc!(self, self.insert_use_place_val(field_place)?);
            self.add_to_scope(binding_name, assign_loc);
        }

        let output = self.lower_to_val(&arm.value)?;

        self.pop_scope();

        Ok(output)
    }
}
