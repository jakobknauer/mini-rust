use crate::{
    ctxt::{mlr, ty},
    h2m::{H2MResult, macros::assign_to_fresh_alloc},
    hlr,
    typechecker::TyErr,
};

impl<'a> super::H2M<'a> {
    pub fn get_arm_indices(
        &self,
        arms: &[hlr::MatchArm],
        enum_def: &ty::EnumDef,
        ty: &ty::Ty,
    ) -> H2MResult<Vec<usize>> {
        arms.iter()
            .map(|arm| -> H2MResult<_> {
                enum_def
                    .variants
                    .iter()
                    .position(|variant| variant.name == arm.pattern.variant)
                    .ok_or(TyErr::NotAnEnumVariant {
                        ty: *ty,
                        variant_name: arm.pattern.variant.clone(),
                    })
                    .map_err(Into::into)
            })
            .collect::<H2MResult<_>>()
    }

    pub fn build_arm_condition(
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

    pub fn build_arm_block(
        &mut self,
        arm: &hlr::MatchArm,
        enum_ty: &ty::Ty,
        variant_index: &usize,
        base_place: &mlr::Place,
    ) -> H2MResult<mlr::Val> {
        let variant_place = self.insert_project_to_variant_place(*base_place, *variant_index)?;

        let enum_def = self.ctxt.tys.get_enum_def_by_ty(enum_ty)?;
        let enum_variant = enum_def
            .variants
            .get(*variant_index)
            .expect("variant index should be valid");
        let enum_variant_struct_def = self.ctxt.tys.get_struct_def_by_ty(&enum_variant.ty)?;

        let field_indices: Vec<usize> = arm
            .pattern
            .fields
            .iter()
            .map(|hlr::StructPatternField { field_name, .. }| -> H2MResult<_> {
                enum_variant_struct_def
                    .fields
                    .iter()
                    .position(|f| f.name == *field_name)
                    .ok_or(TyErr::NotAStructField {
                        ty: enum_variant.ty,
                        field_name: field_name.clone(),
                    })
                    .map_err(Into::into)
            })
            .collect::<H2MResult<_>>()?;

        self.push_scope();

        // bind pattern variables
        for (hlr::StructPatternField { binding_name, .. }, field_index) in arm.pattern.fields.iter().zip(field_indices)
        {
            let field_place = self.insert_field_access_place(variant_place, field_index)?;
            let field_ty = self.ctxt.mlr.get_place_ty(&field_place);
            let assign_loc = self.ctxt.mlr.insert_typed_loc(field_ty);
            self.add_to_scope(binding_name, assign_loc);
        }

        let output = self.lower_to_val(&arm.value)?;

        self.pop_scope();

        Ok(output)
    }
}
