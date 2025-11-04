use crate::{
    ctxt::types::{EnumDefinition, EnumId, NamedType, Type, TypeId},
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
                    .ok_or(MlrBuilderError::TypeError(TypeError::InvalidVariantName {
                        type_id: *type_id,
                        variant_name: arm.pattern.variant.clone(),
                    }))
            })
            .collect::<Result<_>>()
    }

    pub fn build_arm_condition(
        &mut self,
        variant_index: &usize,
        eq_fn_loc: &mlr::LocId,
        discriminant_loc: &mlr::LocId,
    ) -> Result<mlr::ValId> {
        let (variant_discriminant_loc, variant_discriminant_stmt) = assign_to_new_loc!(self, {
            let val = mlr::Value::Constant(mlr::Constant::Int(*variant_index as i64));
            self.insert_val(val)?
        });

        let (cond_loc, cond_stmt) = assign_to_new_loc!(self, {
            let init = mlr::Value::Call {
                callable: *eq_fn_loc,
                args: vec![*discriminant_loc, variant_discriminant_loc],
            };
            self.insert_val(init)?
        });

        let statements: Vec<mlr::StmtId> = vec![variant_discriminant_stmt, cond_stmt];
        let block = mlr::Value::Block(mlr::Block {
            statements,
            output: cond_loc,
        });
        self.insert_val(block)
    }

    pub fn build_arm_block(
        &mut self,
        arm: &hlr::MatchArm,
        enum_id: &EnumId,
        variant_index: &usize,
        base_place: &mlr::PlaceId,
    ) -> Result<mlr::Block> {
        let variant_place = mlr::Place::ProjectToVariant {
            base: *base_place,
            enum_id: *enum_id,
            variant_index: *variant_index,
        };
        let variant_place = self.insert_place(variant_place)?;

        let enum_def = self
            .ctxt
            .type_registry
            .get_enum_definition(enum_id)
            .expect("enum definition should be registered");
        let enum_variant = enum_def
            .variants
            .get(*variant_index)
            .expect("variant index should be valid");
        let enum_variant_type = self.ctxt.type_registry.get_type_by_id(&enum_variant.type_id).unwrap();
        let &Type::NamedType(_, NamedType::Struct(enum_variant_struct_id)) = enum_variant_type else {
            return Err(MlrBuilderError::TypeError(TypeError::NotAStruct {
                type_id: enum_variant.type_id,
            }));
        };
        let enum_variant_struct_def = self
            .ctxt
            .type_registry
            .get_struct_definition(&enum_variant_struct_id)
            .unwrap();

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

        // create a new scope for the arm
        self.scopes.push_back(super::Scope::new());

        let mut statements: Vec<mlr::StmtId> = vec![];

        // bind pattern variables
        for (hlr::StructPatternField { binding_name, .. }, field_index) in arm.pattern.fields.iter().zip(field_indices)
        {
            let field_place = mlr::Place::FieldAccess {
                base: variant_place,
                struct_id: enum_variant_struct_id,
                field_index,
            };
            let field_place = self.insert_place(field_place)?;

            let (assign_loc, assign_stmt) = assign_to_new_loc!(self, {
                let val = mlr::Value::Use(field_place);
                self.insert_val(val)?
            });

            self.current_scope().vars.insert(binding_name.clone(), assign_loc);

            statements.push(assign_stmt);
        }

        // build arm value
        let (value_loc, value_stmt) = assign_to_new_loc!(self, self.lower_to_val(&arm.value)?);
        statements.push(value_stmt);

        // pop arm scope
        self.scopes.pop_back();

        let block = mlr::Block {
            statements,
            output: value_loc,
        };
        Ok(block)
    }
}
