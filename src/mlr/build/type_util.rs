use std::collections::HashSet;

use crate::{
    ctxt::{functions::FnId, types::*},
    mlr::{
        self,
        build::{MlrBuilderError, TypeError},
    },
};

impl<'a> mlr::MlrBuilder<'a> {
    pub fn infer_val_type(&mut self, val: mlr::ValId) -> mlr::build::Result<TypeId> {
        use mlr::Value::*;

        let val = self
            .output
            .vals
            .get(&val)
            .expect("infer_type should only be called with a valid ValId");

        match val {
            Block(block) => self.infer_type_of_block(block),
            Constant(constant) => self.infer_type_of_constant(constant),
            Use(place) => self.infer_place_type(place),
            Call { callable, args } => self.infer_type_of_call(*callable, args),
            Function(fn_id) => self.infer_type_of_function(*fn_id),
            If(if_) => self.infer_type_of_if(if_),
            Loop { .. } => self
                .ctxt
                .type_registry
                .get_primitive_type_id(PrimitiveType::Unit)
                .ok_or(MlrBuilderError::UnknownPrimitiveType),
            Struct {
                type_id,
                field_initializers,
            } => self.infer_type_of_struct(type_id, field_initializers),
        }
    }

    fn infer_type_of_block(&self, block: &mlr::Block) -> mlr::build::Result<TypeId> {
        Ok(*self
            .output
            .loc_types
            .get(&block.output)
            .expect("type of block.output should have been inferred before"))
    }

    fn infer_type_of_constant(&self, constant: &mlr::Constant) -> mlr::build::Result<TypeId> {
        let type_ = match constant {
            mlr::Constant::Int(_) => PrimitiveType::Integer32,
            mlr::Constant::Bool(_) => PrimitiveType::Boolean,
            mlr::Constant::Unit => PrimitiveType::Unit,
        };

        self.ctxt
            .type_registry
            .get_primitive_type_id(type_)
            .ok_or(MlrBuilderError::UnknownPrimitiveType)
    }

    fn infer_type_of_call(
        &self,
        callable: mlr::LocId,
        args: &[mlr::LocId],
    ) -> std::result::Result<TypeId, MlrBuilderError> {
        let callable_type = self
            .output
            .loc_types
            .get(&callable)
            .expect("type of location should be registered");
        let callable_type = self
            .ctxt
            .type_registry
            .get_type_by_id(callable_type)
            .expect("type of callable should be registered");

        let Type::Function {
            param_types,
            return_type,
        } = callable_type
        else {
            return TypeError::ValNotCallable.into();
        };

        let arg_types = args.iter().map(|arg_loc| {
            self.output
                .loc_types
                .get(arg_loc)
                .expect("type of location should be registered")
        });

        if param_types.len() != arg_types.len() {
            return TypeError::CallArgumentCountMismatch {
                expected: param_types.len(),
                actual: arg_types.len(),
            }
            .into();
        }

        for (i, (param_type, arg_type)) in param_types.iter().zip(arg_types).enumerate() {
            if !self.ctxt.type_registry.types_equal(param_type, arg_type) {
                return TypeError::CallArgumentTypeMismatch {
                    index: i,
                    expected: *param_type,
                    actual: *arg_type,
                }
                .into();
            }
        }

        Ok(*return_type)
    }

    fn infer_type_of_function(&mut self, fn_id: FnId) -> mlr::build::Result<TypeId> {
        let signature = self
            .ctxt
            .function_registry
            .get_signature_by_id(&fn_id)
            .expect("function signature should be registered");

        let param_types: Vec<_> = signature.parameters.iter().map(|param| param.type_).collect();
        let return_type = signature.return_type;
        let function_type_id = self.ctxt.type_registry.register_function_type(param_types, return_type);

        Ok(function_type_id)
    }

    fn infer_type_of_if(&self, if_: &mlr::If) -> mlr::build::Result<TypeId> {
        let condition_type = self
            .output
            .loc_types
            .get(&if_.condition)
            .expect("type of condition should be registered");

        let bool_type_id = self
            .ctxt
            .type_registry
            .get_primitive_type_id(PrimitiveType::Boolean)
            .expect("boolean primitive type should be registered");

        if !self.ctxt.type_registry.types_equal(condition_type, &bool_type_id) {
            return TypeError::IfConditionNotBoolean {
                actual: *condition_type,
            }
            .into();
        }

        let then_type = *self
            .output
            .loc_types
            .get(&if_.then_block.output)
            .expect("type of then_block.output should be registered");

        let else_type = *self
            .output
            .loc_types
            .get(&if_.else_block.output)
            .expect("type of else_block.output should be registered");

        if self.ctxt.type_registry.types_equal(&then_type, &else_type) {
            Ok(then_type)
        } else {
            TypeError::IfBranchTypeMismatch { then_type, else_type }.into()
        }
    }

    fn infer_type_of_struct(
        &self,
        type_id: &TypeId,
        field_initializers: &[(String, mlr::LocId)],
    ) -> std::result::Result<TypeId, MlrBuilderError> {
        // Get struct definition
        let struct_def = self
            .ctxt
            .type_registry
            .get_struct_definition_by_type_id(type_id)
            .ok_or(MlrBuilderError::TypeError(TypeError::NotAStruct { type_id: *type_id }))?;

        // Check for missing or extra fields
        let specified_field_names: HashSet<_> = field_initializers.iter().map(|(name, _)| name.as_str()).collect();
        let required_field_names: HashSet<_> = struct_def.fields.iter().map(|field| field.name.as_str()).collect();

        let missing_fields: Vec<_> = required_field_names
            .difference(&specified_field_names)
            .cloned()
            .collect();
        if !missing_fields.is_empty() {
            return TypeError::StructValMissingFields {
                type_id: *type_id,
                missing_fields: missing_fields.into_iter().map(|s| s.to_string()).collect(),
            }
            .into();
        }

        let extra_fields: Vec<_> = specified_field_names
            .difference(&required_field_names)
            .cloned()
            .collect();
        if !extra_fields.is_empty() {
            return TypeError::StructValExtraFields {
                type_id: *type_id,
                extra_fields: extra_fields.into_iter().map(|s| s.to_string()).collect(),
            }
            .into();
        }

        // Check for type mismatches
        for StructField {
            name: field_name,
            type_id: field_type_id,
        } in &struct_def.fields
        {
            let specified_type_id = field_initializers
                .iter()
                .find(|(name, _)| name == field_name)
                .map(|(_, loc_id)| {
                    self.output
                        .loc_types
                        .get(loc_id)
                        .expect("type of location should be registered")
                })
                .unwrap();

            if !self.ctxt.type_registry.types_equal(field_type_id, specified_type_id) {
                return TypeError::StructValTypeMismatch {
                    type_id: *type_id,
                    field_name: field_name.clone(),
                    expected: *field_type_id,
                    actual: *specified_type_id,
                }
                .into();
            }
        }

        Ok(*type_id)
    }

    pub fn infer_place_type(&self, place_id: &mlr::PlaceId) -> mlr::build::Result<TypeId> {
        let place = self
            .output
            .places
            .get(place_id)
            .expect("infer_type_of_place should only be called with a valid PlaceId");

        match place {
            mlr::Place::Local(loc_id) => self.infer_type_of_local_place(loc_id),
            mlr::Place::FieldAccess { base, field_name } => self.infer_type_of_field_access_place(base, field_name),
        }
    }

    fn infer_type_of_local_place(&self, loc_id: &mlr::LocId) -> mlr::build::Result<TypeId> {
        Ok(*self
            .output
            .loc_types
            .get(loc_id)
            .expect("infer_type_of_local_place: type of loc_id should be registered"))
    }

    fn infer_type_of_field_access_place(
        &self,
        base: &mlr::PlaceId,
        field_name: &str,
    ) -> std::result::Result<TypeId, MlrBuilderError> {
        // Get struct definition
        let base_type_id = self
            .output
            .place_types
            .get(base)
            .expect("type of base place should be registered");
        let struct_def = self
            .ctxt
            .type_registry
            .get_struct_definition_by_type_id(base_type_id)
            .ok_or(MlrBuilderError::TypeError(TypeError::NotAStruct {
                type_id: *base_type_id,
            }))?;

        // Find field
        let field =
            struct_def
                .fields
                .iter()
                .find(|field| field.name == field_name)
                .ok_or(MlrBuilderError::TypeError(TypeError::NotAStructField {
                    type_id: *base_type_id,
                    field_name: field_name.to_string(),
                }))?;

        Ok(field.type_id)
    }
}
