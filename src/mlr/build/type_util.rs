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
            Call { callable, args } => self.infer_type_of_call(callable, args),
            Function(fn_id) => self.infer_type_of_function(*fn_id),
            If(if_) => self.infer_type_of_if(if_),
            Loop { .. } => self
                .ctxt
                .type_registry
                .get_primitive_type_id(PrimitiveType::Unit)
                .ok_or(MlrBuilderError::UnknownPrimitiveType),
            Empty { type_id } => Ok(*type_id),
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
        callable: &mlr::LocId,
        args: &[mlr::LocId],
    ) -> std::result::Result<TypeId, MlrBuilderError> {
        let callable_type = self
            .output
            .loc_types
            .get(callable)
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

    pub fn infer_place_type(&self, place_id: &mlr::PlaceId) -> mlr::build::Result<TypeId> {
        use mlr::Place::*;

        let place = self
            .output
            .places
            .get(place_id)
            .expect("infer_type_of_place should only be called with a valid PlaceId");

        match place {
            Local(loc_id) => self.infer_type_of_local_place(loc_id),
            FieldAccess {
                base,
                struct_id,
                field_index,
            } => self.infer_type_of_field_access_place(base, struct_id, field_index),
            EnumDiscriminant { base, enum_id } => self.infer_type_of_enum_discriminant(base, enum_id),
            ProjectToVariant {
                base,
                enum_id,
                variant_index,
            } => self.infer_type_of_project_to_variant_place(base, enum_id, variant_index),
        }
    }

    fn infer_type_of_local_place(&self, loc_id: &mlr::LocId) -> mlr::build::Result<TypeId> {
        Ok(*self
            .output
            .loc_types
            .get(loc_id)
            .expect("type of loc_id should be registered"))
    }

    fn infer_type_of_field_access_place(
        &self,
        base: &mlr::PlaceId,
        struct_id: &StructId,
        field_index: &usize,
    ) -> std::result::Result<TypeId, MlrBuilderError> {
        let base_type_id = self
            .output
            .place_types
            .get(base)
            .expect("type of base place should be registered");

        let base_type = self
            .ctxt
            .type_registry
            .get_type_by_id(base_type_id)
            .expect("type of base place should be registered");

        let Type::NamedType(_, NamedType::Struct(base_struct_id)) = base_type else {
            return TypeError::NotAStruct { type_id: *base_type_id }.into();
        };

        if base_struct_id != struct_id {
            return TypeError::FieldAccessBaseTypeMismatch {
                expected: *struct_id,
                actual: *base_struct_id,
            }
            .into();
        }

        let struct_def = self
            .ctxt
            .type_registry
            .get_struct_definition(base_struct_id)
            .expect("struct definition should be registered");

        let field_type = struct_def
            .fields
            .get(*field_index)
            .expect("field index should be valid")
            .type_id;

        Ok(field_type)
    }

    fn infer_type_of_enum_discriminant(
        &self,
        base: &mlr::PlaceId,
        enum_id: &EnumId,
    ) -> std::result::Result<TypeId, MlrBuilderError> {
        let base_type_id = self
            .output
            .place_types
            .get(base)
            .expect("type of base place should be registered");

        let base_type = self
            .ctxt
            .type_registry
            .get_type_by_id(base_type_id)
            .expect("type of base place should be registered");

        let Type::NamedType(_, NamedType::Enum(_)) = base_type else {
            return TypeError::NotAnEnum { type_id: *base_type_id }.into();
        };

        let _enum_def = self
            .ctxt
            .type_registry
            .get_enum_definition(enum_id)
            .expect("enum definition should be registered");

        // the discriminant is always an integer
        let int_type_id = self
            .ctxt
            .type_registry
            .get_primitive_type_id(PrimitiveType::Integer32)
            .expect("integer primitive type should be registered");
        Ok(int_type_id)
    }

    fn infer_type_of_project_to_variant_place(
        &self,
        base: &mlr::PlaceId,
        enum_id: &EnumId,
        variant_index: &usize,
    ) -> std::result::Result<TypeId, MlrBuilderError> {
        let base_type_id = self
            .output
            .place_types
            .get(base)
            .expect("type of base place should be registered");

        let base_type = self
            .ctxt
            .type_registry
            .get_type_by_id(base_type_id)
            .expect("type of base place should be registered");

        let Type::NamedType(_, NamedType::Enum(base_enum_id)) = base_type else {
            return TypeError::NotAnEnum { type_id: *base_type_id }.into();
        };

        if base_enum_id != enum_id {
            return TypeError::ProjectToVariantBaseTypeMismatch {
                expected: *enum_id,
                actual: *base_enum_id,
            }
            .into();
        }

        let variant_type = self
            .ctxt
            .type_registry
            .get_enum_variant(enum_id, variant_index)
            .expect("enum variant should be registered")
            .type_id;

        Ok(variant_type)
    }
}
