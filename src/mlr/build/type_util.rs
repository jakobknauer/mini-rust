use crate::{
    ctxt::{functions::FnId, types::*},
    mlr::{
        self,
        build::{MlrBuilderError, Result, TypeError},
    },
};

impl<'a> mlr::MlrBuilder<'a> {
    pub fn infer_val_type(&mut self, val: mlr::ValId) -> Result<TypeId> {
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

    fn infer_type_of_block(&self, block: &mlr::Block) -> Result<TypeId> {
        Ok(self.get_val_type(&block.output))
    }

    fn infer_type_of_constant(&self, constant: &mlr::Constant) -> Result<TypeId> {
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

    fn infer_type_of_call(&self, callable: &mlr::LocId, args: &[mlr::LocId]) -> Result<TypeId> {
        let callable_type = self.get_loc_type(callable);
        let callable_type = self
            .ctxt
            .type_registry
            .get_type_by_id(&callable_type)
            .expect("type of callable should be registered");

        let Type::Function {
            param_types,
            return_type,
        } = callable_type
        else {
            return TypeError::ValNotCallable.into();
        };

        let arg_types = args.iter().map(|arg_loc| self.get_loc_type(arg_loc));

        if param_types.len() != arg_types.len() {
            return TypeError::CallArgumentCountMismatch {
                expected: param_types.len(),
                actual: arg_types.len(),
            }
            .into();
        }

        for (i, (param_type, arg_type)) in param_types.iter().zip(arg_types).enumerate() {
            if !self.ctxt.type_registry.types_equal(param_type, &arg_type) {
                return TypeError::CallArgumentTypeMismatch {
                    index: i,
                    expected: *param_type,
                    actual: arg_type,
                }
                .into();
            }
        }

        Ok(*return_type)
    }

    fn infer_type_of_function(&mut self, fn_id: FnId) -> Result<TypeId> {
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

    fn infer_type_of_if(&self, if_: &mlr::If) -> Result<TypeId> {
        let condition_type = self.get_loc_type(&if_.condition);

        let bool_type_id = self
            .ctxt
            .type_registry
            .get_primitive_type_id(PrimitiveType::Boolean)
            .expect("boolean primitive type should be registered");

        if !self.ctxt.type_registry.types_equal(&condition_type, &bool_type_id) {
            return TypeError::IfConditionNotBoolean { actual: condition_type }.into();
        }

        let then_type = self.get_val_type(&if_.then_block.output);
        let else_type = self.get_val_type(&if_.else_block.output);

        if self.ctxt.type_registry.types_equal(&then_type, &else_type) {
            Ok(then_type)
        } else {
            TypeError::IfBranchTypeMismatch { then_type, else_type }.into()
        }
    }

    pub fn infer_place_type(&self, place_id: &mlr::PlaceId) -> Result<TypeId> {
        use mlr::Place::*;

        let place = self
            .output
            .places
            .get(place_id)
            .expect("infer_type_of_place should only be called with a valid PlaceId");

        match place {
            Local(loc_id) => self.infer_type_of_local_place(loc_id),
            FieldAccess { base, field_index } => self.infer_type_of_field_access_place(base, field_index),
            EnumDiscriminant { base } => self.infer_type_of_enum_discriminant(base),
            ProjectToVariant { base, variant_index } => {
                self.infer_type_of_project_to_variant_place(base, variant_index)
            }
        }
    }

    fn infer_type_of_local_place(&self, loc_id: &mlr::LocId) -> Result<TypeId> {
        Ok(self.get_loc_type(loc_id))
    }

    fn infer_type_of_field_access_place(&self, base: &mlr::PlaceId, field_index: &usize) -> Result<TypeId> {
        let base_type_id = self.get_place_type(base);
        let struct_def = self.get_struct_def(&base_type_id)?;

        let field_type = struct_def
            .fields
            .get(*field_index)
            .expect("field index should be valid")
            .type_id;

        Ok(field_type)
    }

    fn infer_type_of_enum_discriminant(&self, base: &mlr::PlaceId) -> Result<TypeId> {
        let base_type_id = self.get_place_type(base);
        let _enum_def = self.get_enum_def(&base_type_id)?;

        // the discriminant is always an integer
        let int_type_id = self
            .ctxt
            .type_registry
            .get_primitive_type_id(PrimitiveType::Integer32)
            .expect("integer primitive type should be registered");
        Ok(int_type_id)
    }

    fn infer_type_of_project_to_variant_place(&self, base: &mlr::PlaceId, variant_index: &usize) -> Result<TypeId> {
        let base_type_id = self.get_place_type(base);
        let enum_def = self.get_enum_def(&base_type_id)?;
        let variant =
            enum_def
                .variants
                .get(*variant_index)
                .ok_or(MlrBuilderError::TypeError(TypeError::NotAnEnumVariant {
                    type_id: base_type_id,
                    variant_name: variant_index.to_string(),
                }))?;

        Ok(variant.type_id)
    }
}
