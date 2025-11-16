use crate::{
    ctxt::{fns::Fn, types::*},
    mlr::{
        self,
        build::{MlrBuilderError, Result, TypeError},
    },
};

impl<'a> mlr::MlrBuilder<'a> {
    pub fn infer_val_type(&mut self, val: mlr::ValId) -> Result<TypeId> {
        use mlr::Val::*;

        let val = self
            .output
            .vals
            .get(&val)
            .expect("infer_type should only be called with a valid ValId");

        match val {
            Call { callable, args } => self.infer_type_of_call(callable, args),
            Empty { type_id } => Ok(*type_id),
            Use(op_id) => Ok(self.get_op_type(op_id)),
        }
    }

    fn infer_type_of_constant(&self, constant: &mlr::Constant) -> Result<TypeId> {
        use mlr::Constant::*;

        let type_ = match constant {
            Int(_) => PrimitiveType::Integer32,
            Bool(_) => PrimitiveType::Boolean,
            Unit => PrimitiveType::Unit,
        };

        self.ctxt
            .types
            .get_primitive_type_id(type_)
            .ok_or(MlrBuilderError::UnknownPrimitiveType)
    }

    fn infer_type_of_call(&self, callable: &mlr::OpId, args: &[mlr::OpId]) -> Result<TypeId> {
        let callable_type = self.get_op_type(callable);
        let callable_type = self
            .ctxt
            .types
            .get_type_by_id(&callable_type)
            .expect("type of callable should be registered");

        let Type::Fn {
            param_types,
            return_type,
        } = callable_type
        else {
            return TypeError::ValNotCallable.into();
        };

        let arg_types = args.iter().map(|arg_loc| self.get_op_type(arg_loc));

        if param_types.len() != arg_types.len() {
            return TypeError::CallArgumentCountMismatch {
                expected: param_types.len(),
                actual: arg_types.len(),
            }
            .into();
        }

        for (i, (param_type, arg_type)) in param_types.iter().zip(arg_types).enumerate() {
            if !self.ctxt.types.types_equal(param_type, &arg_type) {
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

    fn infer_type_of_fn(&mut self, fn_: Fn) -> Result<TypeId> {
        let signature = self
            .ctxt
            .fns
            .get_signature_by_id(&fn_)
            .expect("function signature should be registered");

        let param_types: Vec<_> = signature.parameters.iter().map(|param| param.type_).collect();
        let return_type = signature.return_type;
        let fn_type = self.ctxt.types.register_fn_type(param_types, return_type);

        Ok(fn_type)
    }

    pub fn try_infer_place_type(&self, place_id: &mlr::PlaceId) -> Result<Option<TypeId>> {
        use mlr::Place::*;

        let place = self
            .output
            .places
            .get(place_id)
            .expect("infer_type_of_place should only be called with a valid PlaceId");

        match place {
            Local(loc_id) => self.try_infer_type_of_local_place(loc_id),
            FieldAccess { base, field_index } => self.infer_type_of_field_access_place(base, field_index).map(Some),
            EnumDiscriminant { base } => self.infer_type_of_enum_discriminant(base).map(Some),
            ProjectToVariant { base, variant_index } => self
                .infer_type_of_project_to_variant_place(base, variant_index)
                .map(Some),
        }
    }

    fn try_infer_type_of_local_place(&self, loc_id: &mlr::LocId) -> Result<Option<TypeId>> {
        Ok(self.try_get_loc_type(loc_id))
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
            .types
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

    pub fn infer_op_type(&mut self, op_id: mlr::OpId) -> Result<TypeId> {
        use mlr::Operand::*;

        let op = self
            .output
            .ops
            .get(&op_id)
            .expect("infer_type_of_operand should only be called with a valid OpId");

        match op {
            Fn(fn_) => self.infer_type_of_fn(*fn_),
            Constant(constant) => self.infer_type_of_constant(constant),
            Copy(place_id) => self.try_infer_place_type(place_id).map(|opt| opt.unwrap()),
        }
    }
}
