use crate::{
    ctxt::{fns::Fn, ty::*},
    mlr::{
        self,
        build::{MlrBuilderError, Result, TyError},
    },
};

impl<'a> mlr::MlrBuilder<'a> {
    pub fn infer_val_ty(&mut self, val: mlr::Val) -> Result<Ty> {
        use mlr::ValDef::*;

        let val = self
            .output
            .vals
            .get(&val)
            .expect("infer_ty should only be called with a valid ValId");

        match val {
            Call { callable, args } => self.infer_ty_of_call(callable, args),
            Empty { ty } => Ok(*ty),
            Use(op) => Ok(self.get_op_ty(op)),
        }
    }

    fn infer_ty_of_constant(&self, constant: &mlr::Const) -> Result<Ty> {
        use mlr::Const::*;

        let ty = match constant {
            Int(_) => Primitive::Integer32,
            Bool(_) => Primitive::Boolean,
            Unit => Primitive::Unit,
        };

        self.ctxt
            .tys
            .get_primitive_ty(ty)
            .ok_or(MlrBuilderError::UnknownPrimitiveTy)
    }

    fn infer_ty_of_call(&self, callable: &mlr::Op, args: &[mlr::Op]) -> Result<Ty> {
        let ty = self.get_op_ty(callable);
        let callable_ty_def = self
            .ctxt
            .tys
            .get_ty_def(&ty)
            .expect("type of callable should be registered");

        let TyDef::Fn { param_tys, return_ty } = callable_ty_def else {
            return TyError::ValNotCallable.into();
        };

        let arg_tys = args.iter().map(|arg_loc| self.get_op_ty(arg_loc));

        if param_tys.len() != arg_tys.len() {
            return TyError::CallArgumentCountMismatch {
                expected: param_tys.len(),
                actual: arg_tys.len(),
            }
            .into();
        }

        for (i, (param_ty, arg_ty)) in param_tys.iter().zip(arg_tys).enumerate() {
            if !self.ctxt.tys.ty_equal(param_ty, &arg_ty) {
                return TyError::CallArgumentTyMismatch {
                    index: i,
                    expected: *param_ty,
                    actual: arg_ty,
                }
                .into();
            }
        }

        Ok(*return_ty)
    }

    fn infer_ty_of_fn(&mut self, fn_: Fn) -> Result<Ty> {
        let signature = self
            .ctxt
            .fns
            .get_signature(&fn_)
            .expect("function signature should be registered");

        let param_tys: Vec<_> = signature.parameters.iter().map(|param| param.ty).collect();
        let return_ty = signature.return_ty;
        let fn_ty = self.ctxt.tys.register_fn_ty(param_tys, return_ty);

        Ok(fn_ty)
    }

    pub fn try_infer_place_ty(&self, place: &mlr::Place) -> Result<Option<Ty>> {
        use mlr::PlaceDef::*;

        let place = self
            .output
            .places
            .get(place)
            .expect("infer_ty_of_place should only be called with a valid PlaceId");

        match place {
            Loc(loc) => self.try_infer_ty_of_local_place(loc),
            FieldAccess { base, field_index } => self.infer_ty_of_field_access_place(base, field_index).map(Some),
            EnumDiscriminant { base } => self.infer_ty_of_enum_discriminant(base).map(Some),
            ProjectToVariant { base, variant_index } => {
                self.infer_ty_of_project_to_variant_place(base, variant_index).map(Some)
            }
        }
    }

    fn try_infer_ty_of_local_place(&self, loc: &mlr::Loc) -> Result<Option<Ty>> {
        Ok(self.try_get_loc_ty(loc))
    }

    fn infer_ty_of_field_access_place(&self, base: &mlr::Place, field_index: &usize) -> Result<Ty> {
        let base_ty = self.get_place_ty(base);
        let struct_def = self.get_struct_def(&base_ty)?;

        let field_ty = struct_def
            .fields
            .get(*field_index)
            .expect("field index should be valid")
            .ty;

        Ok(field_ty)
    }

    fn infer_ty_of_enum_discriminant(&self, base: &mlr::Place) -> Result<Ty> {
        let base_ty = self.get_place_ty(base);
        let _enum_def = self.get_enum_def(&base_ty)?;

        // the discriminant is always an integer
        let int_ty = self
            .ctxt
            .tys
            .get_primitive_ty(Primitive::Integer32)
            .expect("integer primitive type should be registered");
        Ok(int_ty)
    }

    fn infer_ty_of_project_to_variant_place(&self, base: &mlr::Place, variant_index: &usize) -> Result<Ty> {
        let base_ty = self.get_place_ty(base);
        let enum_def = self.get_enum_def(&base_ty)?;
        let variant =
            enum_def
                .variants
                .get(*variant_index)
                .ok_or(MlrBuilderError::TyError(TyError::NotAnEnumVariant {
                    ty: base_ty,
                    variant_name: variant_index.to_string(),
                }))?;

        Ok(variant.ty)
    }

    pub fn infer_op_ty(&mut self, op: mlr::Op) -> Result<Ty> {
        use mlr::OpDef::*;

        let op = self
            .output
            .ops
            .get(&op)
            .expect("infer_ty_of_operand should only be called with a valid OpId");

        match op {
            Fn(fn_) => self.infer_ty_of_fn(*fn_),
            Const(constant) => self.infer_ty_of_constant(constant),
            Copy(place) => self.try_infer_place_ty(place).map(|opt| opt.unwrap()),
        }
    }
}
