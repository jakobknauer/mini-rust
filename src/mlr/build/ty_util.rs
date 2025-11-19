use crate::{
    ctxt::{fns, ty},
    mlr::{
        self,
        build::{MlrBuilderError, Result, TyError},
    },
};

impl<'a> mlr::MlrBuilder<'a> {
    pub fn infer_val_ty(&mut self, val: mlr::Val) -> Result<ty::Ty> {
        use mlr::ValDef::*;

        let val = self
            .output
            .vals
            .get(&val)
            .expect("infer_ty should only be called with a valid ValId")
            .clone();

        match val {
            Call { callable, args } => self.infer_ty_of_call(&callable, &args),
            Empty { ty } => Ok(ty),
            Use(op) => Ok(self.get_op_ty(&op)),
        }
    }

    fn infer_ty_of_constant(&self, constant: &mlr::Const) -> Result<ty::Ty> {
        use mlr::Const::*;

        let ty = match constant {
            Int(_) => ty::Primitive::Integer32,
            Bool(_) => ty::Primitive::Boolean,
            Unit => ty::Primitive::Unit,
        };

        self.ctxt
            .tys
            .get_primitive_ty(ty)
            .ok_or(MlrBuilderError::UnknownPrimitiveTy)
    }

    fn infer_ty_of_call(&mut self, callable: &mlr::Op, args: &[mlr::Op]) -> Result<ty::Ty> {
        let ty = self.get_op_ty(callable);
        let callable_ty_def = self
            .ctxt
            .tys
            .get_ty_def(&ty)
            .expect("type of callable should be registered")
            .clone();

        let ty::TyDef::Fn { param_tys, return_ty } = callable_ty_def else {
            return TyError::ValNotCallable.into();
        };

        let arg_tys = args.iter().map(|arg_loc| self.get_op_ty(arg_loc)).collect::<Vec<_>>();

        if param_tys.len() != arg_tys.len() {
            return TyError::CallArgumentCountMismatch {
                expected: param_tys.len(),
                actual: arg_tys.len(),
            }
            .into();
        }

        for (i, (param_ty, arg_ty)) in param_tys.iter().zip(arg_tys).enumerate() {
            self.ctxt
                .tys
                .unify(param_ty, &arg_ty)
                .map_err(|_| TyError::CallArgumentTyMismatch {
                    index: i,
                    expected: *param_ty,
                    actual: arg_ty,
                })
                .map_err(MlrBuilderError::TyError)?;
        }

        Ok(return_ty)
    }

    fn infer_ty_of_fn(&mut self, fn_: fns::Fn) -> Result<ty::Ty> {
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

    pub fn infer_place_ty(&self, place: &mlr::Place) -> Result<ty::Ty> {
        use mlr::PlaceDef::*;

        let place = self
            .output
            .places
            .get(place)
            .expect("infer_ty_of_place should only be called with a valid PlaceId");

        match place {
            Loc(loc) => self.infer_ty_of_loc(loc),
            FieldAccess { base, field_index } => self.infer_ty_of_field_access_place(base, field_index),
            EnumDiscriminant { base } => self.infer_ty_of_enum_discriminant(base),
            ProjectToVariant { base, variant_index } => self.infer_ty_of_project_to_variant_place(base, variant_index),
        }
    }

    fn infer_ty_of_loc(&self, loc: &mlr::Loc) -> Result<ty::Ty> {
        Ok(self.get_loc_ty(loc))
    }

    fn infer_ty_of_field_access_place(&self, base: &mlr::Place, field_index: &usize) -> Result<ty::Ty> {
        let base_ty = self.get_place_ty(base);
        let struct_def = self.get_struct_def(&base_ty)?;

        let field_ty = struct_def
            .fields
            .get(*field_index)
            .expect("field index should be valid")
            .ty;

        Ok(field_ty)
    }

    fn infer_ty_of_enum_discriminant(&self, base: &mlr::Place) -> Result<ty::Ty> {
        let base_ty = self.get_place_ty(base);
        let _enum_def = self.get_enum_def(&base_ty)?;

        // the discriminant is always an integer
        let int_ty = self
            .ctxt
            .tys
            .get_primitive_ty(ty::Primitive::Integer32)
            .expect("integer primitive type should be registered");
        Ok(int_ty)
    }

    fn infer_ty_of_project_to_variant_place(&self, base: &mlr::Place, variant_index: &usize) -> Result<ty::Ty> {
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

    pub fn infer_op_ty(&mut self, op: mlr::Op) -> Result<ty::Ty> {
        use mlr::OpDef::*;

        let op = self
            .output
            .ops
            .get(&op)
            .expect("infer_ty_of_operand should only be called with a valid OpId");

        match op {
            Fn(fn_) => self.infer_ty_of_fn(*fn_),
            Const(constant) => self.infer_ty_of_constant(constant),
            Copy(place) => self.infer_place_ty(place),
        }
    }
}
