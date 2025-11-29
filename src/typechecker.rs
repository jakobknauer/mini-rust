use crate::{
    ctxt::{self, fns, mlr::*, ty},
    hlr2mlr::{Hlr2MlrErr, TyErr},
};

type MlrBuilderResult<T> = std::result::Result<T, Hlr2MlrErr>;

pub struct Typechecker<'a> {
    tys: &'a mut ctxt::TyReg,
    fns: &'a mut ctxt::FnReg,
    mlr: &'a mut ctxt::mlr::Mlr,
}

impl<'a> Typechecker<'a> {
    pub fn new(ctxt: &'a mut ctxt::Ctxt) -> Self {
        Typechecker {
            tys: &mut ctxt.tys,
            fns: &mut ctxt.fns,
            mlr: &mut ctxt.mlr,
        }
    }

    pub fn infer_val_ty(&mut self, val: Val) -> MlrBuilderResult<ty::Ty> {
        use ValDef::*;

        let val_def = self.mlr.get_val_def(&val).clone();

        let ty = match val_def {
            Call { callable, args } => self.infer_ty_of_call(&callable, &args),
            Empty { ty } => Ok(ty),
            Use(op) => Ok(self.mlr.get_op_ty(&op)),
            AddrOf(place) => self.infer_ty_of_addr_of_place(&place),
        }?;

        self.mlr.set_val_ty(val, ty);
        Ok(ty)
    }

    pub fn infer_place_ty(&mut self, place: Place) -> MlrBuilderResult<ty::Ty> {
        use PlaceDef::*;

        let place_def = self.mlr.get_place_def(place);

        let ty = match place_def {
            Loc(loc) => self.infer_ty_of_loc(loc),
            FieldAccess { base, field_index } => self.infer_ty_of_field_access_place(base, field_index),
            EnumDiscriminant { base } => self.infer_ty_of_enum_discriminant(base),
            ProjectToVariant { base, variant_index } => self.infer_ty_of_project_to_variant_place(base, variant_index),
            &Deref(op) => self.infer_ty_of_deref_place(&op),
        }?;

        self.mlr.set_place_ty(place, ty);
        Ok(ty)
    }

    pub fn infer_op_ty(&mut self, op: Op) -> MlrBuilderResult<ty::Ty> {
        use OpDef::*;

        let op_def = self.mlr.get_op_def(&op).clone();

        let ty = match op_def {
            Fn(fn_spec) => self.infer_ty_of_fn(&fn_spec),
            Const(constant) => self.infer_ty_of_constant(&constant),
            Copy(place) => self.infer_place_ty(place),
        }?;

        self.mlr.set_op_ty(op, ty);
        Ok(ty)
    }

    fn infer_ty_of_constant(&self, constant: &Const) -> MlrBuilderResult<ty::Ty> {
        use Const::*;

        let ty = match constant {
            Int(_) => ty::Primitive::Integer32,
            Bool(_) => ty::Primitive::Boolean,
            Unit => ty::Primitive::Unit,
        };

        self.tys.get_primitive_ty(ty).ok_or(Hlr2MlrErr::UnknownPrimitiveTy)
    }

    fn infer_ty_of_call(&mut self, callable: &Op, args: &[Op]) -> MlrBuilderResult<ty::Ty> {
        let ty = self.mlr.get_op_ty(callable);
        let callable_ty_def = self
            .tys
            .get_ty_def(&ty)
            .expect("type of callable should be registered")
            .clone();

        let ty::TyDef::Fn { param_tys, return_ty } = callable_ty_def else {
            return TyErr::ValNotCallable.into();
        };

        let arg_tys = args
            .iter()
            .map(|arg_loc| self.mlr.get_op_ty(arg_loc))
            .collect::<Vec<_>>();

        if param_tys.len() != arg_tys.len() {
            return TyErr::CallArgumentCountMismatch {
                expected: param_tys.len(),
                actual: arg_tys.len(),
            }
            .into();
        }

        for (i, (param_ty, arg_ty)) in param_tys.iter().zip(arg_tys).enumerate() {
            self.tys
                .unify(param_ty, &arg_ty)
                .map_err(|_| TyErr::CallArgumentTyMismatch {
                    index: i,
                    expected: *param_ty,
                    actual: arg_ty,
                })
                .map_err(Hlr2MlrErr::TyErr)?;
        }

        Ok(return_ty)
    }

    fn infer_ty_of_addr_of_place(&mut self, place: &Place) -> MlrBuilderResult<ty::Ty> {
        let place_ty = self.mlr.get_place_ty(place);
        let ref_ty = self.tys.register_ref_ty(place_ty);
        Ok(ref_ty)
    }

    fn infer_ty_of_fn(&mut self, fn_specialization: &fns::FnSpecialization) -> MlrBuilderResult<ty::Ty> {
        let signature = self
            .fns
            .get_sig(&fn_specialization.fn_)
            .expect("function signature should be registered");

        if signature.gen_params.len() != fn_specialization.gen_args.len() {
            return TyErr::GenericArgCountMismatch {
                fn_: fn_specialization.fn_,
                expected: signature.gen_params.len(),
                actual: fn_specialization.gen_args.len(),
            }
            .into();
        }

        let param_tys: Vec<_> = signature.params.iter().map(|param| param.ty).collect();
        let fn_ty = self.tys.register_fn_ty(param_tys, signature.return_ty);

        let substitutions = self.fns.get_substitutions_for_specialization(fn_specialization);
        let fn_spec_ty = self.tys.substitute_gen_vars(&fn_ty, &substitutions);

        Ok(fn_spec_ty)
    }

    fn infer_ty_of_loc(&self, loc: &Loc) -> MlrBuilderResult<ty::Ty> {
        Ok(self.mlr.get_loc_ty(loc))
    }

    fn infer_ty_of_field_access_place(&self, base: &Place, field_index: &usize) -> MlrBuilderResult<ty::Ty> {
        let base_ty = self.mlr.get_place_ty(base);
        let struct_def = self.tys.get_struct_def_by_ty(&base_ty).map_err(Hlr2MlrErr::TyErr)?;

        let field_ty = struct_def
            .fields
            .get(*field_index)
            .expect("field index should be valid")
            .ty;

        Ok(field_ty)
    }

    fn infer_ty_of_enum_discriminant(&self, base: &Place) -> MlrBuilderResult<ty::Ty> {
        let base_ty = self.mlr.get_place_ty(base);
        let _enum_def = self.tys.get_enum_def_by_ty(&base_ty).map_err(Hlr2MlrErr::TyErr)?;

        // the discriminant is always an integer
        let int_ty = self
            .tys
            .get_primitive_ty(ty::Primitive::Integer32)
            .expect("integer primitive type should be registered");
        Ok(int_ty)
    }

    fn infer_ty_of_project_to_variant_place(&self, base: &Place, variant_index: &usize) -> MlrBuilderResult<ty::Ty> {
        let base_ty = self.mlr.get_place_ty(base);
        let enum_def = self.tys.get_enum_def_by_ty(&base_ty).map_err(Hlr2MlrErr::TyErr)?;
        let variant = enum_def
            .variants
            .get(*variant_index)
            .ok_or(Hlr2MlrErr::TyErr(TyErr::NotAnEnumVariant {
                ty: base_ty,
                variant_name: variant_index.to_string(),
            }))?;

        Ok(variant.ty)
    }

    fn infer_ty_of_deref_place(&mut self, op: &Op) -> MlrBuilderResult<ty::Ty> {
        let ref_ty = self.mlr.get_op_ty(op);
        let ty_def = self
            .tys
            .get_ty_def(&ref_ty)
            .expect("type of dereferenced op should be registered")
            .clone();

        match ty_def {
            ty::TyDef::Ref(referenced_ty) => Ok(referenced_ty),
            _ => TyErr::DereferenceOfNonRefTy { ty: ref_ty }.into(),
        }
    }
}
