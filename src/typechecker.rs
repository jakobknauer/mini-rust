mod err;

use std::collections::HashSet;

pub use err::{TyError, TyResult};

use crate::ctxt::{self, fns, mlr::*, ty};

pub struct Typechecker<'a> {
    tys: &'a mut ctxt::TyReg,
    fns: &'a mut ctxt::FnReg,
    mlr: &'a mut ctxt::mlr::Mlr,
    fn_: fns::Fn,
}

impl<'a> Typechecker<'a> {
    pub fn new(ctxt: &'a mut ctxt::Ctxt, fn_: fns::Fn) -> Self {
        Typechecker {
            tys: &mut ctxt.tys,
            fns: &mut ctxt.fns,
            mlr: &mut ctxt.mlr,
            fn_,
        }
    }

    pub fn infer_val_ty(&mut self, val: Val) -> TyResult<ty::Ty> {
        use ValDef::*;

        let val_def = self.mlr.get_val_def(&val);

        let ty = match *val_def {
            Call { callable, ref args } => self.infer_ty_of_call(&callable, &args.clone()),
            Use(op) => Ok(self.mlr.get_op_ty(&op)),
            AddrOf(place) => self.infer_ty_of_addr_of_place(&place),
            As { op, target_ty } => self.infer_ty_of_as_expr(op, target_ty),
        }?;

        self.mlr.set_val_ty(val, ty);
        Ok(ty)
    }

    pub fn infer_place_ty(&mut self, place: Place) -> TyResult<ty::Ty> {
        use PlaceDef::*;

        let place_def = self.mlr.get_place_def(place);

        let ty = match place_def {
            Loc(loc) => self.infer_ty_of_loc(loc),
            &FieldAccess { base, field_index } => self.infer_ty_of_field_access_place(base, field_index),
            EnumDiscriminant { base } => self.infer_ty_of_enum_discriminant(base),
            &ProjectToVariant { base, variant_index } => self.infer_ty_of_project_to_variant_place(base, variant_index),
            &Deref(op) => self.infer_ty_of_deref_place(&op),
        }?;

        self.mlr.set_place_ty(place, ty);
        Ok(ty)
    }

    pub fn infer_op_ty(&mut self, op: Op) -> TyResult<ty::Ty> {
        use OpDef::*;

        let op_def = self.mlr.get_op_def(&op);

        let ty = match *op_def {
            Fn(ref fn_spec) => self.infer_ty_of_fn(&fn_spec.clone()),
            Const(ref constant) => self.infer_ty_of_constant(constant),
            Copy(place) => self.infer_place_ty(place),
        }?;

        self.mlr.set_op_ty(op, ty);
        Ok(ty)
    }

    pub fn check_stmt_ty(&mut self, stmt: Stmt) -> TyResult<()> {
        use StmtDef::*;

        let stmt_def = self.mlr.get_stmt_def(&stmt);

        match *stmt_def {
            Assign { place, value } => self.check_assign_stmt(place, value)?,
            Return { value } => self.check_return_stmt(value)?,
            Alloc { .. } | Block(..) | If(_) | Loop { .. } | Break => (),
        }

        Ok(())
    }

    fn infer_ty_of_constant(&self, constant: &Const) -> TyResult<ty::Ty> {
        use Const::*;

        let ty = match constant {
            Int(_) => ty::Primitive::Integer32,
            Bool(_) => ty::Primitive::Boolean,
            Unit => ty::Primitive::Unit,
        };

        let ty = self.tys.get_primitive_ty(ty);

        Ok(ty)
    }

    fn infer_ty_of_call(&mut self, callable: &Op, args: &[Op]) -> TyResult<ty::Ty> {
        let ty = self.mlr.get_op_ty(callable);
        let callable_ty_def = self.tys.get_ty_def(ty).expect("type of callable should be registered");

        let ty::TyDef::Fn { param_tys, return_ty } = callable_ty_def.clone() else {
            return TyError::ValNotCallable.into();
        };

        let arg_tys = args
            .iter()
            .map(|arg_loc| self.mlr.get_op_ty(arg_loc))
            .collect::<Vec<_>>();

        if param_tys.len() != arg_tys.len() {
            return TyError::CallArgumentCountMismatch {
                expected: param_tys.len(),
                actual: arg_tys.len(),
            }
            .into();
        }

        for (i, (&param_ty, arg_ty)) in param_tys.iter().zip(arg_tys).enumerate() {
            self.tys
                .unify(param_ty, arg_ty)
                .map_err(|_| TyError::CallArgumentTyMismatch {
                    index: i,
                    expected: param_ty,
                    actual: arg_ty,
                })?;
        }

        Ok(return_ty)
    }

    fn infer_ty_of_addr_of_place(&mut self, place: &Place) -> TyResult<ty::Ty> {
        let place_ty = self.mlr.get_place_ty(place);
        let ref_ty = self.tys.register_ref_ty(place_ty);
        Ok(ref_ty)
    }

    fn infer_ty_of_as_expr(&mut self, op: Op, target_ty: ty::Ty) -> Result<ty::Ty, TyError> {
        let op_ty = self.mlr.get_op_ty(&op);
        let op_ty_def = self
            .tys
            .get_ty_def(op_ty)
            .expect("type of as-expr operand should be registered");
        let &ty::TyDef::Ref(op_base_ty) = op_ty_def else {
            return TyError::AsExprOpOfNonRefTy { op_ty }.into();
        };

        let ty_def = self
            .tys
            .get_ty_def(target_ty)
            .expect("type of as-expr target type should be registered");
        let &ty::TyDef::Ptr(target_base_ty) = ty_def else {
            return TyError::AsExprTargetNonPtrTy { target_ty }.into();
        };

        self.tys
            .unify(op_base_ty, target_base_ty)
            .map_err(|_| TyError::AsExprTyMismatch { op_ty, target_ty })?;

        Ok(target_ty)
    }

    fn infer_ty_of_fn(&mut self, fn_specialization: &fns::FnSpecialization) -> TyResult<ty::Ty> {
        let signature = self
            .fns
            .get_sig(&fn_specialization.fn_)
            .expect("function signature should be registered");

        if signature.gen_params.len() != fn_specialization.gen_args.len() {
            return TyError::FnGenericArgCountMismatch {
                fn_: fn_specialization.fn_,
                expected: signature.gen_params.len(),
                actual: fn_specialization.gen_args.len(),
            }
            .into();
        }

        let param_tys: Vec<_> = signature.params.iter().map(|param| param.ty).collect();
        let fn_ty = self.tys.register_fn_ty(param_tys, signature.return_ty);

        let substitutions = self.fns.get_substitutions_for_specialization(fn_specialization);
        let fn_spec_ty = self.tys.substitute_gen_vars(fn_ty, &substitutions);

        Ok(fn_spec_ty)
    }

    fn infer_ty_of_loc(&self, loc: &Loc) -> TyResult<ty::Ty> {
        Ok(self.mlr.get_loc_ty(loc))
    }

    fn infer_ty_of_field_access_place(&mut self, base: Place, field_index: usize) -> TyResult<ty::Ty> {
        let base_ty = self.mlr.get_place_ty(&base);
        let field_ty = self.tys.get_struct_field_ty(base_ty, field_index)?;
        Ok(field_ty)
    }

    fn infer_ty_of_enum_discriminant(&self, base: &Place) -> TyResult<ty::Ty> {
        let base_ty = self.mlr.get_place_ty(base);
        if !self.tys.is_enum_ty(base_ty) {
            return TyError::NotAnEnum { ty: base_ty }.into();
        }

        // the discriminant is always an integer
        let int_ty = self.tys.get_primitive_ty(ty::Primitive::Integer32);
        Ok(int_ty)
    }

    fn infer_ty_of_project_to_variant_place(&mut self, base: Place, variant_index: usize) -> TyResult<ty::Ty> {
        let base_ty = self.mlr.get_place_ty(&base);
        self.get_enum_variant_ty(base_ty, variant_index)
    }

    fn infer_ty_of_deref_place(&mut self, op: &Op) -> TyResult<ty::Ty> {
        let ref_ty = self.mlr.get_op_ty(op);
        let ty_def = self
            .tys
            .get_ty_def(ref_ty)
            .expect("type of dereferenced op should be registered");

        match *ty_def {
            ty::TyDef::Ref(base_ty) | ty::TyDef::Ptr(base_ty) => Ok(base_ty),
            _ => TyError::InvalidDereference { ty: ref_ty }.into(),
        }
    }

    fn check_assign_stmt(&mut self, place: Place, val: Val) -> TyResult<()> {
        let place_ty = self.mlr.get_place_ty(&place);
        let val_ty = self.mlr.get_val_ty(&val);

        self.tys
            .unify(place_ty, val_ty)
            .map_err(|_| TyError::AssignStmtTyMismatch {
                place,
                expected: place_ty,
                actual: val_ty,
            })
    }

    fn check_return_stmt(&mut self, val: Val) -> TyResult<()> {
        let return_ty = self
            .fns
            .get_sig(&self.fn_)
            .expect("function signature should be registered")
            .return_ty;

        let val_ty = self.mlr.get_val_ty(&val);

        self.tys
            .unify(return_ty, val_ty)
            .map_err(|_| TyError::ReturnTyMismatch {
                expected: return_ty,
                actual: val_ty,
            })
    }

    pub fn resolve_struct_field(&mut self, struct_ty: ty::Ty, field_name: &str) -> TyResult<usize> {
        let field_index = self.tys.get_struct_field_index_by_name(struct_ty, field_name)?;
        Ok(field_index)
    }

    pub fn resolve_struct_fields<'b>(
        &mut self,
        struct_ty: ty::Ty,
        field_names: impl IntoIterator<Item = &'b str>,
    ) -> TyResult<Vec<usize>> {
        let provided_names: Vec<&str> = field_names.into_iter().collect();
        let provided_names_set: HashSet<&str> = provided_names.iter().cloned().collect();

        let expected_names: Vec<&str> = self.tys.get_struct_field_names(struct_ty)?.into_iter().collect();
        let expected_names_set: HashSet<&str> = expected_names.iter().cloned().collect();

        let missing_fields: Vec<&str> = expected_names_set.difference(&provided_names_set).cloned().collect();
        if !missing_fields.is_empty() {
            return TyError::InitializerMissingFields {
                ty: struct_ty,
                missing_fields: missing_fields.iter().map(|s| s.to_string()).collect(),
            }
            .into();
        }

        let extra_fields: Vec<&str> = provided_names_set.difference(&expected_names_set).cloned().collect();
        if !extra_fields.is_empty() {
            return TyError::InitializerExtraFields {
                ty: struct_ty,
                extra_fields: extra_fields.iter().map(|s| s.to_string()).collect(),
            }
            .into();
        }

        let field_indices = provided_names
            .iter()
            .map(|field_name| {
                expected_names
                    .iter()
                    .position(|name| name == field_name)
                    .ok_or_else(|| TyError::NotAStructField {
                        ty: struct_ty,
                        field_name: field_name.to_string(),
                    })
            })
            .collect::<Result<_, _>>()?;

        Ok(field_indices)
    }

    pub fn resolve_enum_variants<'b>(
        &self,
        enum_ty: ty::Ty,
        variant_names: impl IntoIterator<Item = &'b str>,
    ) -> TyResult<Vec<usize>> {
        let provided_names: Vec<&str> = variant_names.into_iter().collect();
        let provided_names_set: HashSet<&str> = provided_names.iter().cloned().collect();

        let expected_names: Vec<&str> = self.tys.get_enum_variant_names(enum_ty)?.into_iter().collect();
        let expected_names_set: HashSet<&str> = expected_names.iter().cloned().collect();

        let missing_variants: Vec<&str> = expected_names_set.difference(&provided_names_set).cloned().collect();
        if !missing_variants.is_empty() {
            return TyError::MissingVariants {
                ty: enum_ty,
                missing_variants: missing_variants.iter().map(|s| s.to_string()).collect(),
            }
            .into();
        }

        let extra_variants: Vec<&str> = provided_names_set.difference(&expected_names_set).cloned().collect();
        if !extra_variants.is_empty() {
            return TyError::ExtraVariants {
                ty: enum_ty,
                extra_variants: extra_variants.iter().map(|s| s.to_string()).collect(),
            }
            .into();
        }

        let variant_indices = provided_names
            .iter()
            .map(|variant_name| {
                expected_names
                    .iter()
                    .position(|name| name == variant_name)
                    .ok_or_else(|| TyError::NotAnEnumVariant {
                        ty: enum_ty,
                        variant_name: variant_name.to_string(),
                    })
            })
            .collect::<TyResult<_>>()?;

        Ok(variant_indices)
    }

    pub fn get_enum_variant_ty(&mut self, ty: ty::Ty, variant_index: usize) -> TyResult<ty::Ty> {
        let ty = self.tys.get_enum_variant_ty(ty, variant_index)?;
        Ok(ty)
    }
}
