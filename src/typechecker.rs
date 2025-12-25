mod err;

use std::collections::HashSet;

pub use err::{TyError, TyResult};

use crate::ctxt::{self, fns, mlr::*, traits, ty};

pub struct Typechecker<'a> {
    ctxt: &'a mut ctxt::Ctxt,
    fn_: fns::Fn,
}

pub enum MethodResolution {
    Inherent { fn_: fns::Fn, env_gen_args: Vec<ty::Ty> },
    Trait { trait_: traits::Trait, method_idx: usize },
}

impl<'a> Typechecker<'a> {
    pub fn new(ctxt: &'a mut ctxt::Ctxt, fn_: fns::Fn) -> Self {
        Typechecker { ctxt, fn_ }
    }

    pub fn infer_val_ty(&mut self, val: Val) -> TyResult<ty::Ty> {
        use ValDef::*;

        let val_def = self.ctxt.mlr.get_val_def(val);

        let ty = match *val_def {
            Call { callable, ref args } => self.infer_ty_of_call(callable, &args.clone()),
            Use(op) => Ok(self.ctxt.mlr.get_op_ty(op)),
            AddrOf(place) => self.infer_ty_of_addr_of_place(place),
            As { op, target_ty } => self.infer_ty_of_as_expr(op, target_ty),
            SizeOf(..) => self.infer_ty_of_size_of_expr(),
        }?;

        self.ctxt.mlr.set_val_ty(val, ty);
        Ok(ty)
    }

    pub fn infer_place_ty(&mut self, place: Place) -> TyResult<ty::Ty> {
        use PlaceDef::*;

        let place_def = self.ctxt.mlr.get_place_def(place);

        let ty = match *place_def {
            Loc(loc) => self.infer_ty_of_loc(loc),
            FieldAccess { base, field_index } => self.infer_ty_of_field_access_place(base, field_index),
            EnumDiscriminant { base } => self.infer_ty_of_enum_discriminant(base),
            ProjectToVariant { base, variant_index } => self.infer_ty_of_project_to_variant_place(base, variant_index),
            Deref(op) => self.infer_ty_of_deref_place(op),
        }?;

        self.ctxt.mlr.set_place_ty(place, ty);
        Ok(ty)
    }

    pub fn infer_op_ty(&mut self, op: Op) -> TyResult<ty::Ty> {
        use OpDef::*;

        let op_def = self.ctxt.mlr.get_op_def(op);

        let ty = match *op_def {
            Fn(ref fn_spec) => self.infer_ty_of_fn(&fn_spec.clone()),
            TraitMethod(ref trait_method) => self.infer_ty_of_trait_method(&trait_method.clone()),
            Const(ref constant) => self.infer_ty_of_constant(&constant.clone()),
            Copy(place) => self.infer_place_ty(place),
        }?;

        self.ctxt.mlr.set_op_ty(op, ty);
        Ok(ty)
    }

    pub fn check_stmt_ty(&mut self, stmt: Stmt) -> TyResult<()> {
        use StmtDef::*;

        let stmt_def = self.ctxt.mlr.get_stmt_def(stmt);

        match *stmt_def {
            Assign { place, value } => self.check_assign_stmt(place, value)?,
            Return { value } => self.check_return_stmt(value)?,
            Alloc { .. } | Block(..) | If(_) | Loop { .. } | Break => (),
        }

        Ok(())
    }

    fn infer_ty_of_constant(&mut self, constant: &Const) -> TyResult<ty::Ty> {
        use Const::*;

        let ty = match constant {
            Int(_) => ty::Primitive::Integer32,
            Bool(_) => ty::Primitive::Boolean,
            Unit => ty::Primitive::Unit,
            CChar(_) => ty::Primitive::CChar,
            CString(..) => {
                let c_char_ty = self.ctxt.tys.get_primitive_ty(ty::Primitive::CChar);
                let ptr_to_c_char_ty = self.ctxt.tys.register_ptr_ty(c_char_ty);
                return Ok(ptr_to_c_char_ty);
            }
        };

        let ty = self.ctxt.tys.get_primitive_ty(ty);

        Ok(ty)
    }

    fn infer_ty_of_call(&mut self, callable: Op, args: &[Op]) -> TyResult<ty::Ty> {
        let ty = self.ctxt.mlr.get_op_ty(callable);
        let callable_ty_def = self
            .ctxt
            .tys
            .get_ty_def(ty)
            .expect("type of callable should be registered");

        let ty::TyDef::Fn {
            param_tys,
            return_ty,
            var_args,
        } = callable_ty_def.clone()
        else {
            return TyError::ValNotCallable.into();
        };

        let arg_tys = args
            .iter()
            .map(|&arg_loc| self.ctxt.mlr.get_op_ty(arg_loc))
            .collect::<Vec<_>>();

        if (var_args && arg_tys.len() < param_tys.len()) || (!var_args && arg_tys.len() != param_tys.len()) {
            return TyError::CallArgumentCountMismatch {
                expected: param_tys.len(),
                actual: arg_tys.len(),
                var_args,
            }
            .into();
        }

        for (i, (&param_ty, arg_ty)) in param_tys.iter().zip(arg_tys).enumerate() {
            self.ctxt
                .tys
                .unify(param_ty, arg_ty)
                .map_err(|_| TyError::CallArgumentTyMismatch {
                    index: i,
                    expected: param_ty,
                    actual: arg_ty,
                })?;
        }

        Ok(return_ty)
    }

    fn infer_ty_of_addr_of_place(&mut self, place: Place) -> TyResult<ty::Ty> {
        let place_ty = self.ctxt.mlr.get_place_ty(place);
        let ref_ty = self.ctxt.tys.register_ref_ty(place_ty);
        Ok(ref_ty)
    }

    fn infer_ty_of_as_expr(&mut self, op: Op, target_ty: ty::Ty) -> Result<ty::Ty, TyError> {
        let op_ty = self.ctxt.mlr.get_op_ty(op);
        let op_ty_def = self
            .ctxt
            .tys
            .get_ty_def(op_ty)
            .expect("type of as-expr operand should be registered");

        let target_ty_def = self
            .ctxt
            .tys
            .get_ty_def(target_ty)
            .expect("type of as-expr target type should be registered");

        match (op_ty_def, target_ty_def) {
            // Allow arbitrary casts between Ptr
            (ty::TyDef::Ptr(_), ty::TyDef::Ptr(_)) => Ok(target_ty),

            // Allow casts from Ref to Ptr with same base type
            (&ty::TyDef::Ref(op_base_ty), &ty::TyDef::Ptr(target_base_ty)) => {
                self.ctxt
                    .tys
                    .unify(op_base_ty, target_base_ty)
                    .map_err(|_| TyError::InvalidAsExpr { op_ty, target_ty })?;

                Ok(target_ty)
            }

            _ => Err(TyError::InvalidAsExpr { op_ty, target_ty }),
        }
    }

    fn infer_ty_of_size_of_expr(&self) -> Result<ty::Ty, TyError> {
        let int_ty = self.ctxt.tys.get_primitive_ty(ty::Primitive::Integer32);
        Ok(int_ty)
    }

    fn infer_ty_of_fn(&mut self, fn_specialization: &fns::FnSpecialization) -> TyResult<ty::Ty> {
        let signature = self
            .ctxt
            .fns
            .get_sig(fn_specialization.fn_)
            .expect("function signature should be registered");

        if signature.gen_params.len() != fn_specialization.gen_args.len() {
            return TyError::FnGenericArgCountMismatch {
                fn_: fn_specialization.fn_,
                expected: signature.gen_params.len(),
                actual: fn_specialization.gen_args.len(),
            }
            .into();
        }

        for (&gen_var, &gen_arg) in signature.gen_params.iter().zip(&fn_specialization.gen_args) {
            let constraints: Vec<_> = self.ctxt.tys.get_constraints_for(gen_var).collect();
            for constraint in constraints {
                self.ctxt.tys.add_obligation(gen_arg, constraint, self.fn_);
            }
        }

        if signature.env_gen_params.len() != fn_specialization.env_gen_args.len() {
            return TyError::FnEnvGenericArgCountMismatch {
                fn_: fn_specialization.fn_,
                expected: signature.gen_params.len(),
                actual: fn_specialization.gen_args.len(),
            }
            .into();
        }

        let param_tys: Vec<_> = signature.params.iter().map(|param| param.ty).collect();
        let fn_ty = self
            .ctxt
            .tys
            .register_fn_ty(param_tys, signature.return_ty, signature.var_args);

        let substitutions = self.ctxt.fns.get_substitutions_for_specialization(fn_specialization);
        let fn_spec_ty = self.ctxt.tys.substitute_gen_vars(fn_ty, &substitutions);

        Ok(fn_spec_ty)
    }

    fn infer_ty_of_trait_method(&mut self, trait_method: &fns::TraitMethod) -> TyResult<ty::Ty> {
        let signature = self
            .ctxt
            .traits
            .get_trait_method_sig(trait_method.trait_, trait_method.method_idx);

        if signature.gen_params.len() != trait_method.gen_args.len() {
            return TyError::TraitMethodGenericArgCountMismatch {
                trait_: trait_method.trait_,
                method_index: trait_method.method_idx,
                impl_ty: trait_method.impl_ty,
                expected: signature.gen_params.len(),
                actual: trait_method.gen_args.len(),
            }
            .into();
        }

        let param_tys: Vec<_> = signature.params.iter().map(|param| param.ty).collect();
        let fn_ty = self
            .ctxt
            .tys
            .register_fn_ty(param_tys, signature.return_ty, signature.var_args);

        let gen_var_substitutions = signature
            .gen_params
            .iter()
            .cloned()
            .zip(trait_method.gen_args.iter().cloned())
            .collect();
        let substituted_fn_ty = self.ctxt.tys.substitute_self_ty(fn_ty, trait_method.impl_ty);
        let substituted_fn_ty = self
            .ctxt
            .tys
            .substitute_gen_vars(substituted_fn_ty, &gen_var_substitutions);

        Ok(substituted_fn_ty)
    }

    fn infer_ty_of_loc(&self, loc: Loc) -> TyResult<ty::Ty> {
        Ok(self.ctxt.mlr.get_loc_ty(loc))
    }

    fn infer_ty_of_field_access_place(&mut self, base: Place, field_index: usize) -> TyResult<ty::Ty> {
        let base_ty = self.ctxt.mlr.get_place_ty(base);
        let field_ty = self.ctxt.tys.get_struct_field_ty(base_ty, field_index)?;
        Ok(field_ty)
    }

    fn infer_ty_of_enum_discriminant(&self, base: Place) -> TyResult<ty::Ty> {
        let base_ty = self.ctxt.mlr.get_place_ty(base);
        if !self.ctxt.tys.is_enum_ty(base_ty) {
            return TyError::NotAnEnum { ty: base_ty }.into();
        }

        // the discriminant is always an integer
        let int_ty = self.ctxt.tys.get_primitive_ty(ty::Primitive::Integer32);
        Ok(int_ty)
    }

    fn infer_ty_of_project_to_variant_place(&mut self, base: Place, variant_index: usize) -> TyResult<ty::Ty> {
        let base_ty = self.ctxt.mlr.get_place_ty(base);
        self.get_enum_variant_ty(base_ty, variant_index)
    }

    fn infer_ty_of_deref_place(&mut self, op: Op) -> TyResult<ty::Ty> {
        let ref_ty = self.ctxt.mlr.get_op_ty(op);
        let ty_def = self
            .ctxt
            .tys
            .get_ty_def(ref_ty)
            .expect("type of dereferenced op should be registered");

        match *ty_def {
            ty::TyDef::Ref(base_ty) | ty::TyDef::Ptr(base_ty) => {
                if self.ctxt.tys.is_c_void_ty(base_ty) {
                    Err(TyError::DereferenceOfCVoidPtr { ty: ref_ty })
                } else {
                    Ok(base_ty)
                }
            }
            _ => TyError::InvalidDereference { ty: ref_ty }.into(),
        }
    }

    fn check_assign_stmt(&mut self, place: Place, val: Val) -> TyResult<()> {
        let place_ty = self.ctxt.mlr.get_place_ty(place);
        let val_ty = self.ctxt.mlr.get_val_ty(val);

        self.ctxt
            .tys
            .unify(place_ty, val_ty)
            .map_err(|_| TyError::AssignStmtTyMismatch {
                place,
                expected: place_ty,
                actual: val_ty,
            })
    }

    fn check_return_stmt(&mut self, val: Val) -> TyResult<()> {
        let return_ty = self
            .ctxt
            .fns
            .get_sig(self.fn_)
            .expect("function signature should be registered")
            .return_ty;

        let val_ty = self.ctxt.mlr.get_val_ty(val);

        self.ctxt
            .tys
            .unify(return_ty, val_ty)
            .map_err(|_| TyError::ReturnTyMismatch {
                expected: return_ty,
                actual: val_ty,
            })
    }

    pub fn resolve_struct_field(&mut self, struct_ty: ty::Ty, field_name: &str) -> TyResult<usize> {
        let field_index = self.ctxt.tys.get_struct_field_index_by_name(struct_ty, field_name)?;
        Ok(field_index)
    }

    pub fn resolve_struct_fields<'b>(
        &mut self,
        struct_ty: ty::Ty,
        field_names: impl IntoIterator<Item = &'b str>,
    ) -> TyResult<Vec<usize>> {
        let provided_names: Vec<&str> = field_names.into_iter().collect();
        let provided_names_set: HashSet<&str> = provided_names.iter().cloned().collect();

        let expected_names: Vec<&str> = self.ctxt.tys.get_struct_field_names(struct_ty)?.collect();
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

        let expected_names: Vec<&str> = self.ctxt.tys.get_enum_variant_names(enum_ty)?.collect();
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
        let ty = self.ctxt.tys.get_enum_variant_ty(ty, variant_index)?;
        Ok(ty)
    }

    pub fn resolve_method(&self, base_ty: ty::Ty, method_name: &str) -> TyResult<MethodResolution> {
        if let Some(inherent) = self.resolve_inherent_method(base_ty, method_name)? {
            Ok(inherent)
        } else if let Some(trait_method) = self.resolve_trait_method(base_ty, method_name)? {
            Ok(trait_method)
        } else {
            TyError::MethodResolutionFailed {
                base_ty,
                method_name: method_name.to_string(),
            }
            .into()
        }
    }

    fn resolve_inherent_method(&self, base_ty: ty::Ty, method_name: &str) -> TyResult<Option<MethodResolution>> {
        let candidate_fn_specs: Vec<_> = self
            .ctxt
            .impls
            .get_inherent_impls()
            .map(|impl_| self.ctxt.impls.get_impl_def(impl_))
            .filter_map(|impl_def| {
                self.ctxt
                    .tys
                    .try_find_instantiation(base_ty, impl_def.ty, &impl_def.gen_params)
                    .ok()
                    .map(|substitution| (impl_def, substitution))
            })
            .flat_map(|(impl_def, subst)| impl_def.methods_by_name.get(method_name).map(|&method| (method, subst)))
            .collect();

        match &candidate_fn_specs[..] {
            [] => Ok(None),
            [(fn_, subst)] => Ok(Some(MethodResolution::Inherent {
                fn_: *fn_,
                env_gen_args: subst.to_vec(),
            })),
            [_, _, ..] => TyError::AmbiguousMethod {
                base_ty,
                method_name: method_name.to_string(),
            }
            .into(),
        }
    }

    fn resolve_trait_method(&self, base_ty: ty::Ty, method_name: &str) -> TyResult<Option<MethodResolution>> {
        let candidate_trait_methods: Vec<_> = self
            .ctxt
            .traits
            .get_trait_methods_with_name(method_name)
            .filter(|&(trait_, _)| self.ctxt.ty_implements_trait(base_ty, trait_))
            .collect();

        match &candidate_trait_methods[..] {
            [] => Ok(None),
            [(trait_, method_idx)] => Ok(Some(MethodResolution::Trait {
                trait_: *trait_,
                method_idx: *method_idx,
            })),
            [_, _, ..] => TyError::AmbiguousMethod {
                base_ty,
                method_name: method_name.to_string(),
            }
            .into(),
        }
    }
}
