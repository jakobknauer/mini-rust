mod err;

use std::collections::HashSet;

pub use err::{TyError, TyResult};

use crate::ctxt::{self, fns, mlr::*, traits, ty};

pub struct Typechecker<'a> {
    ctxt: &'a mut ctxt::Ctxt,
    fn_: fns::Fn,
}

pub enum MthdResolution {
    Inherent {
        fn_: fns::Fn,
        env_gen_args: Vec<ty::Ty>,
    },
    Trait {
        trait_inst: traits::TraitInst,
        mthd_idx: usize,
    },
}

pub struct FieldAccessResolution {
    pub num_derefs: usize,
    pub field_index: usize,
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
            PtrOffset(op, offset) => self.infer_ty_of_ptr_offset_expr(op, offset),
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
            ClosureCaptures(place) => self.infer_ty_of_closure_captures(place),
        }?;

        self.ctxt.mlr.set_place_ty(place, ty);
        Ok(ty)
    }

    pub fn infer_op_ty(&mut self, op: Op) -> TyResult<ty::Ty> {
        use OpDef::*;

        let op_def = self.ctxt.mlr.get_op_def(op);

        let ty = match *op_def {
            Fn(ref fn_inst) => self.infer_ty_of_fn(&fn_inst.clone()),
            TraitMthd(ref trait_mthd_inst) => self.infer_ty_of_trait_mthd(&trait_mthd_inst.clone()),
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
            If(if_) => self.check_if_stmt(if_)?,
            Alloc { .. } | Block(..) | Loop { .. } | Break => (),
        }

        Ok(())
    }

    fn infer_ty_of_constant(&mut self, constant: &Const) -> TyResult<ty::Ty> {
        use Const::*;

        let ty = match constant {
            Int(_) => ty::Primitive::Integer32,
            Bool(_) => ty::Primitive::Boolean,
            Unit => return Ok(self.ctxt.tys.register_unit_ty()),
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
        let callable_ty = self.ctxt.mlr.get_op_ty(callable);

        let Some((param_tys, return_ty, var_args)) = self.ctxt.ty_is_callable(callable_ty) else {
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

    fn infer_ty_of_size_of_expr(&self) -> TyResult<ty::Ty> {
        let int_ty = self.ctxt.tys.get_primitive_ty(ty::Primitive::Integer32);
        Ok(int_ty)
    }

    fn infer_ty_of_ptr_offset_expr(&self, op: Op, offset: Op) -> TyResult<ty::Ty> {
        let op_ty = self.ctxt.mlr.get_op_ty(op);
        let op_ty_def = self.ctxt.tys.get_ty_def(op_ty).unwrap();

        if !matches!(op_ty_def, ty::TyDef::Ptr(_)) {
            return TyError::NotAPtrTy { ty: op_ty }.into();
        }

        let offset_ty = self.ctxt.mlr.get_op_ty(offset);
        let offset_ty_def = self.ctxt.tys.get_ty_def(offset_ty).unwrap();

        if !matches!(offset_ty_def, ty::TyDef::Primitive(ty::Primitive::Integer32)) {
            return TyError::NotAnIntTy { ty: offset_ty }.into();
        }

        Ok(op_ty)
    }

    fn infer_ty_of_fn(&mut self, fn_inst: &fns::FnInst) -> TyResult<ty::Ty> {
        let signature = self
            .ctxt
            .fns
            .get_sig(fn_inst.fn_)
            .expect("function signature should be registered");

        if signature.gen_params.len() != fn_inst.gen_args.len() {
            return TyError::FnGenericArgCountMismatch {
                fn_: fn_inst.fn_,
                expected: signature.gen_params.len(),
                actual: fn_inst.gen_args.len(),
            }
            .into();
        }

        let subst = self.ctxt.fns.get_subst_for_fn_inst(fn_inst);

        for (&gen_var, &gen_arg) in signature.gen_params.iter().zip(&fn_inst.gen_args) {
            let requirements: Vec<_> = self.ctxt.tys.get_requirements_for(gen_var).cloned().collect();
            for requirement in requirements {
                match requirement {
                    ty::ConstraintRequirement::Trait(trait_inst) => {
                        let mut trait_inst = trait_inst.clone();
                        for gen_arg in &mut trait_inst.gen_args {
                            *gen_arg = self.ctxt.tys.substitute_gen_vars(*gen_arg, &subst);
                        }
                        self.ctxt.tys.add_implements_trait_inst_obligation(gen_arg, trait_inst)
                    }
                    ty::ConstraintRequirement::Callable { param_tys, return_ty } => {
                        let param_tys: Vec<_> = param_tys
                            .iter()
                            .map(|&ty| self.ctxt.tys.substitute_gen_vars(ty, &subst))
                            .collect();
                        let return_ty = self.ctxt.tys.substitute_gen_vars(return_ty, &subst);
                        self.ctxt.tys.add_callable_obligation(gen_arg, param_tys, return_ty)
                    }
                }
            }
        }

        if signature.env_gen_params.len() != fn_inst.env_gen_args.len() {
            return TyError::FnEnvGenericArgCountMismatch {
                fn_: fn_inst.fn_,
                expected: signature.env_gen_params.len(),
                actual: fn_inst.env_gen_args.len(),
            }
            .into();
        }

        let param_tys: Vec<_> = signature.params.iter().map(|param| param.ty).collect();
        let fn_ty = self
            .ctxt
            .tys
            .register_fn_ty(param_tys, signature.return_ty, signature.var_args);

        let fn_inst_ty = self.ctxt.tys.substitute_gen_vars(fn_ty, &subst);

        Ok(fn_inst_ty)
    }

    fn infer_ty_of_trait_mthd(&mut self, trait_mthd_inst: &fns::TraitMthdInst) -> TyResult<ty::Ty> {
        self.ctxt
            .tys
            .add_implements_trait_inst_obligation(trait_mthd_inst.impl_ty, trait_mthd_inst.trait_inst.clone());

        let signature = self
            .ctxt
            .traits
            .get_trait_mthd_sig(trait_mthd_inst.trait_inst.trait_, trait_mthd_inst.mthd_idx);

        let trait_def = self.ctxt.traits.get_trait_def(trait_mthd_inst.trait_inst.trait_);

        if signature.gen_params.len() != trait_mthd_inst.gen_args.len() {
            return TyError::TraitMthdGenericArgCountMismatch {
                trait_: trait_mthd_inst.trait_inst.trait_,
                mthd_idx: trait_mthd_inst.mthd_idx,
                impl_ty: trait_mthd_inst.impl_ty,
                expected: signature.gen_params.len(),
                actual: trait_mthd_inst.gen_args.len(),
            }
            .into();
        }

        let param_tys: Vec<_> = signature.params.iter().map(|param| param.ty).collect();
        let fn_ty = self
            .ctxt
            .tys
            .register_fn_ty(param_tys, signature.return_ty, signature.var_args);

        let trait_gen_var_subst =
            ty::GenVarSubst::new(&trait_def.gen_params, &trait_mthd_inst.trait_inst.gen_args).unwrap();
        let gen_var_subst = ty::GenVarSubst::new(&signature.gen_params, &trait_mthd_inst.gen_args).unwrap();
        let all_gen_var_subst = ty::GenVarSubst::compose(trait_gen_var_subst, gen_var_subst);

        let substituted_fn_ty = self.ctxt.tys.substitute_self_ty(fn_ty, trait_mthd_inst.impl_ty);
        let substituted_fn_ty = self.ctxt.tys.substitute_gen_vars(substituted_fn_ty, &all_gen_var_subst);

        Ok(substituted_fn_ty)
    }

    fn infer_ty_of_loc(&self, loc: Loc) -> TyResult<ty::Ty> {
        Ok(self.ctxt.mlr.get_loc_ty(loc))
    }

    fn infer_ty_of_field_access_place(&mut self, base: Place, field_index: usize) -> TyResult<ty::Ty> {
        let base_ty = self.ctxt.mlr.get_place_ty(base);
        let base_ty_def = self.ctxt.tys.get_ty_def(base_ty).unwrap();

        let field_ty = match base_ty_def {
            ty::TyDef::Struct { .. } => self.ctxt.tys.get_struct_field_ty(base_ty, field_index)?,
            ty::TyDef::Tuple(tys) => *tys.get(field_index).ok_or(TyError::InvalidTupleIndex {
                ty: base_ty,
                index: field_index,
            })?,
            _ => return TyError::NotAStructOrTuple { ty: base_ty }.into(),
        };

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

    fn infer_ty_of_closure_captures(&self, place: Place) -> TyResult<ty::Ty> {
        let closure_ty = self.ctxt.mlr.get_place_ty(place);
        let closure_ty_def = self.ctxt.tys.get_ty_def(closure_ty).unwrap();

        let ty::TyDef::Closure { captures_ty, .. } = closure_ty_def else {
            panic!("closure type should be registered");
        };

        Ok(*captures_ty)
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

    fn check_if_stmt(&mut self, if_: If) -> TyResult<()> {
        let condition_ty = self.ctxt.mlr.get_op_ty(if_.cond);
        self.ctxt
            .tys
            .unify(condition_ty, self.ctxt.tys.get_primitive_ty(ty::Primitive::Boolean))
            .map_err(|_| TyError::IfConditionNotBoolean { actual: condition_ty })?;

        Ok(())
    }

    pub fn resolve_struct_field(&mut self, mut obj_ty: ty::Ty, field_name: &str) -> TyResult<FieldAccessResolution> {
        let mut obj_ty_def = self.ctxt.tys.get_ty_def(obj_ty).unwrap();
        let mut num_derefs = 0;

        while let ty::TyDef::Ptr(base_ty) | ty::TyDef::Ref(base_ty) = obj_ty_def {
            obj_ty = *base_ty;
            obj_ty_def = self.ctxt.tys.get_ty_def(obj_ty).unwrap();
            num_derefs += 1;
        }

        let field_index = self.ctxt.tys.get_struct_field_index_by_name(obj_ty, field_name)?;

        let result = FieldAccessResolution {
            field_index,
            num_derefs,
        };
        Ok(result)
    }

    pub fn resolve_tuple_field(&mut self, mut obj_ty: ty::Ty, field_index: usize) -> TyResult<FieldAccessResolution> {
        let mut obj_ty_def = self.ctxt.tys.get_ty_def(obj_ty).unwrap();
        let mut num_derefs = 0;

        while let ty::TyDef::Ptr(base_ty) | ty::TyDef::Ref(base_ty) = obj_ty_def {
            obj_ty = *base_ty;
            obj_ty_def = self.ctxt.tys.get_ty_def(obj_ty).unwrap();
            num_derefs += 1;
        }

        if !matches!(obj_ty_def, ty::TyDef::Tuple(_)) {
            return TyError::NotATuple { ty: obj_ty }.into();
        }

        let result = FieldAccessResolution {
            field_index,
            num_derefs,
        };
        Ok(result)
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

    pub fn resolve_mthd(&mut self, base_ty: ty::Ty, mthd_name: &str) -> TyResult<MthdResolution> {
        if let Some(inherent) = self.resolve_inherent_mthd(base_ty, mthd_name)? {
            Ok(inherent)
        } else if let Some(trait_mthd) = self.resolve_trait_mthd(base_ty, mthd_name)? {
            Ok(trait_mthd)
        } else {
            TyError::MthdResolutionFailed {
                base_ty,
                mthd_name: mthd_name.to_string(),
            }
            .into()
        }
    }

    fn resolve_inherent_mthd(&self, base_ty: ty::Ty, mthd_name: &str) -> TyResult<Option<MthdResolution>> {
        let candidate_fn_insts: Vec<_> = self
            .ctxt
            .impls
            .get_inherent_impls()
            .map(|impl_| self.ctxt.impls.get_impl_def(impl_))
            .filter_map(|impl_def| {
                self.ctxt
                    .tys
                    .try_find_instantiation(base_ty, impl_def.ty, &impl_def.gen_params)
                    .ok()
                    .map(|inst| (impl_def, inst))
            })
            .flat_map(|(impl_def, subst)| impl_def.mthds_by_name.get(mthd_name).map(|&mthd| (mthd, subst)))
            .filter(|&(mthd, _)| self.ctxt.fns.get_sig(mthd).unwrap().has_receiver())
            .collect();

        match &candidate_fn_insts[..] {
            [] => Ok(None),
            [(fn_, subst)] => Ok(Some(MthdResolution::Inherent {
                fn_: *fn_,
                env_gen_args: subst.to_vec(),
            })),
            [_, _, ..] => TyError::AmbiguousMthd {
                base_ty,
                mthd_name: mthd_name.to_string(),
            }
            .into(),
        }
    }

    fn resolve_trait_mthd(&mut self, base_ty: ty::Ty, mthd_name: &str) -> TyResult<Option<MthdResolution>> {
        let candidate_trait_mthds: Vec<_> = self
            .ctxt
            .traits
            .get_trait_mthd_with_receiver_and_name(mthd_name)
            .filter(|&(trait_, _)| self.ctxt.ty_implements_trait(base_ty, trait_))
            .collect();

        match &candidate_trait_mthds[..] {
            [] => Ok(None),
            [(trait_, mthd_idx)] => {
                let trait_def = self.ctxt.traits.get_trait_def(*trait_);
                let n_gen_params = trait_def.gen_params.len();
                let gen_args = (0..n_gen_params).map(|_| self.ctxt.tys.new_undefined_ty()).collect();

                let trait_inst = traits::TraitInst {
                    trait_: *trait_,
                    gen_args,
                };
                Ok(Some(MthdResolution::Trait {
                    trait_inst,
                    mthd_idx: *mthd_idx,
                }))
            }
            [_, _, ..] => TyError::AmbiguousMthd {
                base_ty,
                mthd_name: mthd_name.to_string(),
            }
            .into(),
        }
    }
}
