use crate::{
    ctxt::{
        fns::{self, FnInst, TraitMthdInst},
        traits::{self, TraitInst},
        ty, ty_match,
    },
    hlr,
    typeck::{DerefStep, TypeckError, TypeckResult},
};

#[derive(Clone)]
pub enum MthdResolution<'ty> {
    Inherent(fns::FnInst<'ty>),
    Trait(fns::TraitMthdInst<'ty>),
}

pub(super) enum FoundMthd<'ty> {
    Inherent {
        fn_: fns::Fn<'ty>,
        env_gen_args: ty::TySlice<'ty>,
    },
    Trait {
        trait_inst: traits::TraitInst<'ty>,
        mthd: traits::TraitMthd<'ty>,
    },
}

impl<'a, 'ctxt: 'a> super::Typeck<'a, 'ctxt> {
    fn build_deref_chain(&mut self, receiver_ty: ty::Ty<'ctxt>) -> (Vec<ty::Ty<'ctxt>>, Vec<DerefStep<'ctxt>>) {
        let mut levels = vec![receiver_ty];
        let mut steps = vec![];
        while let Some((next, step)) = self.try_deref_step(*levels.last().unwrap()) {
            levels.push(next);
            steps.push(step);
        }
        (levels, steps)
    }

    pub(super) fn resolve_mthd_with_deref(
        &mut self,
        receiver_ty: ty::Ty<'ctxt>,
        mthd_name: &str,
    ) -> TypeckResult<'ctxt, (FoundMthd<'ctxt>, Vec<DerefStep<'ctxt>>, ty::Ty<'ctxt>)> {
        let (levels, mut steps) = self.build_deref_chain(receiver_ty);

        // Pass 1: inherent methods across all deref levels.
        for (i, &ty) in levels.iter().enumerate() {
            if let Some(found) = self.resolve_inherent_mthd(ty, mthd_name, true)? {
                steps.truncate(i);
                return Ok((found, steps, ty));
            }
        }

        // Pass 2: trait methods across all deref levels.
        for (i, &ty) in levels.iter().enumerate() {
            if let Some(found) = self.resolve_trait_mthd(ty, mthd_name, true)? {
                steps.truncate(i);
                return Ok((found, steps, ty));
            }
        }

        Err(TypeckError::MthdResolutionFailed {
            base_ty: receiver_ty,
            mthd_name: mthd_name.to_string(),
        })
    }

    pub(super) fn resolve_mthd(
        &mut self,
        base_ty: ty::Ty<'ctxt>,
        mthd_name: &str,
        require_receiver: bool,
    ) -> TypeckResult<'ctxt, FoundMthd<'ctxt>> {
        if let Some(res) = self.resolve_inherent_mthd(base_ty, mthd_name, require_receiver)? {
            Ok(res)
        } else if let Some(res) = self.resolve_trait_mthd(base_ty, mthd_name, require_receiver)? {
            Ok(res)
        } else {
            Err(TypeckError::MthdResolutionFailed {
                base_ty,
                mthd_name: mthd_name.to_string(),
            })
        }
    }

    fn resolve_inherent_mthd(
        &mut self,
        base_ty: ty::Ty<'ctxt>,
        mthd_name: &str,
        require_receiver: bool,
    ) -> TypeckResult<'ctxt, Option<FoundMthd<'ctxt>>> {
        let candidates: Vec<_> = self
            .ctxt
            .impls
            .get_inherent_impls()
            .filter_map(|impl_| {
                let mthd_fn = *impl_.mthds.borrow().iter().find(|&&m| m.name == mthd_name)?;
                let env_gen_args = ty_match::try_find_instantiation(base_ty, impl_.ty, &impl_.gen_params)
                    .ok()
                    .map(|v| self.ctxt.tys.ty_slice(&v))?;
                let subst = ty::GenVarSubst::new(&impl_.gen_params, env_gen_args).unwrap();

                let impl_constraints_satisfied =
                    self.ctxt
                        .impl_constraints_satisfied(&self.constraints, &impl_.constraints, &subst);

                if impl_constraints_satisfied && (!require_receiver || mthd_fn.has_receiver()) {
                    Some((mthd_fn, env_gen_args))
                } else {
                    None
                }
            })
            .collect();

        match candidates.as_slice() {
            [] => Ok(None),
            &[(fn_, env_gen_args)] => Ok(Some(FoundMthd::Inherent { fn_, env_gen_args })),
            _ => Err(TypeckError::AmbiguousMthd {
                base_ty,
                mthd_name: mthd_name.to_string(),
            }),
        }
    }

    fn resolve_trait_mthd(
        &mut self,
        base_ty: ty::Ty<'ctxt>,
        mthd_name: &str,
        require_receiver: bool,
    ) -> TypeckResult<'ctxt, Option<FoundMthd<'ctxt>>> {
        let candidates: Vec<_> = self
            .ctxt
            .traits
            .get_trait_mthd_with_name(mthd_name, require_receiver)
            .filter(|mthd| self.ctxt.ty_implements_trait(&self.constraints, base_ty, mthd.trait_))
            .collect();

        match candidates.as_slice() {
            [] => Ok(None),
            &[mthd] => {
                let n_trait_gen_params = mthd.trait_.gen_params.len();
                let trait_gen_args: Vec<_> = (0..n_trait_gen_params).map(|_| self.ctxt.tys.inf_var()).collect();
                let trait_gen_args_slice = self.ctxt.tys.ty_slice(&trait_gen_args);
                let trait_inst = TraitInst::new(mthd.trait_, trait_gen_args_slice).unwrap();
                Ok(Some(FoundMthd::Trait { trait_inst, mthd }))
            }
            _ => Err(TypeckError::AmbiguousMthd {
                base_ty,
                mthd_name: mthd_name.to_string(),
            }),
        }
    }

    pub(super) fn instantiate_mthd(
        &mut self,
        found: FoundMthd<'ctxt>,
        base_ty: ty::Ty<'ctxt>,
        mthd_name: &str,
        gen_args: Option<hlr::TyAnnotSlice<'ctxt>>,
    ) -> TypeckResult<'ctxt, MthdResolution<'ctxt>> {
        match found {
            FoundMthd::Inherent { fn_, env_gen_args } => {
                let n_gen_params = fn_.gen_params.len();
                let resolved_gen_args = self.resolve_optional_gen_args(gen_args, n_gen_params, |actual| {
                    TypeckError::MthdGenArgCountMismatch {
                        mthd_name: mthd_name.to_string(),
                        expected: n_gen_params,
                        actual,
                    }
                })?;

                let env_subst = ty::GenVarSubst::new(&fn_.env_gen_params, env_gen_args).unwrap();
                let fn_subst = ty::GenVarSubst::new(&fn_.gen_params, resolved_gen_args.iter().copied()).unwrap();
                let full_subst = ty::GenVarSubst::compose(env_subst, fn_subst);
                self.add_constraint_obligations(fn_, &full_subst);

                let fn_gen_args = self.ctxt.tys.ty_slice(&resolved_gen_args);
                let fn_inst = FnInst::new(fn_, fn_gen_args, env_gen_args).unwrap();
                Ok(MthdResolution::Inherent(fn_inst.with_self_ty(fn_.associated_ty)))
            }
            FoundMthd::Trait { trait_inst, mthd } => {
                let decl = mthd.fn_;
                let n_mthd_gen_params = decl.gen_params.len();
                let resolved_gen_args = self.resolve_optional_gen_args(gen_args, n_mthd_gen_params, |actual| {
                    TypeckError::MthdGenArgCountMismatch {
                        mthd_name: mthd_name.to_string(),
                        expected: n_mthd_gen_params,
                        actual,
                    }
                })?;

                let trait_subst = ty::GenVarSubst::new(&trait_inst.trait_.gen_params, trait_inst.gen_args).unwrap();
                let mthd_subst = ty::GenVarSubst::new(&decl.gen_params, resolved_gen_args.iter().copied()).unwrap();
                let full_subst = ty::GenVarSubst::compose(trait_subst, mthd_subst);

                self.add_trait_mthd_constraint_obligations(&decl.constraints, &full_subst, base_ty);

                let mthd_gen_args = self.ctxt.tys.ty_slice(&resolved_gen_args);
                Ok(MthdResolution::Trait(
                    TraitMthdInst::new(trait_inst, mthd, base_ty, mthd_gen_args).unwrap(),
                ))
            }
        }
    }

    pub(super) fn fn_ty_of_mthd_resolution(&mut self, resolution: &MthdResolution<'ctxt>) -> ty::Ty<'ctxt> {
        match resolution {
            MthdResolution::Inherent(fn_inst) => self.fn_ty_of_inherent_resolution(*fn_inst),
            MthdResolution::Trait(trait_mthd_inst) => self.fn_ty_of_trait_mthd_resolution(*trait_mthd_inst),
        }
    }

    fn fn_ty_of_inherent_resolution(&mut self, fn_inst: fns::FnInst<'ctxt>) -> ty::Ty<'ctxt> {
        let param_tys: Vec<_> = fn_inst.fn_.params.iter().map(|p| p.ty).collect();
        let return_ty = fn_inst.fn_.return_ty;
        let var_args = fn_inst.fn_.var_args;
        let _ = fn_inst.fn_;
        let fn_ty = self.ctxt.tys.fn_(&param_tys, return_ty, var_args);
        let subst = fn_inst.get_subst();
        self.ctxt.tys.substitute_gen_vars(fn_ty, &subst)
    }

    fn fn_ty_of_trait_mthd_resolution(&mut self, inst: fns::TraitMthdInst<'ctxt>) -> ty::Ty<'ctxt> {
        let param_tys: Vec<_> = inst.mthd.fn_.params.iter().map(|p| p.ty).collect();
        let fn_ty = self
            .ctxt
            .tys
            .fn_(&param_tys, inst.mthd.fn_.return_ty, inst.mthd.fn_.var_args);

        let trait_gen_var_subst =
            ty::GenVarSubst::new(&inst.trait_inst.trait_.gen_params, inst.trait_inst.gen_args).unwrap();
        let gen_var_subst = ty::GenVarSubst::new(&inst.mthd.fn_.gen_params, inst.gen_args).unwrap();
        let all_gen_var_subst = ty::GenVarSubst::compose(trait_gen_var_subst, gen_var_subst);

        self.ctxt.tys.substitute(fn_ty, &all_gen_var_subst, Some(inst.impl_ty))
    }
}
