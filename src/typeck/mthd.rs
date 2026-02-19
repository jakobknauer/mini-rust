use crate::ctxt::{fns, traits, ty};
use crate::hlr;
use crate::typeck::{TypeckError, TypeckResult};

pub enum MthdResolution {
    Inherent(fns::FnInst),
    Trait(fns::TraitMthdInst),
}

pub(super) enum FoundMthd {
    Inherent {
        fn_: fns::Fn,
        env_gen_args: ty::TySlice,
    },
    Trait {
        trait_inst: traits::TraitInst,
        mthd_idx: usize,
    },
}

impl<'ctxt, 'hlr> super::Typeck<'ctxt, 'hlr> {
    pub(super) fn resolve_mthd(
        &mut self,
        base_ty: ty::Ty,
        mthd_name: &str,
        require_receiver: bool,
    ) -> TypeckResult<FoundMthd> {
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
        base_ty: ty::Ty,
        mthd_name: &str,
        require_receiver: bool,
    ) -> TypeckResult<Option<FoundMthd>> {
        let candidates: Vec<_> = self
            .ctxt
            .impls
            .get_inherent_impls()
            .filter_map(|impl_id| {
                let impl_def = self.ctxt.impls.get_impl_def(impl_id);
                let impl_ty = impl_def.ty;
                let gen_params = impl_def.gen_params.clone();
                let mthd_fn = *impl_def.mthds_by_name.get(mthd_name)?;
                let env_gen_args = self
                    .ctxt
                    .tys
                    .try_find_instantiation(base_ty, impl_ty, &gen_params)
                    .ok()?;
                let has_receiver = self.ctxt.fns.get_sig(mthd_fn).unwrap().has_receiver();
                (!require_receiver || has_receiver).then_some((mthd_fn, env_gen_args))
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
        base_ty: ty::Ty,
        mthd_name: &str,
        require_receiver: bool,
    ) -> TypeckResult<Option<FoundMthd>> {
        let candidates: Vec<_> = self
            .ctxt
            .traits
            .get_trait_mthd_with_name(mthd_name, require_receiver)
            .collect();
        let candidates: Vec<_> = candidates
            .into_iter()
            .filter(|&(trait_, _)| self.ctxt.ty_implements_trait(base_ty, trait_))
            .collect();

        match candidates.as_slice() {
            [] => Ok(None),
            &[(trait_, mthd_idx)] => {
                let n_trait_gen_params = self.ctxt.traits.get_trait_def(trait_).gen_params.len();
                let trait_gen_args: Vec<_> = (0..n_trait_gen_params).map(|_| self.ctxt.tys.inf_var()).collect();
                let trait_gen_args_slice = self.ctxt.tys.ty_slice(&trait_gen_args);
                let trait_inst = traits::TraitInst {
                    trait_,
                    gen_args: trait_gen_args_slice,
                };
                Ok(Some(FoundMthd::Trait { trait_inst, mthd_idx }))
            }
            _ => Err(TypeckError::AmbiguousMthd {
                base_ty,
                mthd_name: mthd_name.to_string(),
            }),
        }
    }

    pub(super) fn instantiate_mthd(
        &mut self,
        found: FoundMthd,
        base_ty: ty::Ty,
        mthd_name: &str,
        gen_args: Option<hlr::TyAnnotSlice<'hlr>>,
    ) -> TypeckResult<MthdResolution> {
        match found {
            FoundMthd::Inherent { fn_, env_gen_args } => {
                let n_gen_params = self.ctxt.fns.get_sig(fn_).unwrap().gen_params.len();
                let resolved_gen_args = self.resolve_optional_gen_args(gen_args, n_gen_params, |actual| {
                    TypeckError::MthdGenArgCountMismatch {
                        mthd_name: mthd_name.to_string(),
                        expected: n_gen_params,
                        actual,
                    }
                })?;
                let fn_gen_args = self.ctxt.tys.ty_slice(&resolved_gen_args);
                Ok(MthdResolution::Inherent(fns::FnInst {
                    fn_,
                    gen_args: fn_gen_args,
                    env_gen_args,
                }))
            }
            FoundMthd::Trait { trait_inst, mthd_idx } => {
                let n_mthd_gen_params = self
                    .ctxt
                    .traits
                    .get_trait_mthd_sig(trait_inst.trait_, mthd_idx)
                    .gen_params
                    .len();
                let resolved_gen_args = self.resolve_optional_gen_args(gen_args, n_mthd_gen_params, |actual| {
                    TypeckError::MthdGenArgCountMismatch {
                        mthd_name: mthd_name.to_string(),
                        expected: n_mthd_gen_params,
                        actual,
                    }
                })?;
                let mthd_gen_args = self.ctxt.tys.ty_slice(&resolved_gen_args);
                Ok(MthdResolution::Trait(fns::TraitMthdInst {
                    trait_inst,
                    mthd_idx,
                    impl_ty: base_ty,
                    gen_args: mthd_gen_args,
                }))
            }
        }
    }

    pub(super) fn fn_ty_of_mthd_resolution(&mut self, resolution: &MthdResolution) -> ty::Ty {
        match resolution {
            MthdResolution::Inherent(fn_inst) => self.fn_ty_of_inherent_resolution(*fn_inst),
            MthdResolution::Trait(trait_mthd_inst) => self.fn_ty_of_trait_mthd_resolution(*trait_mthd_inst),
        }
    }

    fn fn_ty_of_inherent_resolution(&mut self, fn_inst: fns::FnInst) -> ty::Ty {
        let sig = self.ctxt.fns.get_sig(fn_inst.fn_).unwrap();
        let param_tys: Vec<_> = sig.params.iter().map(|p| p.ty).collect();
        let fn_ty = self.ctxt.tys.fn_(&param_tys, sig.return_ty, sig.var_args);
        let subst = self.ctxt.get_subst_for_fn_inst(fn_inst);
        self.ctxt.tys.substitute_gen_vars(fn_ty, &subst)
    }

    fn fn_ty_of_trait_mthd_resolution(&mut self, inst: fns::TraitMthdInst) -> ty::Ty {
        let sig = self
            .ctxt
            .traits
            .get_trait_mthd_sig(inst.trait_inst.trait_, inst.mthd_idx);
        let param_tys: Vec<_> = sig.params.iter().map(|p| p.ty).collect();
        let sig_gen_params = sig.gen_params.clone();
        let fn_ty = self.ctxt.tys.fn_(&param_tys, sig.return_ty, sig.var_args);

        let trait_def = self.ctxt.traits.get_trait_def(inst.trait_inst.trait_);
        let trait_gen_params = trait_def.gen_params.clone();

        let trait_gen_var_subst =
            ty::GenVarSubst::new(&trait_gen_params, self.ctxt.tys.get_ty_slice(inst.trait_inst.gen_args)).unwrap();
        let gen_var_subst = ty::GenVarSubst::new(&sig_gen_params, self.ctxt.tys.get_ty_slice(inst.gen_args)).unwrap();
        let all_gen_var_subst = ty::GenVarSubst::compose(trait_gen_var_subst, gen_var_subst);

        let fn_ty = self.ctxt.tys.substitute_self_ty(fn_ty, inst.impl_ty);
        self.ctxt.tys.substitute_gen_vars(fn_ty, &all_gen_var_subst)
    }
}
