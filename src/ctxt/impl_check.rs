use crate::ctxt::{
    fns, impls, traits,
    ty::{self, GenVarSubst},
};

impl super::Ctxt {
    pub fn resolve_trait_mthd_to_fn(
        &mut self,
        trait_mthd_inst: fns::TraitMthdInst,
        subst: &GenVarSubst,
    ) -> fns::FnInst {
        let mut trait_mthd_inst = self.subst_trait_mthd_inst(trait_mthd_inst, subst);
        // Resolve opaque types to their concrete types for monomorphization
        trait_mthd_inst.impl_ty = self.tys.resolve_opaque_in_ty(trait_mthd_inst.impl_ty);

        let matching_impl_insts: Vec<_> = self
            .get_impl_insts_for_ty_and_trait_inst(trait_mthd_inst.impl_ty, trait_mthd_inst.trait_inst)
            .collect();

        // TODO proper error handling
        assert_eq!(matching_impl_insts.len(), 1);
        let [impl_inst] = matching_impl_insts.try_into().unwrap();

        let trait_mthd_name = self
            .traits
            .get_trait_mthd_name(trait_mthd_inst.trait_inst.trait_, trait_mthd_inst.mthd_idx);

        let impl_def = self.impls.get_impl_def(impl_inst.impl_);
        let fn_ = impl_def.mthds_by_name[trait_mthd_name];

        self.fns
            .inst_fn(fn_, trait_mthd_inst.gen_args, impl_inst.gen_args)
            .unwrap()
    }

    pub(crate) fn get_impl_insts_for_ty_and_trait_inst(
        &mut self,
        ty: ty::Ty,
        trait_inst: traits::TraitInst,
    ) -> impl Iterator<Item = impls::ImplInst> {
        let impl_insts = self.get_impl_insts_for_ty_and_trait(ty, trait_inst.trait_);

        impl_insts.into_iter().filter(move |impl_inst| {
            let impl_def = self.impls.get_impl_def(impl_inst.impl_).clone();
            let subst = GenVarSubst::new(&impl_def.gen_params, self.tys.get_ty_slice(impl_inst.gen_args)).unwrap();

            let inst_impl_trait_inst = self.subst_trait_inst(impl_def.trait_inst.unwrap(), &subst);

            zip_ty_slices!(
                self.tys,
                (inst_impl_trait_inst.gen_args, trait_inst.gen_args),
                all(|ty1, ty2| self.tys.tys_eq(ty1, ty2))
            )
        })
    }

    pub fn ty_implements_trait_inst(
        &mut self,
        constraints: &[ty::Constraint],
        ty: ty::Ty,
        trait_inst: traits::TraitInst,
    ) -> bool {
        if self
            .tys
            .implements_trait_inst_constraint_exists(constraints, ty, trait_inst)
        {
            return true;
        }

        let ty_def = self.tys.get_ty_def(ty);
        if let &ty::TyDef::Opaque { id, .. } = ty_def
            && self.tys.opaque_satisfies_trait_inst(id, trait_inst)
        {
            return true;
        }

        self.get_impl_insts_for_ty_and_trait_inst(ty, trait_inst)
            .next()
            .is_some()
    }

    pub fn ty_implements_trait(&mut self, constraints: &[ty::Constraint], ty: ty::Ty, trait_: traits::Trait) -> bool {
        if self.tys.implements_trait_constraint_exists(constraints, ty, trait_) {
            return true;
        }

        let ty_def = self.tys.get_ty_def(ty);
        if let &ty::TyDef::TraitSelf(trait_2) = ty_def
            && trait_2 == trait_
        {
            return true;
        }

        !self.get_impl_insts_for_ty_and_trait(ty, trait_).is_empty()
    }

    pub fn ty_is_callable(
        &mut self,
        constraints: &[ty::Constraint],
        ty: ty::Ty,
    ) -> Option<(ty::TySlice, ty::Ty, bool)> {
        if let &ty::TyDef::Fn {
            param_tys,
            return_ty,
            var_args,
        } = self.tys.get_ty_def(ty)
        {
            Some((param_tys, return_ty, var_args))
        } else if let Some((param_tys, return_ty)) = self.tys.try_get_callable_constraint(constraints, ty) {
            Some((param_tys, return_ty, false))
        } else if let &ty::TyDef::Closure { fn_inst, .. } = self.tys.get_ty_def(ty) {
            let signature = self.get_fn_inst_sig(fn_inst);

            let params_without_captures: Vec<_> = signature.params.iter().skip(1).map(|p| p.ty).collect();
            let params_without_captures = self.tys.ty_slice(&params_without_captures);

            Some((params_without_captures, signature.return_ty, false))
        } else if let &ty::TyDef::Opaque { id, gen_args } = self.tys.get_ty_def(ty) {
            self.tys
                .try_get_opaque_callable_constraint(id)
                .map(|(param_tys, return_ty)| {
                    let opaque_def = self.tys.get_opaque_def(id);
                    let subst = GenVarSubst::new(&opaque_def.gen_params, self.tys.get_ty_slice(gen_args)).unwrap();

                    let param_tys = self.tys.substitute_gen_vars_on_slice(param_tys, &subst);
                    let return_ty = self.tys.substitute_gen_vars(return_ty, &subst);

                    (param_tys, return_ty, false)
                })
        } else {
            None
        }
    }

    pub(crate) fn resolve_associated_ty_completely(
        &mut self,
        constraints: &[ty::Constraint],
        base_ty: ty::Ty,
        ident: &str,
    ) -> Option<ty::Ty> {
        let base_ty_def = self.tys.get_ty_def(base_ty);

        if let &ty::TyDef::TraitSelf(trait_) = base_ty_def {
            let trait_def = self.traits.get_trait_def(trait_);
            let assoc_ty_index = trait_def.assoc_tys.iter().position(|name| name == ident)?;
            let gen_args: Vec<_> = trait_def.gen_params.iter().map(|gp| self.tys.gen_var(*gp)).collect();
            let gen_args = self.tys.ty_slice(&gen_args);
            let default_trait_inst = self.traits.inst_trait(trait_, gen_args).unwrap();
            let ty = self.tys.assoc_ty(base_ty, default_trait_inst, assoc_ty_index);
            return Some(ty);
        }

        let candidate_assoc_tys: Vec<_> = self.traits.get_trait_assoc_ty_with_name(ident).collect::<Vec<_>>();
        let candidate_assoc_tys: Vec<_> = candidate_assoc_tys
            .into_iter()
            .filter(|&(trait_, _)| self.ty_implements_trait(constraints, base_ty, trait_))
            .collect();

        match &candidate_assoc_tys[..] {
            [] => None,
            [(trait_, assoc_ty_idx)] => {
                let impl_insts = self.get_impl_insts_for_ty_and_trait(base_ty, *trait_);

                let [impl_inst] = &impl_insts[..] else {
                    if let Some(trait_inst) = self.tys.get_trait_inst_constraint(constraints, base_ty, *trait_) {
                        return Some(self.tys.assoc_ty(base_ty, trait_inst, *assoc_ty_idx));
                    }
                    let gen_args: Vec<_> = self
                        .traits
                        .get_trait_def(*trait_)
                        .gen_params
                        .iter()
                        .map(|gp| self.tys.gen_var(*gp))
                        .collect();
                    let gen_args = self.tys.ty_slice(&gen_args);
                    let trait_inst = self.traits.inst_trait(*trait_, gen_args).unwrap();
                    return Some(self.tys.assoc_ty(base_ty, trait_inst, *assoc_ty_idx));
                };

                let impl_def = self.impls.get_impl_def(impl_inst.impl_);
                let assoc_ty = *impl_def.assoc_tys.get(assoc_ty_idx).unwrap();

                Some(assoc_ty)
            }
            [_, _, ..] => None,
        }
    }

    fn get_impl_insts_for_ty_and_trait(&mut self, ty: ty::Ty, trait_: traits::Trait) -> Vec<impls::ImplInst> {
        self.impls
            .get_impls_for_trait(trait_)
            .filter_map(|impl_| {
                let impl_def = self.impls.get_impl_def(impl_).clone();

                let gen_args = self
                    .tys
                    .try_find_instantiation(ty, impl_def.ty, &impl_def.gen_params)
                    .ok()?;

                let impl_inst = self.impls.inst_impl(impl_, gen_args).unwrap();
                Some(impl_inst)
            })
            .collect()
    }

    fn subst_trait_inst(&mut self, trait_inst: traits::TraitInst, subst: &GenVarSubst) -> traits::TraitInst {
        let gen_args = self.tys.substitute_gen_vars_on_slice(trait_inst.gen_args, subst);
        self.traits.inst_trait(trait_inst.trait_, gen_args).unwrap()
    }

    fn subst_trait_mthd_inst(
        &mut self,
        trait_mthd_inst: fns::TraitMthdInst,
        subst: &GenVarSubst,
    ) -> fns::TraitMthdInst {
        fns::TraitMthdInst {
            impl_ty: self.tys.substitute_gen_vars(trait_mthd_inst.impl_ty, subst),
            trait_inst: self.subst_trait_inst(trait_mthd_inst.trait_inst, subst),
            gen_args: self.tys.substitute_gen_vars_on_slice(trait_mthd_inst.gen_args, subst),
            mthd_idx: trait_mthd_inst.mthd_idx,
        }
    }
}
