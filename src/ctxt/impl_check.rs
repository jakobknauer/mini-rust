use crate::ctxt::{
    fns, impls, traits,
    ty::{self, GenVarSubst},
};

impl<'ctxt> super::Ctxt<'ctxt> {
    pub fn resolve_trait_mthd_to_fn(
        &mut self,
        trait_mthd_inst: fns::TraitMthdInst<'ctxt>,
        subst: &GenVarSubst<'ctxt>,
    ) -> fns::FnInst<'ctxt> {
        let mut trait_mthd_inst = self.subst_trait_mthd_inst(trait_mthd_inst, subst);
        // Resolve opaque and associated types to their concrete types for monomorphization
        trait_mthd_inst.impl_ty = self.normalize_ty(trait_mthd_inst.impl_ty);
        let trait_gen_args = self.tys.get_ty_slice(trait_mthd_inst.trait_inst.gen_args).to_vec();
        let trait_gen_args: Vec<_> = trait_gen_args.into_iter().map(|t| self.normalize_ty(t)).collect();
        let trait_gen_args = self.tys.ty_slice(&trait_gen_args);
        let trait_inst = trait_mthd_inst.trait_inst.with_gen_args(trait_gen_args).unwrap();
        trait_mthd_inst = trait_mthd_inst
            .with_updated(trait_mthd_inst.impl_ty, trait_inst, trait_mthd_inst.gen_args)
            .unwrap();

        let matching_impl_insts: Vec<_> = self
            .get_impl_insts_for_ty_and_trait_inst(&[], trait_mthd_inst.impl_ty, trait_mthd_inst.trait_inst)
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
        constraints: &[ty::Constraint<'ctxt>],
        ty: ty::Ty<'ctxt>,
        trait_inst: traits::TraitInst<'ctxt>,
    ) -> impl Iterator<Item = impls::ImplInst<'ctxt>> {
        let impl_insts = self.get_impl_insts_for_ty_and_trait(constraints, ty, trait_inst.trait_);

        impl_insts.into_iter().filter(move |impl_inst| {
            let impl_def = self.impls.get_impl_def(impl_inst.impl_).clone();
            let subst = GenVarSubst::new(&impl_def.gen_params, self.tys.get_ty_slice(impl_inst.gen_args)).unwrap();

            let inst_impl_trait_inst = self.subst_trait_inst(impl_def.trait_inst.unwrap(), &subst);

            self.tys.slices_eq(inst_impl_trait_inst.gen_args, trait_inst.gen_args)
        })
    }

    pub fn ty_implements_trait_inst(
        &mut self,
        constraints: &[ty::Constraint<'ctxt>],
        ty: ty::Ty<'ctxt>,
        trait_inst: traits::TraitInst<'ctxt>,
    ) -> bool {
        if self
            .tys
            .implements_trait_inst_constraint_exists(constraints, ty, trait_inst)
        {
            return true;
        }

        if let &ty::TyDef::TraitSelf(trait_2) = ty.0
            && trait_2 == trait_inst.trait_
        {
            return true;
        }
        if let &ty::TyDef::Opaque { id, .. } = ty.0
            && self.tys.opaque_satisfies_trait_inst(id, trait_inst)
        {
            return true;
        }

        self.get_impl_insts_for_ty_and_trait_inst(constraints, ty, trait_inst)
            .next()
            .is_some()
    }

    pub fn ty_implements_trait(
        &mut self,
        constraints: &[ty::Constraint<'ctxt>],
        ty: ty::Ty<'ctxt>,
        trait_: traits::Trait,
    ) -> bool {
        if self.tys.implements_trait_constraint_exists(constraints, ty, trait_) {
            return true;
        }

        if let &ty::TyDef::TraitSelf(trait_2) = ty.0
            && trait_2 == trait_
        {
            return true;
        }

        !self.get_impl_insts_for_ty_and_trait(constraints, ty, trait_).is_empty()
    }

    pub fn ty_is_callable(
        &mut self,
        constraints: &[ty::Constraint<'ctxt>],
        ty: ty::Ty<'ctxt>,
    ) -> Option<(ty::TySlice<'ctxt>, ty::Ty<'ctxt>, bool)> {
        if let &ty::TyDef::Fn {
            param_tys,
            return_ty,
            var_args,
        } = ty.0
        {
            Some((param_tys, return_ty, var_args))
        } else if let Some((param_tys, return_ty)) = self.tys.try_get_callable_constraint(constraints, ty) {
            Some((param_tys, return_ty, false))
        } else if let &ty::TyDef::Closure { fn_inst, .. } = ty.0 {
            let signature = self.get_fn_inst_sig(fn_inst);
            let params_without_captures: Vec<_> = signature.params.iter().skip(1).map(|p| p.ty).collect();
            let return_ty = signature.return_ty;
            drop(signature);
            let params_without_captures = self.tys.ty_slice(&params_without_captures);

            Some((params_without_captures, return_ty, false))
        } else if let &ty::TyDef::Opaque { id, gen_args } = ty.0 {
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
        constraints: &[ty::Constraint<'ctxt>],
        base_ty: ty::Ty<'ctxt>,
        ident: &str,
    ) -> Option<ty::Ty<'ctxt>> {
        if let &ty::TyDef::TraitSelf(trait_) = base_ty.0 {
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
                let impl_insts = self.get_impl_insts_for_ty_and_trait(constraints, base_ty, *trait_);

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

    pub fn impl_constraints_satisfied(
        &mut self,
        ambient_constraints: &[ty::Constraint<'ctxt>],
        impl_constraints: &[ty::Constraint<'ctxt>],
        subst: &ty::GenVarSubst<'ctxt>,
    ) -> bool {
        let subst_constraints: Vec<_> = impl_constraints
            .iter()
            .map(|c| self.subst_constraint(c, subst))
            .collect();
        subst_constraints
            .iter()
            .all(|c| self.constraint_satisfied(ambient_constraints, c))
    }

    fn constraint_satisfied(
        &mut self,
        ambient_constraints: &[ty::Constraint<'ctxt>],
        constraint: &ty::Constraint<'ctxt>,
    ) -> bool {
        let subject = constraint.subject;
        match constraint.requirement {
            ty::ConstraintRequirement::Trait(trait_inst) => {
                self.ty_implements_trait_inst(ambient_constraints, subject, trait_inst)
            }
            ty::ConstraintRequirement::Callable { .. } => self.ty_is_callable(ambient_constraints, subject).is_some(),
            ty::ConstraintRequirement::AssocTyEq(eq_ty) => {
                let subject = self.normalize_ty(subject);
                let eq_ty = self.normalize_ty(eq_ty);
                self.tys.tys_eq(subject, eq_ty)
            }
        }
    }

    fn subst_constraint(
        &mut self,
        constraint: &ty::Constraint<'ctxt>,
        subst: &ty::GenVarSubst<'ctxt>,
    ) -> ty::Constraint<'ctxt> {
        let subject = self.tys.substitute_gen_vars(constraint.subject, subst);
        let requirement = match constraint.requirement {
            ty::ConstraintRequirement::Trait(trait_inst) => {
                ty::ConstraintRequirement::Trait(self.subst_trait_inst(trait_inst, subst))
            }
            ty::ConstraintRequirement::Callable { param_tys, return_ty } => {
                let param_tys = self.tys.substitute_gen_vars_on_slice(param_tys, subst);
                let return_ty = self.tys.substitute_gen_vars(return_ty, subst);
                ty::ConstraintRequirement::Callable { param_tys, return_ty }
            }
            ty::ConstraintRequirement::AssocTyEq(eq_ty) => {
                ty::ConstraintRequirement::AssocTyEq(self.tys.substitute_gen_vars(eq_ty, subst))
            }
        };
        ty::Constraint { subject, requirement }
    }

    fn get_impl_insts_for_ty_and_trait(
        &mut self,
        constraints: &[ty::Constraint<'ctxt>],
        ty: ty::Ty<'ctxt>,
        trait_: traits::Trait,
    ) -> Vec<impls::ImplInst<'ctxt>> {
        let candidate_impls: Vec<_> = self.impls.get_impls_for_trait(trait_).collect();
        candidate_impls
            .into_iter()
            .filter_map(|impl_| {
                let impl_def = self.impls.get_impl_def(impl_).clone();

                let gen_args = self
                    .tys
                    .try_find_instantiation(ty, impl_def.ty, &impl_def.gen_params)
                    .ok()?;

                let gen_args_vec = self.tys.get_ty_slice(gen_args).to_vec();
                let subst = ty::GenVarSubst::new(&impl_def.gen_params, &gen_args_vec).unwrap();

                if !self.impl_constraints_satisfied(constraints, &impl_def.constraints, &subst) {
                    return None;
                }

                let impl_inst = self.impls.inst_impl(impl_, gen_args).unwrap();
                Some(impl_inst)
            })
            .collect()
    }

    fn subst_trait_inst(
        &mut self,
        trait_inst: traits::TraitInst<'ctxt>,
        subst: &GenVarSubst<'ctxt>,
    ) -> traits::TraitInst<'ctxt> {
        let gen_args = self.tys.substitute_gen_vars_on_slice(trait_inst.gen_args, subst);
        self.traits.inst_trait(trait_inst.trait_, gen_args).unwrap()
    }

    fn subst_trait_mthd_inst(
        &mut self,
        trait_mthd_inst: fns::TraitMthdInst<'ctxt>,
        subst: &GenVarSubst<'ctxt>,
    ) -> fns::TraitMthdInst<'ctxt> {
        let impl_ty = self.tys.substitute_gen_vars(trait_mthd_inst.impl_ty, subst);
        let trait_inst = self.subst_trait_inst(trait_mthd_inst.trait_inst, subst);
        let gen_args = self.tys.substitute_gen_vars_on_slice(trait_mthd_inst.gen_args, subst);
        trait_mthd_inst.with_updated(impl_ty, trait_inst, gen_args).unwrap()
    }
}
