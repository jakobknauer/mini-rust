use crate::ctxt::{
    fns::{self, FnInst},
    impls::{self, ImplInst},
    traits::{self, TraitInst},
    ty::{self, GenVarSubst},
};

impl<'ctxt> super::Ctxt<'ctxt> {
    pub fn resolve_trait_mthd_to_fn(
        &self,
        trait_mthd_inst: fns::TraitMthdInst<'ctxt>,
        subst: &GenVarSubst<'ctxt>,
        self_ty: Option<ty::Ty<'ctxt>>,
    ) -> fns::FnInst<'ctxt> {
        let mut trait_mthd_inst = self.subst_trait_mthd_inst(trait_mthd_inst, subst);
        // Substitute TraitSelf with the concrete self type (for default trait method bodies)
        if let &ty::TyDef::TraitSelf(_) = trait_mthd_inst.impl_ty.0
            && let Some(concrete_self_ty) = self_ty
        {
            trait_mthd_inst = trait_mthd_inst
                .with_updated(concrete_self_ty, trait_mthd_inst.trait_inst, trait_mthd_inst.gen_args)
                .unwrap();
        }
        // Resolve opaque and associated types to their concrete types for monomorphization
        trait_mthd_inst.impl_ty = self.normalize_ty(trait_mthd_inst.impl_ty);
        let trait_gen_args: Vec<_> = trait_mthd_inst
            .trait_inst
            .gen_args
            .iter()
            .map(|&t| self.normalize_ty(t))
            .collect();
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

        let trait_mthd_name = trait_mthd_inst.mthd.fn_.name.as_str();

        let (fn_, env_gen_args) = match impl_inst
            .impl_
            .mthds
            .borrow()
            .iter()
            .find(|&&m| m.name == trait_mthd_name)
            .copied()
        {
            Some(impl_fn) => (impl_fn, impl_inst.gen_args),
            None => {
                // Fall back to the default trait method body
                assert!(
                    trait_mthd_inst.mthd.has_default_body,
                    "missing impl for method without default body"
                );
                (trait_mthd_inst.mthd.fn_, trait_mthd_inst.trait_inst.gen_args)
            }
        };

        FnInst::new(fn_, trait_mthd_inst.gen_args, env_gen_args)
            .unwrap()
            .with_self_ty(Some(trait_mthd_inst.impl_ty))
    }

    pub(crate) fn get_impl_insts_for_ty_and_trait_inst(
        &self,
        constraints: &[ty::Constraint<'ctxt>],
        ty: ty::Ty<'ctxt>,
        trait_inst: traits::TraitInst<'ctxt>,
    ) -> impl Iterator<Item = impls::ImplInst<'ctxt>> {
        self.impls
            .get_impls_for_trait(trait_inst.trait_)
            .filter_map(move |impl_| {
                let impl_trait_inst = impl_.trait_inst.unwrap();

                // Build (target, generic) pairs from both the impl type and the trait gen args,
                // so gen vars that only appear in the trait gen args (not the `for` type) are resolved.
                let pairs: Vec<_> = std::iter::once((ty, impl_.ty))
                    .chain(
                        trait_inst
                            .gen_args
                            .iter()
                            .copied()
                            .zip(impl_trait_inst.gen_args.iter().copied()),
                    )
                    .collect();
                let gen_args = self
                    .tys
                    .try_find_instantiation_from_pairs(&pairs, &impl_.gen_params)
                    .ok()?;

                let subst = GenVarSubst::new(&impl_.gen_params, gen_args).unwrap();
                if !self.impl_constraints_satisfied(constraints, &impl_.constraints, &subst) {
                    return None;
                }

                let inst_impl_trait_inst = self.subst_trait_inst(impl_trait_inst, &subst);
                if inst_impl_trait_inst != trait_inst {
                    return None;
                }

                ImplInst::new(impl_, gen_args).ok()
            })
    }

    pub fn ty_implements_trait_inst(
        &self,
        constraints: &[ty::Constraint<'ctxt>],
        ty: ty::Ty<'ctxt>,
        trait_inst: traits::TraitInst<'ctxt>,
    ) -> bool {
        if implements_trait_inst_constraint_exists(constraints, ty, trait_inst) {
            return true;
        }

        if let &ty::TyDef::TraitSelf(trait_2) = ty.0
            && trait_2 == trait_inst.trait_
        {
            return true;
        }
        if let &ty::TyDef::Opaque { opaque, .. } = ty.0
            && opaque
                .constraints
                .iter()
                .any(|r| matches!(r, ty::ConstraintRequirement::Trait(ti) if ti.trait_ == trait_inst.trait_))
        {
            return true;
        }

        self.get_impl_insts_for_ty_and_trait_inst(constraints, ty, trait_inst)
            .next()
            .is_some()
    }

    pub fn ty_implements_trait(
        &self,
        constraints: &[ty::Constraint<'ctxt>],
        ty: ty::Ty<'ctxt>,
        trait_: traits::Trait<'ctxt>,
    ) -> bool {
        if implements_trait_constraint_exists(constraints, ty, trait_) {
            return true;
        }

        if let &ty::TyDef::TraitSelf(trait_2) = ty.0
            && trait_2 == trait_
        {
            return true;
        }

        self.get_impl_insts_for_ty_and_trait(constraints, ty, trait_)
            .next()
            .is_some()
    }

    pub fn ty_is_callable(
        &self,
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
        } else if let Some((param_tys, return_ty)) = try_get_callable_constraint(constraints, ty) {
            Some((param_tys, return_ty, false))
        } else if let &ty::TyDef::Closure {
            param_tys, return_ty, ..
        } = ty.0
        {
            Some((param_tys, return_ty, false))
        } else if let &ty::TyDef::Opaque { opaque, gen_args } = ty.0 {
            opaque.constraints.iter().find_map(|r| {
                if let &ty::ConstraintRequirement::Callable { param_tys, return_ty } = r {
                    let subst = GenVarSubst::new(&opaque.gen_params, gen_args).unwrap();
                    let param_tys = self.tys.substitute_gen_vars_on_slice(param_tys, &subst);
                    let return_ty = self.tys.substitute_gen_vars(return_ty, &subst);
                    Some((param_tys, return_ty, false))
                } else {
                    None
                }
            })
        } else {
            None
        }
    }

    pub(crate) fn resolve_associated_ty_completely(
        &self,
        constraints: &[ty::Constraint<'ctxt>],
        base_ty: ty::Ty<'ctxt>,
        ident: &str,
    ) -> Option<ty::Ty<'ctxt>> {
        if let &ty::TyDef::TraitSelf(trait_) = base_ty.0 {
            let assoc_ty_index = trait_.assoc_tys.iter().position(|name| name == ident)?;
            let gen_args: Vec<_> = trait_.gen_params.iter().map(|gp| self.tys.gen_var(*gp)).collect();
            let gen_args = self.tys.ty_slice(&gen_args);
            let default_trait_inst = TraitInst::new(trait_, gen_args).unwrap();
            let ty = self.tys.assoc_ty(base_ty, default_trait_inst, assoc_ty_index);
            return Some(ty);
        }

        let candidate_assoc_tys: Vec<_> = self
            .traits
            .get_trait_assoc_ty_with_name(ident)
            .filter(|&(trait_, _)| self.ty_implements_trait(constraints, base_ty, trait_))
            .collect();

        match &candidate_assoc_tys[..] {
            [] => None,
            [(trait_, assoc_ty_idx)] => {
                let impl_insts: Vec<_> = self
                    .get_impl_insts_for_ty_and_trait(constraints, base_ty, trait_)
                    .collect();

                let [impl_inst] = &impl_insts[..] else {
                    if let Some(trait_inst) = get_trait_inst_constraint(constraints, base_ty, trait_) {
                        return Some(self.tys.assoc_ty(base_ty, trait_inst, *assoc_ty_idx));
                    }
                    let gen_args: Vec<_> = trait_.gen_params.iter().map(|gp| self.tys.gen_var(*gp)).collect();
                    let gen_args = self.tys.ty_slice(&gen_args);
                    let trait_inst = TraitInst::new(trait_, gen_args).unwrap();
                    return Some(self.tys.assoc_ty(base_ty, trait_inst, *assoc_ty_idx));
                };

                let assoc_ty = *impl_inst.impl_.assoc_tys.get(assoc_ty_idx).unwrap();

                Some(assoc_ty)
            }
            [_, _, ..] => None,
        }
    }

    pub fn impl_constraints_satisfied(
        &self,
        ambient_constraints: &[ty::Constraint<'ctxt>],
        impl_constraints: &[ty::Constraint<'ctxt>],
        subst: &ty::GenVarSubst<'ctxt>,
    ) -> bool {
        impl_constraints
            .iter()
            .map(|c| self.subst_constraint(c, subst))
            .all(|c| self.constraint_satisfied(ambient_constraints, &c))
    }

    fn constraint_satisfied(
        &self,
        ambient_constraints: &[ty::Constraint<'ctxt>],
        constraint: &ty::Constraint<'ctxt>,
    ) -> bool {
        let subject = constraint.subject;
        match constraint.requirement {
            ty::ConstraintRequirement::Trait(trait_inst) => {
                self.ty_implements_trait_inst(ambient_constraints, subject, trait_inst)
            }
            ty::ConstraintRequirement::Callable { param_tys, return_ty } => {
                let Some((actual_param_tys, actual_return_ty, _)) = self.ty_is_callable(ambient_constraints, subject)
                else {
                    return false;
                };
                actual_param_tys == param_tys && actual_return_ty == return_ty
            }
            ty::ConstraintRequirement::AssocTyEq(eq_ty) => {
                let subject = self.normalize_ty(subject);
                let eq_ty = self.normalize_ty(eq_ty);
                subject == eq_ty
            }
        }
    }

    fn subst_constraint(
        &self,
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
        &self,
        constraints: &[ty::Constraint<'ctxt>],
        ty: ty::Ty<'ctxt>,
        trait_: traits::Trait<'ctxt>,
    ) -> impl Iterator<Item = impls::ImplInst<'ctxt>> {
        self.impls.get_impls_for_trait(trait_).filter_map(move |impl_| {
            let gen_args = self.tys.try_find_instantiation(ty, impl_.ty, &impl_.gen_params).ok()?;

            let subst = ty::GenVarSubst::new(&impl_.gen_params, gen_args).unwrap();

            if !self.impl_constraints_satisfied(constraints, &impl_.constraints, &subst) {
                return None;
            }

            let impl_inst = ImplInst::new(impl_, gen_args).unwrap();
            Some(impl_inst)
        })
    }

    fn subst_trait_inst(
        &self,
        trait_inst: traits::TraitInst<'ctxt>,
        subst: &GenVarSubst<'ctxt>,
    ) -> traits::TraitInst<'ctxt> {
        let gen_args = self.tys.substitute_gen_vars_on_slice(trait_inst.gen_args, subst);
        TraitInst::new(trait_inst.trait_, gen_args).unwrap()
    }

    fn subst_trait_mthd_inst(
        &self,
        trait_mthd_inst: fns::TraitMthdInst<'ctxt>,
        subst: &GenVarSubst<'ctxt>,
    ) -> fns::TraitMthdInst<'ctxt> {
        let impl_ty = self.tys.substitute_gen_vars(trait_mthd_inst.impl_ty, subst);
        let trait_inst = self.subst_trait_inst(trait_mthd_inst.trait_inst, subst);
        let gen_args = self.tys.substitute_gen_vars_on_slice(trait_mthd_inst.gen_args, subst);
        trait_mthd_inst.with_updated(impl_ty, trait_inst, gen_args).unwrap()
    }
}

fn get_trait_inst_constraint<'ty>(
    constraints: &[ty::Constraint<'ty>],
    subject: ty::Ty<'ty>,
    trait_: traits::Trait<'ty>,
) -> Option<TraitInst<'ty>> {
    constraints.iter().find_map(|c| {
        if c.subject != subject {
            return None;
        }
        match c.requirement {
            ty::ConstraintRequirement::Trait(trait_inst) if trait_inst.trait_ == trait_ => Some(trait_inst),
            _ => None,
        }
    })
}

fn implements_trait_constraint_exists<'ty>(
    constraints: &[ty::Constraint<'ty>],
    subject: ty::Ty<'ty>,
    trait_: traits::Trait<'ty>,
) -> bool {
    constraints.iter().any(|c| {
        c.subject == subject
            && matches!(c.requirement,
                ty::ConstraintRequirement::Trait(TraitInst { trait_: the_trait_, .. }) if the_trait_ == trait_)
    })
}

fn implements_trait_inst_constraint_exists<'ty>(
    constraints: &[ty::Constraint<'ty>],
    subject: ty::Ty<'ty>,
    trait_inst: TraitInst<'ty>,
) -> bool {
    constraints.iter().any(|c| {
        c.subject == subject && matches!(&c.requirement, ty::ConstraintRequirement::Trait(ti) if *ti == trait_inst)
    })
}

fn try_get_callable_constraint<'ty>(
    constraints: &[ty::Constraint<'ty>],
    subject: ty::Ty<'ty>,
) -> Option<(ty::TySlice<'ty>, ty::Ty<'ty>)> {
    constraints.iter().find_map(|c| {
        if c.subject == subject {
            if let &ty::ConstraintRequirement::Callable { param_tys, return_ty } = &c.requirement {
                Some((param_tys, return_ty))
            } else {
                None
            }
        } else {
            None
        }
    })
}
