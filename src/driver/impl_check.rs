use std::collections::HashSet;

use crate::ctxt::{
    self, fns,
    impls::Impl,
    traits::{Trait, TraitInst},
    ty,
};

pub struct ImplCheckError<'ctxt> {
    pub impl_: Impl<'ctxt>,
    pub trait_inst: TraitInst<'ctxt>,
    pub kind: ImplCheckErrorKind<'ctxt>,
}

pub enum ImplCheckErrorKind<'ctxt> {
    MissingMthds(Vec<String>),
    ExtraMthds(Vec<String>),
    ParamCountMismatch {
        mthd: String,
        expected: usize,
        actual: usize,
    },
    ArgTypeMismatch {
        mthd: String,
        expected: ty::Ty<'ctxt>,
        actual: ty::Ty<'ctxt>,
        arg_idx: usize,
    },
    ReturnTypeMismatch {
        mthd: String,
        expected: ty::Ty<'ctxt>,
        actual: ty::Ty<'ctxt>,
    },
    MthdGenParamCountMismatch {
        mthd: String,
        expected: usize,
        actual: usize,
    },
    ReceiverMismatch {
        mthd: String,
        expected: Option<fns::FnParamKind>,
        actual: Option<fns::FnParamKind>,
    },
    ImplGenParamCountMismatch {
        actual: usize,
        expected: usize,
    },
    MissingAssocTy(String),
    AssocTyBoundViolation {
        assoc_ty_name: String,
        bound: ty::ConstraintRequirement<'ctxt>,
    },
    ConstraintMismatch {
        mthd: String,
    },
}

pub fn check_trait_impls<'ctxt>(ctxt: &mut ctxt::Ctxt<'ctxt>) -> Result<(), ImplCheckError<'ctxt>> {
    let all_impls: Vec<_> = ctxt.impls.get_all_impls().collect();
    for impl_ in all_impls {
        let Some(trait_inst) = impl_.trait_inst else {
            continue;
        };
        check_trait_impl(ctxt, impl_, trait_inst)?;
    }

    Ok(())
}

fn check_trait_impl<'ctxt>(
    ctxt: &mut ctxt::Ctxt<'ctxt>,
    impl_: Impl<'ctxt>,
    trait_inst: TraitInst<'ctxt>,
) -> Result<(), ImplCheckError<'ctxt>> {
    if trait_inst.trait_.gen_params.len() != trait_inst.gen_args.len() {
        return Err(ImplCheckError {
            impl_,
            trait_inst,
            kind: ImplCheckErrorKind::ImplGenParamCountMismatch {
                actual: trait_inst.gen_args.len(),
                expected: trait_inst.trait_.gen_params.len(),
            },
        });
    }

    let trait_subst = trait_inst.get_subst();

    for (assoc_ty_idx, assoc_ty_def) in trait_inst.trait_.assoc_tys.iter().enumerate() {
        if !impl_.assoc_tys.contains_key(&assoc_ty_idx) {
            return Err(ImplCheckError {
                impl_,
                trait_inst,
                kind: ImplCheckErrorKind::MissingAssocTy(assoc_ty_def.name.clone()),
            });
        }

        for bound in assoc_ty_def.bounds.borrow().iter() {
            let subst_bound = subst_constraint(ctxt, bound, &trait_subst, impl_.ty, &impl_.constraints);
            if !ctxt.constraint_satisfied(&impl_.constraints, &subst_bound) {
                return Err(ImplCheckError {
                    impl_,
                    trait_inst,
                    kind: ImplCheckErrorKind::AssocTyBoundViolation {
                        assoc_ty_name: assoc_ty_def.name.clone(),
                        bound: subst_bound.requirement,
                    },
                });
            }
        }
    }

    check_mthd_names(ctxt, impl_, trait_inst.trait_).map_err(|kind| ImplCheckError {
        impl_,
        trait_inst,
        kind,
    })?;

    for &mthd in impl_.mthds.borrow().iter() {
        let trait_mthd = ctxt.traits.resolve_trait_method(trait_inst.trait_, &mthd.name).unwrap();

        check_mthd_decl(ctxt, mthd, trait_mthd.fn_, impl_.ty, &trait_subst, &impl_.constraints).map_err(|kind| {
            ImplCheckError {
                impl_,
                trait_inst,
                kind,
            }
        })?;
    }

    Ok(())
}

fn check_mthd_names<'ctxt>(
    ctxt: &mut ctxt::Ctxt<'ctxt>,
    impl_: Impl<'ctxt>,
    trait_: Trait<'ctxt>,
) -> Result<(), ImplCheckErrorKind<'ctxt>> {
    let all_trait_mthd_names: HashSet<&str> = ctxt
        .traits
        .get_trait_mthds(trait_)
        .map(|m| m.fn_.name.as_str())
        .collect();
    let required_trait_mthd_names: HashSet<&str> = ctxt
        .traits
        .get_trait_mthds(trait_)
        .filter(|m| !m.has_default_body)
        .map(|m| m.fn_.name.as_str())
        .collect();
    let impl_mthd_names: HashSet<&str> = impl_.mthds.borrow().iter().map(|&mthd| mthd.name.as_str()).collect();

    let missing_mthds: Vec<&str> = required_trait_mthd_names
        .difference(&impl_mthd_names)
        .cloned()
        .collect();
    if !missing_mthds.is_empty() {
        return Err(ImplCheckErrorKind::MissingMthds(
            missing_mthds.iter().map(|s| s.to_string()).collect(),
        ));
    }

    let extra_mthds: Vec<&str> = impl_mthd_names.difference(&all_trait_mthd_names).cloned().collect();
    if !extra_mthds.is_empty() {
        return Err(ImplCheckErrorKind::ExtraMthds(
            extra_mthds.iter().map(|s| s.to_string()).collect(),
        ));
    }

    Ok(())
}

fn check_mthd_decl<'ctxt>(
    ctxt: &mut ctxt::Ctxt<'ctxt>,
    impl_mthd_decl: &fns::FnDecl<'ctxt>,
    trait_mthd_decl: &fns::FnDecl<'ctxt>,
    impl_ty: ty::Ty<'ctxt>,
    trait_gen_params_subst: &ty::GenVarSubst<'ctxt>,
    impl_constraints: &[ty::Constraint<'ctxt>],
) -> Result<(), ImplCheckErrorKind<'ctxt>> {
    // Compare mthd gen params
    if impl_mthd_decl.gen_params.len() != trait_mthd_decl.gen_params.len() {
        return Err(ImplCheckErrorKind::MthdGenParamCountMismatch {
            mthd: impl_mthd_decl.name.to_string(),
            expected: trait_mthd_decl.gen_params.len(),
            actual: impl_mthd_decl.gen_params.len(),
        });
    }

    // Substitution of the generic params of the trait method with the generic params of the impl.
    // This is necessary because the generic params of the impl are different from those of the
    // trait (even if they have the same name).
    let impl_gen_vars = impl_mthd_decl.gen_params.iter().map(|&gp| ctxt.tys.gen_var(gp));
    let mthd_gen_params_subst = ty::GenVarSubst::new(&trait_mthd_decl.gen_params, impl_gen_vars).unwrap();

    // Concat with the substitutions for the concrete trait instantiation of the impl
    let all_gen_params_subst = ty::GenVarSubst::compose(trait_gen_params_subst.clone(), mthd_gen_params_subst);

    // Compare receiver
    let receiver_kind = |decl: &fns::FnDecl| -> Option<fns::FnParamKind> {
        match decl.params.first().map(|p| &p.kind) {
            Some(fns::FnParamKind::Self_) => Some(fns::FnParamKind::Self_),
            Some(fns::FnParamKind::SelfByRef) => Some(fns::FnParamKind::SelfByRef),
            Some(fns::FnParamKind::SelfByRefMut) => Some(fns::FnParamKind::SelfByRefMut),
            _ => None,
        }
    };
    let expected_receiver = receiver_kind(trait_mthd_decl);
    let actual_receiver = receiver_kind(impl_mthd_decl);
    if expected_receiver != actual_receiver {
        return Err(ImplCheckErrorKind::ReceiverMismatch {
            mthd: impl_mthd_decl.name.to_string(),
            expected: expected_receiver,
            actual: actual_receiver,
        });
    }

    // Compare param count
    if impl_mthd_decl.params.len() != trait_mthd_decl.params.len() {
        return Err(ImplCheckErrorKind::ParamCountMismatch {
            mthd: impl_mthd_decl.name.to_string(),
            expected: trait_mthd_decl.params.len(),
            actual: impl_mthd_decl.params.len(),
        });
    }

    // Little helper closure to avoid code duplication
    let do_substitutions = |ctxt: &mut ctxt::Ctxt<'ctxt>, ty: ty::Ty<'ctxt>| -> ty::Ty<'ctxt> {
        let ty = ctxt.tys.substitute(ty, &all_gen_params_subst, Some(impl_ty));
        ctxt.normalize_ty_with_constraints(ty, impl_constraints)
    };

    // Compare params
    for (idx, (expected, actual)) in trait_mthd_decl
        .params
        .iter()
        .zip(impl_mthd_decl.params.iter())
        .enumerate()
    {
        let subst_expected = do_substitutions(ctxt, expected.ty);

        let actual_ty = ctxt.normalize_ty_with_constraints(actual.ty, impl_constraints);
        if subst_expected != actual_ty {
            return Err(ImplCheckErrorKind::ArgTypeMismatch {
                mthd: impl_mthd_decl.name.to_string(),
                arg_idx: idx,
                expected: subst_expected,
                actual: actual_ty,
            });
        }
    }

    // Compare return type
    let subst_return_ty = do_substitutions(ctxt, trait_mthd_decl.return_ty);
    let actual_return_ty = ctxt.normalize_ty_with_constraints(impl_mthd_decl.return_ty, impl_constraints);
    if subst_return_ty != actual_return_ty {
        return Err(ImplCheckErrorKind::ReturnTypeMismatch {
            mthd: impl_mthd_decl.name.to_string(),
            expected: subst_return_ty,
            actual: actual_return_ty,
        });
    }

    // Compare constraints
    let subst_trait_constraints: Vec<_> = trait_mthd_decl
        .constraints
        .iter()
        .map(|c| subst_constraint(ctxt, c, &all_gen_params_subst, impl_ty, impl_constraints))
        .collect();
    #[allow(clippy::mutable_key_type)]
    let trait_set: HashSet<_> = subst_trait_constraints.iter().collect();
    #[allow(clippy::mutable_key_type)]
    let impl_set: HashSet<_> = impl_mthd_decl.constraints.iter().collect();
    if trait_set != impl_set {
        return Err(ImplCheckErrorKind::ConstraintMismatch {
            mthd: impl_mthd_decl.name.to_string(),
        });
    }

    Ok(())
}

fn subst_constraint<'ctxt>(
    ctxt: &mut ctxt::Ctxt<'ctxt>,
    c: &ty::Constraint<'ctxt>,
    subst: &ty::GenVarSubst<'ctxt>,
    self_ty: ty::Ty<'ctxt>,
    ambient: &[ty::Constraint<'ctxt>],
) -> ty::Constraint<'ctxt> {
    let subject = subst_normalize_ty(ctxt, c.subject, subst, self_ty, ambient);
    let requirement = match c.requirement {
        ty::ConstraintRequirement::Trait(trait_inst) => {
            let gen_args: Vec<_> = trait_inst
                .gen_args
                .iter()
                .map(|&t| subst_normalize_ty(ctxt, t, subst, self_ty, ambient))
                .collect();
            let gen_args = ctxt.tys.ty_slice(&gen_args);
            ty::ConstraintRequirement::Trait(trait_inst.with_gen_args(gen_args).unwrap())
        }
        ty::ConstraintRequirement::Callable { param_tys, return_ty } => {
            let param_tys: Vec<_> = param_tys
                .iter()
                .map(|&t| subst_normalize_ty(ctxt, t, subst, self_ty, ambient))
                .collect();
            let param_tys = ctxt.tys.ty_slice(&param_tys);
            let return_ty = subst_normalize_ty(ctxt, return_ty, subst, self_ty, ambient);
            ty::ConstraintRequirement::Callable { param_tys, return_ty }
        }
        ty::ConstraintRequirement::AssocTyEq(eq_ty) => {
            ty::ConstraintRequirement::AssocTyEq(subst_normalize_ty(ctxt, eq_ty, subst, self_ty, ambient))
        }
    };
    ty::Constraint { subject, requirement }
}

fn subst_normalize_ty<'ctxt>(
    ctxt: &mut ctxt::Ctxt<'ctxt>,
    ty: ty::Ty<'ctxt>,
    subst: &ty::GenVarSubst<'ctxt>,
    self_ty: ty::Ty<'ctxt>,
    ambient: &[ty::Constraint<'ctxt>],
) -> ty::Ty<'ctxt> {
    let ty = ctxt.tys.substitute(ty, subst, Some(self_ty));
    ctxt.normalize_ty_with_constraints(ty, ambient)
}
