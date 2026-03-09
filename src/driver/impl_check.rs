use std::collections::HashSet;

use crate::ctxt::{
    self, fns,
    impls::Impl,
    traits::{Trait, TraitInst},
    ty::{self, zip_ty_slices},
};

pub struct ImplCheckError {
    pub impl_: Impl,
    pub trait_inst: TraitInst,
    pub kind: ImplCheckErrorKind,
}

pub enum ImplCheckErrorKind {
    MissingMthds(Vec<String>),
    ExtraMthds(Vec<String>),
    ParamCountMismatch {
        mthd: String,
        expected: usize,
        actual: usize,
    },
    ArgTypeMismatch {
        mthd: String,
        expected: ty::Ty,
        actual: ty::Ty,
        arg_idx: usize,
    },
    ReturnTypeMismatch {
        mthd: String,
        expected: ty::Ty,
        actual: ty::Ty,
    },
    MthdGenParamCountMismatch {
        mthd: String,
        expected: usize,
        actual: usize,
    },
    ReceiverMismatch {
        mthd: String,
        expected: bool,
        actual: bool,
    },
    ImplGenParamCountMismatch {
        actual: usize,
        expected: usize,
    },
    MissingAssocTy(String),
    ConstraintMismatch {
        mthd: String,
    },
}

pub fn check_trait_impls(ctxt: &mut ctxt::Ctxt) -> Result<(), ImplCheckError> {
    let all_impls: Vec<_> = ctxt.impls.get_all_impls().collect();
    for impl_ in all_impls {
        let Some(trait_inst) = ctxt.impls.get_impl_trait_inst(impl_) else {
            continue;
        };
        check_trait_impl(ctxt, impl_, trait_inst)?;
    }

    Ok(())
}

fn check_trait_impl(ctxt: &mut ctxt::Ctxt, impl_: Impl, trait_inst: TraitInst) -> Result<(), ImplCheckError> {
    let trait_def = ctxt.traits.get_trait_def(trait_inst.trait_);
    if trait_def.gen_params.len() != trait_inst.gen_args.len {
        return Err(ImplCheckError {
            impl_,
            trait_inst,
            kind: ImplCheckErrorKind::ImplGenParamCountMismatch {
                actual: trait_inst.gen_args.len,
                expected: trait_def.gen_params.len(),
            },
        });
    }

    let impl_def = ctxt.impls.get_impl_def(impl_).clone();
    for (assoc_ty_idx, assoc_ty_name) in trait_def.assoc_tys.iter().enumerate() {
        if !impl_def.assoc_tys.contains_key(&assoc_ty_idx) {
            return Err(ImplCheckError {
                impl_,
                trait_inst,
                kind: ImplCheckErrorKind::MissingAssocTy(assoc_ty_name.clone()),
            });
        }
    }

    check_mthd_names(ctxt, impl_, trait_inst.trait_).map_err(|kind| ImplCheckError {
        impl_,
        trait_inst,
        kind,
    })?;

    let trait_def = ctxt.traits.get_trait_def(trait_inst.trait_).clone();

    // Substitution of the generic params of the trait with the arguments of the impl.
    // E.g. if we have a trait `trait Into<T>` and an impl `impl Into<u32> for Foo`,
    // then we have to substitute `T` with `u32`.
    let trait_gen_params_subst =
        ty::GenVarSubst::new(&trait_def.gen_params, ctxt.tys.get_ty_slice(trait_inst.gen_args)).unwrap();

    for &mthd in &impl_def.mthds {
        let impl_mthd_sig = ctxt.fns.get_sig(mthd).unwrap().clone();

        let mthd_name = ctxt.fns.get_fn_name(mthd);
        let trait_mthd_sig = trait_def.mthds.iter().find(|m| m.name == mthd_name).unwrap();

        check_mthd_sig(
            ctxt,
            &impl_mthd_sig,
            trait_mthd_sig,
            impl_def.ty,
            &trait_gen_params_subst,
        )
        .map_err(|kind| ImplCheckError {
            impl_,
            trait_inst,
            kind,
        })?;
    }

    Ok(())
}

fn check_mthd_names(ctxt: &mut ctxt::Ctxt, impl_: Impl, trait_: Trait) -> Result<(), ImplCheckErrorKind> {
    let impl_def = ctxt.impls.get_impl_def(impl_);
    let trait_def = ctxt.traits.get_trait_def(trait_);

    let trait_mthd_names: HashSet<&str> = trait_def.mthds.iter().map(|mthd| mthd.name.as_str()).collect();
    let impl_mthd_names: HashSet<&str> = impl_def.mthds.iter().map(|&mthd| ctxt.fns.get_fn_name(mthd)).collect();

    let missing_mthds: Vec<&str> = trait_mthd_names.difference(&impl_mthd_names).cloned().collect();
    if !missing_mthds.is_empty() {
        return Err(ImplCheckErrorKind::MissingMthds(
            missing_mthds.iter().map(|s| s.to_string()).collect(),
        ));
    }

    let extra_mthds: Vec<&str> = impl_mthd_names.difference(&trait_mthd_names).cloned().collect();
    if !extra_mthds.is_empty() {
        return Err(ImplCheckErrorKind::ExtraMthds(
            extra_mthds.iter().map(|s| s.to_string()).collect(),
        ));
    }

    Ok(())
}

fn check_mthd_sig(
    ctxt: &mut ctxt::Ctxt,
    impl_mthd_sig: &fns::FnSig,
    trait_mthd_sig: &fns::FnSig,
    impl_ty: ty::Ty,
    trait_gen_params_subst: &ty::GenVarSubst,
) -> Result<(), ImplCheckErrorKind> {
    // Compare mthd gen params
    if impl_mthd_sig.gen_params.len() != trait_mthd_sig.gen_params.len() {
        return Err(ImplCheckErrorKind::MthdGenParamCountMismatch {
            mthd: impl_mthd_sig.name.to_string(),
            expected: trait_mthd_sig.gen_params.len(),
            actual: impl_mthd_sig.gen_params.len(),
        });
    }

    // Substitution of the generic params of the trait method with the generic params of the impl.
    // This is necessary because the generic params of the impl are different from those of the
    // trait (even if they have the same name).
    let impl_gen_vars = impl_mthd_sig.gen_params.iter().map(|&gp| ctxt.tys.gen_var(gp));
    let mthd_gen_params_subst = ty::GenVarSubst::new(&trait_mthd_sig.gen_params, impl_gen_vars).unwrap();

    // Concat with the substitutions for the concrete trait instantiation of the impl
    let all_gen_params_subst = ty::GenVarSubst::compose(trait_gen_params_subst.clone(), mthd_gen_params_subst);

    // Compare receiver
    if impl_mthd_sig.has_receiver() != trait_mthd_sig.has_receiver() {
        return Err(ImplCheckErrorKind::ReceiverMismatch {
            mthd: impl_mthd_sig.name.to_string(),
            expected: trait_mthd_sig.has_receiver(),
            actual: impl_mthd_sig.has_receiver(),
        });
    }

    // Compare param count
    if impl_mthd_sig.params.len() != trait_mthd_sig.params.len() {
        return Err(ImplCheckErrorKind::ParamCountMismatch {
            mthd: impl_mthd_sig.name.to_string(),
            expected: trait_mthd_sig.params.len(),
            actual: impl_mthd_sig.params.len(),
        });
    }

    // Little helper closure to avoid code duplication
    let do_substitutions = |ctxt: &mut ctxt::Ctxt, ty: ty::Ty| {
        let ty = ctxt.tys.substitute(ty, &all_gen_params_subst, Some(impl_ty));
        ctxt.normalize_ty(ty)
    };

    // Compare params
    for (idx, (expected, actual)) in trait_mthd_sig
        .params
        .iter()
        .zip(impl_mthd_sig.params.iter())
        .enumerate()
    {
        let subst_expected = do_substitutions(ctxt, expected.ty);

        let actual_ty = ctxt.normalize_ty(actual.ty);
        if !ctxt.tys.tys_eq(subst_expected, actual_ty) {
            return Err(ImplCheckErrorKind::ArgTypeMismatch {
                mthd: impl_mthd_sig.name.to_string(),
                arg_idx: idx,
                expected: subst_expected,
                actual: actual_ty,
            });
        }
    }

    // Compare return type
    let subst_return_ty = do_substitutions(ctxt, trait_mthd_sig.return_ty);
    let actual_return_ty = ctxt.normalize_ty(impl_mthd_sig.return_ty);
    if !ctxt.tys.tys_eq(subst_return_ty, actual_return_ty) {
        return Err(ImplCheckErrorKind::ReturnTypeMismatch {
            mthd: impl_mthd_sig.name.to_string(),
            expected: subst_return_ty,
            actual: actual_return_ty,
        });
    }

    // Compare constraints
    let subst_trait_constraints: Vec<_> = trait_mthd_sig
        .constraints
        .iter()
        .map(|c| subst_constraint(ctxt, c, &all_gen_params_subst, impl_ty))
        .collect();
    let impl_constraints = impl_mthd_sig.constraints.clone();

    let constraints_equal = constraints_subset(ctxt, &subst_trait_constraints, &impl_constraints)
        && constraints_subset(ctxt, &impl_constraints, &subst_trait_constraints);

    if !constraints_equal {
        return Err(ImplCheckErrorKind::ConstraintMismatch {
            mthd: impl_mthd_sig.name.to_string(),
        });
    }

    Ok(())
}

fn constraint_req_eq(ctxt: &mut ctxt::Ctxt, a: &ty::ConstraintRequirement, b: &ty::ConstraintRequirement) -> bool {
    match (a, b) {
        (ty::ConstraintRequirement::Trait(ta), ty::ConstraintRequirement::Trait(tb)) => {
            ta.trait_ == tb.trait_
                && ta.gen_args.len == tb.gen_args.len
                && zip_ty_slices!(
                    ctxt.tys,
                    (ta.gen_args, tb.gen_args),
                    all(|t1, t2| ctxt.tys.tys_eq(t1, t2))
                )
        }
        (
            ty::ConstraintRequirement::Callable {
                param_tys: pa,
                return_ty: ra,
            },
            ty::ConstraintRequirement::Callable {
                param_tys: pb,
                return_ty: rb,
            },
        ) => {
            ctxt.tys.tys_eq(*ra, *rb)
                && pa.len == pb.len
                && zip_ty_slices!(ctxt.tys, (*pa, *pb), all(|t1, t2| ctxt.tys.tys_eq(t1, t2)))
        }
        _ => false,
    }
}

fn constraints_subset(ctxt: &mut ctxt::Ctxt, a: &[ty::Constraint], b: &[ty::Constraint]) -> bool {
    a.iter().all(|ca| {
        b.iter().any(|cb| {
            ctxt.tys.tys_eq(ca.subject, cb.subject) && constraint_req_eq(ctxt, &ca.requirement, &cb.requirement)
        })
    })
}

fn subst_constraint(
    ctxt: &mut ctxt::Ctxt,
    c: &ty::Constraint,
    subst: &ty::GenVarSubst,
    self_ty: ty::Ty,
) -> ty::Constraint {
    let subject = subst_normalize_ty(ctxt, c.subject, subst, self_ty);
    let requirement = match c.requirement {
        ty::ConstraintRequirement::Trait(trait_inst) => {
            let gen_args = ctxt.tys.get_ty_slice(trait_inst.gen_args).to_vec();
            let gen_args: Vec<_> = gen_args
                .into_iter()
                .map(|t| subst_normalize_ty(ctxt, t, subst, self_ty))
                .collect();
            let gen_args = ctxt.tys.ty_slice(&gen_args);
            ty::ConstraintRequirement::Trait(trait_inst.with_gen_args(gen_args).unwrap())
        }
        ty::ConstraintRequirement::Callable { param_tys, return_ty } => {
            let params = ctxt.tys.get_ty_slice(param_tys).to_vec();
            let param_tys: Vec<_> = params
                .into_iter()
                .map(|t| subst_normalize_ty(ctxt, t, subst, self_ty))
                .collect();
            let param_tys = ctxt.tys.ty_slice(&param_tys);
            let return_ty = subst_normalize_ty(ctxt, return_ty, subst, self_ty);
            ty::ConstraintRequirement::Callable { param_tys, return_ty }
        }
    };
    ty::Constraint { subject, requirement }
}

fn subst_normalize_ty(ctxt: &mut ctxt::Ctxt, ty: ty::Ty, subst: &ty::GenVarSubst, self_ty: ty::Ty) -> ty::Ty {
    let ty = ctxt.tys.substitute(ty, subst, Some(self_ty));
    ctxt.normalize_ty(ty)
}
