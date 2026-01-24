use std::collections::HashSet;

use crate::ctxt::{
    self, fns,
    impls::Impl,
    traits::{Trait, TraitInst},
    ty,
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
}

pub fn check_trait_impls(ctxt: &mut ctxt::Ctxt) -> Result<(), ImplCheckError> {
    let all_impls: Vec<_> = ctxt.impls.get_all_impls().collect();
    for impl_ in all_impls {
        let Some(trait_inst) = ctxt.impls.get_impl_trait_inst(impl_) else {
            continue;
        };
        check_trait_impl(ctxt, impl_, &trait_inst.clone())?;
    }

    Ok(())
}

fn check_trait_impl(ctxt: &mut ctxt::Ctxt, impl_: Impl, trait_inst: &TraitInst) -> Result<(), ImplCheckError> {
    let trait_def = ctxt.traits.get_trait_def(trait_inst.trait_);
    if trait_def.gen_params.len() != trait_inst.gen_args.len() {
        return Err(ImplCheckError {
            impl_,
            trait_inst: trait_inst.clone(),
            kind: ImplCheckErrorKind::ImplGenParamCountMismatch {
                actual: trait_inst.gen_args.len(),
                expected: trait_def.gen_params.len(),
            },
        });
    }

    let impl_def = ctxt.impls.get_impl_def(impl_).clone();
    for (assoc_ty_idx, assoc_ty_name) in trait_def.assoc_tys.iter().enumerate() {
        if !impl_def.assoc_tys.contains_key(&assoc_ty_idx) {
            return Err(ImplCheckError {
                impl_,
                trait_inst: trait_inst.clone(),
                kind: ImplCheckErrorKind::MissingAssocTy(assoc_ty_name.clone()),
            });
        }
    }

    check_mthd_names(ctxt, impl_, trait_inst.trait_).map_err(|kind| ImplCheckError {
        impl_,
        trait_inst: trait_inst.clone(),
        kind,
    })?;

    let trait_def = ctxt.traits.get_trait_def(trait_inst.trait_).clone();

    // Substitution of the generic params of the trait with the arguments of the impl.
    // E.g. if we have a trait `trait Into<T>` and an impl `impl Into<u32> for Foo`,
    // then we have to substitute `T` with `u32`.
    let trait_gen_params_subst = ty::GenVarSubst::new(&trait_def.gen_params, &trait_inst.gen_args).unwrap();

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
            trait_inst: trait_inst.clone(),
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
        let ty = ctxt.tys.substitute_gen_vars(ty, &all_gen_params_subst);
        let ty = ctxt.tys.substitute_self_ty(ty, impl_ty);
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

        if !ctxt.tys.tys_eq(subst_expected, actual.ty) {
            return Err(ImplCheckErrorKind::ArgTypeMismatch {
                mthd: impl_mthd_sig.name.to_string(),
                arg_idx: idx,
                expected: subst_expected,
                actual: actual.ty,
            });
        }
    }

    // Compare return type
    let subst_return_ty = do_substitutions(ctxt, trait_mthd_sig.return_ty);
    if !ctxt.tys.tys_eq(subst_return_ty, impl_mthd_sig.return_ty) {
        return Err(ImplCheckErrorKind::ReturnTypeMismatch {
            mthd: impl_mthd_sig.name.to_string(),
            expected: subst_return_ty,
            actual: impl_mthd_sig.return_ty,
        });
    }

    Ok(())
}
