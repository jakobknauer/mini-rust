use std::collections::HashSet;

use crate::ctxt::{self, impls::Impl, traits::Trait};

pub struct TraitCheckError {
    pub impl_: Impl,
    pub trait_: Trait,
    pub kind: TraitCheckErrorKind,
}

pub enum TraitCheckErrorKind {
    MissingMethods(Vec<String>),
    ExtraMethods(Vec<String>),
    ArgCountMismatch {
        method: String,
        expected: usize,
        actual: usize,
    },
    ArgTypeMismatch {
        method: String,
        expected: ctxt::ty::Ty,
        actual: ctxt::ty::Ty,
        arg_idx: usize,
    },
    ReturnTypeMismatch {
        method: String,
        expected: ctxt::ty::Ty,
        actual: ctxt::ty::Ty,
    },
}

pub fn check_trait_impls(ctxt: &mut ctxt::Ctxt) -> Result<(), TraitCheckError> {
    let all_impls: Vec<_> = ctxt.impls.get_all_impls().collect();
    for impl_ in all_impls {
        let Some(trait_) = ctxt.impls.get_impl_trait(impl_) else {
            continue;
        };
        check_trait_impl(ctxt, impl_, trait_)?;
    }

    Ok(())
}

fn check_trait_impl(ctxt: &mut ctxt::Ctxt, impl_: Impl, trait_: Trait) -> Result<(), TraitCheckError> {
    check_method_names(ctxt, impl_, trait_).map_err(|kind| TraitCheckError { impl_, trait_, kind })?;

    let impl_def = ctxt.impls.get_impl_def(impl_);
    let trait_def = ctxt.traits.get_trait_def(trait_);

    for &method in &impl_def.methods {
        let impl_method_sig = ctxt.fns.get_sig(method).unwrap();

        let method_name = ctxt.fns.get_fn_name(method);
        let trait_method_sig = trait_def.methods.iter().find(|m| m.name == method_name).unwrap();

        check_method_sigs(&mut ctxt.tys, impl_method_sig, trait_method_sig, impl_def.ty)
            .map_err(|kind| TraitCheckError { impl_, trait_, kind })?;
    }

    Ok(())
}

fn check_method_names(ctxt: &mut ctxt::Ctxt, impl_: Impl, trait_: Trait) -> Result<(), TraitCheckErrorKind> {
    let impl_def = ctxt.impls.get_impl_def(impl_);
    let trait_def = ctxt.traits.get_trait_def(trait_);

    let trait_method_names: HashSet<&str> = trait_def.methods.iter().map(|method| method.name.as_str()).collect();
    let impl_method_names: HashSet<&str> = impl_def
        .methods
        .iter()
        .map(|&method| ctxt.fns.get_fn_name(method))
        .collect();

    let missing_methods: Vec<&str> = trait_method_names.difference(&impl_method_names).cloned().collect();
    if !missing_methods.is_empty() {
        return Err(TraitCheckErrorKind::MissingMethods(
            missing_methods.iter().map(|s| s.to_string()).collect(),
        ));
    }

    let extra_methods: Vec<&str> = impl_method_names.difference(&trait_method_names).cloned().collect();
    if !extra_methods.is_empty() {
        return Err(TraitCheckErrorKind::ExtraMethods(
            extra_methods.iter().map(|s| s.to_string()).collect(),
        ));
    }

    Ok(())
}

fn check_method_sigs(
    tys: &mut ctxt::TyReg,
    impl_method_sig: &ctxt::fns::FnSig,
    trait_method_sig: &ctxt::fns::FnSig,
    impl_ty: ctxt::ty::Ty,
) -> Result<(), TraitCheckErrorKind> {
    // TODO: consider generic vars also
    // TODO: other fields of signature, e.g. receiver, var_args etc.

    // Compare param count
    if impl_method_sig.params.len() != trait_method_sig.params.len() {
        return Err(TraitCheckErrorKind::ArgCountMismatch {
            method: impl_method_sig.name.to_string(),
            expected: trait_method_sig.params.len(),
            actual: impl_method_sig.params.len(),
        });
    }

    // Compare params
    for (idx, (expected, actual)) in trait_method_sig
        .params
        .iter()
        .zip(impl_method_sig.params.iter())
        .enumerate()
    {
        let expected_with_self_substituted = tys.substitute_self_ty(expected.ty, impl_ty);
        if !tys.tys_eq(expected_with_self_substituted, actual.ty) {
            return Err(TraitCheckErrorKind::ArgTypeMismatch {
                method: impl_method_sig.name.to_string(),
                arg_idx: idx,
                expected: expected_with_self_substituted,
                actual: actual.ty,
            });
        }
    }

    // Compare return type
    let return_type_with_self_substituted = tys.substitute_self_ty(trait_method_sig.return_ty, impl_ty);
    if !tys.tys_eq(return_type_with_self_substituted, impl_method_sig.return_ty) {
        return Err(TraitCheckErrorKind::ReturnTypeMismatch {
            method: impl_method_sig.name.to_string(),
            expected: return_type_with_self_substituted,
            actual: impl_method_sig.return_ty,
        });
    }

    Ok(())
}
