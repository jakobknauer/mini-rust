use std::collections::{HashMap, HashSet};

use crate::ctxt::{
    self,
    impls::Impl,
    traits::{Trait, TraitInst},
};

pub struct ImplCheckError {
    pub impl_: Impl,
    pub trait_inst: TraitInst,
    pub kind: ImplCheckErrorKind,
}

pub enum ImplCheckErrorKind {
    MissingMethods(Vec<String>),
    ExtraMethods(Vec<String>),
    ParamCountMismatch {
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
    MthdGenParamCountMismatch {
        method: String,
        expected: usize,
        actual: usize,
    },
    ReceiverMismatch {
        method: String,
        expected: bool,
        actual: bool,
    },
    ImplGenParamCountMismatch {
        actual: usize,
        expected: usize,
    },
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

    check_method_names(ctxt, impl_, trait_inst.trait_).map_err(|kind| ImplCheckError {
        impl_,
        trait_inst: trait_inst.clone(),
        kind,
    })?;

    let trait_def = ctxt.traits.get_trait_def(trait_inst.trait_);
    let impl_def = ctxt.impls.get_impl_def(impl_);

    let trait_gen_params_subst: HashMap<_, _> = trait_def
        .gen_params
        .iter()
        .cloned()
        .zip(trait_inst.gen_args.iter().cloned())
        .collect();

    for &method in &impl_def.methods {
        let impl_method_sig = ctxt.fns.get_sig(method).unwrap();

        let method_name = ctxt.fns.get_fn_name(method);
        let trait_method_sig = trait_def.methods.iter().find(|m| m.name == method_name).unwrap();

        check_method_sig(
            &mut ctxt.tys,
            impl_method_sig,
            trait_method_sig,
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

fn check_method_names(ctxt: &mut ctxt::Ctxt, impl_: Impl, trait_: Trait) -> Result<(), ImplCheckErrorKind> {
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
        return Err(ImplCheckErrorKind::MissingMethods(
            missing_methods.iter().map(|s| s.to_string()).collect(),
        ));
    }

    let extra_methods: Vec<&str> = impl_method_names.difference(&trait_method_names).cloned().collect();
    if !extra_methods.is_empty() {
        return Err(ImplCheckErrorKind::ExtraMethods(
            extra_methods.iter().map(|s| s.to_string()).collect(),
        ));
    }

    Ok(())
}

fn check_method_sig(
    tys: &mut ctxt::TyReg,
    impl_method_sig: &ctxt::fns::FnSig,
    trait_method_sig: &ctxt::fns::FnSig,
    impl_ty: ctxt::ty::Ty,
    trait_gen_params_subst: &HashMap<ctxt::ty::GenVar, ctxt::ty::Ty>,
) -> Result<(), ImplCheckErrorKind> {
    // Compare method gen params
    if impl_method_sig.gen_params.len() != trait_method_sig.gen_params.len() {
        return Err(ImplCheckErrorKind::MthdGenParamCountMismatch {
            method: impl_method_sig.name.to_string(),
            expected: trait_method_sig.gen_params.len(),
            actual: impl_method_sig.gen_params.len(),
        });
    }
    // Create substitution of method generic params
    let mthd_gen_params_subst = trait_method_sig
        .gen_params
        .iter()
        .cloned()
        .zip(impl_method_sig.gen_params.iter().map(|&gp| tys.register_gen_var_ty(gp)));
    let all_gen_params_subst: HashMap<_, _> = trait_gen_params_subst
        .iter()
        .map(|(&gv, &ty)| (gv, ty))
        .chain(mthd_gen_params_subst)
        .collect();

    // Compare receiver
    if impl_method_sig.has_receiver() != trait_method_sig.has_receiver() {
        return Err(ImplCheckErrorKind::ReceiverMismatch {
            method: impl_method_sig.name.to_string(),
            expected: trait_method_sig.has_receiver(),
            actual: impl_method_sig.has_receiver(),
        });
    }

    // Compare param count
    if impl_method_sig.params.len() != trait_method_sig.params.len() {
        return Err(ImplCheckErrorKind::ParamCountMismatch {
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
        let expected_with_gen_params_substituted =
            tys.substitute_gen_vars(expected_with_self_substituted, &all_gen_params_subst);

        if !tys.tys_eq(expected_with_gen_params_substituted, actual.ty) {
            return Err(ImplCheckErrorKind::ArgTypeMismatch {
                method: impl_method_sig.name.to_string(),
                arg_idx: idx,
                expected: expected_with_gen_params_substituted,
                actual: actual.ty,
            });
        }
    }

    // Compare return type
    let return_type_with_self_substituted = tys.substitute_self_ty(trait_method_sig.return_ty, impl_ty);
    let return_type_with_gen_params_substituted =
        tys.substitute_gen_vars(return_type_with_self_substituted, &all_gen_params_subst);
    if !tys.tys_eq(return_type_with_gen_params_substituted, impl_method_sig.return_ty) {
        return Err(ImplCheckErrorKind::ReturnTypeMismatch {
            method: impl_method_sig.name.to_string(),
            expected: return_type_with_gen_params_substituted,
            actual: impl_method_sig.return_ty,
        });
    }

    Ok(())
}
