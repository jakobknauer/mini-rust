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
}

pub fn check_trait_impls(ctxt: &ctxt::Ctxt) -> Result<(), TraitCheckError> {
    for (impl_, impl_def) in ctxt.impls.get_all_impls() {
        // Get the impl and trait to compare
        let Some(trait_) = impl_def.trait_ else { continue };
        let trait_def = ctxt.traits.get_trait_def(trait_);

        // check for missing and extra methods
        let trait_method_names: HashSet<&str> = trait_def.methods.iter().map(|method| method.name.as_str()).collect();
        let impl_method_names: HashSet<&str> = impl_def
            .methods
            .iter()
            .map(|&method| ctxt.fns.get_fn_name(method))
            .collect();

        let missing_methods: Vec<&str> = trait_method_names.difference(&impl_method_names).cloned().collect();
        if !missing_methods.is_empty() {
            return Err(TraitCheckError {
                impl_,
                trait_,
                kind: TraitCheckErrorKind::MissingMethods(missing_methods.iter().map(|s| s.to_string()).collect()),
            });
        }

        let extra_methods: Vec<&str> = impl_method_names.difference(&trait_method_names).cloned().collect();
        if !extra_methods.is_empty() {
            return Err(TraitCheckError {
                impl_,
                trait_,
                kind: TraitCheckErrorKind::ExtraMethods(extra_methods.iter().map(|s| s.to_string()).collect()),
            });
        }

        // Now compare the method signatures
        // TODO
    }

    Ok(())
}
