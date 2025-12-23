pub mod fns;
pub mod impls;
pub mod mlr;
pub mod traits;
pub mod ty;

mod fn_reg;
mod impl_reg;
mod trait_reg;
mod ty_reg;

pub use fn_reg::FnReg;
pub use impl_reg::ImplReg;
pub use trait_reg::TraitReg;
pub use ty_reg::*;

use mlr::Mlr;

use crate::ctxt::traits::Trait;

#[derive(Default)]
pub struct Ctxt {
    pub tys: TyReg,
    pub fns: FnReg,
    pub mlr: Mlr,
    pub impls: ImplReg,
    pub traits: TraitReg,
}

impl Ctxt {
    pub fn get_fn_spec_name(&self, fn_spec: &fns::FnSpecialization) -> String {
        let signature = self.fns.get_sig(&fn_spec.fn_).unwrap();

        let prefix = if let Some(assoc_ty) = signature.associated_ty {
            let substitutions = signature
                .env_gen_params
                .iter()
                .cloned()
                .zip(fn_spec.env_gen_args.iter().cloned())
                .collect();
            if let Some(assoc_trait) = signature.associated_trait {
                let trait_name = self.traits.get_trait_name(assoc_trait);
                format!(
                    "({} as {})::",
                    self.tys.get_string_rep_with_subst(assoc_ty, &substitutions),
                    trait_name
                )
            } else {
                format!("{}::", self.tys.get_string_rep_with_subst(assoc_ty, &substitutions))
            }
        } else {
            "".to_string()
        };

        let postfix = if signature.gen_params.is_empty() {
            signature.name.to_string()
        } else {
            format!(
                "{}<{}>",
                signature.name,
                fn_spec
                    .gen_args
                    .iter()
                    .map(|&ty| self.tys.get_string_rep(ty))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };

        format!("{}{}", prefix, postfix)
    }

    pub fn get_specialized_fn_sig(&mut self, fn_spec: &fns::FnSpecialization) -> fns::FnSig {
        let signature = self.fns.get_sig(&fn_spec.fn_).unwrap();
        let substitutions = self.fns.get_substitutions_for_specialization(fn_spec);

        let specialized_params = signature
            .params
            .iter()
            .map(|param| fns::FnParam {
                name: param.name.clone(),
                ty: self.tys.substitute_gen_vars(param.ty, &substitutions),
            })
            .collect();

        let specialized_return_ty = self.tys.substitute_gen_vars(signature.return_ty, &substitutions);

        fns::FnSig {
            name: signature.name.clone(),
            associated_ty: signature.associated_ty,
            associated_trait: signature.associated_trait,
            gen_params: Vec::new(),
            env_gen_params: Vec::new(),
            params: specialized_params,
            var_args: signature.var_args,
            return_ty: specialized_return_ty,
            has_receiver: signature.has_receiver,
        }
    }

    pub fn get_fn_spec_for_trait_call(
        &mut self,
        trait_: Trait,
        method_index: usize,
        receiver_ty: ty::Ty,
    ) -> fns::FnSpecialization {
        // Find impl for trait trait_method.trait_ and concrete type concrete_impl_ty
        let (impl_, impl_instantiation) = self
            .impls
            .get_all_impls()
            .into_iter()
            .filter_map(|impl_| {
                let impl_def = self.impls.get_impl_def(impl_).unwrap();
                if impl_def.trait_ != Some(trait_) {
                    return None;
                }
                self.tys
                    .try_find_instantiation(receiver_ty, impl_def.ty, &impl_def.gen_params)
                    .ok()
                    .map(|inst| (impl_, inst))
            })
            .next()
            .unwrap();

        let trait_method_name = &self.traits.get_trait_def(trait_).methods[method_index].name;

        let impl_def = self.impls.get_impl_def(impl_).unwrap();

        fns::FnSpecialization {
            fn_: impl_def.methods_by_name[trait_method_name],
            gen_args: vec![],
            env_gen_args: impl_instantiation,
        }
    }
}
