pub mod fns;
pub mod impls;
pub mod mlr;
pub mod traits;
pub mod ty;

mod fn_reg;
mod impl_reg;
mod trait_reg;
mod ty_reg;

use std::collections::HashMap;

pub use fn_reg::FnReg;
pub use impl_reg::ImplReg;
pub use trait_reg::TraitReg;
pub use ty_reg::*;

use mlr::Mlr;

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
        let signature = self.fns.get_sig(fn_spec.fn_).unwrap();

        let assoc_ty = if let Some(assoc_ty) = signature.associated_ty {
            let assoc_ty_name = self.tys.get_string_rep(assoc_ty);
            if let Some(assoc_trait) = signature.associated_trait {
                let assoc_trait_name = self.traits.get_trait_name(assoc_trait);
                format!("({} as {})::", assoc_ty_name, assoc_trait_name)
            } else {
                format!("{}::", assoc_ty_name)
            }
        } else {
            "".to_string()
        };

        let env_gen_args = if fn_spec.env_gen_args.is_empty() {
            "".to_string()
        } else {
            format!(
                "{{{}}}",
                fn_spec
                    .env_gen_args
                    .iter()
                    .map(|&ty| self.tys.get_string_rep(ty))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };

        let gen_args = if fn_spec.gen_args.is_empty() {
            "".to_string()
        } else {
            format!(
                "<{}>",
                fn_spec
                    .gen_args
                    .iter()
                    .map(|&ty| self.tys.get_string_rep(ty))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };

        format!("{}{}{}{}", assoc_ty, signature.name, env_gen_args, gen_args)
    }

    pub fn fn_specs_eq(&self, fn_spec1: &fns::FnSpecialization, fn_spec2: &fns::FnSpecialization) -> bool {
        fn_spec1.fn_ == fn_spec2.fn_
            && fn_spec1.gen_args.len() == fn_spec2.gen_args.len()
            && fn_spec1
                .gen_args
                .iter()
                .zip(fn_spec2.gen_args.iter())
                .all(|(a, b)| self.tys.tys_eq(*a, *b))
            && fn_spec1.env_gen_args.len() == fn_spec2.env_gen_args.len()
            && fn_spec1
                .env_gen_args
                .iter()
                .zip(fn_spec2.env_gen_args.iter())
                .all(|(a, b)| self.tys.tys_eq(*a, *b))
    }

    pub fn get_specialized_fn_sig(&mut self, fn_spec: &fns::FnSpecialization) -> fns::FnSig {
        let signature = self.fns.get_sig(fn_spec.fn_).unwrap();
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

    pub fn specialize_trait_method_call(
        &mut self,
        trait_method: &fns::TraitMethod,
        substitutions: &HashMap<ty::GenVar, ty::Ty>,
    ) -> fns::FnSpecialization {
        let &fns::TraitMethod {
            trait_,
            method_idx,
            impl_ty,
            ref gen_args,
        } = trait_method;

        let impl_ty = self.tys.substitute_gen_vars(impl_ty, substitutions);

        let (impl_def, impl_instantiation) = self
            .impls
            .get_impls_for_trait(trait_)
            .map(|impl_| self.impls.get_impl_def(impl_))
            .filter_map(|impl_def| {
                self.tys
                    .try_find_instantiation(impl_ty, impl_def.ty, &impl_def.gen_params)
                    .ok()
                    .map(|inst| (impl_def, inst))
            })
            .next()
            .unwrap();

        let trait_method_name = self.traits.get_trait_method_name(trait_, method_idx);

        let new_gen_args = gen_args
            .iter()
            .map(|&ty| self.tys.substitute_gen_vars(ty, substitutions))
            .collect();

        fns::FnSpecialization {
            fn_: impl_def.methods_by_name[trait_method_name],
            gen_args: new_gen_args,
            env_gen_args: impl_instantiation,
        }
    }

    pub fn ty_implements_trait(&self, ty: ty::Ty, trait_: traits::Trait) -> bool {
        let ty_def = self.tys.get_ty_def(ty);
        if let Some(&ty::TyDef::GenVar(gen_var)) = ty_def
            && self.tys.implements_trait_constraint_exists(gen_var, trait_)
        {
            return true;
        }

        self.impls
            .get_impls_for_trait(trait_)
            .map(|impl_| self.impls.get_impl_def(impl_))
            .filter_map(|impl_def| {
                self.tys
                    .try_find_instantiation(ty, impl_def.ty, &impl_def.gen_params)
                    .ok()
            })
            .next()
            .is_some()
    }

    pub fn ty_is_callable(&mut self, ty: ty::Ty) -> Option<(Vec<ty::Ty>, ty::Ty, bool)> {
        if let Some((param_tys, return_ty)) = self.tys.try_get_callable_obligation(ty) {
            Some((param_tys, return_ty, false))
        } else if let Some(ty::TyDef::Fn {
            param_tys,
            return_ty,
            var_args,
        }) = self.tys.get_ty_def(ty)
        {
            Some((param_tys.clone(), *return_ty, *var_args))
        } else if let Some(ty::TyDef::GenVar(gen_var)) = self.tys.get_ty_def(ty)
            && let Some((param_tys, return_ty)) = self.tys.try_get_callable_constraint(*gen_var)
        {
            Some((param_tys, return_ty, false))
        } else if let Some(ty::TyDef::Closure { fn_spec, .. }) = self.tys.get_ty_def(ty) {
            let fn_spec = fn_spec.clone();
            let signature = self.get_specialized_fn_sig(&fn_spec);
            Some((
                signature.params.iter().skip(1).map(|p| p.ty).collect(),
                signature.return_ty,
                false,
            ))
        } else {
            None
        }
    }
}
