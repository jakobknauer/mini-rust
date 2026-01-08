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

use crate::ctxt::ty::GenVarSubst;

#[derive(Default)]
pub struct Ctxt {
    pub tys: TyReg,
    pub fns: FnReg,
    pub mlr: Mlr,
    pub impls: ImplReg,
    pub traits: TraitReg,
}

impl Ctxt {
    pub fn get_fn_inst_name(&self, fn_inst: &fns::FnInst) -> String {
        let signature = self.fns.get_sig(fn_inst.fn_).unwrap();

        let assoc_ty = if let Some(assoc_ty) = signature.associated_ty {
            let assoc_ty_name = self.tys.get_string_rep(assoc_ty);
            if let Some(assoc_trait_inst) = &signature.associated_trait_inst {
                let assoc_trait_name = self.traits.get_trait_name(assoc_trait_inst.trait_);
                let assoc_trait_gen_params = if assoc_trait_inst.gen_args.is_empty() {
                    "".to_string()
                } else {
                    format!(
                        "<{}>",
                        assoc_trait_inst
                            .gen_args
                            .iter()
                            .map(|&ty| self.tys.get_string_rep(ty))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                };

                format!(
                    "<{} as {}{}>::",
                    assoc_ty_name, assoc_trait_name, assoc_trait_gen_params
                )
            } else {
                format!("{}::", assoc_ty_name)
            }
        } else {
            "".to_string()
        };

        let env_gen_args = if fn_inst.env_gen_args.is_empty() {
            "".to_string()
        } else {
            format!(
                "{{{}}}",
                fn_inst
                    .env_gen_args
                    .iter()
                    .map(|&ty| self.tys.get_string_rep(ty))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };

        let gen_args = if fn_inst.gen_args.is_empty() {
            "".to_string()
        } else {
            format!(
                "<{}>",
                fn_inst
                    .gen_args
                    .iter()
                    .map(|&ty| self.tys.get_string_rep(ty))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };

        format!("{}{}{}{}", assoc_ty, signature.name, env_gen_args, gen_args)
    }

    pub fn fn_insts_eq(&self, fn_inst1: &fns::FnInst, fn_inst2: &fns::FnInst) -> bool {
        fn_inst1.fn_ == fn_inst2.fn_
            && fn_inst1.gen_args.len() == fn_inst2.gen_args.len()
            && fn_inst1
                .gen_args
                .iter()
                .zip(fn_inst2.gen_args.iter())
                .all(|(a, b)| self.tys.tys_eq(*a, *b))
            && fn_inst1.env_gen_args.len() == fn_inst2.env_gen_args.len()
            && fn_inst1
                .env_gen_args
                .iter()
                .zip(fn_inst2.env_gen_args.iter())
                .all(|(a, b)| self.tys.tys_eq(*a, *b))
    }

    pub fn get_fn_inst_sig(&mut self, fn_inst: &fns::FnInst) -> fns::FnSig {
        let signature = self.fns.get_sig(fn_inst.fn_).unwrap();
        let subst = self.fns.get_subst_for_fn_inst(fn_inst);

        let inst_params = signature
            .params
            .iter()
            .map(|param| fns::FnParam {
                kind: param.kind.clone(),
                ty: self.tys.substitute_gen_vars(param.ty, &subst),
            })
            .collect();

        let inst_return_ty = self.tys.substitute_gen_vars(signature.return_ty, &subst);

        fns::FnSig {
            name: signature.name.clone(),
            // TODO subst associated_ty?
            associated_ty: signature.associated_ty,
            // TODO subst associated_trait_inst?
            associated_trait_inst: signature.associated_trait_inst.clone(),
            gen_params: Vec::new(),
            env_gen_params: Vec::new(),
            params: inst_params,
            var_args: signature.var_args,
            return_ty: inst_return_ty,
        }
    }

    pub fn resolve_trait_method_to_fn(&mut self, trait_method: &fns::TraitMethod, subst: &GenVarSubst) -> fns::FnInst {
        let trait_method = self.subst_trait_method(trait_method, subst);

        let matching_impl_insts: Vec<_> = self
            .get_impl_insts_for_ty_and_trait_inst(trait_method.impl_ty, &trait_method.trait_inst)
            .collect();

        // TODO proper error handling
        assert_eq!(matching_impl_insts.len(), 1);
        let [impl_inst] = matching_impl_insts.try_into().unwrap();

        let trait_method_name = self
            .traits
            .get_trait_method_name(trait_method.trait_inst.trait_, trait_method.method_idx);

        let impl_def = self.impls.get_impl_def(impl_inst.impl_);
        let fn_ = impl_def.methods_by_name[trait_method_name];

        fns::FnInst {
            fn_,
            gen_args: trait_method.gen_args,
            env_gen_args: impl_inst.gen_args,
        }
    }

    fn get_impl_insts_for_ty_and_trait(
        &self,
        ty: ty::Ty,
        trait_: traits::Trait,
    ) -> impl Iterator<Item = impls::ImplInst> + use<'_> {
        self.impls.get_impls_for_trait(trait_).filter_map(move |impl_| {
            let impl_def = self.impls.get_impl_def(impl_).clone();

            let gen_args = self
                .tys
                .try_find_instantiation(ty, impl_def.ty, &impl_def.gen_params)
                .ok()?;

            let impl_inst = impls::ImplInst { impl_, gen_args };
            Some(impl_inst)
        })
    }

    fn get_impl_insts_for_ty_and_trait_inst(
        &mut self,
        ty: ty::Ty,
        trait_inst: &traits::TraitInst,
    ) -> impl Iterator<Item = impls::ImplInst> {
        let impl_insts: Vec<_> = self.get_impl_insts_for_ty_and_trait(ty, trait_inst.trait_).collect();

        impl_insts.into_iter().filter(move |impl_inst| {
            let impl_def = self.impls.get_impl_def(impl_inst.impl_).clone();
            let subst = GenVarSubst::new(&impl_def.gen_params, &impl_inst.gen_args).unwrap();

            let inst_impl_trait_inst = self.subst_trait_inst(impl_def.trait_inst.as_ref().unwrap(), &subst);

            inst_impl_trait_inst
                .gen_args
                .iter()
                .zip(trait_inst.gen_args.iter())
                .all(|(gen_arg1, gen_arg2)| self.tys.tys_eq(*gen_arg1, *gen_arg2))
        })
    }

    fn subst_trait_inst(&mut self, trait_inst: &traits::TraitInst, subst: &GenVarSubst) -> traits::TraitInst {
        let mut trait_inst = trait_inst.clone();
        for gen_arg in &mut trait_inst.gen_args {
            *gen_arg = self.tys.substitute_gen_vars(*gen_arg, subst);
        }
        trait_inst
    }

    fn subst_trait_method(&mut self, trait_method: &fns::TraitMethod, subst: &GenVarSubst) -> fns::TraitMethod {
        let mut trait_method = trait_method.clone();
        trait_method.impl_ty = self.tys.substitute_gen_vars(trait_method.impl_ty, subst);
        trait_method.trait_inst = self.subst_trait_inst(&trait_method.trait_inst, subst);
        for gen_arg in &mut trait_method.gen_args {
            *gen_arg = self.tys.substitute_gen_vars(*gen_arg, subst);
        }
        trait_method
    }

    pub fn ty_implements_trait_inst(&mut self, ty: ty::Ty, trait_inst: &traits::TraitInst) -> bool {
        let ty_def = self.tys.get_ty_def(ty);
        if let Some(&ty::TyDef::GenVar(gen_var)) = ty_def
            && self.tys.implements_trait_inst_constraint_exists(gen_var, trait_inst)
        {
            return true;
        }

        self.get_impl_insts_for_ty_and_trait_inst(ty, trait_inst)
            .next()
            .is_some()
    }

    pub fn ty_implements_trait(&self, ty: ty::Ty, trait_: traits::Trait) -> bool {
        let ty_def = self.tys.get_ty_def(ty);
        if let Some(&ty::TyDef::GenVar(gen_var)) = ty_def
            && self.tys.implements_trait_constraint_exists(gen_var, trait_)
        {
            return true;
        }

        self.get_impl_insts_for_ty_and_trait(ty, trait_).next().is_some()
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
        } else if let Some(ty::TyDef::Closure { fn_inst, .. }) = self.tys.get_ty_def(ty) {
            let fn_inst = fn_inst.clone();
            let signature = self.get_fn_inst_sig(&fn_inst);
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
