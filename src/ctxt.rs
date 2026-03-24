pub mod fns;
pub mod impls;
pub mod language_items;
pub mod traits;
#[macro_use]
pub mod ty;

mod fn_reg;
mod impl_check;
mod impl_reg;
mod trait_reg;
mod ty_reg;

pub use fn_reg::FnReg;
pub use impl_reg::ImplReg;
pub use trait_reg::TraitReg;
pub use ty_reg::*;

use crate::ctxt::ty::GenVarSubst;

pub struct Ctxt<'ctxt> {
    pub tys: TyReg<'ctxt>,
    pub fns: FnReg<'ctxt>,
    pub impls: ImplReg<'ctxt>,
    pub traits: TraitReg<'ctxt>,
    pub language_items: language_items::LanguageItems<'ctxt>,
}

impl<'ctxt> Ctxt<'ctxt> {
    pub fn new(arena: &'ctxt bumpalo::Bump) -> Self {
        Self {
            tys: TyReg::new(arena),
            fns: FnReg::new(arena),
            impls: Default::default(),
            traits: TraitReg::new(arena),
            language_items: Default::default(),
        }
    }

    pub fn get_fn_inst_name(&self, fn_inst: fns::FnInst<'ctxt>) -> String {
        let assoc_ty = if let Some(assoc_ty) = fn_inst.fn_.associated_ty {
            let assoc_ty_name = self.tys.get_string_rep(assoc_ty);
            if let Some(assoc_trait_inst) = &fn_inst.fn_.associated_trait_inst {
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
                    assoc_ty_name, assoc_trait_inst.trait_.name, assoc_trait_gen_params
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

        format!("{}{}{}{}", assoc_ty, fn_inst.fn_.name, env_gen_args, gen_args)
    }

    pub fn get_fn_inst_decl(&self, fn_inst: fns::FnInst<'ctxt>) -> (Vec<ty::Ty<'ctxt>>, ty::Ty<'ctxt>, bool) {
        let subst = self.get_subst_for_fn_inst(fn_inst);

        let param_tys = fn_inst
            .fn_
            .params
            .iter()
            .map(|p| self.tys.substitute_gen_vars(p.ty, &subst))
            .collect();
        let return_ty = self.tys.substitute_gen_vars(fn_inst.fn_.return_ty, &subst);

        (param_tys, return_ty, fn_inst.fn_.var_args)
    }

    // TODO check the relevance of this function. Is it only use to resolve associated types?
    // If so, perhaps rename, but compare to normalize() in typeck.rs
    pub fn normalize_ty(&mut self, ty: ty::Ty<'ctxt>) -> ty::Ty<'ctxt> {
        use ty::TyDef::*;

        match ty.0 {
            Primitive(_) => ty,
            &Tuple(items) => {
                let items: Vec<_> = items.iter().map(|&ty| self.normalize_ty(ty)).collect();
                self.tys.tuple(&items)
            }
            &Struct { struct_, gen_args } => {
                let gen_args: Vec<_> = gen_args.iter().map(|&ty| self.normalize_ty(ty)).collect();
                self.tys.inst_struct(struct_, &gen_args).unwrap()
            }
            &Enum { enum_, gen_args } => {
                let gen_args: Vec<_> = gen_args.iter().map(|&ty| self.normalize_ty(ty)).collect();
                self.tys.inst_enum(enum_, &gen_args).unwrap()
            }
            &Fn {
                param_tys,
                return_ty,
                var_args,
            } => {
                let param_tys: Vec<_> = param_tys.iter().map(|&ty| self.normalize_ty(ty)).collect();
                let return_ty = self.normalize_ty(return_ty);
                self.tys.fn_(&param_tys, return_ty, var_args)
            }
            &Ref(ty) => {
                let ty = self.normalize_ty(ty);
                self.tys.ref_(ty)
            }
            &Ptr(ty) => {
                let ty = self.normalize_ty(ty);
                self.tys.ptr(ty)
            }
            GenVar(_) => ty,
            TraitSelf(_) => ty,
            Closure { .. } => ty,
            &AssocTy {
                base_ty,
                trait_inst,
                assoc_ty_idx,
            } => {
                let base_ty = self.normalize_ty(base_ty);
                let gen_args: Vec<_> = trait_inst.gen_args.iter().map(|&ty| self.normalize_ty(ty)).collect();
                let gen_args = self.tys.ty_slice(&gen_args);
                let trait_inst = self.traits.inst_trait(trait_inst.trait_, gen_args).unwrap();

                let impl_insts: Vec<_> = self
                    .get_impl_insts_for_ty_and_trait_inst(&[], base_ty, trait_inst)
                    .collect();

                let [impl_inst] = &impl_insts[..] else { return ty };

                let impl_def = self.impls.get_impl_def(impl_inst.impl_);
                let assoc_ty = impl_def.assoc_tys[&assoc_ty_idx];

                let subst = GenVarSubst::new(&impl_def.gen_params, impl_inst.gen_args).unwrap();

                let resolved = self.tys.substitute_gen_vars(assoc_ty, &subst);
                self.normalize_ty(resolved)
            }
            InfVar(_) => unreachable!(),
            &Opaque { opaque, gen_args } => {
                let Some(resolved) = self.tys.get_opaque_resolution(opaque) else {
                    return ty;
                };
                let subst = GenVarSubst::new(&opaque.gen_params, gen_args).unwrap();
                let instantiated = self.tys.substitute_gen_vars(resolved, &subst);
                self.normalize_ty(instantiated)
            }
        }
    }

    pub fn get_subst_for_fn_inst(&self, fn_inst: fns::FnInst<'ctxt>) -> GenVarSubst<'ctxt> {
        let gen_param_subst = GenVarSubst::new(&fn_inst.fn_.gen_params, fn_inst.gen_args).unwrap();
        let env_gen_param_subst = GenVarSubst::new(&fn_inst.fn_.env_gen_params, fn_inst.env_gen_args).unwrap();
        GenVarSubst::compose(env_gen_param_subst, gen_param_subst)
    }
}
