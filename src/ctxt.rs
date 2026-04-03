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
pub(crate) mod ty_match;
mod ty_reg;

pub use fn_reg::FnReg;
pub use impl_reg::ImplReg;
pub use trait_reg::TraitReg;
pub use ty_reg::*;

use crate::ctxt::{traits::TraitInst, ty::GenVarSubst};

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
            impls: ImplReg::new(arena),
            traits: TraitReg::new(arena),
            language_items: Default::default(),
        }
    }

    pub fn get_fn_inst_sig(&self, fn_inst: fns::FnInst<'ctxt>) -> (ty::TySlice<'ctxt>, ty::Ty<'ctxt>, bool) {
        let subst = fn_inst.get_subst();

        let param_tys: Vec<_> = fn_inst
            .fn_
            .params
            .iter()
            .map(|p| self.tys.substitute(p.ty, &subst, fn_inst.self_ty))
            .collect();
        let param_tys = self.tys.ty_slice(&param_tys);

        let return_ty = self.tys.substitute(fn_inst.fn_.return_ty, &subst, fn_inst.self_ty);

        (param_tys, return_ty, fn_inst.fn_.var_args)
    }

    // TODO check the relevance of this function. Is it only use to resolve associated types?
    // If so, perhaps rename, but compare to normalize() in typeck.rs
    pub fn normalize_ty(&self, ty: ty::Ty<'ctxt>) -> ty::Ty<'ctxt> {
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
            &RefMut(ty) => {
                let ty = self.normalize_ty(ty);
                self.tys.ref_mut(ty)
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
                let trait_inst = TraitInst::new(trait_inst.trait_, gen_args).unwrap();

                let impl_insts: Vec<_> = self
                    .get_impl_insts_for_ty_and_trait_inst(&[], base_ty, trait_inst)
                    .collect();

                let [impl_inst] = &impl_insts[..] else { return ty };

                let assoc_ty = impl_inst.impl_.assoc_tys[&assoc_ty_idx];

                let subst = impl_inst.get_subst();

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
}
