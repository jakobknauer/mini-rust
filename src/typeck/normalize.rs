use crate::ctxt::{impls, traits, ty};

impl<'ctxt, 'hlr> super::Typeck<'ctxt, 'hlr> {
    pub(super) fn normalize(&mut self, ty: ty::Ty) -> ty::Ty {
        use ty::TyDef::*;

        let Some(ty_def) = self.ctxt.tys.get_ty_def(ty).cloned() else {
            return ty;
        };

        match ty_def {
            InfVar(iv) => match self.type_vars.get(&iv).copied() {
                Some(resolved) => self.normalize(resolved),
                None => ty,
            },

            Primitive(_) | GenVar(_) | Closure { .. } => ty,

            TraitSelf(_) => {
                let self_ty = self
                    .ctxt
                    .fns
                    .get_sig(self.fn_.fn_)
                    .unwrap()
                    .associated_ty
                    .expect("TraitSelf in a function with no associated type");
                self.normalize(self_ty)
            }

            Alias(_) => unreachable!(),

            Tuple(items) => {
                let items = self.normalize_slice(items);
                self.ctxt.tys.tuple(&items)
            }

            Struct { struct_, gen_args } => {
                let gen_args = self.normalize_slice(gen_args);
                self.ctxt.tys.inst_struct(struct_, &gen_args).unwrap()
            }

            Enum { enum_, gen_args } => {
                let gen_args = self.normalize_slice(gen_args);
                self.ctxt.tys.inst_enum(enum_, &gen_args).unwrap()
            }

            Fn {
                param_tys,
                return_ty,
                var_args,
            } => {
                let param_tys = self.normalize_slice(param_tys);
                let return_ty = self.normalize(return_ty);
                self.ctxt.tys.fn_(&param_tys, return_ty, var_args)
            }

            Ref(inner) => {
                let inner = self.normalize(inner);
                self.ctxt.tys.ref_(inner)
            }

            Ptr(inner) => {
                let inner = self.normalize(inner);
                self.ctxt.tys.ptr(inner)
            }

            AssocTy {
                base_ty,
                trait_inst,
                assoc_ty_idx,
            } => {
                let base_ty = self.normalize(base_ty);
                let trait_gen_args = self.normalize_slice(trait_inst.gen_args);
                let trait_inst = traits::TraitInst {
                    trait_: trait_inst.trait_,
                    gen_args: self.ctxt.tys.ty_slice(&trait_gen_args),
                };

                let impl_insts: Vec<_> = self
                    .ctxt
                    .get_impl_insts_for_ty_and_trait_inst(base_ty, trait_inst)
                    .collect();
                let [impl_inst] = &impl_insts[..] else {
                    return self.ctxt.tys.assoc_ty(base_ty, trait_inst, assoc_ty_idx);
                };

                let impl_def = self.ctxt.impls.get_impl_def(impl_inst.impl_).clone();
                let assoc_ty = impl_def.assoc_tys[&assoc_ty_idx];
                let impl_gen_args = self.ctxt.tys.get_ty_slice(impl_inst.gen_args).to_vec();
                let subst = ty::GenVarSubst::new(&impl_def.gen_params, &impl_gen_args).unwrap();
                let result = self.ctxt.tys.substitute_gen_vars(assoc_ty, &subst);
                self.normalize(result)
            }
        }
    }

    fn normalize_slice(&mut self, slice: ty::TySlice) -> Vec<ty::Ty> {
        let mut tys = self.ctxt.tys.get_ty_slice(slice).to_vec();
        for t in &mut tys {
            *t = self.normalize(*t);
        }
        tys
    }
}
