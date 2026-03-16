use crate::ctxt::ty;

impl<'a, 'f, 'ctxt: 'a + 'hlr, 'hlr: 'ctxt> super::Typeck<'a, 'f, 'ctxt, 'hlr> {
    pub(super) fn post_check(&self) {
        for &ty in self.typing.var_types.values() {
            debug_assert!(!self.contains_inf_var(ty));
        }

        for &ty in self.typing.expr_types.values() {
            debug_assert!(!self.contains_inf_var(ty));
        }

        for &fn_ in &self.created_closure_fns {
            let sig = self.ctxt.fns.get_sig(fn_).unwrap();
            debug_assert!(!self.contains_inf_var(sig.return_ty));
            for param in &sig.params {
                debug_assert!(!self.contains_inf_var(param.ty));
            }
        }

        for &(struct_, _) in &self.created_closure_structs {
            for field in struct_.get_fields() {
                debug_assert!(!self.contains_inf_var(field.ty));
            }
        }
    }

    fn contains_inf_var(&self, ty: ty::Ty) -> bool {
        use ty::TyDef::*;

        match ty.0 {
            InfVar(_) => true,
            Primitive(_) | GenVar(_) | TraitSelf(_) => false,
            &Opaque { gen_args, .. } => self.slice_contains_inf_var(gen_args),
            &Tuple(items) => self.slice_contains_inf_var(items),
            &Ref(inner) | &Ptr(inner) => self.contains_inf_var(inner),
            &Struct { gen_args, .. } => self.slice_contains_inf_var(gen_args),
            &Enum { gen_args, .. } => self.slice_contains_inf_var(gen_args),
            &Fn {
                param_tys, return_ty, ..
            } => self.slice_contains_inf_var(param_tys) || self.contains_inf_var(return_ty),
            &Closure {
                fn_inst, captures_ty, ..
            } => {
                self.contains_inf_var(captures_ty)
                    || self.slice_contains_inf_var(fn_inst.gen_args)
                    || self.slice_contains_inf_var(fn_inst.env_gen_args)
            }
            &AssocTy {
                base_ty, trait_inst, ..
            } => self.contains_inf_var(base_ty) || self.slice_contains_inf_var(trait_inst.gen_args),
        }
    }

    fn slice_contains_inf_var(&self, slice: ty::TySlice) -> bool {
        slice.iter().any(|&ty| self.contains_inf_var(ty))
    }
}
