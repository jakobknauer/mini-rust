use std::collections::HashMap;

use crate::ctxt::{
    fns::Fn,
    impls::{Impl, ImplDef, ImplInst, ImplInstError},
    traits::{Trait, TraitInst},
    ty::{Constraint, GenVar, Ty, TySlice},
};

#[derive(Default)]
pub struct ImplReg<'impls> {
    _phantom: std::marker::PhantomData<&'impls ()>,
    impls: Vec<ImplDef<'impls>>,
}

impl<'impls> ImplReg<'impls> {
    pub fn inst_impl(&self, impl_: Impl, gen_args: TySlice<'impls>) -> Result<ImplInst<'impls>, ImplInstError> {
        let impl_def = self.impls.get(impl_.0).unwrap();
        if impl_def.gen_params.len() != gen_args.len {
            return Err(ImplInstError {
                impl_,
                expected: impl_def.gen_params.len(),
                actual: gen_args.len,
            });
        }
        Ok(ImplInst {
            impl_,
            gen_args,
            _private: (),
            _phantom: std::marker::PhantomData,
        })
    }

    pub fn register_impl(
        &mut self,
        ty: Ty<'impls>,
        gen_params: Vec<GenVar>,
        trait_inst: Option<TraitInst<'impls>>,
        constraints: Vec<Constraint<'impls>>,
        assoc_tys: HashMap<usize, Ty<'impls>>,
    ) -> Impl {
        let impl_ = Impl(self.impls.len());
        let impl_def = ImplDef {
            gen_params,
            ty,
            mthds: Vec::new(),
            mthds_by_name: HashMap::new(),
            trait_inst,
            assoc_tys,
            constraints,
        };
        self.impls.push(impl_def);
        impl_
    }

    pub fn register_mthd(&mut self, impl_: Impl, mthd: Fn, name: &str) {
        let impl_ = self.impls.get_mut(impl_.0).unwrap();
        impl_.mthds.push(mthd);
        impl_.mthds_by_name.insert(name.to_string(), mthd);
    }

    pub fn get_all_impls(&self) -> impl Iterator<Item = Impl> {
        (0..self.impls.len()).map(Impl)
    }

    pub fn get_impls_for_trait(&self, trait_: Trait) -> impl Iterator<Item = Impl> {
        self.get_all_impls().filter(move |impl_| {
            self.get_impl_def(*impl_)
                .trait_inst
                .as_ref()
                .is_some_and(|trait_inst| trait_inst.trait_ == trait_)
        })
    }

    pub fn get_inherent_impls(&self) -> impl Iterator<Item = Impl> {
        self.get_all_impls()
            .filter(|impl_| self.get_impl_def(*impl_).trait_inst.is_none())
    }

    pub fn get_impl_def(&self, impl_: Impl) -> &ImplDef<'impls> {
        self.impls.get(impl_.0).unwrap()
    }

    pub fn get_impl_trait_inst(&self, impl_: Impl) -> Option<TraitInst<'impls>> {
        self.get_impl_def(impl_).trait_inst
    }
}
