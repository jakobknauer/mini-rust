use std::{cell::RefCell, collections::HashMap};

use crate::ctxt::{
    fns::Fn,
    impls::{Impl, ImplDef, ImplInst, ImplInstError},
    traits::{Trait, TraitInst},
    ty::{Constraint, GenVar, Ty, TySlice},
};

pub struct ImplReg<'impls> {
    arena: &'impls bumpalo::Bump,
    impls: RefCell<Vec<Impl<'impls>>>,
}

impl<'impls> ImplReg<'impls> {
    pub fn new(arena: &'impls bumpalo::Bump) -> Self {
        Self {
            arena,
            impls: RefCell::new(Vec::new()),
        }
    }

    pub fn inst_impl(
        &self,
        impl_: Impl<'impls>,
        gen_args: TySlice<'impls>,
    ) -> Result<ImplInst<'impls>, ImplInstError<'impls>> {
        if impl_.gen_params.len() != gen_args.len() {
            return Err(ImplInstError {
                impl_,
                expected: impl_.gen_params.len(),
                actual: gen_args.len(),
            });
        }
        Ok(ImplInst {
            impl_,
            gen_args,
            _private: (),
        })
    }

    pub fn register_impl(
        &self,
        ty: Ty<'impls>,
        gen_params: Vec<GenVar<'impls>>,
        trait_inst: Option<TraitInst<'impls>>,
        constraints: Vec<Constraint<'impls>>,
        assoc_tys: HashMap<usize, Ty<'impls>>,
    ) -> Impl<'impls> {
        let impl_def = ImplDef {
            gen_params,
            ty,
            mthds: RefCell::new(Vec::new()),
            trait_inst,
            assoc_tys,
            constraints,
        };
        let impl_: Impl<'impls> = self.arena.alloc(impl_def);
        self.impls.borrow_mut().push(impl_);
        impl_
    }

    pub fn register_mthd(&self, impl_: Impl<'impls>, mthd: Fn<'impls>) {
        impl_.mthds.borrow_mut().push(mthd);
    }

    pub fn get_all_impls(&self) -> impl Iterator<Item = Impl<'impls>> {
        self.impls.borrow().iter().copied().collect::<Vec<_>>().into_iter()
    }

    pub fn get_impls_for_trait(&self, trait_: Trait<'impls>) -> impl Iterator<Item = Impl<'impls>> {
        self.get_all_impls()
            .filter(move |impl_| impl_.trait_inst.as_ref().is_some_and(|ti| ti.trait_ == trait_))
    }

    pub fn get_inherent_impls(&self) -> impl Iterator<Item = Impl<'impls>> {
        self.get_all_impls().filter(|impl_| impl_.trait_inst.is_none())
    }
}
