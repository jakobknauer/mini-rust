use std::cell::RefCell;

use crate::ctxt::{
    fns::Fn,
    traits::{Trait, TraitDef, TraitId, TraitMthd, TraitMthdDef},
    ty::GenVar,
};

pub struct TraitReg<'traits> {
    arena: &'traits bumpalo::Bump,
    traits: RefCell<Vec<Trait<'traits>>>,
    mthds: RefCell<Vec<TraitMthd<'traits>>>,
}

impl<'traits> TraitReg<'traits> {
    pub fn new(arena: &'traits bumpalo::Bump) -> Self {
        Self {
            arena,
            traits: RefCell::default(),
            mthds: RefCell::default(),
        }
    }

    pub fn register_trait(
        &self,
        name: &str,
        gen_params: Vec<GenVar<'traits>>,
        assoc_tys: Vec<String>,
    ) -> Trait<'traits> {
        let idx = self.traits.borrow().len();
        let trait_: Trait<'traits> = self.arena.alloc(TraitDef {
            id: TraitId(idx),
            name: name.to_string(),
            gen_params,
            assoc_tys,
        });
        self.traits.borrow_mut().push(trait_);
        trait_
    }

    pub fn register_mthd(
        &self,
        trait_: Trait<'traits>,
        fn_: Fn<'traits>,
        has_default_body: bool,
    ) -> TraitMthd<'traits> {
        let mthd: TraitMthd<'traits> = self.arena.alloc(TraitMthdDef {
            trait_,
            fn_,
            has_default_body,
        });
        self.mthds.borrow_mut().push(mthd);
        mthd
    }

    pub fn resolve_trait_name(&self, trait_name: &str) -> Option<Trait<'traits>> {
        self.traits.borrow().iter().copied().find(|t| t.name == trait_name)
    }

    pub fn get_trait_mthds(&self, trait_: Trait<'traits>) -> impl Iterator<Item = TraitMthd<'traits>> {
        self.mthds
            .borrow()
            .iter()
            .copied()
            .filter(move |mthd| std::ptr::eq(mthd.trait_, trait_))
            .collect::<Vec<_>>()
            .into_iter()
    }

    pub fn get_trait_mthd_with_name(
        &self,
        mthd_name: &str,
        must_have_receiver: bool,
    ) -> impl Iterator<Item = TraitMthd<'traits>> {
        self.mthds
            .borrow()
            .iter()
            .copied()
            .filter(move |mthd| mthd.fn_.name == mthd_name && (!must_have_receiver || mthd.fn_.has_receiver()))
            .collect::<Vec<_>>()
            .into_iter()
    }

    pub fn resolve_trait_method(&self, trait_: Trait<'traits>, ident: &str) -> Option<TraitMthd<'traits>> {
        self.mthds
            .borrow()
            .iter()
            .copied()
            .find(|mthd| std::ptr::eq(mthd.trait_, trait_) && mthd.fn_.name == ident)
    }

    pub fn get_trait_assoc_ty_with_name(&self, ident: &str) -> impl Iterator<Item = (Trait<'traits>, usize)> {
        self.traits
            .borrow()
            .iter()
            .copied()
            .filter_map(move |trait_| {
                trait_
                    .assoc_tys
                    .iter()
                    .position(|assoc_ty| assoc_ty == ident)
                    .map(|assoc_ty_idx| (trait_, assoc_ty_idx))
            })
            .collect::<Vec<_>>()
            .into_iter()
    }
}
