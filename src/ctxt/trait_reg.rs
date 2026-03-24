use std::cell::RefCell;

use crate::ctxt::{
    fns::{Fn, TraitMthdInst, TraitMthdInstError},
    traits::{Trait, TraitDef, TraitId, TraitInst, TraitMthd, TraitMthdDef},
    ty::{GenVar, Ty, TySlice},
};

pub use crate::ctxt::traits::TraitInstError;

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

    // TODO must this be a method?
    pub fn inst_trait_mthd(
        &self,
        trait_inst: TraitInst<'traits>,
        mthd: TraitMthd<'traits>,
        impl_ty: Ty<'traits>,
        gen_args: TySlice<'traits>,
    ) -> Result<TraitMthdInst<'traits>, TraitMthdInstError<'traits>> {
        let n_gen_params = mthd.fn_.gen_params.len();
        if n_gen_params != gen_args.len() {
            return Err(TraitMthdInstError::GenArgCountMismatch {
                mthd,
                expected: n_gen_params,
                actual: gen_args.len(),
            });
        }
        Ok(TraitMthdInst {
            trait_inst,
            mthd,
            impl_ty,
            gen_args,
            _private: (),
            _phantom: std::marker::PhantomData,
        })
    }

    // TODO must this be a method?
    pub fn inst_trait(
        &self,
        trait_: Trait<'traits>,
        gen_args: TySlice<'traits>,
    ) -> Result<TraitInst<'traits>, TraitInstError<'traits>> {
        if trait_.gen_params.len() != gen_args.len() {
            return Err(TraitInstError {
                trait_,
                expected: trait_.gen_params.len(),
                actual: gen_args.len(),
            });
        }
        Ok(TraitInst { trait_, gen_args })
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

    pub fn register_mthd(&self, trait_: Trait<'traits>, fn_: Fn<'traits>) -> TraitMthd<'traits> {
        let mthd: TraitMthd<'traits> = self.arena.alloc(TraitMthdDef { trait_, fn_ });
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

    // TODO make this a method of Trait?
    pub fn resolve_assoc_ty_name(&self, trait_: Trait<'traits>, name: &str) -> Option<usize> {
        trait_.assoc_tys.iter().position(|n| n == name)
    }

    // TODO make this a method of Trait?
    pub fn get_trait_assoc_ty_index(&self, trait_: Trait<'traits>, name: &str) -> usize {
        trait_.assoc_tys.iter().position(|assoc_ty| assoc_ty == name).unwrap()
    }
}
