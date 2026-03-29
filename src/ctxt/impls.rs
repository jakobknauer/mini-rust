use std::{cell::RefCell, collections::HashMap};

use crate::ctxt::{
    fns::Fn,
    traits::TraitInst,
    ty::{self, Constraint, GenVar, Ty, TySlice},
};

pub type Impl<'impls> = &'impls ImplDef<'impls>;

#[derive(Debug)]
#[allow(unused)]
pub struct ImplInstError<'impls> {
    pub impl_: Impl<'impls>,
    pub expected: usize,
    pub actual: usize,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ImplInst<'impls> {
    pub impl_: Impl<'impls>,
    pub gen_args: TySlice<'impls>,
    _private: (),
}

impl<'impls> ImplInst<'impls> {
    pub fn new(impl_: Impl<'impls>, gen_args: TySlice<'impls>) -> Result<ImplInst<'impls>, ImplInstError<'impls>> {
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

    pub fn get_subst(&self) -> ty::GenVarSubst<'impls> {
        ty::GenVarSubst::new(&self.impl_.gen_params, self.gen_args).unwrap()
    }
}

pub struct ImplDef<'impls> {
    pub gen_params: Vec<GenVar<'impls>>,
    pub ty: Ty<'impls>,
    pub mthds: RefCell<Vec<Fn<'impls>>>,
    pub trait_inst: Option<TraitInst<'impls>>,
    pub assoc_tys: HashMap<usize, Ty<'impls>>,
    pub constraints: Vec<Constraint<'impls>>,
}

impl std::fmt::Debug for ImplDef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ImplDef")
    }
}

impl PartialEq for ImplDef<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Eq for ImplDef<'_> {}

impl std::hash::Hash for ImplDef<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self, state);
    }
}
