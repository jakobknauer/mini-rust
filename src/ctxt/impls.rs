use std::collections::HashMap;

use crate::ctxt::{
    fns::Fn,
    traits::TraitInst,
    ty::{Constraint, GenVar, Ty, TySlice},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Impl(pub(in crate::ctxt) usize);

#[derive(Debug)]
pub struct ImplInstError {
    #[allow(unused)]
    pub impl_: Impl,
    #[allow(unused)]
    pub expected: usize,
    #[allow(unused)]
    pub actual: usize,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ImplInst<'impls> {
    pub impl_: Impl,
    pub gen_args: TySlice<'impls>,
    pub(in crate::ctxt) _private: (),
}

#[derive(Clone)]
pub struct ImplDef<'impls> {
    pub gen_params: Vec<GenVar<'impls>>,
    pub ty: Ty<'impls>,
    pub mthds: Vec<Fn<'impls>>,
    pub mthds_by_name: HashMap<String, Fn<'impls>>,
    pub trait_inst: Option<TraitInst<'impls>>,
    pub assoc_tys: HashMap<usize, Ty<'impls>>,
    pub constraints: Vec<Constraint<'impls>>,
}
