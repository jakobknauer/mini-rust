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
pub struct ImplInst {
    pub impl_: Impl,
    pub gen_args: TySlice,
    pub(in crate::ctxt) _private: (),
}

#[derive(Clone)]
pub struct ImplDef {
    pub gen_params: Vec<GenVar>,
    pub ty: Ty,
    pub mthds: Vec<Fn>,
    pub mthds_by_name: HashMap<String, Fn>,
    pub trait_inst: Option<TraitInst>,
    pub assoc_tys: HashMap<usize, Ty>,
    pub constraints: Vec<Constraint>,
}
