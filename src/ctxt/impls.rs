use std::collections::HashMap;

use crate::ctxt::{
    fns::Fn,
    traits::TraitInst,
    ty::{GenVar, Ty, TySlice},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Impl(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ImplInst {
    pub impl_: Impl,
    pub gen_args: TySlice,
}

#[derive(Debug, Clone)]
pub struct ImplDef {
    pub gen_params: Vec<GenVar>,
    pub ty: Ty,
    pub mthds: Vec<Fn>,
    pub mthds_by_name: HashMap<String, Fn>,
    pub trait_inst: Option<TraitInst>,
    pub assoc_tys: HashMap<usize, Ty>,
}
