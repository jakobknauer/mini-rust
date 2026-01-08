use std::collections::HashMap;

use crate::ctxt::{
    fns::Fn,
    traits::TraitInst,
    ty::{GenVar, Ty},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Impl(pub usize);

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ImplInst {
    pub impl_: Impl,
    pub gen_args: Vec<Ty>,
}

#[derive(Debug, Clone)]
pub struct ImplDef {
    pub gen_params: Vec<GenVar>,
    pub ty: Ty,
    pub mthds: Vec<Fn>,
    pub mthds_by_name: HashMap<String, Fn>,
    pub trait_inst: Option<TraitInst>,
}
