use crate::ctxt::{
    fns::FnSig,
    ty::{GenVar, TySlice},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Trait(pub usize);

#[derive(Clone)]
pub struct TraitDef {
    pub name: String,
    pub gen_params: Vec<GenVar>,
    pub mthds: Vec<FnSig>,
    pub assoc_tys: Vec<String>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct TraitInst {
    pub trait_: Trait,
    pub gen_args: TySlice,
}
