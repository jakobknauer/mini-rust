use crate::ctxt::{
    fns::FnSig,
    ty::{GenVar, Ty},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Trait(pub usize);

pub struct TraitDef {
    pub name: String,
    #[expect(unused)]
    pub gen_params: Vec<GenVar>,
    pub methods: Vec<FnSig>,
}

#[expect(unused)]
pub struct TraitInstance {
    pub trait_: Trait,
    pub gen_args: Vec<Ty>,
}
