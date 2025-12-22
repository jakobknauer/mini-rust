use std::collections::HashMap;

use crate::ctxt::{
    fns::Fn,
    traits::Trait,
    ty::{GenVar, Ty},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Impl(pub usize);

#[derive(Debug)]
pub struct ImplDef {
    pub gen_params: Vec<GenVar>,
    pub ty: Ty,
    pub methods: Vec<Fn>,
    pub methods_by_name: HashMap<String, Fn>,
    pub trait_: Option<Trait>,
}
