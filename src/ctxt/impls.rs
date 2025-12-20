use std::collections::HashMap;

use crate::ctxt::{fns::Fn, ty::Ty};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Impl(pub usize);

#[derive(Debug)]
pub struct ImplDef {
    pub ty: Ty,
    pub methods: Vec<Fn>,
    pub methods_by_name: HashMap<String, Fn>,
}
