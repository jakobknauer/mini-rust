use crate::ctxt::{fns::Fn, ty::Ty};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Impl(pub usize);

pub struct ImplDef {
    pub ty: Ty,
    pub methods: Vec<Fn>,
}
