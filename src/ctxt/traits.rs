use crate::ctxt::fns::FnSig;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Trait(pub usize);

pub struct TraitDef {
    pub name: String,
    pub methods: Vec<FnSig>,
}
