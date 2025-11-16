use crate::ctxt::ty::Ty;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Fn(pub usize);

#[derive(Clone)]
pub struct FnSig {
    pub name: String,
    pub return_ty: Ty,
    pub parameters: Vec<FnParam>,
}

#[derive(Clone)]
pub struct FnParam {
    pub name: String,
    pub ty: Ty,
}
