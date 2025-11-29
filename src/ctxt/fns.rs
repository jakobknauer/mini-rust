use crate::ctxt::{mlr, ty::Ty};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Fn(pub usize);

#[derive(Clone)]
pub struct FnSig {
    pub name: String,
    pub gen_params: Vec<GenParam>,
    pub params: Vec<FnParam>,
    pub return_ty: Ty,
}

#[derive(Clone)]
pub struct GenParam {
    pub name: String,
    pub ty: Ty,
}

#[derive(Clone)]
pub struct FnParam {
    pub name: String,
    pub ty: Ty,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FnSpecialization {
    pub fn_: Fn,
    pub gen_args: Vec<Ty>,
}

#[derive(Debug, Clone)]
pub struct FnMlr {
    pub body: mlr::Stmt,
    pub param_locs: Vec<mlr::Loc>,
}
