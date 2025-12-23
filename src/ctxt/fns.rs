use crate::ctxt::{
    mlr,
    traits::Trait,
    ty::{GenVar, Ty},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Fn(pub usize);

#[derive(Clone)]
pub struct FnSig {
    pub name: String,
    /// The type of which the function is an associated method, if any
    /// At the moment, this is only used for printing names.
    pub associated_ty: Option<Ty>,
    pub associated_trait: Option<Trait>,
    /// The generic parameters appearing in the function signature
    pub gen_params: Vec<GenVar>,
    /// Generic used in the functions' body that do not appear in the signature,
    /// e.g. the generic parameters of the surrounding impl block of the function,
    /// or the generic parameters of the surrounding function of a closure.
    pub env_gen_params: Vec<GenVar>,
    pub params: Vec<FnParam>,
    pub var_args: bool,
    pub return_ty: Ty,
    pub has_receiver: bool,
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
    pub env_gen_args: Vec<Ty>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct TraitMethod {
    pub trait_: Trait,
    pub method_idx: usize,
    pub impl_ty: Ty,
}

#[derive(Debug, Clone)]
pub struct FnMlr {
    pub body: mlr::Stmt,
    pub param_locs: Vec<mlr::Loc>,
}
