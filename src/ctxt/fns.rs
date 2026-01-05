use crate::ctxt::{
    mlr,
    traits::TraitInstance,
    ty::{GenVar, Ty},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Fn(pub usize);

#[derive(Clone)]
pub struct FnSig {
    pub name: String,
    /// The type of which the function is an associated method, if any.
    /// At the moment, this is only used for printing names.
    pub associated_ty: Option<Ty>,
    /// The trait of which the function is an associated method, if any.
    /// At the moment, this is only used for printing names.
    pub associated_trait_inst: Option<TraitInstance>,
    /// The generic parameters appearing in the function signature
    pub gen_params: Vec<GenVar>,
    /// Generic used in the functions' body that do not appear in the signature,
    /// e.g. the generic parameters of the surrounding impl block of the function,
    /// or the generic parameters of the surrounding function of a closure.
    pub env_gen_params: Vec<GenVar>,
    pub params: Vec<FnParam>,
    pub var_args: bool,
    pub return_ty: Ty,
}

impl FnSig {
    pub fn has_receiver(&self) -> bool {
        self.params
            .first()
            .map(|param| matches!(param.kind, FnParamKind::Self_ | FnParamKind::SelfByRef))
            .unwrap_or(false)
    }
}

#[derive(Clone)]
pub struct FnParam {
    pub kind: FnParamKind,
    pub ty: Ty,
}

#[derive(Clone, PartialEq, Eq)]
pub enum FnParamKind {
    Regular(String),
    Self_,
    SelfByRef,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FnSpecialization {
    pub fn_: Fn,
    pub gen_args: Vec<Ty>,
    pub env_gen_args: Vec<Ty>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct TraitMethod {
    pub trait_instance: TraitInstance,
    pub method_idx: usize,
    pub impl_ty: Ty,
    pub gen_args: Vec<Ty>,
}

#[derive(Debug, Clone)]
pub struct FnMlr {
    pub body: mlr::Stmt,
    pub param_locs: Vec<mlr::Loc>,
}
