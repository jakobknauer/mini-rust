use crate::ctxt::{
    traits::{Trait, TraitInst},
    ty::{Constraint, GenVar, Ty, TySlice},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Fn(pub(in crate::ctxt) usize);

impl std::fmt::Display for Fn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone)]
pub struct FnSig {
    pub name: String,
    /// The type of which the function is an associated method, if any.
    /// At the moment, this is only used for printing names.
    pub associated_ty: Option<Ty>,
    /// The trait of which the function is an associated method, if any.
    /// At the moment, this is only used for printing names.
    pub associated_trait_inst: Option<TraitInst>,
    /// The generic parameters appearing in the function signature
    pub gen_params: Vec<GenVar>,
    /// Generic used in the functions' body that do not appear in the signature,
    /// e.g. the generic parameters of the surrounding impl block of the function,
    /// or the generic parameters of the surrounding function of a closure.
    pub env_gen_params: Vec<GenVar>,
    pub params: Vec<FnParam>,
    pub var_args: bool,
    pub return_ty: Ty,
    pub constraints: Vec<Constraint>,
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

#[derive(Debug)]
pub enum FnInstError {
    GenArgCountMismatch {
        #[allow(unused)]
        fn_: Fn,
        #[allow(unused)]
        expected: usize,
        #[allow(unused)]
        actual: usize,
    },
    EnvGenArgCountMismatch {
        #[allow(unused)]
        fn_: Fn,
        #[allow(unused)]
        expected: usize,
        #[allow(unused)]
        actual: usize,
    },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct FnInst {
    pub fn_: Fn,
    pub gen_args: TySlice,
    pub env_gen_args: TySlice,
    pub(in crate::ctxt) _private: (),
}

impl FnInst {
    pub fn with_gen_args(self, gen_args: TySlice, env_gen_args: TySlice) -> Result<FnInst, FnInstError> {
        if gen_args.len != self.gen_args.len {
            return Err(FnInstError::GenArgCountMismatch {
                fn_: self.fn_,
                expected: self.gen_args.len,
                actual: gen_args.len,
            });
        }
        if env_gen_args.len != self.env_gen_args.len {
            return Err(FnInstError::EnvGenArgCountMismatch {
                fn_: self.fn_,
                expected: self.env_gen_args.len,
                actual: env_gen_args.len,
            });
        }
        Ok(FnInst {
            fn_: self.fn_,
            gen_args,
            env_gen_args,
            _private: (),
        })
    }
}

#[derive(Debug)]
pub enum TraitMthdInstError {
    MthdIdxOutOfRange {
        #[allow(unused)]
        trait_: Trait,
        #[allow(unused)]
        mthd_count: usize,
        #[allow(unused)]
        actual: usize,
    },
    GenArgCountMismatch {
        #[allow(unused)]
        trait_: Trait,
        #[allow(unused)]
        mthd_idx: usize,
        #[allow(unused)]
        expected: usize,
        #[allow(unused)]
        actual: usize,
    },
    TraitMismatch {
        #[allow(unused)]
        expected: Trait,
        #[allow(unused)]
        actual: Trait,
    },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct TraitMthdInst {
    pub trait_inst: TraitInst,
    pub mthd_idx: usize,
    pub impl_ty: Ty,
    pub gen_args: TySlice,
    pub(in crate::ctxt) _private: (),
}

impl TraitMthdInst {
    pub fn with_updated(
        self,
        impl_ty: Ty,
        trait_inst: TraitInst,
        gen_args: TySlice,
    ) -> Result<TraitMthdInst, TraitMthdInstError> {
        if trait_inst.trait_ != self.trait_inst.trait_ {
            return Err(TraitMthdInstError::TraitMismatch {
                expected: self.trait_inst.trait_,
                actual: trait_inst.trait_,
            });
        }
        if gen_args.len != self.gen_args.len {
            return Err(TraitMthdInstError::GenArgCountMismatch {
                trait_: self.trait_inst.trait_,
                mthd_idx: self.mthd_idx,
                expected: self.gen_args.len,
                actual: gen_args.len,
            });
        }
        Ok(TraitMthdInst {
            trait_inst,
            mthd_idx: self.mthd_idx,
            impl_ty,
            gen_args,
            _private: (),
        })
    }
}
