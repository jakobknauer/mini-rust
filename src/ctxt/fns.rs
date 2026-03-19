use crate::ctxt::{
    traits::{Trait, TraitInst},
    ty::{Constraint, GenVar, Ty, TySlice},
};

pub type Fn<'fns> = &'fns FnSig<'fns>;

pub struct FnSig<'fns> {
    pub name: String,
    /// The type of which the function is an associated method, if any.
    /// At the moment, this is only used for printing names.
    pub associated_ty: Option<Ty<'fns>>,
    /// The trait of which the function is an associated method, if any.
    /// At the moment, this is only used for printing names.
    pub associated_trait_inst: Option<TraitInst<'fns>>,
    /// The generic parameters appearing in the function signature
    pub gen_params: Vec<GenVar<'fns>>,
    /// Generic used in the functions' body that do not appear in the signature,
    /// e.g. the generic parameters of the surrounding impl block of the function,
    /// or the generic parameters of the surrounding function of a closure.
    pub env_gen_params: Vec<GenVar<'fns>>,
    /// Constraints from the surrounding context (e.g. impl block where clause).
    /// Not compared during impl checking; only used during typechecking.
    pub env_constraints: Vec<Constraint<'fns>>,
    pub params: Vec<FnParam<'fns>>,
    pub var_args: bool,
    pub return_ty: Ty<'fns>,
    pub constraints: Vec<Constraint<'fns>>,
}

impl PartialEq for FnSig<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Eq for FnSig<'_> {}

impl std::hash::Hash for FnSig<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const Self).hash(state);
    }
}

impl std::fmt::Debug for FnSig<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FnSig({})", self.name)
    }
}

impl<'fns> FnSig<'fns> {
    pub fn all_constraints(&self) -> impl Iterator<Item = &Constraint<'fns>> {
        self.env_constraints.iter().chain(&self.constraints)
    }

    pub fn has_receiver(&self) -> bool {
        self.params
            .first()
            .map(|param| matches!(param.kind, FnParamKind::Self_ | FnParamKind::SelfByRef))
            .unwrap_or(false)
    }
}

#[derive(Clone)]
pub struct FnParam<'fns> {
    pub kind: FnParamKind,
    pub ty: Ty<'fns>,
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
        expected: usize,
        #[allow(unused)]
        actual: usize,
    },
    EnvGenArgCountMismatch {
        #[allow(unused)]
        expected: usize,
        #[allow(unused)]
        actual: usize,
    },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct FnInst<'fns> {
    pub fn_: Fn<'fns>,
    pub gen_args: TySlice<'fns>,
    pub env_gen_args: TySlice<'fns>,
    pub(in crate::ctxt) _private: (),
    pub(in crate::ctxt) _phantom: std::marker::PhantomData<&'fns ()>,
}

impl<'fns> FnInst<'fns> {
    pub fn with_gen_args(
        self,
        gen_args: TySlice<'fns>,
        env_gen_args: TySlice<'fns>,
    ) -> Result<FnInst<'fns>, FnInstError> {
        if gen_args.len() != self.gen_args.len() {
            return Err(FnInstError::GenArgCountMismatch {
                expected: self.gen_args.len(),
                actual: gen_args.len(),
            });
        }
        if env_gen_args.len() != self.env_gen_args.len() {
            return Err(FnInstError::EnvGenArgCountMismatch {
                expected: self.env_gen_args.len(),
                actual: env_gen_args.len(),
            });
        }
        Ok(FnInst {
            fn_: self.fn_,
            gen_args,
            env_gen_args,
            _private: (),
            _phantom: std::marker::PhantomData,
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
pub struct TraitMthdInst<'fns> {
    pub trait_inst: TraitInst<'fns>,
    pub mthd_idx: usize,
    pub impl_ty: Ty<'fns>,
    pub gen_args: TySlice<'fns>,
    pub(in crate::ctxt) _private: (),
    pub(in crate::ctxt) _phantom: std::marker::PhantomData<&'fns ()>,
}

impl<'fns> TraitMthdInst<'fns> {
    pub fn with_updated(
        self,
        impl_ty: Ty<'fns>,
        trait_inst: TraitInst<'fns>,
        gen_args: TySlice<'fns>,
    ) -> Result<TraitMthdInst<'fns>, TraitMthdInstError> {
        if trait_inst.trait_ != self.trait_inst.trait_ {
            return Err(TraitMthdInstError::TraitMismatch {
                expected: self.trait_inst.trait_,
                actual: trait_inst.trait_,
            });
        }
        if gen_args.len() != self.gen_args.len() {
            return Err(TraitMthdInstError::GenArgCountMismatch {
                trait_: self.trait_inst.trait_,
                mthd_idx: self.mthd_idx,
                expected: self.gen_args.len(),
                actual: gen_args.len(),
            });
        }
        Ok(TraitMthdInst {
            trait_inst,
            mthd_idx: self.mthd_idx,
            impl_ty,
            gen_args,
            _private: (),
            _phantom: std::marker::PhantomData,
        })
    }
}
