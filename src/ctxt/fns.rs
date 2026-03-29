use crate::ctxt::{
    traits::{Trait, TraitInst, TraitMthd},
    ty::{self, Constraint, GenVar, Ty, TySlice},
};

pub type Fn<'fns> = &'fns FnDecl<'fns>;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct FnId(pub(in crate::ctxt) usize);

pub struct FnDecl<'fns> {
    pub(crate) id: FnId,
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

impl PartialEq for FnDecl<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for FnDecl<'_> {}

impl std::hash::Hash for FnDecl<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl std::fmt::Debug for FnDecl<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FnDecl({})", self.name)
    }
}

impl<'fns> FnDecl<'fns> {
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

pub struct FnParam<'fns> {
    pub kind: FnParamKind,
    pub ty: Ty<'fns>,
}

#[derive(PartialEq, Eq)]
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
    pub self_ty: Option<ty::Ty<'fns>>,
    _private: (),
}

impl std::fmt::Display for FnInst<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(assoc_ty) = self.fn_.associated_ty {
            if let Some(assoc_trait_inst) = &self.fn_.associated_trait_inst {
                write!(f, "<{} as {}>::", assoc_ty, assoc_trait_inst)?;
            } else {
                write!(f, "{}::", assoc_ty)?;
            }
        }
        write!(f, "{}", self.fn_.name)?;
        if !self.env_gen_args.is_empty() {
            write!(f, "{{")?;
            for (i, ty) in self.env_gen_args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", ty)?;
            }
            write!(f, "}}")?;
        }
        if !self.gen_args.is_empty() {
            write!(f, "<")?;
            for (i, ty) in self.gen_args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", ty)?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl<'fns> FnInst<'fns> {
    pub fn new(
        fn_: Fn<'fns>,
        gen_args: TySlice<'fns>,
        env_gen_args: TySlice<'fns>,
    ) -> Result<FnInst<'fns>, FnInstError> {
        if fn_.gen_params.len() != gen_args.len() {
            return Err(FnInstError::GenArgCountMismatch {
                expected: fn_.gen_params.len(),
                actual: gen_args.len(),
            });
        }
        if fn_.env_gen_params.len() != env_gen_args.len() {
            return Err(FnInstError::EnvGenArgCountMismatch {
                expected: fn_.env_gen_params.len(),
                actual: env_gen_args.len(),
            });
        }
        Ok(FnInst {
            fn_,
            gen_args,
            env_gen_args,
            self_ty: None,
            _private: (),
        })
    }

    pub fn get_subst(&self) -> ty::GenVarSubst<'fns> {
        let gen_param_subst = ty::GenVarSubst::new(&self.fn_.gen_params, self.gen_args).unwrap();
        let env_gen_param_subst = ty::GenVarSubst::new(&self.fn_.env_gen_params, self.env_gen_args).unwrap();
        ty::GenVarSubst::compose(env_gen_param_subst, gen_param_subst)
    }

    pub fn with_self_ty(self, self_ty: Option<ty::Ty<'fns>>) -> Self {
        FnInst { self_ty, ..self }
    }

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
            self_ty: self.self_ty,
            _private: (),
        })
    }
}

#[derive(Debug)]
#[allow(unused)]
pub enum TraitMthdInstError<'fns> {
    GenArgCountMismatch {
        mthd: TraitMthd<'fns>,
        expected: usize,
        actual: usize,
    },
    TraitMismatch {
        expected: Trait<'fns>,
        actual: Trait<'fns>,
    },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct TraitMthdInst<'fns> {
    pub trait_inst: TraitInst<'fns>,
    pub mthd: TraitMthd<'fns>,
    pub impl_ty: Ty<'fns>,
    pub gen_args: TySlice<'fns>,
    _private: (),
}

impl<'fns> TraitMthdInst<'fns> {
    pub fn new(
        trait_inst: TraitInst<'fns>,
        mthd: TraitMthd<'fns>,
        impl_ty: Ty<'fns>,
        gen_args: TySlice<'fns>,
    ) -> Result<TraitMthdInst<'fns>, TraitMthdInstError<'fns>> {
        if mthd.fn_.gen_params.len() != gen_args.len() {
            return Err(TraitMthdInstError::GenArgCountMismatch {
                mthd,
                expected: mthd.fn_.gen_params.len(),
                actual: gen_args.len(),
            });
        }
        Ok(TraitMthdInst {
            trait_inst,
            mthd,
            impl_ty,
            gen_args,
            _private: (),
        })
    }

    pub fn with_updated(
        self,
        impl_ty: Ty<'fns>,
        trait_inst: TraitInst<'fns>,
        gen_args: TySlice<'fns>,
    ) -> Result<TraitMthdInst<'fns>, TraitMthdInstError<'fns>> {
        if trait_inst.trait_ != self.trait_inst.trait_ {
            return Err(TraitMthdInstError::TraitMismatch {
                expected: self.trait_inst.trait_,
                actual: trait_inst.trait_,
            });
        }
        if gen_args.len() != self.gen_args.len() {
            return Err(TraitMthdInstError::GenArgCountMismatch {
                mthd: self.mthd,
                expected: self.gen_args.len(),
                actual: gen_args.len(),
            });
        }
        Ok(TraitMthdInst {
            trait_inst,
            mthd: self.mthd,
            impl_ty,
            gen_args,
            _private: (),
        })
    }
}
