use std::collections::HashMap;

use crate::ctxt::{fns, traits};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Ty(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct TySlice(pub usize, pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Struct(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Enum(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct GenVar(pub usize);

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TyDef {
    Primitive(Primitive),
    Tuple(TySlice),
    Struct {
        struct_: Struct,
        gen_args: Vec<Ty>,
    },
    Enum {
        enum_: Enum,
        gen_args: Vec<Ty>,
    },
    Fn {
        param_tys: Vec<Ty>,
        return_ty: Ty,
        var_args: bool,
    },
    Ref(Ty),
    Ptr(Ty),
    Alias(Ty),
    GenVar(GenVar),
    TraitSelf(traits::Trait),
    Closure {
        fn_inst: fns::FnInst,
        name: String,
        captures_ty: Ty,
    },
    AssocTy {
        base_ty: Ty,
        trait_inst: traits::TraitInst,
        assoc_ty_idx: usize,
    },
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Primitive {
    Integer32,
    Boolean,
    CVoid,
    CChar,
}

#[derive(Clone)]
pub struct StructDef {
    pub name: String,
    pub gen_params: Vec<GenVar>,
    pub fields: Vec<StructField>,
}

#[derive(Clone)]
pub struct StructField {
    pub name: String,
    pub ty: Ty,
}

#[derive(Clone)]
pub struct EnumDef {
    pub name: String,
    pub gen_params: Vec<GenVar>,
    pub variants: Vec<EnumVariant>,
}

#[derive(Clone)]
pub struct EnumVariant {
    pub name: String,
    pub struct_: Struct,
}

#[derive(Clone)]
pub struct Constraint {
    pub subject: GenVar,
    pub requirement: ConstraintRequirement,
}

#[derive(Clone, PartialEq, Eq)]
pub enum ConstraintRequirement {
    Trait(traits::TraitInst),
    Callable { param_tys: Vec<Ty>, return_ty: Ty },
}

#[derive(Clone)]
pub enum Obligation {
    ImplementsTraitInst { ty: Ty, trait_inst: traits::TraitInst },
    Callable { ty: Ty, param_tys: Vec<Ty>, return_ty: Ty },
}

#[derive(Clone)]
pub struct GenVarSubst(HashMap<GenVar, Ty>);

impl GenVarSubst {
    pub fn new<G, T>(gen_vars: G, tys: T) -> Option<GenVarSubst>
    where
        G: IntoIterator<Item: std::borrow::Borrow<GenVar>, IntoIter: ExactSizeIterator>,
        T: IntoIterator<Item: std::borrow::Borrow<Ty>, IntoIter: ExactSizeIterator>,
    {
        use std::borrow::Borrow;

        let gen_vars = gen_vars.into_iter();
        let tys = tys.into_iter();
        if gen_vars.len() != tys.len() {
            return None;
        }

        let pairs = gen_vars.zip(tys).map(|(gv, ty)| (*gv.borrow(), *ty.borrow())).collect();

        Some(GenVarSubst(pairs))
    }

    pub fn get(&self, gen_var: GenVar) -> Option<Ty> {
        self.0.get(&gen_var).copied()
    }

    pub fn compose(self, other: GenVarSubst) -> GenVarSubst {
        let pairs = self.0.into_iter().chain(other.0).collect();
        GenVarSubst(pairs)
    }
}
