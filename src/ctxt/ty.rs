use std::cell::OnceCell;
use std::collections::HashMap;

use crate::ctxt::{fns, traits};

#[derive(Clone, Copy, Debug)]
pub struct Ty<'ty>(pub &'ty TyDef<'ty>, pub(in crate::ctxt) TyId);

impl<'ty> PartialEq for Ty<'ty> {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
    }
}
impl<'ty> Eq for Ty<'ty> {}
impl<'ty> std::hash::Hash for Ty<'ty> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.1.hash(state);
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct TyId(pub(in crate::ctxt) usize);

pub type TySlice<'ty> = &'ty [Ty<'ty>];

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TyDef<'ty> {
    Primitive(Primitive),
    Tuple(TySlice<'ty>),
    Struct {
        struct_: Struct<'ty>,
        gen_args: TySlice<'ty>,
    },
    Enum {
        enum_: Enum<'ty>,
        gen_args: TySlice<'ty>,
    },
    Fn {
        param_tys: TySlice<'ty>,
        return_ty: Ty<'ty>,
        var_args: bool,
    },
    Ref(Ty<'ty>),
    Ptr(Ty<'ty>),
    GenVar(GenVar<'ty>),
    TraitSelf(traits::Trait),
    Closure {
        fn_inst: fns::FnInst<'ty>,
        name: String,
        captures_ty: Ty<'ty>,
    },
    AssocTy {
        base_ty: Ty<'ty>,
        trait_inst: traits::TraitInst<'ty>,
        assoc_ty_idx: usize,
    },
    InfVar(InfVar),
    Opaque {
        opaque: Opaque<'ty>,
        gen_args: TySlice<'ty>,
    },
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Primitive {
    Integer32,
    Boolean,
    CVoid,
    CChar,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct StructId(pub(in crate::ctxt) usize);

pub type Struct<'ty> = &'ty StructDef<'ty>;

#[derive(Debug)]
pub struct StructDef<'ty> {
    pub name: String,
    pub gen_params: Vec<GenVar<'ty>>,
    pub(in crate::ctxt) fields: OnceCell<&'ty [StructField<'ty>]>,
    pub(in crate::ctxt) id: StructId,
}

impl<'ty> StructDef<'ty> {
    pub fn get_fields(&self) -> &[StructField<'ty>] {
        self.fields.get().copied().expect("struct fields not yet defined")
    }
}

impl PartialEq for StructDef<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Eq for StructDef<'_> {}
impl std::hash::Hash for StructDef<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

#[derive(Clone, Debug)]
pub struct StructField<'ty> {
    pub name: String,
    pub ty: Ty<'ty>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct EnumId(pub(in crate::ctxt) usize);

pub type Enum<'ty> = &'ty EnumDef<'ty>;

#[derive(Debug)]
pub struct EnumDef<'ty> {
    pub name: String,
    pub gen_params: Vec<GenVar<'ty>>,
    pub(in crate::ctxt) variants: OnceCell<&'ty [EnumVariant<'ty>]>,
    pub(in crate::ctxt) id: EnumId,
}

impl<'ty> EnumDef<'ty> {
    pub fn get_variants(&self) -> &[EnumVariant<'ty>] {
        self.variants.get().copied().expect("enum variants not yet defined")
    }
}

impl PartialEq for EnumDef<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Eq for EnumDef<'_> {}
impl std::hash::Hash for EnumDef<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

#[derive(Clone, Debug)]
pub struct EnumVariant<'ty> {
    pub name: String,
    pub struct_: Struct<'ty>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct OpaqueId(pub(in crate::ctxt) usize);

pub type Opaque<'ty> = &'ty OpaqueDef<'ty>;

#[derive(Debug)]
pub struct OpaqueDef<'ty> {
    pub gen_params: Vec<GenVar<'ty>>,
    pub constraints: Vec<ConstraintRequirement<'ty>>,
    pub(in crate::ctxt) resolution: OnceCell<Ty<'ty>>,
    pub(in crate::ctxt) id: OpaqueId,
}

impl PartialEq for OpaqueDef<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Eq for OpaqueDef<'_> {}
impl std::hash::Hash for OpaqueDef<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

#[derive(Clone, Copy, Debug)]
pub struct GenVar<'ty>(pub(in crate::ctxt) usize, pub(in crate::ctxt) &'ty str);

impl<'ty> GenVar<'ty> {
    pub fn name(self) -> &'ty str {
        self.1
    }
}

impl PartialEq for GenVar<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl Eq for GenVar<'_> {}
impl std::hash::Hash for GenVar<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

#[derive(Clone)]
pub struct GenVarSubst<'ty>(HashMap<GenVar<'ty>, Ty<'ty>>);

impl<'ty> GenVarSubst<'ty> {
    pub fn new<G, T>(gen_vars: G, tys: T) -> Option<GenVarSubst<'ty>>
    where
        G: IntoIterator<Item: std::borrow::Borrow<GenVar<'ty>>, IntoIter: ExactSizeIterator>,
        T: IntoIterator<Item: std::borrow::Borrow<Ty<'ty>>, IntoIter: ExactSizeIterator>,
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

    pub fn get(&self, gen_var: GenVar<'ty>) -> Option<Ty<'ty>> {
        self.0.get(&gen_var).copied()
    }

    pub fn compose(self, other: GenVarSubst<'ty>) -> GenVarSubst<'ty> {
        let pairs = self.0.into_iter().chain(other.0).collect();
        GenVarSubst(pairs)
    }
}

#[derive(Clone, Copy, Default, PartialEq, Eq, Hash, Debug)]
pub struct InfVar(pub(in crate::ctxt) usize);

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Constraint<'ty> {
    pub subject: Ty<'ty>,
    pub requirement: ConstraintRequirement<'ty>,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ConstraintRequirement<'ty> {
    Trait(traits::TraitInst<'ty>),
    Callable {
        param_tys: TySlice<'ty>,
        return_ty: Ty<'ty>,
    },
    AssocTyEq(Ty<'ty>),
}
