use std::collections::HashMap;

use crate::ctxt::{fns, traits};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Ty<'ty>(
    pub(in crate::ctxt) usize,
    pub(in crate::ctxt) std::marker::PhantomData<&'ty ()>,
);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct TySlice<'ty> {
    pub offset: usize,
    pub len: usize,
    pub(in crate::ctxt) _phantom: std::marker::PhantomData<&'ty ()>,
}

impl<'ty> TySlice<'ty> {
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Struct(pub(in crate::ctxt) usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Enum(pub(in crate::ctxt) usize);

impl std::fmt::Display for Enum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct GenVar(pub(in crate::ctxt) usize);

#[derive(Clone, Copy, Default, PartialEq, Eq, Hash, Debug)]
pub struct InfVar(pub(in crate::ctxt) usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct OpaqueId(pub(in crate::ctxt) usize);

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TyDef<'ty> {
    Primitive(Primitive),
    Tuple(TySlice<'ty>),
    Struct {
        struct_: Struct,
        gen_args: TySlice<'ty>,
    },
    Enum {
        enum_: Enum,
        gen_args: TySlice<'ty>,
    },
    Fn {
        param_tys: TySlice<'ty>,
        return_ty: Ty<'ty>,
        var_args: bool,
    },
    Ref(Ty<'ty>),
    Ptr(Ty<'ty>),
    GenVar(GenVar),
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
        id: OpaqueId,
        gen_args: TySlice<'ty>,
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
pub struct StructDef<'ty> {
    pub name: String,
    pub gen_params: Vec<GenVar>,
    pub fields: Vec<StructField<'ty>>,
}

#[derive(Clone)]
pub struct StructField<'ty> {
    pub name: String,
    pub ty: Ty<'ty>,
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
pub struct OpaqueDef<'ty> {
    pub gen_params: Vec<GenVar>,
    pub constraints: Vec<ConstraintRequirement<'ty>>,
}

#[derive(Clone)]
pub struct Constraint<'ty> {
    pub subject: Ty<'ty>,
    pub requirement: ConstraintRequirement<'ty>,
}

#[derive(Clone, PartialEq, Eq)]
pub enum ConstraintRequirement<'ty> {
    Trait(traits::TraitInst<'ty>),
    Callable {
        param_tys: TySlice<'ty>,
        return_ty: Ty<'ty>,
    },
    AssocTyEq(Ty<'ty>),
}

#[derive(Clone)]
pub struct GenVarSubst<'ty>(HashMap<GenVar, Ty<'ty>>);

impl<'ty> GenVarSubst<'ty> {
    pub fn new<G, T>(gen_vars: G, tys: T) -> Option<GenVarSubst<'ty>>
    where
        G: IntoIterator<Item: std::borrow::Borrow<GenVar>, IntoIter: ExactSizeIterator>,
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

    pub fn get(&self, gen_var: GenVar) -> Option<Ty<'ty>> {
        self.0.get(&gen_var).copied()
    }

    pub fn compose(self, other: GenVarSubst<'ty>) -> GenVarSubst<'ty> {
        let pairs = self.0.into_iter().chain(other.0).collect();
        GenVarSubst(pairs)
    }
}

macro_rules! iter_ty_slice {
    ($self:expr, $tys:expr, $adapter:ident(|$a:ident| $body:expr)) => {{
        let len = $tys.len;
        (0..len).$adapter(|idx| {
            let $a = ($self.get_ty_slice($tys))[idx];
            $body
        })
    }};
}

pub(crate) use iter_ty_slice;

macro_rules! zip_ty_slices {
    // $adapter is the iterator method: all, any, try_for_each, etc.
    ($self:expr, ($tys1:expr, $tys2:expr), $adapter:ident(|$a:ident, $b:ident| $body:expr)) => {{
        let len = $tys1.len;
        (0..len).$adapter(|idx| {
            let $a = ($self.get_ty_slice($tys1))[idx];
            let $b = ($self.get_ty_slice($tys2))[idx];
            $body
        })
    }};
}

pub(crate) use zip_ty_slices;
