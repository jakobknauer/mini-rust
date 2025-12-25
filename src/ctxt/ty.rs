use crate::ctxt::{fns::Fn, traits::Trait};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Ty(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Struct(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Enum(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct GenVar(pub usize);

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TyDef {
    Primitive(Primitive),
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
    TraitSelf(Trait),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Primitive {
    Integer32,
    Boolean,
    Unit,
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
    pub gen_var: GenVar,
    pub trait_: Trait,
}

#[derive(Clone)]
pub struct Obligation {
    pub ty: Ty,
    pub trait_: Trait,
    pub fn_: Fn,
}
