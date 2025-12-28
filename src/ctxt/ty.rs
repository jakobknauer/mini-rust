use crate::ctxt::{fns, traits};

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
    TraitSelf(traits::Trait),
    Closure {
        fn_spec: fns::FnSpecialization,
        name: String,
    },
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
    pub subject: GenVar,
    pub requirement: ConstraintRequirement,
}

#[derive(Clone, PartialEq, Eq)]
pub enum ConstraintRequirement {
    Trait(traits::Trait),
    Callable { param_tys: Vec<Ty>, return_ty: Ty },
}

#[derive(Clone)]
pub enum Obligation {
    ImplementsTrait { ty: Ty, trait_: traits::Trait },
    Callable { ty: Ty, param_tys: Vec<Ty>, return_ty: Ty },
}
