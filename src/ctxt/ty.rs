#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Ty(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Struct(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Enum(pub usize);

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TyDef {
    Primitve(Primitive),
    Struct(Struct),
    Enum(Enum),
    Fn { param_tys: Vec<Ty>, return_ty: Ty },
    Ref(Ty),
    Alias(Ty),
    GenVar(String),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Primitive {
    Integer32,
    Boolean,
    Unit,
}

#[derive(Clone)]
pub struct StructDef {
    pub name: String,
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
    pub variants: Vec<EnumVariant>,
}

#[derive(Clone)]
pub struct EnumVariant {
    pub name: String,
    pub ty: Ty,
}
