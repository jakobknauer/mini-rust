#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Ty(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct StructId(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct EnumId(pub usize);

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TyDef {
    Named(String, Named),
    Fn { param_tys: Vec<Ty>, return_ty: Ty },
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Named {
    Primitve(Primitive),
    Struct(StructId),
    Enum(EnumId),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Primitive {
    Integer32,
    Boolean,
    Unit,
}

pub struct StructDefinition {
    pub fields: Vec<StructField>,
}

pub struct StructField {
    pub name: String,
    pub ty: Ty,
}

pub struct EnumDefinition {
    pub variants: Vec<EnumVariant>,
}

pub struct EnumVariant {
    pub name: String,
    pub ty: Ty,
}
