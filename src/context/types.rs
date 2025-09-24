#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct TypeId(pub usize);

#[derive(Clone, Copy)]
pub enum Type {
    NamedType(NamedType),
}

#[derive(Clone, Copy)]
pub enum NamedType {
    Primitve(PrimitiveType),
    Struct(StructId),
    Enum(EnumId),
}

#[derive(Clone, Copy)]
pub enum PrimitiveType {
    Integer32,
    Boolean,
    Unit,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructId(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct EnumId(pub usize);

pub struct StructDefinition {
    pub fields: Vec<StructField>,
}

pub struct StructField {
    pub name: String,
    pub type_id: TypeId,
}

pub struct EnumDefinition {
    pub variants: Vec<EnumVariant>,
}

pub struct EnumVariant {
    pub name: String,
}
