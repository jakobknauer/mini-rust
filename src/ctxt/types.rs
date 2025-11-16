#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct TypeId(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct StructId(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct EnumId(pub usize);

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    NamedType(String, NamedType),
    Fn {
        param_types: Vec<TypeId>,
        return_type: TypeId,
    },
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum NamedType {
    Primitve(PrimitiveType),
    Struct(StructId),
    Enum(EnumId),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Integer32,
    Boolean,
    Unit,
}

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
    pub type_id: TypeId,
}
