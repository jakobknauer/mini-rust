use crate::context::types::TypeId;

pub struct FunctionSignature {
    pub return_type: TypeId,
    pub parameters: Vec<FunctionParameter>,
}

pub struct FunctionParameter {
    pub name: String,
    pub type_: TypeId,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct FnId(pub usize);
