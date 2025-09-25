use crate::ctxt::types::TypeId;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct FnId(pub usize);

pub struct FunctionSignature {
    pub name: String,
    pub return_type: TypeId,
    pub parameters: Vec<FunctionParameter>,
}

pub struct FunctionParameter {
    pub name: String,
    pub type_: TypeId,
}
