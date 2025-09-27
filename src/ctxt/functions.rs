use crate::ctxt::types::TypeId;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct FnId(pub usize);

pub struct FunctionSignature {
    pub name: String,
    pub return_type: TypeId,
    pub parameters: Vec<FunctionParameter>,
}

#[derive(Clone)]
pub struct FunctionParameter {
    pub name: String,
    pub type_: TypeId,
}
