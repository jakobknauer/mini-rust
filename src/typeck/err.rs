use crate::ctxt::ty;

pub type TypeckResult<T> = Result<T, TypeckError>;

pub enum TypeckError {
    ReturnTypeMismatch { expected: ty::Ty, actual: ty::Ty },
}
