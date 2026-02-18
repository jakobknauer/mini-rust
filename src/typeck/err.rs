use crate::ctxt::{fns, ty};

pub type TypeckResult<T> = Result<T, TypeckError>;

pub enum TypeckError {
    ReturnTypeMismatch {
        expected: ty::Ty,
        actual: ty::Ty,
    },
    FnGenArgCountMismatch {
        fn_: fns::Fn,
        expected: usize,
        actual: usize,
    },
}
