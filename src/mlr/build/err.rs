use crate::{ctxt, mlr};

#[derive(Debug)]
pub enum MlrBuilderError {
    MissingOperatorImpl { name: String },
    UnresolvableSymbol { name: String },
    UnknownPrimitiveType,
    TypeError(TypeError),
}

#[derive(Debug)]
pub enum TypeError {
    ReassignTypeMismatch {
        loc: mlr::LocId,
        expected: ctxt::types::TypeId,
        actual: ctxt::types::TypeId,
    },
    ExpressionNotCallable,
    CallArgumentTypeMismatch {
        index: usize,
        expected: ctxt::types::TypeId,
        actual: ctxt::types::TypeId,
    },
    CallArgumentCountMismatch {
        expected: usize,
        actual: usize,
    },
    IfConditionNotBoolean {
        actual: ctxt::types::TypeId,
    },
    IfBranchTypeMismatch {
        then_type: ctxt::types::TypeId,
        else_type: ctxt::types::TypeId,
    },
    ReturnTypeMismatch {
        expected: ctxt::types::TypeId,
        actual: ctxt::types::TypeId,
    },
}

impl<T> Into<Result<T>> for MlrBuilderError {
    fn into(self) -> Result<T> {
        Err(self)
    }
}

impl<T> Into<Result<T>> for TypeError {
    fn into(self) -> Result<T> {
        MlrBuilderError::TypeError(self).into()
    }
}

pub type Result<T> = std::result::Result<T, MlrBuilderError>;
