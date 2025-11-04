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
    AssignStmtTypeMismatch {
        place: mlr::PlaceId,
        expected: ctxt::types::TypeId,
        actual: ctxt::types::TypeId,
    },
    ValNotCallable,
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
    OperatorResolutionFailed {
        operator: String,
        operand_types: (ctxt::types::TypeId, ctxt::types::TypeId),
    },
    UnresolvableTypeName {
        type_name: String,
    },
    NotAStruct {
        type_id: ctxt::types::TypeId,
    },
    InitializerMissingFields {
        type_id: ctxt::types::TypeId,
        missing_fields: Vec<String>,
    },
    InitializerExtraFields {
        type_id: ctxt::types::TypeId,
        extra_fields: Vec<String>,
    },
    NotAStructField {
        type_id: ctxt::types::TypeId,
        field_name: String,
    },
    NotAnEnum {
        type_id: ctxt::types::TypeId,
    },
    NotAnEnumVariant {
        type_id: ctxt::types::TypeId,
        variant_name: String,
    },
}

pub type Result<T> = std::result::Result<T, MlrBuilderError>;

impl<T> From<MlrBuilderError> for Result<T> {
    fn from(val: MlrBuilderError) -> Self {
        Err(val)
    }
}

impl<T> From<TypeError> for Result<T> {
    fn from(val: TypeError) -> Self {
        MlrBuilderError::TypeError(val).into()
    }
}
