use crate::{ctxt::ty::Ty, mlr};

#[derive(Debug)]
pub enum MlrBuilderError {
    MissingOperatorImpl { name: String },
    UnresolvableSymbol { name: String },
    UnknownPrimitiveTy,
    TyError(TyError),
}

#[derive(Debug)]
pub enum TyError {
    AssignStmtTyMismatch {
        place: mlr::Place,
        expected: Ty,
        actual: Ty,
    },
    ValNotCallable,
    CallArgumentTyMismatch {
        index: usize,
        expected: Ty,
        actual: Ty,
    },
    CallArgumentCountMismatch {
        expected: usize,
        actual: usize,
    },
    IfConditionNotBoolean {
        actual: Ty,
    },
    IfBranchTyMismatch {
        then_ty: Ty,
        else_ty: Ty,
    },
    ReturnTyMismatch {
        expected: Ty,
        actual: Ty,
    },
    OperatorResolutionFailed {
        operator: String,
        operand_tys: (Ty, Ty),
    },
    UnresolvableTyName {
        ty_name: String,
    },
    NotAStruct {
        ty: Ty,
    },
    InitializerMissingFields {
        ty: Ty,
        missing_fields: Vec<String>,
    },
    InitializerExtraFields {
        ty: Ty,
        extra_fields: Vec<String>,
    },
    NotAStructField {
        ty: Ty,
        field_name: String,
    },
    NotAnEnum {
        ty: Ty,
    },
    NotAnEnumVariant {
        ty: Ty,
        variant_name: String,
    },
    UnresolvableTyAnnot,
}

pub type Result<T> = std::result::Result<T, MlrBuilderError>;

impl<T> From<MlrBuilderError> for Result<T> {
    fn from(val: MlrBuilderError) -> Self {
        Err(val)
    }
}

impl<T> From<TyError> for Result<T> {
    fn from(val: TyError) -> Self {
        MlrBuilderError::TyError(val).into()
    }
}
