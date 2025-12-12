use crate::ctxt::{NotAStruct, NotAStructField, NotAnEnum, TyInstantiationError, fns::Fn, mlr, ty::Ty};

pub type TyResult<T> = Result<T, TyError>;

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
    DereferenceOfNonRefTy {
        ty: Ty,
    },
    FnGenericArgCountMismatch {
        fn_: Fn,
        expected: usize,
        actual: usize,
    },
    TyGenericArgCountMismatch {
        ty: Ty,
        expected: usize,
        actual: usize,
    },
    MissingVariants {
        ty: Ty,
        missing_variants: Vec<String>,
    },
    ExtraVariants {
        ty: Ty,
        extra_variants: Vec<String>,
    },
}

impl<T> From<TyError> for TyResult<T> {
    fn from(val: TyError) -> Self {
        Err(val)
    }
}

impl From<NotAStruct> for TyError {
    fn from(NotAStruct(ty): NotAStruct) -> Self {
        TyError::NotAStruct { ty }
    }
}

impl From<NotAnEnum> for TyError {
    fn from(NotAnEnum(ty): NotAnEnum) -> Self {
        TyError::NotAnEnum { ty }
    }
}

impl From<NotAStructField> for TyError {
    fn from(err: NotAStructField) -> Self {
        match err {
            NotAStructField::NotAStruct(ty) => TyError::NotAStruct { ty },
            NotAStructField::NotAFieldName(ty, field_name) => TyError::NotAStructField { ty, field_name },
        }
    }
}

impl From<TyInstantiationError> for TyError {
    fn from(err: TyInstantiationError) -> Self {
        match err {
            TyInstantiationError::NotAStruct(ty) => TyError::NotAStruct { ty },
            TyInstantiationError::NotAnEnum(ty) => TyError::NotAnEnum { ty },
            TyInstantiationError::GenericArgCountMismatch { ty, expected, actual } => {
                TyError::TyGenericArgCountMismatch { ty, expected, actual }
            }
        }
    }
}
