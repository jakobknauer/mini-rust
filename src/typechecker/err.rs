use crate::ctxt::{NotAStruct, NotAStructField, NotAnEnum, fns::Fn, mlr, ty::Ty};

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
    GenericArgCountMismatch {
        fn_: Fn,
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
