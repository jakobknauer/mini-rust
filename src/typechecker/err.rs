use crate::ctxt::{
    NotAStruct, NotAStructField, NotAnEnum, TyInstError,
    fns::Fn,
    mlr,
    traits::Trait,
    ty::{Enum, GenVar, Struct, Ty},
};

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
        var_args: bool,
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
    NotAStructOrTuple {
        ty: Ty,
    },
    NotATuple {
        ty: Ty,
    },
    InvalidTupleIndex {
        ty: Ty,
        index: usize,
    },
    InvalidDereference {
        ty: Ty,
    },
    FnGenericArgCountMismatch {
        fn_: Fn,
        expected: usize,
        actual: usize,
    },
    FnEnvGenericArgCountMismatch {
        fn_: Fn,
        expected: usize,
        actual: usize,
    },
    StructGenericArgCountMismatch {
        struct_: Struct,
        expected: usize,
        actual: usize,
    },
    EnumGenericArgCountMismatch {
        enum_: Enum,
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
    InvalidAsExpr {
        op_ty: Ty,
        target_ty: Ty,
    },
    DereferenceOfCVoidPtr {
        ty: Ty,
    },
    AmbiguousMthd {
        base_ty: Ty,
        mthd_name: String,
    },
    MthdResolutionFailed {
        base_ty: Ty,
        mthd_name: String,
    },
    TraitMthdGenericArgCountMismatch {
        trait_: Trait,
        mthd_idx: usize,
        impl_ty: Ty,
        expected: usize,
        actual: usize,
    },
    UnfulfilledConstraint {
        fn_: Fn,
        gen_var: GenVar,
        constraint: Trait,
        gen_arg: Ty,
    },
    ClosureMismatchWithExpected,
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

impl From<TyInstError> for TyError {
    fn from(err: TyInstError) -> Self {
        match err {
            TyInstError::StructGenericArgCountMismatch {
                struct_,
                expected,
                actual,
            } => TyError::StructGenericArgCountMismatch {
                struct_,
                expected,
                actual,
            },
            TyInstError::EnumGenericArgCountMismatch {
                enum_,
                expected,
                actual,
            } => TyError::EnumGenericArgCountMismatch {
                enum_,
                expected,
                actual,
            },
        }
    }
}
