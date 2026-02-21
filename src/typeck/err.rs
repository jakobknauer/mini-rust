use crate::{
    ctxt::{fns, ty},
    hlr,
};

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
    StructGenArgCountMismatch {
        struct_: ty::Struct,
        expected: usize,
        actual: usize,
    },
    EnumGenArgCountMismatch {
        enum_: ty::Enum,
        expected: usize,
        actual: usize,
    },
    UnresolvableAssocTy {
        base: ty::Ty,
        name: String,
    },
    MthdResolutionFailed {
        base_ty: ty::Ty,
        mthd_name: String,
    },
    AmbiguousMthd {
        base_ty: ty::Ty,
        mthd_name: String,
    },
    MthdGenArgCountMismatch {
        mthd_name: String,
        expected: usize,
        actual: usize,
    },
    NamedFieldAccessOnNonStruct {
        ty: ty::Ty,
    },
    IndexedFieldAccessOnNonTuple {
        ty: ty::Ty,
    },
    StructFieldNotFound {
        struct_ty: ty::Ty,
        field: String,
    },
    StructFieldTypeMismatch {
        struct_ty: ty::Ty,
        field_idx: usize,
        expected: ty::Ty,
        actual: ty::Ty,
    },
    CalleeNotCallable {
        ty: ty::Ty,
    },
    CallArgCountMismatch {
        expected: usize,
        actual: usize,
        var_args: bool,
    },
    CallArgTypeMismatch {
        index: usize,
        expected: ty::Ty,
        actual: ty::Ty,
    },
    BinaryOpTypeMismatch {
        operator: hlr::BinaryOperator,
        left_ty: ty::Ty,
        right_ty: ty::Ty,
    },
    UnaryOpTypeMismatch {
        operator: hlr::UnaryOperator,
        operand_ty: ty::Ty,
    },
    AssignmentTypeMismatch {
        expected: ty::Ty,
        actual: ty::Ty,
    },
    DereferenceOfNonRef {
        ty: ty::Ty,
    },
    DereferenceOfCVoid {
        ty: ty::Ty,
    },
    InvalidAsConversion {
        op_ty: ty::Ty,
        target_ty: ty::Ty,
    },
    IfConditionNotBoolean,
    IfBranchesTypeMismatch {
        then_ty: ty::Ty,
        else_ty: ty::Ty,
    },
    NonMatchableScrutinee {
        ty: ty::Ty,
    },
    MatchArmWrongEnum {
        expected: ty::Ty,
        found: ty::Ty,
    },
    MatchArmTypeMismatch {
        expected: ty::Ty,
        actual: ty::Ty,
    },
}
