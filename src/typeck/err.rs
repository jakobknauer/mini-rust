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
}
