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
}
