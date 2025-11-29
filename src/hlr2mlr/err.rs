use crate::ctxt::{fns::Fn, mlr, ty::Ty};

#[derive(Debug)]
pub enum Hlr2MlrErr {
    MissingOperatorImpl { name: String },
    UnresolvableSymbol { name: String },
    UnknownPrimitiveTy,
    NotAPlace,
    TyErr(TyErr),
}

#[derive(Debug)]
pub enum TyErr {
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
    DereferenceOfNonRefTy {
        ty: Ty,
    },
    GenericArgCountMismatch {
        fn_: Fn,
        expected: usize,
        actual: usize,
    },
}

pub type Result<T> = std::result::Result<T, Hlr2MlrErr>;

impl<T> From<Hlr2MlrErr> for Result<T> {
    fn from(val: Hlr2MlrErr) -> Self {
        Err(val)
    }
}

impl<T> From<TyErr> for Result<T> {
    fn from(val: TyErr) -> Self {
        Hlr2MlrErr::TyErr(val).into()
    }
}
