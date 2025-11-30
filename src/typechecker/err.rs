use crate::ctxt::{NotAStructErr, NotAnEnumErr, fns::Fn, mlr, ty::Ty};

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
}

pub type TyResult<T> = Result<T, TyErr>;

impl<T> From<TyErr> for TyResult<T> {
    fn from(val: TyErr) -> Self {
        Err(val)
    }
}

impl From<NotAStructErr> for TyErr {
    fn from(val: NotAStructErr) -> Self {
        TyErr::NotAStruct { ty: val.ty }
    }
}

impl From<NotAnEnumErr> for TyErr {
    fn from(val: NotAnEnumErr) -> Self {
        TyErr::NotAnEnum { ty: val.ty }
    }
}

pub(super) fn into_ty_err<E>(err: E) -> TyErr
where
    TyErr: From<E>,
{
    From::from(err)
}
