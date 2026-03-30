use crate::{
    ctxt::{fns, traits, ty},
    hlr,
};

pub type TypeckResult<'ty, T> = Result<T, TypeckError<'ty>>;

#[derive(Debug)]
#[allow(unused)]
pub enum TypeckError<'ctxt> {
    ReturnTypeMismatch {
        expected: ty::Ty<'ctxt>,
        actual: ty::Ty<'ctxt>,
    },
    FnGenArgCountMismatch {
        fn_: fns::Fn<'ctxt>,
        expected: usize,
        actual: usize,
    },
    StructGenArgCountMismatch {
        struct_: ty::Struct<'ctxt>,
        expected: usize,
        actual: usize,
    },
    EnumGenArgCountMismatch {
        enum_: ty::Enum<'ctxt>,
        expected: usize,
        actual: usize,
    },
    UnresolvableAssocTy {
        base: ty::Ty<'ctxt>,
        name: String,
    },
    MthdResolutionFailed {
        base_ty: ty::Ty<'ctxt>,
        mthd_name: String,
    },
    AmbiguousMthd {
        base_ty: ty::Ty<'ctxt>,
        mthd_name: String,
    },
    MthdGenArgCountMismatch {
        mthd_name: String,
        expected: usize,
        actual: usize,
    },
    NamedFieldAccessOnNonStruct {
        ty: ty::Ty<'ctxt>,
    },
    IndexedFieldAccessOnNonTuple {
        ty: ty::Ty<'ctxt>,
    },
    StructFieldNotFound {
        struct_ty: ty::Ty<'ctxt>,
        field: String,
    },
    StructFieldTypeMismatch {
        struct_ty: ty::Ty<'ctxt>,
        field_idx: usize,
        expected: ty::Ty<'ctxt>,
        actual: ty::Ty<'ctxt>,
    },
    CalleeNotCallable {
        ty: ty::Ty<'ctxt>,
    },
    CallArgCountMismatch {
        expected: usize,
        actual: usize,
        var_args: bool,
    },
    CallArgTypeMismatch {
        index: usize,
        expected: ty::Ty<'ctxt>,
        actual: ty::Ty<'ctxt>,
    },
    BinaryOpTypeMismatch {
        operator: hlr::BinaryOperator,
        left_ty: ty::Ty<'ctxt>,
        right_ty: ty::Ty<'ctxt>,
    },
    ArithTraitNotImplemented {
        operator: hlr::BinaryOperator,
        left_ty: ty::Ty<'ctxt>,
        right_ty: ty::Ty<'ctxt>,
    },
    UnaryOpTypeMismatch {
        operator: hlr::UnaryOperator,
        operand_ty: ty::Ty<'ctxt>,
    },
    AssignmentTargetNotAPlace,
    AssignmentTypeMismatch {
        expected: ty::Ty<'ctxt>,
        actual: ty::Ty<'ctxt>,
    },
    DereferenceOfNonRef {
        ty: ty::Ty<'ctxt>,
    },
    DereferenceOfCVoid {
        ty: ty::Ty<'ctxt>,
    },
    InvalidAsConversion {
        op_ty: ty::Ty<'ctxt>,
        target_ty: ty::Ty<'ctxt>,
    },
    IfConditionNotBoolean,
    IfBranchesTypeMismatch {
        then_ty: ty::Ty<'ctxt>,
        else_ty: ty::Ty<'ctxt>,
    },
    NonMatchableScrutinee {
        ty: ty::Ty<'ctxt>,
    },
    MatchArmWrongEnum {
        expected: ty::Ty<'ctxt>,
        found: ty::Ty<'ctxt>,
    },
    MatchArmTypeMismatch {
        expected: ty::Ty<'ctxt>,
        actual: ty::Ty<'ctxt>,
    },
    TraitGenArgCountMismatch {
        trait_: traits::Trait<'ctxt>,
        expected: usize,
        actual: usize,
    },
    ReturnExprTypeMismatch {
        expected: ty::Ty<'ctxt>,
        actual: ty::Ty<'ctxt>,
    },
    LetTypeMismatch {
        expected: ty::Ty<'ctxt>,
        actual: ty::Ty<'ctxt>,
    },
    VarTypeNotSet {
        var_id: hlr::VarId,
        expr_id: hlr::ExprId,
    },
    ConstraintNotSatisfied {
        ty: ty::Ty<'ctxt>,
        trait_inst: traits::TraitInst<'ctxt>,
    },
    CallableConstraintNotSatisfied {
        ty: ty::Ty<'ctxt>,
        expected_param_tys: ty::TySlice<'ctxt>,
        expected_return_ty: ty::Ty<'ctxt>,
    },
    AssocTyEqNotSatisfied {
        subject: ty::Ty<'ctxt>,
        expected: ty::Ty<'ctxt>,
    },
    NonAssignablePlace(hlr::Expr<'ctxt>),
}
