use crate::{
    ctxt::{fns, traits, ty},
    hlr,
};

pub type TypeckResult<'ty, T> = Result<T, TypeckError<'ty>>;

#[derive(Debug)]
#[allow(unused)]
pub enum TypeckError<'ty> {
    ReturnTypeMismatch {
        expected: ty::Ty<'ty>,
        actual: ty::Ty<'ty>,
    },
    FnGenArgCountMismatch {
        fn_: fns::Fn<'ty>,
        expected: usize,
        actual: usize,
    },
    StructGenArgCountMismatch {
        struct_: ty::Struct<'ty>,
        expected: usize,
        actual: usize,
    },
    EnumGenArgCountMismatch {
        enum_: ty::Enum<'ty>,
        expected: usize,
        actual: usize,
    },
    UnresolvableAssocTy {
        base: ty::Ty<'ty>,
        name: String,
    },
    MthdResolutionFailed {
        base_ty: ty::Ty<'ty>,
        mthd_name: String,
    },
    AmbiguousMthd {
        base_ty: ty::Ty<'ty>,
        mthd_name: String,
    },
    MthdGenArgCountMismatch {
        mthd_name: String,
        expected: usize,
        actual: usize,
    },
    NamedFieldAccessOnNonStruct {
        ty: ty::Ty<'ty>,
    },
    IndexedFieldAccessOnNonTuple {
        ty: ty::Ty<'ty>,
    },
    StructFieldNotFound {
        struct_ty: ty::Ty<'ty>,
        field: String,
    },
    StructFieldTypeMismatch {
        struct_ty: ty::Ty<'ty>,
        field_idx: usize,
        expected: ty::Ty<'ty>,
        actual: ty::Ty<'ty>,
    },
    CalleeNotCallable {
        ty: ty::Ty<'ty>,
    },
    CallArgCountMismatch {
        expected: usize,
        actual: usize,
        var_args: bool,
    },
    CallArgTypeMismatch {
        index: usize,
        expected: ty::Ty<'ty>,
        actual: ty::Ty<'ty>,
    },
    BinaryOpTypeMismatch {
        operator: hlr::BinaryOperator,
        left_ty: ty::Ty<'ty>,
        right_ty: ty::Ty<'ty>,
    },
    ArithTraitNotImplemented {
        operator: hlr::BinaryOperator,
        left_ty: ty::Ty<'ty>,
        right_ty: ty::Ty<'ty>,
    },
    UnaryOpTypeMismatch {
        operator: hlr::UnaryOperator,
        operand_ty: ty::Ty<'ty>,
    },
    AssignmentTargetNotAPlace,
    AssignmentTypeMismatch {
        expected: ty::Ty<'ty>,
        actual: ty::Ty<'ty>,
    },
    DereferenceOfNonRef {
        ty: ty::Ty<'ty>,
    },
    DereferenceOfCVoid {
        ty: ty::Ty<'ty>,
    },
    InvalidAsConversion {
        op_ty: ty::Ty<'ty>,
        target_ty: ty::Ty<'ty>,
    },
    IfConditionNotBoolean,
    IfBranchesTypeMismatch {
        then_ty: ty::Ty<'ty>,
        else_ty: ty::Ty<'ty>,
    },
    NonMatchableScrutinee {
        ty: ty::Ty<'ty>,
    },
    MatchArmWrongEnum {
        expected: ty::Ty<'ty>,
        found: ty::Ty<'ty>,
    },
    MatchArmTypeMismatch {
        expected: ty::Ty<'ty>,
        actual: ty::Ty<'ty>,
    },
    TraitGenArgCountMismatch {
        trait_: traits::Trait<'ty>,
        expected: usize,
        actual: usize,
    },
    ReturnExprTypeMismatch {
        expected: ty::Ty<'ty>,
        actual: ty::Ty<'ty>,
    },
    LetTypeMismatch {
        expected: ty::Ty<'ty>,
        actual: ty::Ty<'ty>,
    },
    VarTypeNotSet {
        var_id: hlr::VarId,
        expr_id: hlr::ExprId,
    },
    ConstraintNotSatisfied {
        ty: ty::Ty<'ty>,
        trait_inst: traits::TraitInst<'ty>,
    },
    CallableConstraintNotSatisfied {
        ty: ty::Ty<'ty>,
        expected_param_tys: ty::TySlice<'ty>,
        expected_return_ty: ty::Ty<'ty>,
    },
    AssocTyEqNotSatisfied {
        subject: ty::Ty<'ty>,
        expected: ty::Ty<'ty>,
    },
}
