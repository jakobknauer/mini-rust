use crate::{
    ast_lowering::AstLoweringError,
    driver::impl_check::{ImplCheckError, ImplCheckErrorKind},
    mlr_lowering::MlrLoweringError,
    mutck::MutckError,
    parse,
    typeck::TypeckError,
};

pub enum DriverError<'ty> {
    Parse(parse::ParserErr),
    ContextBuild(&'static str),
    NoMainFunction,
    ImplCheck(ImplCheckError<'ty>),
    AstLowering(AstLoweringError),
    Typeck { fn_name: String, error: TypeckError<'ty> },
    MlrLowering(MlrLoweringError<'ty>),
    Mutck(MutckError),
    Io(&'static str),
}

pub fn format_driver_error<'ctxt>(err: DriverError<'ctxt>) -> String {
    match err {
        DriverError::Parse(e) => format_parse_err(&e),
        DriverError::ContextBuild(msg) => format!("Compilation failed: {msg}"),
        DriverError::NoMainFunction => "Compilation failed: no 'main' function found".to_string(),
        DriverError::ImplCheck(e) => format!("Compilation failed: {}", format_impl_check_error(e)),
        DriverError::AstLowering(e) => format!("Compilation failed: {}", e.msg),
        DriverError::Typeck { fn_name, error } => {
            format!("Type error in '{}': {}", fn_name, format_typeck_error(&error))
        }
        DriverError::MlrLowering(MlrLoweringError::FnLowering { fn_inst, .. }) => {
            format!("Failed to lower function '{}' to LLVM IR", fn_inst)
        }
        DriverError::Mutck(e) => format!("Mutability error: {:?}", e),
        DriverError::Io(msg) => format!("IO error: {msg}"),
    }
}

fn format_parse_err(err: &parse::ParserErr) -> String {
    use parse::ParserErr::*;
    match err {
        LexerErr(e) => format!("Lexer error at position {}", e.position),
        UnexpectedToken(token) => format!("Parser error: unexpected token {:?}", token),
        UndelimitedStmt => "Parser error: undelimited statement".to_string(),
        InvalidLiteral => "Parser error: invalid literal".to_string(),
        UnexpectedEOF => "Parser error: unexpected end of file".to_string(),
        ExpectedTraitName => "Parser error: expected trait name".to_string(),
        UnexpectedReceiverArg => "Parser error: unexpected receiver argument".to_string(),
    }
}

fn format_impl_check_error<'ctxt>(err: ImplCheckError<'ctxt>) -> String {
    use ImplCheckErrorKind::*;

    let desc = match err.kind {
        MissingMthds(items) => format!("missing methods: {}", items.join(", ")),
        ExtraMthds(items) => format!("extra methods: {}", items.join(", ")),
        ParamCountMismatch { mthd, expected, actual } => format!(
            "argument count mismatch for method '{}': expected {}, got {}",
            mthd, expected, actual
        ),
        ArgTypeMismatch {
            mthd,
            arg_idx,
            expected,
            actual,
        } => format!(
            "argument {} type mismatch for method '{}': expected '{}', got '{}'",
            arg_idx, mthd, expected, actual
        ),
        ReturnTypeMismatch { mthd, expected, actual } => format!(
            "return type mismatch for method '{}': expected '{}', got '{}'",
            mthd, expected, actual
        ),
        MthdGenParamCountMismatch { mthd, expected, actual } => format!(
            "generic argument count mismatch for method '{}': expected {}, got {}",
            mthd, expected, actual
        ),
        ReceiverMismatch { mthd, expected, actual } => format!(
            "receiver mismatch for method '{}': expected '{}', got '{}'",
            mthd,
            expected.as_ref().map_or("none", |k| k.as_str()),
            actual.as_ref().map_or("none", |k| k.as_str()),
        ),
        ImplGenParamCountMismatch { expected, actual } => format!(
            "generic argument count mismatch for implementation: expected {}, got {}",
            expected, actual
        ),
        MissingAssocTy(name) => format!("missing associated type '{}'", name),
        ConstraintMismatch { mthd } => format!("constraint mismatch for method '{}'", mthd),
    };

    format!(
        "error checking implementation of trait '{}' for type '{}': {}",
        err.trait_inst.trait_.name, err.impl_.ty, desc
    )
}

fn format_typeck_error<'ctxt>(err: &TypeckError<'ctxt>) -> String {
    use TypeckError::*;

    match err {
        ReturnTypeMismatch { expected, actual } => {
            format!("return type mismatch: expected '{}', got '{}'", expected, actual)
        }
        ReturnExprTypeMismatch { expected, actual } => format!(
            "return expression type mismatch: expected '{}', got '{}'",
            expected, actual
        ),
        FnGenArgCountMismatch { fn_, expected, actual } => format!(
            "wrong number of generic arguments for function '{}': expected {}, got {}",
            fn_.name, expected, actual
        ),
        StructGenArgCountMismatch {
            struct_: s,
            expected,
            actual,
        } => format!(
            "wrong number of generic arguments for struct '{}': expected {}, got {}",
            s.name, expected, actual
        ),
        EnumGenArgCountMismatch {
            enum_: e,
            expected,
            actual,
        } => format!(
            "wrong number of generic arguments for enum '{}': expected {}, got {}",
            e.name, expected, actual
        ),
        TraitGenArgCountMismatch {
            trait_: t,
            expected,
            actual,
        } => format!(
            "wrong number of generic arguments for trait '{}': expected {}, got {}",
            t.name, expected, actual
        ),
        UnresolvableAssocTy { base, name } => format!("cannot resolve associated type '{}' on '{}'", name, base),
        MthdResolutionFailed { base_ty, mthd_name } => {
            format!("no method '{}' found for type '{}'", mthd_name, base_ty)
        }
        AmbiguousMthd { base_ty, mthd_name } => {
            format!("method '{}' is ambiguous for type '{}'", mthd_name, base_ty)
        }
        MthdGenArgCountMismatch {
            mthd_name,
            expected,
            actual,
        } => format!(
            "wrong number of generic arguments for method '{}': expected {}, got {}",
            mthd_name, expected, actual
        ),
        NamedFieldAccessOnNonStruct { ty: t } => format!("named field access on non-struct type '{}'", t),
        IndexedFieldAccessOnNonTuple { ty: t } => format!("indexed field access on non-tuple type '{}'", t),
        StructFieldNotFound { struct_ty, field } => format!("no field '{}' on type '{}'", field, struct_ty),
        StructFieldTypeMismatch {
            struct_ty,
            field_idx,
            expected,
            actual,
        } => format!(
            "field {} type mismatch in '{}': expected '{}', got '{}'",
            field_idx, struct_ty, expected, actual
        ),
        CalleeNotCallable { ty: t } => format!("type '{}' is not callable", t),
        CallArgCountMismatch {
            expected,
            actual,
            var_args,
        } => {
            if *var_args {
                format!(
                    "wrong number of arguments: expected at least {}, got {}",
                    expected, actual
                )
            } else {
                format!("wrong number of arguments: expected {}, got {}", expected, actual)
            }
        }
        CallArgTypeMismatch {
            index,
            expected,
            actual,
        } => format!(
            "argument {} type mismatch: expected '{}', got '{}'",
            index, expected, actual
        ),
        BinaryOpTypeMismatch {
            operator,
            left_ty,
            right_ty,
        } => format!(
            "operator '{:?}' cannot be applied to '{}' and '{}'",
            operator, left_ty, right_ty
        ),
        ArithTraitNotImplemented {
            operator,
            left_ty,
            right_ty,
        } => format!(
            "operator '{:?}' is not implemented for '{}' and '{}'",
            operator, left_ty, right_ty
        ),
        UnaryOpTypeMismatch { operator, operand_ty } => {
            format!("operator '{:?}' cannot be applied to '{}'", operator, operand_ty)
        }
        AssignmentTargetNotAPlace => "assignment target is not a place".to_string(),
        AssignmentTypeMismatch { expected, actual } => {
            format!("assignment type mismatch: expected '{}', got '{}'", expected, actual)
        }
        DereferenceOfNonRef { ty: t } => format!("cannot dereference non-reference type '{}'", t),
        DereferenceOfCVoid { ty: t } => format!("cannot dereference c_void pointer '{}'", t),
        InvalidAsConversion { op_ty, target_ty } => format!("cannot cast '{}' to '{}'", op_ty, target_ty),
        IfConditionNotBoolean => "if condition must be boolean".to_string(),
        IfBranchesTypeMismatch { then_ty, else_ty } => {
            format!("if branches have mismatched types: '{}' vs '{}'", then_ty, else_ty)
        }
        NonMatchableScrutinee { ty: t } => format!("type '{}' cannot be matched on", t),
        MatchArmWrongEnum { expected, found } => format!(
            "match arm has wrong enum type: expected '{}', got '{}'",
            expected, found
        ),
        MatchArmTypeMismatch { expected, actual } => {
            format!("match arm type mismatch: expected '{}', got '{}'", expected, actual)
        }
        LetTypeMismatch { expected, actual } => {
            format!("let binding type mismatch: expected '{}', got '{}'", expected, actual)
        }
        VarTypeNotSet { var_id, expr_id } => format!(
            "internal error: type of variable {} not set at expression {:?}",
            var_id, expr_id
        ),
        ConstraintNotSatisfied { ty: t, trait_inst } => {
            format!("'{}' does not implement '{}'", t, trait_inst.trait_.name)
        }
        CallableConstraintNotSatisfied {
            ty: t,
            expected_param_tys,
            expected_return_ty,
        } => {
            let params: Vec<_> = expected_param_tys.iter().map(|p| p.to_string()).collect();
            format!(
                "'{}' is not callable as fn({}) -> '{}'",
                t,
                params.join(", "),
                expected_return_ty
            )
        }
        TypeckError::AssocTyEqNotSatisfied { subject, expected } => {
            format!("associated type '{}' does not equal '{}'", subject, expected)
        }
        NonAssignablePlace(expr) => format!("expression {:?} is not an assignable place", expr),
    }
}
