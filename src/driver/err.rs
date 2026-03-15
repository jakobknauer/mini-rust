use crate::{
    ast_lowering::AstLoweringError,
    ctxt,
    driver::impl_check::{ImplCheckError, ImplCheckErrorKind},
    mlr_lowering::MlrLoweringError,
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
    Io(&'static str),
}

pub fn format_driver_error<'ctxt>(err: DriverError<'ctxt>, ctxt: &ctxt::Ctxt<'ctxt>) -> String {
    match err {
        DriverError::Parse(e) => format_parse_err(&e),
        DriverError::ContextBuild(msg) => format!("Compilation failed: {msg}"),
        DriverError::NoMainFunction => "Compilation failed: no 'main' function found".to_string(),
        DriverError::ImplCheck(e) => format!("Compilation failed: {}", format_impl_check_error(e, ctxt)),
        DriverError::AstLowering(e) => format!("Compilation failed: {}", e.msg),
        DriverError::Typeck { fn_name, error } => {
            format!("Type error in '{}': {}", fn_name, format_typeck_error(&error, ctxt))
        }
        DriverError::MlrLowering(MlrLoweringError::FnLowering { fn_inst, .. }) => format!(
            "Failed to lower function '{}' to LLVM IR",
            ctxt.get_fn_inst_name(fn_inst)
        ),
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
        TraitMthdWithBody => "Parser error: trait method with body".to_string(),
        ExpectedTraitName => "Parser error: expected trait name".to_string(),
        UnexpectedReceiverArg => "Parser error: unexpected receiver argument".to_string(),
    }
}

fn format_impl_check_error<'ctxt>(err: ImplCheckError<'ctxt>, ctxt: &ctxt::Ctxt<'ctxt>) -> String {
    use ImplCheckErrorKind::*;

    let ty = |t| ctxt.tys.get_string_rep(t);

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
            arg_idx,
            mthd,
            ty(expected),
            ty(actual)
        ),
        ReturnTypeMismatch { mthd, expected, actual } => format!(
            "return type mismatch for method '{}': expected '{}', got '{}'",
            mthd,
            ty(expected),
            ty(actual)
        ),
        MthdGenParamCountMismatch { mthd, expected, actual } => format!(
            "generic argument count mismatch for method '{}': expected {}, got {}",
            mthd, expected, actual
        ),
        ReceiverMismatch { mthd, expected, actual } => format!(
            "receiver type mismatch for method '{}': expected '{}', got '{}'",
            mthd, expected, actual
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
        ctxt.traits.get_trait_name(err.trait_inst.trait_),
        ctxt.tys.get_string_rep(ctxt.impls.get_impl_def(err.impl_).ty),
        desc
    )
}

fn format_typeck_error<'ctxt>(err: &TypeckError<'ctxt>, ctxt: &ctxt::Ctxt<'ctxt>) -> String {
    use TypeckError::*;

    let ty = |t| ctxt.tys.get_string_rep(t);
    let trait_ = |t| ctxt.traits.get_trait_name(t).to_string();
    let _fn_ = |f| ctxt.fns.get_fn_name(f).to_string();
    let struct_ = |s| ctxt.tys.get_struct_name(s);
    let enum_ = |e| ctxt.tys.get_enum_name(e);

    match err {
        ReturnTypeMismatch { expected, actual } => format!(
            "return type mismatch: expected '{}', got '{}'",
            ty(*expected),
            ty(*actual)
        ),
        ReturnExprTypeMismatch { expected, actual } => format!(
            "return expression type mismatch: expected '{}', got '{}'",
            ty(*expected),
            ty(*actual)
        ),
        FnGenArgCountMismatch { fn_, expected, actual } => format!(
            "wrong number of generic arguments for function '{}': expected {}, got {}",
            ctxt.fns.get_fn_name(*fn_),
            expected,
            actual
        ),
        StructGenArgCountMismatch {
            struct_: s,
            expected,
            actual,
        } => format!(
            "wrong number of generic arguments for struct '{}': expected {}, got {}",
            struct_(*s),
            expected,
            actual
        ),
        EnumGenArgCountMismatch {
            enum_: e,
            expected,
            actual,
        } => format!(
            "wrong number of generic arguments for enum '{}': expected {}, got {}",
            enum_(*e),
            expected,
            actual
        ),
        TraitGenArgCountMismatch {
            trait_: t,
            expected,
            actual,
        } => format!(
            "wrong number of generic arguments for trait '{}': expected {}, got {}",
            trait_(*t),
            expected,
            actual
        ),
        UnresolvableAssocTy { base, name } => format!("cannot resolve associated type '{}' on '{}'", name, ty(*base)),
        MthdResolutionFailed { base_ty, mthd_name } => {
            format!("no method '{}' found for type '{}'", mthd_name, ty(*base_ty))
        }
        AmbiguousMthd { base_ty, mthd_name } => {
            format!("method '{}' is ambiguous for type '{}'", mthd_name, ty(*base_ty))
        }
        MthdGenArgCountMismatch {
            mthd_name,
            expected,
            actual,
        } => format!(
            "wrong number of generic arguments for method '{}': expected {}, got {}",
            mthd_name, expected, actual
        ),
        NamedFieldAccessOnNonStruct { ty: t } => format!("named field access on non-struct type '{}'", ty(*t)),
        IndexedFieldAccessOnNonTuple { ty: t } => format!("indexed field access on non-tuple type '{}'", ty(*t)),
        StructFieldNotFound { struct_ty, field } => format!("no field '{}' on type '{}'", field, ty(*struct_ty)),
        StructFieldTypeMismatch {
            struct_ty,
            field_idx,
            expected,
            actual,
        } => format!(
            "field {} type mismatch in '{}': expected '{}', got '{}'",
            field_idx,
            ty(*struct_ty),
            ty(*expected),
            ty(*actual)
        ),
        CalleeNotCallable { ty: t } => format!("type '{}' is not callable", ty(*t)),
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
            index,
            ty(*expected),
            ty(*actual)
        ),
        BinaryOpTypeMismatch {
            operator,
            left_ty,
            right_ty,
        } => format!(
            "operator '{:?}' cannot be applied to '{}' and '{}'",
            operator,
            ty(*left_ty),
            ty(*right_ty)
        ),
        ArithTraitNotImplemented {
            operator,
            left_ty,
            right_ty,
        } => format!(
            "operator '{:?}' is not implemented for '{}' and '{}'",
            operator,
            ty(*left_ty),
            ty(*right_ty)
        ),
        UnaryOpTypeMismatch { operator, operand_ty } => {
            format!("operator '{:?}' cannot be applied to '{}'", operator, ty(*operand_ty))
        }
        AssignmentTargetNotAPlace => "assignment target is not a place".to_string(),
        AssignmentTypeMismatch { expected, actual } => format!(
            "assignment type mismatch: expected '{}', got '{}'",
            ty(*expected),
            ty(*actual)
        ),
        DereferenceOfNonRef { ty: t } => format!("cannot dereference non-reference type '{}'", ty(*t)),
        DereferenceOfCVoid { ty: t } => format!("cannot dereference c_void pointer '{}'", ty(*t)),
        InvalidAsConversion { op_ty, target_ty } => format!("cannot cast '{}' to '{}'", ty(*op_ty), ty(*target_ty)),
        IfConditionNotBoolean => "if condition must be boolean".to_string(),
        IfBranchesTypeMismatch { then_ty, else_ty } => format!(
            "if branches have mismatched types: '{}' vs '{}'",
            ty(*then_ty),
            ty(*else_ty)
        ),
        NonMatchableScrutinee { ty: t } => format!("type '{}' cannot be matched on", ty(*t)),
        MatchArmWrongEnum { expected, found } => format!(
            "match arm has wrong enum type: expected '{}', got '{}'",
            ty(*expected),
            ty(*found)
        ),
        MatchArmTypeMismatch { expected, actual } => format!(
            "match arm type mismatch: expected '{}', got '{}'",
            ty(*expected),
            ty(*actual)
        ),
        LetTypeMismatch { expected, actual } => format!(
            "let binding type mismatch: expected '{}', got '{}'",
            ty(*expected),
            ty(*actual)
        ),
        VarTypeNotSet { var_id, expr_id } => format!(
            "internal error: type of variable {} not set at expression {:?}",
            var_id, expr_id
        ),
        ConstraintNotSatisfied { ty: t, trait_inst } => {
            format!("'{}' does not implement '{}'", ty(*t), trait_(trait_inst.trait_))
        }
        CallableConstraintNotSatisfied {
            ty: t,
            expected_param_tys,
            expected_return_ty,
        } => {
            let params: Vec<_> = ctxt
                .tys
                .get_ty_slice(*expected_param_tys)
                .iter()
                .map(|&p| ty(p))
                .collect();
            format!(
                "'{}' is not callable as fn({}) -> '{}'",
                ty(*t),
                params.join(", "),
                ty(*expected_return_ty)
            )
        }
        &TypeckError::AssocTyEqNotSatisfied { subject, expected } => {
            format!("associated type '{}' does not equal '{}'", ty(subject), ty(expected))
        }
    }
}
