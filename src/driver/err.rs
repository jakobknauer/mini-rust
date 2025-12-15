use crate::{ctxt, h2m, hlr, typechecker::TyError};

pub fn print_parser_err(err: &hlr::ParserErr, _: &str) -> String {
    use hlr::ParserErr::*;

    match err {
        LexerErr(lexer_err) => format!("Lexer error at position {}", lexer_err.position),
        UnexpectedToken(token) => format!("Parser error: Unexpected token {:?}", token),
        UndelimitedStmt => "Parser error: Undelimited statement".to_string(),
        InvalidLiteral => "Parser error: Invalid literal".to_string(),
        UnexpectedEOF => "Parser error: Unexpected end of file".to_string(),
    }
}

pub fn print_mlr_builder_error(fn_name: &str, err: h2m::H2MError, ctxt: &ctxt::Ctxt) -> String {
    use h2m::H2MError::*;

    match err {
        TyErr(err) => print_ty_error(fn_name, err, ctxt),
        MissingOperatorImpl { name } => format!("Missing operator implementation for {}", name),
        UnresolvableSymbol { name } => format!("Unresolvable symbol {}", name),
        NotAPlace => {
            "Only variables, field access expressions, and derefs of references are supported as places.".to_string()
        }
        OperatorResolutionFailed {
            operator,
            operand_tys: (left, right),
        } => format!(
            "Cannot resolve operator '{}' for operand types '{}' and '{}'",
            operator,
            ctxt.tys.get_string_rep(left),
            ctxt.tys.get_string_rep(right)
        ),
        UnresolvableStructOrEnum { ty_name } => {
            format!("Cannot find struct or enum with name '{}'", ty_name)
        }
        UnresolvableTyAnnot => "Cannot resolve type annotation".to_string(),
    }
}

fn print_ty_error(fn_name: &str, err: TyError, ctxt: &ctxt::Ctxt) -> String {
    use TyError::*;

    let msg = match err {
        AssignStmtTyMismatch {
            place,
            expected,
            actual,
        } => format!(
            "Cannot reassign location {:?} of type '{}' with value of type '{}'",
            place,
            ctxt.tys.get_string_rep(expected),
            ctxt.tys.get_string_rep(actual)
        ),
        ValNotCallable => "Val is not callable".to_string(),
        CallArgumentTyMismatch {
            index,
            expected,
            actual,
        } => format!(
            "Argument {} type mismatch: expected '{}', got '{}'",
            index,
            ctxt.tys.get_string_rep(expected),
            ctxt.tys.get_string_rep(actual)
        ),
        CallArgumentCountMismatch { expected, actual } => {
            format!("Argument count mismatch: expected {}, got {}", expected, actual)
        }
        IfConditionNotBoolean { actual } => format!(
            "If condition must be of type 'bool', got '{}'",
            ctxt.tys.get_string_rep(actual)
        ),
        IfBranchTyMismatch { then_ty, else_ty } => format!(
            "If branches must have the same type: then is '{}', else is '{}'",
            ctxt.tys.get_string_rep(then_ty),
            ctxt.tys.get_string_rep(else_ty)
        ),
        ReturnTyMismatch { expected, actual } => format!(
            "Return type mismatch: expected '{}', got '{}'",
            ctxt.tys.get_string_rep(expected),
            ctxt.tys.get_string_rep(actual)
        ),
        NotAStruct { ty } => format!("Type '{}' is not a struct type", ctxt.tys.get_string_rep(ty)),
        InitializerMissingFields { ty, missing_fields } => {
            format!(
                "Struct val of type '{}' is missing fields: {}",
                ctxt.tys.get_string_rep(ty),
                missing_fields.join(", ")
            )
        }
        InitializerExtraFields { ty, extra_fields } => {
            format!(
                "Struct val of type '{}' has extra fields: {}",
                ctxt.tys.get_string_rep(ty),
                extra_fields.join(", ")
            )
        }
        NotAStructField { ty, field_name } => format!(
            "Type '{}' does not have a field named '{}'",
            ctxt.tys.get_string_rep(ty),
            field_name
        ),
        NotAnEnum { ty } => format!("Type '{}' is not an enum type", ctxt.tys.get_string_rep(ty)),
        NotAnEnumVariant { ty, variant_name } => format!(
            "Enum type '{}' does not have a variant named '{}'",
            ctxt.tys.get_string_rep(ty),
            variant_name
        ),
        InvalidDereference { ty } => format!(
            "Cannot dereference type '{}', which is not a reference or pointer type",
            ctxt.tys.get_string_rep(ty)
        ),
        FnGenericArgCountMismatch { fn_, expected, actual } => format!(
            "Generic argument count mismatch in function '{}': expected {}, got {}",
            ctxt.fns
                .get_sig(&fn_)
                .map(|sig| sig.name.as_str())
                .unwrap_or("<unknown>"),
            expected,
            actual
        ),
        MissingVariants { ty, missing_variants } => format!(
            "Enum type '{}' is missing variants: {}",
            ctxt.tys.get_string_rep(ty),
            missing_variants.join(", ")
        ),
        ExtraVariants { ty, extra_variants } => format!(
            "Enum type '{}' has extra variants: {}",
            ctxt.tys.get_string_rep(ty),
            extra_variants.join(", ")
        ),
        StructGenericArgCountMismatch {
            struct_,
            expected,
            actual,
        } => format!(
            "Generic argument count mismatch for struct '{}': expected {}, got {}",
            ctxt.tys.get_struct_name(struct_),
            expected,
            actual
        ),
        EnumGenericArgCountMismatch {
            enum_,
            expected,
            actual,
        } => format!(
            "Generic argument count mismatch for enum '{}': expected {}, got {}",
            ctxt.tys.get_enum_name(enum_),
            expected,
            actual
        ),
        DereferenceOfCVoidPtr { ty } => format!(
            "Cannot dereference pointer or reference of type '{}', which points to 'c_void'",
            ctxt.tys.get_string_rep(ty)
        ),
        InvalidAsExpr { op_ty, target_ty } => format!(
            "Cannot cast from type '{}' to type '{}' using 'as' expression",
            ctxt.tys.get_string_rep(op_ty),
            ctxt.tys.get_string_rep(target_ty)
        ),
    };
    format!("Type error in function '{}': {}", fn_name, msg)
}
