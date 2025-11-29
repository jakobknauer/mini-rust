use crate::{ctxt, hlr, hlr2mlr};

pub fn print_parser_error(err: &hlr::ParserError, _: &str) -> String {
    use hlr::ParserError::*;

    match err {
        LexerError(lexer_error) => format!("Lexer error at position {}", lexer_error.position),
        UnexpectedToken(token) => format!("Parser error: Unexpected token {:?}", token),
        UndelimitedStmt => "Parser error: Undelimited statement".to_string(),
        InvalidLiteral => "Parser error: Invalid literal".to_string(),
        UnexpectedEOF => "Parser error: Unexpected end of file".to_string(),
    }
}

pub fn print_mlr_builder_error(fn_name: &str, err: hlr2mlr::Hlr2MlrErr, ctxt: &ctxt::Ctxt) -> String {
    use hlr2mlr::Hlr2MlrErr::*;

    match err {
        TyErr(err) => print_ty_error(fn_name, err, ctxt),
        MissingOperatorImpl { name } => format!("Missing operator implementation for {}", name),
        UnresolvableSymbol { name } => format!("Unresolvable symbol {}", name),
        UnknownPrimitiveTy => "Unknown primitive type".to_string(),
        NotAPlace => {
            "Only variables, field access expressions, and derefs of references are supported as places.".to_string()
        }
    }
}

fn print_ty_error(fn_name: &str, err: hlr2mlr::TyErr, ctxt: &ctxt::Ctxt) -> String {
    use hlr2mlr::TyErr::*;

    let msg = match err {
        AssignStmtTyMismatch {
            place,
            expected,
            actual,
        } => format!(
            "Cannot reassign location {:?} of type '{}' with value of type '{}'",
            place,
            ctxt.tys.get_string_rep(&expected),
            ctxt.tys.get_string_rep(&actual)
        ),
        ValNotCallable => "Val is not callable".to_string(),
        CallArgumentTyMismatch {
            index,
            expected,
            actual,
        } => format!(
            "Argument {} type mismatch: expected '{}', got '{}'",
            index,
            ctxt.tys.get_string_rep(&expected),
            ctxt.tys.get_string_rep(&actual)
        ),
        CallArgumentCountMismatch { expected, actual } => {
            format!("Argument count mismatch: expected {}, got {}", expected, actual)
        }
        IfConditionNotBoolean { actual } => format!(
            "If condition must be of type 'bool', got '{}'",
            ctxt.tys.get_string_rep(&actual)
        ),
        IfBranchTyMismatch { then_ty, else_ty } => format!(
            "If branches must have the same type: then is '{}', else is '{}'",
            ctxt.tys.get_string_rep(&then_ty),
            ctxt.tys.get_string_rep(&else_ty)
        ),
        ReturnTyMismatch { expected, actual } => format!(
            "Return type mismatch: expected '{}', got '{}'",
            ctxt.tys.get_string_rep(&expected),
            ctxt.tys.get_string_rep(&actual)
        ),
        OperatorResolutionFailed {
            operator,
            operand_tys: (left, right),
        } => format!(
            "Cannot resolve operator '{}' for operand types '{}' and '{}'",
            operator,
            ctxt.tys.get_string_rep(&left),
            ctxt.tys.get_string_rep(&right)
        ),
        UnresolvableTyName { ty_name } => {
            format!("Cannot find type with name '{}'", ty_name)
        }
        NotAStruct { ty } => format!("Type '{}' is not a struct type", ctxt.tys.get_string_rep(&ty)),
        InitializerMissingFields { ty, missing_fields } => {
            format!(
                "Struct val of type '{}' is missing fields: {}",
                ctxt.tys.get_string_rep(&ty),
                missing_fields.join(", ")
            )
        }
        InitializerExtraFields { ty, extra_fields } => {
            format!(
                "Struct val of type '{}' has extra fields: {}",
                ctxt.tys.get_string_rep(&ty),
                extra_fields.join(", ")
            )
        }
        NotAStructField { ty, field_name } => format!(
            "Type '{}' does not have a field named '{}'",
            ctxt.tys.get_string_rep(&ty),
            field_name
        ),
        NotAnEnum { ty } => format!("Type '{}' is not an enum type", ctxt.tys.get_string_rep(&ty)),
        NotAnEnumVariant { ty, variant_name } => format!(
            "Enum type '{}' does not have a variant named '{}'",
            ctxt.tys.get_string_rep(&ty),
            variant_name
        ),
        UnresolvableTyAnnot => "Cannot resolve type annotation".to_string(),
        DereferenceOfNonRefTy { ty } => format!(
            "Cannot dereference type '{}', which is not a reference type",
            ctxt.tys.get_string_rep(&ty)
        ),
        GenericArgCountMismatch { fn_, expected, actual } => format!(
            "Generic argument count mismatch in function '{}': expected {}, got {}",
            ctxt.fns
                .get_sig(&fn_)
                .map(|sig| sig.name.as_str())
                .unwrap_or("<unknown>"),
            expected,
            actual
        ),
    };
    format!("Type error in function '{}': {}", fn_name, msg)
}
