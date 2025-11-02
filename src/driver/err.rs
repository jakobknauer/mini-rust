use crate::{ctxt, hlr, mlr};

pub fn print_parser_error(err: &hlr::ParserError, _: &str) -> String {
    use hlr::ParserError::*;

    match err {
        LexerError(lexer_error) => format!("Lexer error at position {}", lexer_error.position),
        UnexpectedToken(token) => format!("Parser error: Unexpected token {:?}", token),
        UndelimitedStatement => "Parser error: Undelimited statement".to_string(),
        InvalidLiteral => "Parser error: Invalid literal".to_string(),
        UnexpectedEOF => "Parser error: Unexpected end of file".to_string(),
    }
}

pub fn print_mlr_builder_error(fn_name: &str, err: mlr::MlrBuilderError, ctxt: &ctxt::Ctxt) -> String {
    use mlr::MlrBuilderError::*;

    match err {
        TypeError(err) => print_type_error(fn_name, err, ctxt),
        MissingOperatorImpl { name } => format!("Missing operator implementation for {}", name),
        UnresolvableSymbol { name } => format!("Unresolvable symbol {}", name),
        UnknownPrimitiveType => "Unknown primitive type".to_string(),
    }
}

fn print_type_error(fn_name: &str, err: mlr::TypeError, ctxt: &ctxt::Ctxt) -> String {
    use mlr::TypeError::*;

    let msg = match err {
        AssignStmtTypeMismatch {
            place,
            expected,
            actual,
        } => format!(
            "Cannot reassign location {:?} of type '{}' with value of type '{}'",
            place,
            ctxt.type_registry.get_string_rep(&expected),
            ctxt.type_registry.get_string_rep(&actual)
        ),
        ValNotCallable => "Val is not callable".to_string(),
        CallArgumentTypeMismatch {
            index,
            expected,
            actual,
        } => format!(
            "Argument {} type mismatch: expected '{}', got '{}'",
            index,
            ctxt.type_registry.get_string_rep(&expected),
            ctxt.type_registry.get_string_rep(&actual)
        ),
        CallArgumentCountMismatch { expected, actual } => {
            format!("Argument count mismatch: expected {}, got {}", expected, actual)
        }
        IfConditionNotBoolean { actual } => format!(
            "If condition must be of type 'bool', got '{}'",
            ctxt.type_registry.get_string_rep(&actual)
        ),
        IfBranchTypeMismatch { then_type, else_type } => format!(
            "If branches must have the same type: then is '{}', else is '{}'",
            ctxt.type_registry.get_string_rep(&then_type),
            ctxt.type_registry.get_string_rep(&else_type)
        ),
        ReturnTypeMismatch { expected, actual } => format!(
            "Return type mismatch: expected '{}', got '{}'",
            ctxt.type_registry.get_string_rep(&expected),
            ctxt.type_registry.get_string_rep(&actual)
        ),
        OperatorResolutionFailed {
            operator,
            operand_types: (left, right),
        } => format!(
            "Cannot resolve operator '{}' for operand types '{}' and '{}'",
            operator,
            ctxt.type_registry.get_string_rep(&left),
            ctxt.type_registry.get_string_rep(&right)
        ),
        UnresolvableTypeName { type_name } => {
            format!("Cannot find type with name '{}'", type_name)
        }
        NotAStruct { type_id } => format!(
            "Type '{}' is not a struct type",
            ctxt.type_registry.get_string_rep(&type_id)
        ),
        InitializerMissingFields {
            type_id,
            missing_fields,
        } => {
            format!(
                "Struct val of type '{}' is missing fields: {}",
                ctxt.type_registry.get_string_rep(&type_id),
                missing_fields.join(", ")
            )
        }
        InitializerExtraFields { type_id, extra_fields } => {
            format!(
                "Struct val of type '{}' has extra fields: {}",
                ctxt.type_registry.get_string_rep(&type_id),
                extra_fields.join(", ")
            )
        }
        StructValTypeMismatch {
            type_id,
            field_name,
            expected,
            actual,
        } => format!(
            "Type mismatch for field '{}' of type '{}': expected '{}', got '{}'",
            field_name,
            ctxt.type_registry.get_string_rep(&type_id),
            ctxt.type_registry.get_string_rep(&expected),
            ctxt.type_registry.get_string_rep(&actual)
        ),
        NotAStructField { type_id, field_name } => format!(
            "Type '{}' does not have a field named '{}'",
            ctxt.type_registry.get_string_rep(&type_id),
            field_name
        ),
        FieldAccessBaseTypeMismatch { expected, actual } => format!(
            "Field access base type mismatch: expected '{}', got '{}'",
            ctxt.type_registry.get_struct_string_rep(&expected),
            ctxt.type_registry.get_struct_string_rep(&actual)
        ),
        NotAnEnum { type_id } => format!(
            "Type '{}' is not an enum type",
            ctxt.type_registry.get_string_rep(&type_id)
        ),
        ProjectToVariantBaseTypeMismatch { expected, actual } => format!(
            "Project to variant base type mismatch: expected '{}', got '{}'",
            ctxt.type_registry.get_enum_string_rep(&expected),
            ctxt.type_registry.get_enum_string_rep(&actual)
        ),
    };
    format!("Type error in function '{}': {}", fn_name, msg)
}
