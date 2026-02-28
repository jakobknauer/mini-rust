use crate::{
    ctxt,
    driver::impl_check::{ImplCheckError, ImplCheckErrorKind},
    parse,
};

pub fn print_parser_err(err: &parse::ParserErr, _: &str) -> String {
    use parse::ParserErr::*;

    match err {
        LexerErr(lexer_err) => format!("Lexer error at position {}", lexer_err.position),
        UnexpectedToken(token) => format!("Parser error: Unexpected token {:?}", token),
        UndelimitedStmt => "Parser error: Undelimited statement".to_string(),
        InvalidLiteral => "Parser error: Invalid literal".to_string(),
        UnexpectedEOF => "Parser error: Unexpected end of file".to_string(),
        TraitMthdWithBody => "Parser error: Trait method with body".to_string(),
        ExpectedTraitName => "Parser error: Expected trait name".to_string(),
        UnexpectedReceiverArg => "Parser error: Unexpected receiver argument".to_string(),
    }
}

pub fn print_impl_check_error(err: ImplCheckError, ctxt: &ctxt::Ctxt) -> String {
    use ImplCheckErrorKind::*;

    let desc = match err.kind {
        MissingMthds(items) => format!("Missing methods: {}", items.join(", ")),
        ExtraMthds(items) => format!("Extra methods: {}", items.join(", ")),
        ParamCountMismatch { mthd, expected, actual } => format!(
            "Argument count mismatch for method '{}': expected {}, got {}",
            mthd, expected, actual
        ),
        ArgTypeMismatch {
            mthd,
            arg_idx,
            expected,
            actual,
        } => format!(
            "Argument {} type mismatch for method '{}': expected '{}', got '{}'",
            arg_idx,
            mthd,
            ctxt.tys.get_string_rep(expected),
            ctxt.tys.get_string_rep(actual)
        ),
        ReturnTypeMismatch { mthd, expected, actual } => format!(
            "Return type mismatch for method '{}': expected '{}', got '{}'",
            mthd,
            ctxt.tys.get_string_rep(expected),
            ctxt.tys.get_string_rep(actual)
        ),
        MthdGenParamCountMismatch { mthd, expected, actual } => format!(
            "Generic argument count mismatch for method '{}': expected {}, got {}",
            mthd, expected, actual
        ),
        ReceiverMismatch { mthd, expected, actual } => format!(
            "Receiver type mismatch for method '{}': expected '{}', got '{}'",
            mthd, expected, actual
        ),
        ImplGenParamCountMismatch { actual, expected } => format!(
            "Generic argument count mismatch for implementation: expected {}, got {}",
            expected, actual
        ),
        MissingAssocTy(name) => format!("Missing associated type '{}'", name),
    };

    format!(
        "Error checking implementation of trait '{}' for type '{}': {}",
        ctxt.traits.get_trait_name(err.trait_inst.trait_),
        ctxt.tys.get_string_rep(ctxt.impls.get_impl_def(err.impl_).ty),
        desc
    )
}
