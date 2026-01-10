use crate::{
    ctxt::{self, ty::Obligation},
    driver::impl_check::{ImplCheckError, ImplCheckErrorKind},
    h2m, hlr,
    obligation_check::ObligationCheckError,
    typechecker::TyError,
};

pub fn print_parser_err(err: &hlr::ParserErr, _: &str) -> String {
    use hlr::ParserErr::*;

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
        VarArgsNotSupported => "Cannot build MLR for variadic function".to_string(),
        NonMatchableScrutinee { ty } => format!(
            "Match scrutinee has type '{}', which is not matchable",
            ctxt.tys.get_string_rep(ty)
        ),
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
        CallArgumentCountMismatch {
            expected,
            actual,
            var_args,
        } => {
            format!(
                "Argument count mismatch: expected {}, got {} in {} call",
                expected,
                actual,
                if var_args { "variadic" } else { "non-variadic" }
            )
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
        NotAStructOrTuple { ty } => format!("Type '{}' is not a struct or tuple type", ctxt.tys.get_string_rep(ty)),
        NotATuple { ty } => format!("Type '{}' is not a tuple type", ctxt.tys.get_string_rep(ty)),
        InvalidTupleIndex { ty, index } => format!(
            "Tuple type '{}' does not have a field at index '{}'",
            ctxt.tys.get_string_rep(ty),
            index
        ),
        InvalidDereference { ty } => format!(
            "Cannot dereference type '{}', which is not a reference or pointer type",
            ctxt.tys.get_string_rep(ty)
        ),
        FnGenericArgCountMismatch { fn_, expected, actual } => format!(
            "Generic argument count mismatch in function '{}': expected {}, got {}",
            ctxt.fns.get_fn_name(fn_),
            expected,
            actual
        ),
        FnEnvGenericArgCountMismatch { fn_, expected, actual } => format!(
            "Environment generic argument count mismatch in function '{}': expected {}, got {}",
            ctxt.fns.get_fn_name(fn_),
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
        AmbiguousMthd { base_ty, mthd_name } => format!(
            "Ambiguous method name '{}' for type '{}'",
            mthd_name,
            ctxt.tys.get_string_rep(base_ty),
        ),
        MthdResolutionFailed { base_ty, mthd_name } => format!(
            "Could not resolve method '{}' for type '{}'",
            mthd_name,
            ctxt.tys.get_string_rep(base_ty),
        ),
        TraitMthdGenericArgCountMismatch {
            trait_,
            mthd_idx: mthd_dix,
            impl_ty,
            expected,
            actual,
        } => format!(
            "Generic argument count mismatch for trait method '{}' in type '{}': expected {}, got {}",
            ctxt.traits.get_trait_mthd_name(trait_, mthd_dix),
            ctxt.tys.get_string_rep(impl_ty),
            expected,
            actual
        ),
        UnfulfilledConstraint {
            fn_,
            gen_var,
            constraint,
            gen_arg,
        } => format!(
            "Constraint '{}: {}' in function '{}' is not fulfilled by type '{}'",
            ctxt.tys.get_gen_var_name(gen_var),
            ctxt.traits.get_trait_name(constraint),
            ctxt.fns.get_fn_name(fn_),
            ctxt.tys.get_string_rep(gen_arg),
        ),
        ClosureMismatchWithExpected => "Closure type cannot be unified with expected type".to_string(),
    };
    format!("Type error in function '{}': {}", fn_name, msg)
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
    };

    format!(
        "Error checking implementation of trait '{}' for type '{}': {}",
        ctxt.traits.get_trait_name(err.trait_inst.trait_),
        ctxt.tys.get_string_rep(ctxt.impls.get_impl_def(err.impl_).ty),
        desc
    )
}

pub fn print_obligation_check_error(err: ObligationCheckError, ctxt: &ctxt::Ctxt) -> String {
    match err.obligation {
        Obligation::ImplementsTraitInst { ty, trait_inst } => format!(
            "Obligation check error:  type '{}' does not implement trait '{}<{}>'",
            ctxt.tys.get_string_rep(ty),
            ctxt.traits.get_trait_name(trait_inst.trait_),
            trait_inst
                .gen_args
                .iter()
                .map(|ty| ctxt.tys.get_string_rep(*ty))
                .collect::<Vec<String>>()
                .join(", "),
        ),
        Obligation::Callable {
            ty,
            param_tys,
            return_ty,
        } => format!(
            "Obligation check error:  type '{}' is not callable with arguments '{}' and return type '{}'",
            ctxt.tys.get_string_rep(ty),
            param_tys
                .iter()
                .map(|ty| ctxt.tys.get_string_rep(*ty))
                .collect::<Vec<String>>()
                .join(", "),
            ctxt.tys.get_string_rep(return_ty),
        ),
    }
}
