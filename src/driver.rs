mod stdlib;

use crate::{
    ctxt::{self, functions, types},
    generate, hlr, mlr,
    util::print,
};

pub fn compile(source: &str, print_fn: impl Fn(&str)) -> Result<String, String> {
    let mut ctxt = ctxt::Ctxt::new();

    print_fn("Building HLR from source");
    let hlr = hlr::build_program(source).map_err(|parser_error| print_parser_error(&parser_error, source))?;

    print_fn("Building MLR from HLR");
    register_and_define_types(&hlr, &mut ctxt.type_registry).map_err(|_| "Error defining types")?;
    register_functions(&hlr, &ctxt.type_registry, &mut ctxt.function_registry)
        .map_err(|_| "Error registering functions")?;
    build_function_mlrs(&hlr, &mut ctxt).map_err(|err| format!("Error building MLR: {err}"))?;
    print_functions(&ctxt).map_err(|_| "Error printing MLR")?;

    print_fn("Building LLVM IR from MLR");
    let llvm_ir = generate::generate_llvm_ir(&ctxt);

    Ok(llvm_ir)
}

fn register_and_define_types(program: &hlr::Program, type_registry: &mut ctxt::TypeRegistry) -> Result<(), ()> {
    type_registry.register_primitive_types()?;

    for struct_ in &program.structs {
        type_registry.register_struct(&struct_.name)?;
    }

    for enum_ in &program.enums {
        type_registry.register_enum(&enum_.name)?;
    }

    for struct_ in &program.structs {
        let fields = struct_
            .fields
            .iter()
            .map(|field| {
                Ok(types::StructField {
                    name: field.name.clone(),
                    type_id: type_registry.get_type_id_by_name(&field.field_type).ok_or(())?,
                })
            })
            .collect::<Result<_, _>>()?;

        let struct_definition = type_registry
            .get_mut_struct_definition_by_name(&struct_.name)
            .ok_or(())?;
        struct_definition.fields = fields;
    }

    for enum_ in &program.enums {
        let variants = enum_
            .variants
            .iter()
            .map(|variant| types::EnumVariant {
                name: variant.name.clone(),
            })
            .collect();

        let enum_definition = type_registry.get_mut_enum_definition_by_name(&enum_.name).ok_or(())?;
        enum_definition.variants = variants;
    }

    Ok(())
}

fn register_functions(
    hlr: &hlr::Program,
    type_registry: &ctxt::TypeRegistry,
    function_registry: &mut ctxt::FunctionRegistry,
) -> Result<(), ()> {
    stdlib::register_functions(type_registry, function_registry)?;

    for function in &hlr.functions {
        let return_type = match function.return_type.as_ref() {
            Some(type_id) => type_registry.get_type_id_by_name(type_id).ok_or(())?,
            None => type_registry.get_type_id_by_name("()").ok_or(())?,
        };

        let parameters = function
            .parameters
            .iter()
            .map(|parameter| {
                Ok(functions::FunctionParameter {
                    name: parameter.name.clone(),
                    type_: type_registry.get_type_id_by_name(&parameter.param_type).ok_or(())?,
                })
            })
            .collect::<Result<_, _>>()?;

        let signature = functions::FunctionSignature {
            name: function.name.clone(),
            return_type,
            parameters,
        };

        function_registry.register_function(signature)?;
    }

    Ok(())
}

fn build_function_mlrs(hlr: &hlr::Program, ctxt: &mut ctxt::Ctxt) -> Result<(), String> {
    for function in &hlr.functions {
        let fn_id = ctxt.function_registry.get_function_by_name(&function.name).unwrap();

        let mlr_builder = mlr::MlrBuilder::new(function, fn_id, ctxt);
        let mlr = mlr_builder
            .build()
            .map_err(|err| print_mlr_builder_error(&function.name, err, ctxt))?;

        ctxt.function_registry.add_function_def(&function.name, mlr);
    }

    Ok(())
}

fn print_parser_error(err: &hlr::ParserError, _: &str) -> String {
    use hlr::ParserError::*;

    match err {
        LexerError(lexer_error) => format!("Lexer error at position {}", lexer_error.position),
        UnexpectedToken(token) => format!("Parser error: Unexpected token {:?}", token),
        UndelimitedStatement => "Parser error: Undelimited statement".to_string(),
        InvalidLiteral => "Parser error: Invalid literal".to_string(),
        UnexpectedEOF => "Parser error: Unexpected end of file".to_string(),
    }
}

fn print_mlr_builder_error(fn_name: &str, err: mlr::MlrBuilderError, ctxt: &ctxt::Ctxt) -> String {
    use mlr::MlrBuilderError::*;

    match err {
        TypeError(err) => print_type_error(fn_name, err, ctxt),
        MissingOperatorImpl { name } => format!("Missing operator implementation for {}", name),
        UnresolvableSymbol { name } => format!("Unresolvable symbol {}", name),
        UnknownPrimitiveType => "Unknown primitive type".to_string(),
        NotAPlace(expr_id) => format!("Expression {:?} is not a place", expr_id),
    }
}

fn print_type_error(fn_name: &str, err: mlr::TypeError, ctxt: &ctxt::Ctxt) -> String {
    use mlr::TypeError::*;

    let msg = match err {
        ReassignTypeMismatch { loc, expected, actual } => format!(
            "Cannot reassign location {:?} of type '{}' with value of type '{}'",
            loc,
            ctxt.type_registry.get_string_rep(&expected),
            ctxt.type_registry.get_string_rep(&actual)
        ),
        ExpressionNotCallable => "Expression is not callable".to_string(),
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
        UnresolvableTypeName { struct_name } => {
            format!("Cannot find struct type with name '{}'", struct_name)
        }
        NotAStruct { type_id } => format!(
            "Type '{}' is not a struct type",
            ctxt.type_registry.get_string_rep(&type_id)
        ),
        StructExpressionMissingFields { missing_fields } => {
            format!("Struct expression is missing fields: {}", missing_fields.join(", "))
        }
        StructExpressionExtraFields { extra_fields } => {
            format!("Struct expression has extra fields: {}", extra_fields.join(", "))
        }
        StructExpressionTypeMismatch {
            field_name,
            expected,
            actual,
        } => format!(
            "Struct field '{}' type mismatch: expected '{}', got '{}'",
            field_name,
            ctxt.type_registry.get_string_rep(&expected),
            ctxt.type_registry.get_string_rep(&actual)
        ),
    };
    format!("Type error in function '{}': {}", fn_name, msg)
}

fn print_functions(ctxt: &ctxt::Ctxt) -> Result<(), ()> {
    for fn_id in ctxt.function_registry.get_all_functions() {
        if ctxt.function_registry.is_function_defined(fn_id) {
            print::print_mlr(fn_id, ctxt, &mut std::io::stdout()).map_err(|_| ())?;
            println!();
        }
    }
    Ok(())
}
