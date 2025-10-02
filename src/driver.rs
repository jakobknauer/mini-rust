use crate::{
    ctxt::{self, functions, types},
    hlr, mlr,
    util::print,
};

pub fn compile(source: &str) -> Option<()> {
    let mut ctxt = ctxt::Ctxt::new();

    let hlr = match hlr::build_program(&source) {
        Ok(hlr) => hlr,
        Err(err) => {
            print_parser_error(&err, &source);
            return None;
        }
    };

    register_and_define_types(&hlr, &mut ctxt.type_registry)?;
    register_functions(&hlr, &ctxt.type_registry, &mut ctxt.function_registry)?;

    for function in &hlr.functions {
        let fn_id = ctxt.function_registry.get_function_by_name(&function.name)?;

        let mlr_builder = mlr::MlrBuilder::new(&function, fn_id, &mut ctxt);
        let mlr = match mlr_builder.build() {
            Ok(mlr) => mlr,
            Err(mlr::MlrBuilderError::TypeError(err)) => {
                print_type_error(&function.name, err, &ctxt);
                return None;
            }
            Err(mlr::MlrBuilderError::MissingOperatorImpl { name }) => {
                eprintln!("Error: Missing operator implementation for {}", name);
                return None;
            }
            Err(mlr::MlrBuilderError::UnresolvableSymbol { name }) => {
                eprintln!("Error: Unresolvable symbol {}", name);
                return None;
            }
            Err(mlr::MlrBuilderError::UnknownPrimitiveType) => {
                eprintln!("Error: Unknown primitive type");
                return None;
            }
        };

        ctxt.function_registry.add_function_def(&function.name, mlr);
    }

    for fn_id in ctxt.function_registry.get_all_functions() {
        print::print_mlr(fn_id, &ctxt, &mut std::io::stdout()).ok()?;
        println!();
    }

    Some(())
}

fn register_and_define_types(program: &hlr::Program, type_registry: &mut ctxt::TypeRegistry) -> Option<()> {
    type_registry.register_primitive_types().ok()?;

    for struct_ in &program.structs {
        type_registry.register_struct(&struct_.name).ok()?;
    }

    for enum_ in &program.enums {
        type_registry.register_enum(&enum_.name).ok();
    }

    for struct_ in &program.structs {
        let fields = struct_
            .fields
            .iter()
            .map(|field| {
                Some(types::StructField {
                    name: field.name.clone(),
                    type_id: type_registry.get_type_id_by_name(&field.field_type)?,
                })
            })
            .collect::<Option<_>>()?;

        let struct_definition = type_registry.get_mut_struct_definition_by_name(&struct_.name)?;
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

        let enum_definition = type_registry.get_mut_enum_definition_by_name(&enum_.name)?;
        enum_definition.variants = variants;
    }

    Some(())
}

fn register_functions(
    program: &hlr::Program,
    type_registry: &ctxt::TypeRegistry,
    function_registry: &mut ctxt::FunctionRegistry,
) -> Option<()> {
    let i32_t = type_registry.get_primitive_type_id(types::PrimitiveType::Integer32)?;
    function_registry
        .register_function(functions::FunctionSignature {
            name: "add::<i32>".to_string(),
            return_type: i32_t,
            parameters: vec![
                functions::FunctionParameter {
                    name: "a".to_string(),
                    type_: i32_t,
                },
                functions::FunctionParameter {
                    name: "b".to_string(),
                    type_: i32_t,
                },
            ],
        })
        .ok()?;
    function_registry
        .register_function(functions::FunctionSignature {
            name: "mul::<i32>".to_string(),
            return_type: i32_t,
            parameters: vec![
                functions::FunctionParameter {
                    name: "a".to_string(),
                    type_: i32_t,
                },
                functions::FunctionParameter {
                    name: "b".to_string(),
                    type_: i32_t,
                },
            ],
        })
        .ok()?;

    for function in &program.functions {
        let return_type = match function.return_type.as_ref() {
            Some(type_id) => type_registry.get_type_id_by_name(type_id)?,
            None => type_registry.get_type_id_by_name("()")?,
        };

        let parameters = function
            .parameters
            .iter()
            .map(|parameter| {
                Some(functions::FunctionParameter {
                    name: parameter.name.clone(),
                    type_: type_registry.get_type_id_by_name(&parameter.param_type)?,
                })
            })
            .collect::<Option<_>>()?;

        let signature = functions::FunctionSignature {
            name: function.name.clone(),
            return_type,
            parameters,
        };

        function_registry.register_function(signature).ok()?;
    }

    Some(())
}

fn print_parser_error(err: &hlr::ParserError, _: &str) -> () {
    match err {
        hlr::ParserError::LexerError(lexer_error) => eprintln!("Lexer error at position {}", lexer_error.position),
        hlr::ParserError::UnexpectedToken(token) => eprintln!("Parser error: Unexpected token {:?}", token),
        hlr::ParserError::UndelimitedStatement => eprintln!("Parser error: Undelimited statement"),
        hlr::ParserError::InvalidLiteral => eprintln!("Parser error: Invalid literal"),
        hlr::ParserError::UnexpectedEOF => eprintln!("Parser error: Unexpected end of file"),
    }
}

fn print_type_error(name: &str, err: mlr::TypeError, ctxt: &ctxt::Ctxt) {
    match err {
        mlr::TypeError::ReassignTypeMismatch { loc, expected, actual } => eprintln!(
            "Type error in function '{}': Cannot reassign location {:?} of type '{}' with value of type '{}'",
            name,
            loc,
            ctxt.type_registry.get_string_rep(expected),
            ctxt.type_registry.get_string_rep(actual)
        ),
        mlr::TypeError::ExpressionNotCallable => {
            eprintln!("Type error in function '{}': Expression is not callable", name)
        }
        mlr::TypeError::CallArgumentTypeMismatch {
            index,
            expected,
            actual,
        } => eprintln!(
            "Type error in function '{}': Argument {} type mismatch: expected '{}', got '{}'",
            name,
            index,
            ctxt.type_registry.get_string_rep(expected),
            ctxt.type_registry.get_string_rep(actual)
        ),
        mlr::TypeError::CallArgumentCountMismatch { expected, actual } => eprintln!(
            "Type error in function '{}': Argument count mismatch: expected {}, got {}",
            name, expected, actual
        ),
        mlr::TypeError::IfConditionNotBoolean { actual } => eprintln!(
            "Type error in function '{}': If condition must be of type 'bool', got '{}'",
            name,
            ctxt.type_registry.get_string_rep(actual)
        ),
        mlr::TypeError::IfBranchTypeMismatch { then_type, else_type } => eprintln!(
            "Type error in function '{}': If branches must have the same type: then is '{}', else is '{}'",
            name,
            ctxt.type_registry.get_string_rep(then_type),
            ctxt.type_registry.get_string_rep(else_type)
        ),
        mlr::TypeError::ReturnTypeMismatch { expected, actual } => eprintln!(
            "Type error in function '{}': Return type mismatch: expected '{}', got '{}'",
            name,
            ctxt.type_registry.get_string_rep(expected),
            ctxt.type_registry.get_string_rep(actual)
        ),
    };
}
