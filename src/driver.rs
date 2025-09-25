use crate::{
    ctxt::{self, functions, types},
    hlr, mlr,
    util::print,
};

pub fn compile(source: &str) -> Option<()> {
    let mut ctxt = ctxt::Ctxt::new();

    let hlr = hlr::build_program(&source).ok()?;

    register_and_define_types(&hlr, &mut ctxt.type_registry)?;
    register_functions(&hlr, &ctxt.type_registry, &mut ctxt.function_registry)?;

    for function in &hlr.functions {
        let mlr_builder = mlr::MlrBuilder::new(&function, &ctxt);
        let mlr = mlr_builder.build().ok()?;

        ctxt.function_registry.register_function_mlr(&function.name, mlr);
        let fn_id = ctxt.function_registry.get_function_by_name(&function.name)?;

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
    let i32_t = type_registry.get_type_id_by_name("i32")?;
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
