use crate::{
    context::{function_registry, functions, type_registry, types},
    hlr,
    util::print,
};

pub fn compile(source: &str) -> () {
    let hlr = hlr::build_program(&source).unwrap();
    let type_registry = build_type_registry(&hlr).unwrap();
    let mut function_registry = build_function_registry(&hlr, &type_registry).unwrap();

    for function in &hlr.functions {
        let mlr_builder = crate::mlr::MlrBuilder::new(&function, &type_registry, &function_registry);
        let mlr = mlr_builder.build().unwrap();

        function_registry.register_function_mlr(&function.name, mlr);
        let fn_id = function_registry.get_function_by_name(&function.name).unwrap();

        print::print_mlr(fn_id, &type_registry, &function_registry, &mut std::io::stdout()).unwrap();
        println!();
    }
}

fn build_type_registry(program: &hlr::Program) -> Result<type_registry::TypeRegistry, ()> {
    let mut type_registry = type_registry::TypeRegistry::new();
    type_registry.register_primitive_types().unwrap();

    for struct_ in &program.structs {
        type_registry.register_struct(&struct_.name).unwrap();
    }

    for enum_ in &program.enums {
        type_registry.register_enum(&enum_.name).unwrap();
    }

    for struct_ in &program.structs {
        let fields = struct_
            .fields
            .iter()
            .map(|field| types::StructField {
                name: field.name.clone(),
                type_id: type_registry.get_type_id_by_name(&field.field_type).unwrap(),
            })
            .collect();

        let struct_definition = type_registry.get_mut_struct_definition_by_name(&struct_.name).unwrap();
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

        let enum_definition = type_registry.get_mut_enum_definition_by_name(&enum_.name).unwrap();
        enum_definition.variants = variants;
    }

    Ok(type_registry)
}

fn build_function_registry(
    program: &hlr::Program,
    type_registry: &type_registry::TypeRegistry,
) -> Result<function_registry::FunctionRegistry, ()> {
    let mut function_registry = function_registry::FunctionRegistry::new();

    let i32_t = type_registry.get_type_id_by_name("i32").unwrap();
    function_registry.register_function(functions::FunctionSignature {
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
    })?;
    function_registry.register_function(functions::FunctionSignature {
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
    })?;

    for function in &program.functions {
        let return_type = match function.return_type.as_ref() {
            Some(type_id) => type_registry.get_type_id_by_name(type_id).unwrap(),
            None => type_registry.get_type_id_by_name("()").unwrap(),
        };

        let parameters = function
            .parameters
            .iter()
            .map(|parameter| functions::FunctionParameter {
                name: parameter.name.clone(),
                type_: type_registry.get_type_id_by_name(&parameter.param_type).unwrap(),
            })
            .collect();

        let signature = functions::FunctionSignature {
            name: function.name.clone(),
            return_type,
            parameters,
        };

        function_registry.register_function(signature)?;
    }

    Ok(function_registry)
}
