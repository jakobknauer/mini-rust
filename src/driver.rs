use crate::{function_registry, functions, hlr, parser, type_registry, types};

pub fn compile(source: &str) -> () {
    let hlr = parser::parse_hlr(&source).unwrap();
    let type_registry = build_type_registry(&hlr).unwrap();
    let function_registry = build_function_registry(&hlr, &type_registry);
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

    for function in &program.functions {
        let return_type = type_registry
            .get_type_id_by_name(&function.return_type.as_ref().unwrap())
            .unwrap();

        let parameters = function
            .parameters
            .iter()
            .map(|parameter| functions::FunctionParameter {
                name: parameter.name.clone(),
                type_: type_registry.get_type_id_by_name(&parameter.param_type).unwrap(),
            })
            .collect();

        let signature = functions::FunctionSignature {
            return_type,
            parameters,
        };

        function_registry.register_function(&function.name, signature)?;
    }

    Ok(function_registry)
}
