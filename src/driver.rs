mod err;
mod stdlib;

use crate::{
    ctxt::{self, functions, types},
    generate, hlr, mlr,
    util::print,
};

pub fn compile(source: &str, print_fn: impl Fn(&str)) -> Result<String, String> {
    let mut ctxt = ctxt::Ctxt::new();

    print_fn("Building HLR from source");
    let hlr = hlr::build_program(source).map_err(|parser_error| err::print_parser_error(&parser_error, source))?;

    print_fn("Building MLR from HLR");
    register_types(&hlr, &mut ctxt.type_registry).map_err(|_| "Error registering types")?;
    define_types(&hlr, &mut ctxt.type_registry).map_err(|_| "Error defining types")?;
    register_functions(&hlr, &ctxt.type_registry, &mut ctxt.function_registry)
        .map_err(|_| "Error registering functions")?;
    build_function_mlrs(&hlr, &mut ctxt).map_err(|err| format!("Error building MLR: {err}"))?;
    print_functions(&ctxt).map_err(|_| "Error printing MLR")?;

    print_fn("Simplifying MLR");
    for (_, mlr) in ctxt.function_registry.iter_defined_functions() {
        mlr::opt::simplify(mlr);
    }

    print_fn("Building LLVM IR from MLR");
    let llvm_ir = generate::generate_llvm_ir(&ctxt);

    Ok(llvm_ir)
}

fn register_types(program: &hlr::Program, type_registry: &mut ctxt::TypeRegistry) -> Result<(), ()> {
    type_registry.register_primitive_types()?;

    for struct_ in &program.structs {
        type_registry.register_struct(&struct_.name)?;
    }

    for enum_ in &program.enums {
        type_registry.register_enum(&enum_.name)?;
        for variant in &enum_.variants {
            let variant_struct_name = format!("{}::{}", enum_.name, variant.name);
            type_registry.register_struct(&variant_struct_name)?;
        }
    }

    Ok(())
}

fn define_types(program: &hlr::Program, type_registry: &mut ctxt::TypeRegistry) -> Result<(), ()> {
    for struct_ in &program.structs {
        set_struct_fields(type_registry, &struct_.name, &struct_.fields)?
    }

    for enum_ in &program.enums {
        let variants = enum_
            .variants
            .iter()
            .map(|variant| {
                let variant_struct_name = format!("{}::{}", enum_.name, variant.name);
                let type_id = type_registry.get_type_id_by_name(&variant_struct_name).ok_or(())?;

                set_struct_fields(type_registry, &variant_struct_name, &variant.fields)?;

                let variant = types::EnumVariant {
                    name: variant.name.clone(),
                    type_id,
                };

                Ok(variant)
            })
            .collect::<Result<_, _>>()?;

        let enum_definition = type_registry.get_mut_enum_definition_by_name(&enum_.name).ok_or(())?;
        enum_definition.variants = variants;
    }

    Ok(())
}

fn set_struct_fields<'a>(
    type_registry: &mut ctxt::TypeRegistry,
    struct_name: &str,
    fields: impl IntoIterator<Item = &'a hlr::StructField>,
) -> Result<(), ()> {
    let fields = fields
        .into_iter()
        .map(|field| {
            Ok(types::StructField {
                name: field.name.clone(),
                type_id: type_registry.get_type_id_by_name(&field.field_type).ok_or(())?,
            })
        })
        .collect::<Result<_, _>>()?;

    let struct_definition = type_registry.get_mut_struct_definition_by_name(struct_name).ok_or(())?;
    struct_definition.fields = fields;

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
            .map_err(|err| err::print_mlr_builder_error(&function.name, err, ctxt))?;

        ctxt.function_registry.add_function_def(&function.name, mlr);
    }

    Ok(())
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
