mod err;
mod stdlib;

use std::path::Path;

use crate::{
    ctxt::{self, fns, types},
    generate, hlr, mlr,
    util::print,
};

#[derive(Default)]
pub struct OutputPaths<T1, T2>
where
    T1: AsRef<Path>,
    T2: AsRef<Path>,
{
    pub mlr: Option<T1>,
    pub llvm_ir: Option<T2>,
}

pub fn compile<T1, T2>(
    source: &str,
    print_pretty: impl Fn(&str),
    print_detail: impl Fn(&str),
    output_paths: OutputPaths<T1, T2>,
) -> Result<(), String>
where
    T1: AsRef<Path>,
    T2: AsRef<Path>,
{
    let mut ctxt = ctxt::Ctxt::new();

    print_pretty("Building HLR from source");
    let hlr = hlr::build_program(source).map_err(|parser_error| err::print_parser_error(&parser_error, source))?;

    print_pretty("Building MLR from HLR");
    register_types(&hlr, &mut ctxt.types).map_err(|_| "Error registering types")?;
    define_types(&hlr, &mut ctxt.types).map_err(|_| "Error defining types")?;
    register_functions(&hlr, &ctxt.types, &mut ctxt.fns).map_err(|_| "Error registering functions")?;
    build_function_mlrs(&hlr, &mut ctxt).map_err(|err| format!("Error building MLR: {err}"))?;

    if let Some(mlr_path) = output_paths.mlr {
        print_detail(&format!("Saving MLR to {}", mlr_path.as_ref().display()));
        print_functions(&ctxt, mlr_path).map_err(|_| "Error printing MLR")?;
    }

    print_pretty("Building LLVM IR from MLR");
    let llvm_ir = generate::generate_llvm_ir(&ctxt);

    if let Some(llvm_ir_path) = output_paths.llvm_ir {
        print_detail(&format!("Saving LLVM IR to {}", llvm_ir_path.as_ref().display()));
        std::fs::write(&llvm_ir_path, &llvm_ir).map_err(|_| "Could not write LLVM IR file")?;
    }

    Ok(())
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

fn register_functions(hlr: &hlr::Program, type_registry: &ctxt::TypeRegistry, fns: &mut ctxt::FnReg) -> Result<(), ()> {
    stdlib::register_fns(type_registry, fns)?;

    for function in &hlr.fns {
        let return_type = match function.return_type.as_ref() {
            Some(type_id) => type_registry.get_type_id_by_name(type_id).ok_or(())?,
            None => type_registry.get_type_id_by_name("()").ok_or(())?,
        };

        let parameters = function
            .parameters
            .iter()
            .map(|parameter| {
                Ok(fns::FnParam {
                    name: parameter.name.clone(),
                    type_: type_registry.get_type_id_by_name(&parameter.param_type).ok_or(())?,
                })
            })
            .collect::<Result<_, _>>()?;

        let signature = fns::FnSig {
            name: function.name.clone(),
            return_type,
            parameters,
        };

        fns.register_fn(signature)?;
    }

    Ok(())
}

fn build_function_mlrs(hlr: &hlr::Program, ctxt: &mut ctxt::Ctxt) -> Result<(), String> {
    for function in &hlr.fns {
        let fn_ = ctxt.fns.get_fn_by_name(&function.name).unwrap();

        let mlr_builder = mlr::MlrBuilder::new(function, fn_, ctxt);
        let mlr = mlr_builder
            .build()
            .map_err(|err| err::print_mlr_builder_error(&function.name, err, ctxt))?;

        ctxt.fns.add_fn_def(&function.name, mlr);
    }

    Ok(())
}

fn print_functions<T>(ctxt: &ctxt::Ctxt, path: T) -> Result<(), ()>
where
    T: AsRef<Path>,
{
    let mut file = std::fs::File::create(path).map_err(|_| ())?;

    for fn_ in ctxt.fns.get_all_fns() {
        if ctxt.fns.is_fn_defined(fn_) {
            print::print_mlr(fn_, ctxt, &mut file).map_err(|_| ())?;
        }
    }

    Ok(())
}
