mod err;
mod stdlib;

use std::path::Path;

use crate::{
    ctxt::{self, fns, ty},
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
    register_tys(&hlr, &mut ctxt.tys).map_err(|_| "Error registering types")?;
    define_tys(&hlr, &mut ctxt.tys).map_err(|_| "Error defining types")?;
    register_functions(&hlr, &ctxt.tys, &mut ctxt.fns).map_err(|_| "Error registering functions")?;
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

fn register_tys(program: &hlr::Program, tys: &mut ctxt::TyReg) -> Result<(), ()> {
    tys.register_primitive_tys()?;

    for struct_ in &program.structs {
        tys.register_struct(&struct_.name)?;
    }

    for enum_ in &program.enums {
        tys.register_enum(&enum_.name)?;
        for variant in &enum_.variants {
            let variant_struct_name = format!("{}::{}", enum_.name, variant.name);
            tys.register_struct(&variant_struct_name)?;
        }
    }

    Ok(())
}

fn define_tys(program: &hlr::Program, tys: &mut ctxt::TyReg) -> Result<(), ()> {
    for struct_ in &program.structs {
        set_struct_fields(tys, &struct_.name, &struct_.fields)?
    }

    for enum_ in &program.enums {
        let variants = enum_
            .variants
            .iter()
            .map(|variant| {
                let variant_struct_name = format!("{}::{}", enum_.name, variant.name);
                let ty = tys.get_ty_by_name(&variant_struct_name).ok_or(())?;

                set_struct_fields(tys, &variant_struct_name, &variant.fields)?;

                let variant = ty::EnumVariant {
                    name: variant.name.clone(),
                    ty,
                };

                Ok(variant)
            })
            .collect::<Result<_, _>>()?;

        let enum_definition = tys.get_mut_enum_definition_by_name(&enum_.name).ok_or(())?;
        enum_definition.variants = variants;
    }

    Ok(())
}

fn set_struct_fields<'a>(
    tys: &mut ctxt::TyReg,
    struct_name: &str,
    fields: impl IntoIterator<Item = &'a hlr::StructField>,
) -> Result<(), ()> {
    let fields = fields
        .into_iter()
        .map(|field| {
            Ok(ty::StructField {
                name: field.name.clone(),
                ty: tys.get_ty_by_name(&field.ty).ok_or(())?,
            })
        })
        .collect::<Result<_, _>>()?;

    let struct_definition = tys.get_mut_struct_definition_by_name(struct_name).ok_or(())?;
    struct_definition.fields = fields;

    Ok(())
}

fn register_functions(hlr: &hlr::Program, tys: &ctxt::TyReg, fns: &mut ctxt::FnReg) -> Result<(), ()> {
    stdlib::register_fns(tys, fns)?;

    for function in &hlr.fns {
        let return_ty = match function.return_ty.as_ref() {
            Some(ty) => tys.get_ty_by_name(ty).ok_or(())?,
            None => tys.get_ty_by_name("()").ok_or(())?,
        };

        let parameters = function
            .parameters
            .iter()
            .map(|parameter| {
                Ok(fns::FnParam {
                    name: parameter.name.clone(),
                    ty: tys.get_ty_by_name(&parameter.ty).ok_or(())?,
                })
            })
            .collect::<Result<_, _>>()?;

        let signature = fns::FnSig {
            name: function.name.clone(),
            return_ty,
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
