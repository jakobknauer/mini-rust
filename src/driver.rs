mod err;
mod stdlib;

use std::{
    collections::{HashSet, VecDeque},
    path::Path,
};

use crate::{
    ctxt::{self, fns, ty},
    h2m, hlr, m2inkwell,
    util::print,
};

#[derive(Default)]
pub struct OutputPaths<'a> {
    pub mlr: Option<&'a Path>,
    pub llvm_ir: Option<&'a Path>,
}

pub fn compile(
    source: &str,
    print_pretty: impl Fn(&str),
    print_detail: impl Fn(&str),
    output_paths: &OutputPaths,
) -> Result<(), String> {
    let mut ctxt = ctxt::Ctxt::default();

    print_pretty("Building HLR from source");
    let hlr = hlr::build_program(source).map_err(|parser_err| err::print_parser_err(&parser_err, source))?;

    print_pretty("Building MLR from HLR");
    register_tys(&hlr, &mut ctxt.tys).map_err(|_| "Error registering types")?;
    define_tys(&hlr, &mut ctxt.tys).map_err(|_| "Error defining types")?;
    register_functions(&hlr, &mut ctxt.tys, &mut ctxt.fns).map_err(|_| "Error registering functions")?;
    register_impls(&hlr, &mut ctxt).map_err(|_| "Error registering impls")?;
    build_function_mlrs(&hlr, &mut ctxt).map_err(|err| format!("Error building MLR: {err}"))?;
    build_impl_fn_mlrs(&hlr, &mut ctxt).map_err(|err| format!("Error building MLR for impls: {err}"))?;

    h2m::opt::canonicalize_types(&mut ctxt);

    if let Some(mlr_path) = output_paths.mlr {
        print_detail(&format!("Saving MLR to {}", mlr_path.display()));
        print_functions(&ctxt, mlr_path).map_err(|_| "Error printing MLR")?;
    }

    print_pretty("Collecting monomorphized function specializations");
    let fn_specs = monomorphize_functions(&mut ctxt).map_err(|_| "Error determining concrete specializations")?;

    print_pretty("Building LLVM IR from MLR");
    let llvm_ir = m2inkwell::mlr_to_llvm_ir(&mut ctxt, fn_specs.into_iter().collect());

    if let Some(llvm_ir_path) = output_paths.llvm_ir {
        print_detail(&format!("Saving LLVM IR to {}", llvm_ir_path.display()));
        std::fs::write(llvm_ir_path, &llvm_ir).map_err(|_| "Could not write LLVM IR file")?;
    }

    Ok(())
}

fn register_tys(program: &hlr::Program, tys: &mut ctxt::TyReg) -> Result<(), ()> {
    tys.register_primitive_tys()?;

    for struct_ in &program.structs {
        tys.register_struct(&struct_.name, &struct_.gen_params)?;
    }

    for enum_ in &program.enums {
        tys.register_enum(&enum_.name, &enum_.gen_params)?;
        for variant in &enum_.variants {
            let variant_struct_name = format!("{}::{}", enum_.name, variant.name);
            tys.register_struct(&variant_struct_name, &enum_.gen_params)?;
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
                let struct_ = tys.get_struct_by_name(&variant_struct_name).ok_or(())?;

                set_struct_fields(tys, &variant_struct_name, &variant.fields)?;

                let variant = ty::EnumVariant {
                    name: variant.name.clone(),
                    struct_,
                };

                Ok(variant)
            })
            .collect::<Result<_, _>>()?;

        let enum_def = tys.get_mut_enum_def_by_name(&enum_.name).ok_or(())?;
        enum_def.variants = variants;
    }

    Ok(())
}

fn set_struct_fields<'a>(
    tys: &mut ctxt::TyReg,
    struct_name: &str,
    fields: impl IntoIterator<Item = &'a hlr::StructField>,
) -> Result<(), ()> {
    let gen_params = tys.get_struct_def_by_name(struct_name).ok_or(())?.gen_params.clone();

    let fields = fields
        .into_iter()
        .map(|field| {
            Ok(ty::StructField {
                name: field.name.clone(),
                ty: tys.try_resolve_hlr_annot(&field.ty, &gen_params, None).ok_or(())?,
            })
        })
        .collect::<Result<_, _>>()?;

    let struct_def = tys.get_mut_struct_def_by_name(struct_name).ok_or(())?;
    struct_def.fields = fields;

    Ok(())
}

fn register_functions(hlr: &hlr::Program, tys: &mut ctxt::TyReg, fns: &mut ctxt::FnReg) -> Result<(), ()> {
    stdlib::register_fns(tys, fns)?;

    for function in &hlr.fns {
        register_function(function, tys, fns, None)?;
    }

    Ok(())
}

fn register_function(
    hlr_fn: &hlr::Fn,
    tys: &mut ctxt::TyReg,
    fns: &mut ctxt::FnReg,
    self_ty: Option<ty::Ty>,
) -> Result<fns::Fn, ()> {
    let gen_params: Vec<_> = hlr_fn.gen_params.iter().map(|gp| tys.register_gen_var(gp)).collect();

    let params = hlr_fn
        .params
        .iter()
        .map(|parameter| {
            Ok(fns::FnParam {
                name: parameter.name.clone(),
                ty: tys
                    .try_resolve_hlr_annot(&parameter.ty, &gen_params, self_ty)
                    .ok_or(())?,
            })
        })
        .collect::<Result<_, _>>()?;

    let return_ty = match hlr_fn.return_ty.as_ref() {
        Some(ty) => tys.try_resolve_hlr_annot(ty, &gen_params, self_ty).ok_or(())?,
        None => tys.get_primitive_ty(ctxt::ty::Primitive::Unit),
    };

    let name = if let Some(self_ty) = self_ty {
        format!("{}::{}", tys.get_string_rep(self_ty), hlr_fn.name)
    } else {
        hlr_fn.name.clone()
    };

    let signature = fns::FnSig {
        name,
        gen_params,
        params,
        var_args: hlr_fn.var_args,
        return_ty,
        has_receiver: hlr_fn.params.first().map(|p| p.is_receiver).unwrap_or(false),
    };

    fns.register_fn(signature)
}

fn register_impls(hlr: &hlr::Program, ctxt: &mut ctxt::Ctxt) -> Result<(), ()> {
    for hlr_impl in &hlr.impls {
        let ty = ctxt.tys.try_resolve_hlr_annot(&hlr_impl.ty, &[], None).ok_or(())?;
        let impl_ = ctxt.impls.register_impl(ty);

        for method in &hlr_impl.methods {
            let fn_ = register_function(method, &mut ctxt.tys, &mut ctxt.fns, Some(ty))?;
            ctxt.impls.register_method(impl_, fn_);
        }
    }

    Ok(())
}

fn build_function_mlrs(hlr: &hlr::Program, ctxt: &mut ctxt::Ctxt) -> Result<(), String> {
    for hlr_fn in &hlr.fns {
        if hlr_fn.body.is_none() {
            continue;
        }

        let target_fn = ctxt.fns.get_fn_by_name(&hlr_fn.name).unwrap();

        let mlr = h2m::hlr_to_mlr(ctxt, hlr_fn, target_fn)
            .map_err(|err| err::print_mlr_builder_error(&hlr_fn.name, err, ctxt))?;

        ctxt.fns.add_fn_def(target_fn, mlr);
    }

    stdlib::define_size_of(ctxt)?;

    Ok(())
}

fn build_impl_fn_mlrs(hlr: &hlr::Program, ctxt: &mut ctxt::Ctxt) -> Result<(), String> {
    for hlr_impl in &hlr.impls {
        let ty = ctxt.tys.try_resolve_hlr_annot(&hlr_impl.ty, &[], None).unwrap();
        let ty_name = ctxt.tys.get_string_rep(ty);

        for hlr_method in &hlr_impl.methods {
            let name = format!("{}::{}", ty_name, hlr_method.name);
            let target_fn = ctxt.fns.get_fn_by_name(&name).unwrap();

            let mlr = h2m::hlr_to_mlr(ctxt, hlr_method, target_fn)
                .map_err(|err| err::print_mlr_builder_error(&hlr_method.name, err, ctxt))?;

            ctxt.fns.add_fn_def(target_fn, mlr);
        }
    }

    Ok(())
}

fn print_functions(ctxt: &ctxt::Ctxt, path: &Path) -> Result<(), ()> {
    let mut file = std::fs::File::create(path).map_err(|_| ())?;

    for fn_ in ctxt.fns.get_all_fns() {
        if ctxt.fns.is_fn_defined(fn_) {
            print::print_mlr(fn_, ctxt, &mut file).map_err(|_| ())?;
        }
    }

    Ok(())
}

fn monomorphize_functions(ctxt: &mut ctxt::Ctxt) -> Result<HashSet<fns::FnSpecialization>, ()> {
    let mut open = VecDeque::new();
    open.push_back(fns::FnSpecialization {
        fn_: ctxt.fns.get_fn_by_name("main").ok_or(())?,
        gen_args: Vec::new(),
    });

    let mut closed = HashSet::new();

    while let Some(current) = open.pop_front() {
        if closed.contains(&current) {
            continue;
        }

        let subst = ctxt.fns.get_substitutions_for_specialization(&current);

        let fn_specs =
            ctxt.fns
                .get_called_specializations(&current.fn_)
                .iter()
                .map(|fns::FnSpecialization { fn_, gen_args }| {
                    let new_gen_args = gen_args
                        .iter()
                        .map(|&ty| ctxt.tys.substitute_gen_vars(ty, &subst))
                        .collect();

                    fns::FnSpecialization {
                        fn_: *fn_,
                        gen_args: new_gen_args,
                    }
                });

        open.extend(fn_specs);
        closed.insert(current);
    }

    Ok(closed)
}
