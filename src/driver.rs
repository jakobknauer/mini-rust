mod err;
mod stdlib;
mod trait_check;

use std::{
    collections::{HashMap, HashSet, VecDeque},
    path::Path,
};

use crate::{
    ctxt::{self, fns, impls, traits, ty},
    driver::{err::print_trait_check_error, trait_check::check_trait_impls},
    h2m, hlr, m2inkwell,
    util::print,
};

#[derive(Default)]
pub struct OutputPaths<'a> {
    pub mlr: Option<&'a Path>,
    pub llvm_ir: Option<&'a Path>,
}

#[derive(Default)]
struct HlrMetadata {
    pub fn_ids: HashMap<usize, fns::Fn>,
    pub struct_ids: HashMap<usize, ty::Struct>,
    pub enum_ids: HashMap<usize, ty::Enum>,
    pub impl_ids: HashMap<usize, impls::Impl>,
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

    let mut hlr_meta = HlrMetadata::default();

    print_pretty("Building MLR from HLR");
    register_tys(&hlr, &mut ctxt.tys, &mut hlr_meta).map_err(|_| "Error registering types")?;
    register_traits(&hlr, &mut ctxt).map_err(|_| "Error registering traits")?;
    define_tys(&hlr, &mut ctxt.tys, &hlr_meta).map_err(|_| "Error defining types")?;
    register_functions(&hlr, &mut ctxt.tys, &mut ctxt.fns, &mut hlr_meta).map_err(|_| "Error registering functions")?;
    register_impls(&hlr, &mut ctxt, &mut hlr_meta).map_err(|_| "Error registering impls")?;
    check_trait_impls(&mut ctxt).map_err(|err| print_trait_check_error(err, &ctxt))?;
    build_function_mlrs(&hlr, &mut ctxt, &hlr_meta).map_err(|err| format!("Error building MLR: {err}"))?;
    build_impl_fn_mlrs(&hlr, &mut ctxt, &hlr_meta).map_err(|err| format!("Error building MLR for impls: {err}"))?;

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

fn register_tys(program: &hlr::Program, tys: &mut ctxt::TyReg, hlr_meta: &mut HlrMetadata) -> Result<(), ()> {
    tys.register_primitive_tys()?;

    for (idx, struct_) in program.structs.iter().enumerate() {
        let ty = tys.register_struct(&struct_.name, &struct_.gen_params)?;
        hlr_meta.struct_ids.insert(idx, ty);
    }

    for (idx, enum_) in program.enums.iter().enumerate() {
        let ty = tys.register_enum(&enum_.name, &enum_.gen_params)?;
        hlr_meta.enum_ids.insert(idx, ty);

        for variant in &enum_.variants {
            let variant_struct_name = format!("{}::{}", enum_.name, variant.name);
            let variant_ty = tys.register_struct(&variant_struct_name, &enum_.gen_params)?;

            let enum_def = tys.get_mut_enum_def(ty).ok_or(())?;

            enum_def.variants.push(ty::EnumVariant {
                name: variant.name.clone(),
                struct_: variant_ty,
            });
        }
    }

    Ok(())
}

fn define_tys(program: &hlr::Program, tys: &mut ctxt::TyReg, hlr_meta: &HlrMetadata) -> Result<(), ()> {
    for (idx, struct_) in program.structs.iter().enumerate() {
        set_struct_fields(tys, hlr_meta.struct_ids[&idx], &struct_.fields)?
    }

    for (idx, hlr_enum) in program.enums.iter().enumerate() {
        let enum_ = hlr_meta.enum_ids[&idx];
        let variants = tys.get_enum_def(enum_).ok_or(())?.variants.clone();

        for (hlr_variant, variant) in hlr_enum.variants.iter().zip(variants) {
            set_struct_fields(tys, variant.struct_, &hlr_variant.fields)?;
        }
    }

    Ok(())
}

fn set_struct_fields<'a>(
    tys: &mut ctxt::TyReg,
    struct_: ty::Struct,
    fields: impl IntoIterator<Item = &'a hlr::StructField>,
) -> Result<(), ()> {
    let gen_params = tys.get_struct_def(struct_).ok_or(())?.gen_params.clone();

    let fields = fields
        .into_iter()
        .map(|field| {
            Ok(ty::StructField {
                name: field.name.clone(),
                ty: tys.try_resolve_hlr_annot(&field.ty, &gen_params, None).ok_or(())?,
            })
        })
        .collect::<Result<_, _>>()?;

    let struct_def = tys.get_mut_struct_def(struct_).ok_or(())?;
    struct_def.fields = fields;

    Ok(())
}

fn register_functions(
    hlr: &hlr::Program,
    tys: &mut ctxt::TyReg,
    fns: &mut ctxt::FnReg,
    hlr_meta: &mut HlrMetadata,
) -> Result<(), ()> {
    stdlib::register_fns(tys, fns)?;

    for (idx, function) in hlr.fns.iter().enumerate() {
        let fn_ = register_function(function, tys, fns, None, None, Vec::new())?;
        hlr_meta.fn_ids.insert(idx, fn_);
    }

    Ok(())
}

fn register_function(
    hlr_fn: &hlr::Fn,
    tys: &mut ctxt::TyReg,
    fns: &mut ctxt::FnReg,
    associated_ty: Option<ty::Ty>,
    associated_trait: Option<traits::Trait>,
    env_gen_params: Vec<ty::GenVar>,
) -> Result<fns::Fn, ()> {
    let gen_params: Vec<_> = hlr_fn.gen_params.iter().map(|gp| tys.register_gen_var(gp)).collect();
    let all_gen_params: Vec<_> = gen_params.iter().chain(env_gen_params.iter()).cloned().collect();

    let params = hlr_fn
        .params
        .iter()
        .map(|parameter| {
            Ok(fns::FnParam {
                name: parameter.name.clone(),
                ty: tys
                    .try_resolve_hlr_annot(&parameter.ty, &all_gen_params, associated_ty)
                    .ok_or(())?,
            })
        })
        .collect::<Result<_, _>>()?;

    let return_ty = match hlr_fn.return_ty.as_ref() {
        Some(ty) => tys
            .try_resolve_hlr_annot(ty, &all_gen_params, associated_ty)
            .ok_or(())?,
        None => tys.get_primitive_ty(ctxt::ty::Primitive::Unit),
    };

    let signature = fns::FnSig {
        name: hlr_fn.name.clone(),
        associated_ty,
        associated_trait,
        gen_params,
        env_gen_params,
        params,
        var_args: hlr_fn.var_args,
        return_ty,
        has_receiver: hlr_fn.params.first().map(|p| p.is_receiver).unwrap_or(false),
    };

    fns.register_fn(signature)
}

fn register_traits(hlr: &hlr::Program, ctxt: &mut ctxt::Ctxt) -> Result<(), ()> {
    for hlr_trait in &hlr.traits {
        let trait_ = ctxt.traits.register_trait(&hlr_trait.name);
        let self_type = ctxt.tys.register_trait_self_type(trait_);

        for method in &hlr_trait.methods {
            let gen_params: Vec<_> = method
                .gen_params
                .iter()
                .map(|gp| ctxt.tys.register_gen_var(gp))
                .collect();
            let params = method
                .params
                .iter()
                .map(|parameter| {
                    Ok(fns::FnParam {
                        name: parameter.name.clone(),
                        ty: ctxt
                            .tys
                            .try_resolve_hlr_annot(&parameter.ty, &gen_params, Some(self_type))
                            .ok_or(())?,
                    })
                })
                .collect::<Result<_, _>>()?;

            let return_ty = match &method.return_ty {
                Some(ty) => ctxt
                    .tys
                    .try_resolve_hlr_annot(ty, &gen_params, Some(self_type))
                    .ok_or(())?,
                None => ctxt.tys.get_primitive_ty(ctxt::ty::Primitive::Unit),
            };

            let sig = fns::FnSig {
                name: method.name.clone(),
                associated_ty: None,
                associated_trait: Some(trait_),
                gen_params,
                env_gen_params: Vec::new(),
                params,
                var_args: false,
                return_ty,
                has_receiver: method.params.first().map(|p| p.is_receiver).unwrap_or(false),
            };

            ctxt.traits.register_method(trait_, sig);
        }
    }

    Ok(())
}

fn register_impls(hlr: &hlr::Program, ctxt: &mut ctxt::Ctxt, hlr_meta: &mut HlrMetadata) -> Result<(), ()> {
    for (idx, hlr_impl) in hlr.impls.iter().enumerate() {
        let gen_params: Vec<_> = hlr_impl
            .gen_params
            .iter()
            .map(|gp| ctxt.tys.register_gen_var(gp))
            .collect();

        let ty = ctxt
            .tys
            .try_resolve_hlr_annot(&hlr_impl.ty, &gen_params, None)
            .ok_or(())?;

        let trait_ = hlr_impl
            .trait_name
            .as_ref()
            .map(|trait_name| ctxt.traits.resolve_trait_name(trait_name).ok_or(()))
            .transpose()?;

        let impl_ = ctxt.impls.register_impl(ty, gen_params.clone(), trait_);
        hlr_meta.impl_ids.insert(idx, impl_);

        for method in &hlr_impl.methods {
            let fn_ = register_function(
                method,
                &mut ctxt.tys,
                &mut ctxt.fns,
                Some(ty),
                trait_,
                gen_params.clone(),
            )?;
            ctxt.impls.register_method(impl_, fn_, &method.name);
        }
    }

    Ok(())
}

fn build_function_mlrs(hlr: &hlr::Program, ctxt: &mut ctxt::Ctxt, hlr_meta: &HlrMetadata) -> Result<(), String> {
    for (idx, hlr_fn) in hlr.fns.iter().enumerate() {
        if hlr_fn.body.is_none() {
            continue;
        }

        let target_fn = hlr_meta.fn_ids[&idx];

        let mlr = h2m::hlr_to_mlr(ctxt, hlr_fn, target_fn)
            .map_err(|err| err::print_mlr_builder_error(&hlr_fn.name, err, ctxt))?;

        ctxt.fns.add_fn_def(target_fn, mlr);
    }

    stdlib::define_size_of(ctxt)?;

    Ok(())
}

fn build_impl_fn_mlrs(hlr: &hlr::Program, ctxt: &mut ctxt::Ctxt, hlr_meta: &HlrMetadata) -> Result<(), String> {
    for (idx, hlr_impl) in hlr.impls.iter().enumerate() {
        let impl_ = hlr_meta.impl_ids[&idx];
        let impl_def = ctxt.impls.get_impl_def(impl_);
        let impl_methods = impl_def.methods.clone();

        for (hlr_method, target_fn) in hlr_impl.methods.iter().zip(impl_methods) {
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
        env_gen_args: Vec::new(),
    });

    let mut closed = HashSet::new();

    while let Some(current) = open.pop_front() {
        if closed.contains(&current) {
            continue;
        }

        let subst = ctxt.fns.get_substitutions_for_specialization(&current);

        let fn_specs = ctxt.fns.get_called_specializations(&current.fn_).iter().map(|fn_spec| {
            let new_gen_args = fn_spec
                .gen_args
                .iter()
                .map(|&ty| ctxt.tys.substitute_gen_vars(ty, &subst))
                .collect();

            fns::FnSpecialization {
                fn_: fn_spec.fn_,
                gen_args: new_gen_args,
                env_gen_args: fn_spec.env_gen_args.clone(),
            }
        });
        open.extend(fn_specs);

        let called_trait_methods = ctxt.fns.get_called_trait_methods(&current.fn_).to_vec();
        let trait_fn_specs = called_trait_methods
            .into_iter()
            .map(|trait_method| ctxt.specialize_trait_method_call(&trait_method, &subst));
        open.extend(trait_fn_specs);

        closed.insert(current);
    }

    Ok(closed)
}
