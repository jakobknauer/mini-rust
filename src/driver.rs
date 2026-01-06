mod err;
mod impl_check;
mod stdlib;

use std::{
    collections::{HashMap, HashSet, VecDeque},
    path::Path,
};

use crate::{
    ctxt::{
        self, fns, impls,
        traits::{self, TraitInst},
        ty,
    },
    driver::{
        err::{print_impl_check_error, print_obligation_check_error},
        impl_check::check_trait_impls,
    },
    h2m, hlr, m2inkwell,
    obligation_check::check_obligations,
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
    sources: &[String],
    print_pretty: impl Fn(&str),
    print_detail: impl Fn(&str),
    output_paths: &OutputPaths,
) -> Result<(), String> {
    let mut ctxt = ctxt::Ctxt::default();

    print_pretty("Building HLR from source");
    let mut hlr = hlr::Program::default();
    for source in sources {
        hlr::parse(source, &mut hlr).map_err(|parser_err| err::print_parser_err(&parser_err, source))?;
    }

    let mut hlr_meta = HlrMetadata::default();

    print_pretty("Building MLR from HLR");
    register_tys(&hlr, &mut ctxt.tys, &mut hlr_meta).map_err(|_| "Error registering types")?;
    register_traits(&hlr, &mut ctxt).map_err(|_| "Error registering traits")?;
    define_tys(&hlr, &mut ctxt.tys, &hlr_meta).map_err(|_| "Error defining types")?;
    register_functions(&hlr, &mut ctxt, &mut hlr_meta).map_err(|_| "Error registering functions")?;
    register_impls(&hlr, &mut ctxt, &mut hlr_meta).map_err(|_| "Error registering impls")?;
    check_trait_impls(&mut ctxt).map_err(|err| print_impl_check_error(err, &ctxt))?;
    build_function_mlrs(&hlr, &mut ctxt, &hlr_meta).map_err(|err| format!("Error building MLR: {err}"))?;
    build_impl_fn_mlrs(&hlr, &mut ctxt, &hlr_meta).map_err(|err| format!("Error building MLR for impls: {err}"))?;
    check_obligations(&mut ctxt).map_err(|err| print_obligation_check_error(err, &ctxt))?;

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
                ty: tys
                    .try_resolve_hlr_annot(&field.ty, &gen_params, None, false)
                    .ok_or(())?,
            })
        })
        .collect::<Result<_, _>>()?;

    let struct_def = tys.get_mut_struct_def(struct_).ok_or(())?;
    struct_def.fields = fields;

    Ok(())
}

fn register_functions(hlr: &hlr::Program, ctxt: &mut ctxt::Ctxt, hlr_meta: &mut HlrMetadata) -> Result<(), ()> {
    stdlib::register_fns(&mut ctxt.tys, &mut ctxt.fns)?;

    for (idx, function) in hlr.fns.iter().enumerate() {
        let fn_ = register_function(function, ctxt, None, None, Vec::new())?;
        hlr_meta.fn_ids.insert(idx, fn_);
    }

    Ok(())
}

fn register_function(
    hlr_fn: &hlr::Fn,
    ctxt: &mut ctxt::Ctxt,
    associated_ty: Option<ty::Ty>,
    associated_trait_inst: Option<traits::TraitInst>,
    env_gen_params: Vec<ty::GenVar>,
) -> Result<fns::Fn, ()> {
    let gen_params: Vec<_> = hlr_fn
        .gen_params
        .iter()
        .map(|gp| ctxt.tys.register_gen_var(gp))
        .collect();
    let all_gen_params: Vec<_> = gen_params.iter().chain(&env_gen_params).cloned().collect();

    let params = hlr_fn
        .params
        .iter()
        .enumerate()
        .map(|(idx, param)| build_fn_param(&mut ctxt.tys, param, &all_gen_params, associated_ty, idx == 0))
        .collect::<Result<_, _>>()?;

    let return_ty = match hlr_fn.return_ty.as_ref() {
        Some(ty) => ctxt
            .tys
            .try_resolve_hlr_annot(ty, &all_gen_params, associated_ty, false)
            .ok_or(())?,
        None => ctxt.tys.register_unit_ty(),
    };

    for constraint in &hlr_fn.constraints {
        let subject = gen_params
            .iter()
            .cloned()
            .find(|&gp| ctxt.tys.get_gen_var_name(gp) == constraint.subject)
            .ok_or(())?;

        match &constraint.requirement {
            hlr::ConstraintRequirement::Trait { trait_name, trait_args } => {
                let trait_ = ctxt.traits.resolve_trait_name(trait_name).ok_or(())?;
                let trait_args = trait_args
                    .iter()
                    .map(|arg| {
                        ctxt.tys
                            .try_resolve_hlr_annot(arg, &all_gen_params, associated_ty, false)
                            .ok_or(())
                    })
                    .collect::<Result<_, _>>()?;
                let trait_inst = TraitInst {
                    trait_,
                    gen_args: trait_args,
                };
                ctxt.tys.add_implements_trait_constraint(subject, trait_inst);
            }
            hlr::ConstraintRequirement::Callable { params, return_ty } => {
                let params = params
                    .iter()
                    .map(|ty| {
                        ctxt.tys
                            .try_resolve_hlr_annot(ty, &all_gen_params, associated_ty, false)
                            .ok_or(())
                    })
                    .collect::<Result<_, _>>()?;
                let return_ty = match return_ty {
                    Some(return_ty) => ctxt
                        .tys
                        .try_resolve_hlr_annot(return_ty, &all_gen_params, associated_ty, false)
                        .ok_or(())?,
                    None => ctxt.tys.register_unit_ty(),
                };
                ctxt.tys.add_callable_constraint(subject, params, return_ty);
            }
        }
    }

    let signature = fns::FnSig {
        name: hlr_fn.name.clone(),
        associated_ty,
        associated_trait_inst,
        gen_params,
        env_gen_params,
        params,
        var_args: hlr_fn.var_args,
        return_ty,
    };

    ctxt.fns.register_fn(signature, associated_ty.is_none())
}

fn build_fn_param(
    tys: &mut ctxt::TyReg,
    param: &hlr::Param,
    gen_params: &[ty::GenVar],
    self_ty: Option<ty::Ty>,
    allow_receiver: bool,
) -> Result<fns::FnParam, ()> {
    match param {
        hlr::Param::Regular { name, ty } => Ok(fns::FnParam {
            kind: fns::FnParamKind::Regular(name.clone()),
            ty: tys.try_resolve_hlr_annot(ty, gen_params, self_ty, false).ok_or(())?,
        }),
        hlr::Param::Receiver if allow_receiver => Ok(fns::FnParam {
            kind: fns::FnParamKind::Self_,
            ty: self_ty.ok_or(())?,
        }),
        hlr::Param::ReceiverByRef if allow_receiver => Ok(fns::FnParam {
            kind: fns::FnParamKind::SelfByRef,
            ty: self_ty.map(|self_ty| tys.register_ref_ty(self_ty)).ok_or(())?,
        }),
        _ => Err(()),
    }
}

fn register_traits(hlr: &hlr::Program, ctxt: &mut ctxt::Ctxt) -> Result<(), ()> {
    for hlr_trait in &hlr.traits {
        let trait_gen_params: Vec<_> = hlr_trait
            .gen_params
            .iter()
            .map(|gp| ctxt.tys.register_gen_var(gp))
            .collect();

        let trait_ = ctxt.traits.register_trait(&hlr_trait.name, trait_gen_params.clone());
        let self_type = ctxt.tys.register_trait_self_type(trait_);

        for method in &hlr_trait.methods {
            let method_gen_params: Vec<_> = method
                .gen_params
                .iter()
                .map(|gp| ctxt.tys.register_gen_var(gp))
                .collect();

            let all_gen_params: Vec<_> = method_gen_params.iter().chain(&trait_gen_params).cloned().collect();

            let params = method
                .params
                .iter()
                .enumerate()
                .map(|(idx, param)| build_fn_param(&mut ctxt.tys, param, &all_gen_params, Some(self_type), idx == 0))
                .collect::<Result<_, _>>()?;

            let return_ty = match &method.return_ty {
                Some(ty) => ctxt
                    .tys
                    .try_resolve_hlr_annot(ty, &all_gen_params, Some(self_type), false)
                    .ok_or(())?,
                None => ctxt.tys.register_unit_ty(),
            };

            let trait_inst = traits::TraitInst {
                trait_,
                gen_args: method_gen_params
                    .iter()
                    .map(|&gp| ctxt.tys.register_gen_var_ty(gp))
                    .collect(),
            };

            let sig = fns::FnSig {
                name: method.name.clone(),
                associated_ty: None,
                associated_trait_inst: Some(trait_inst),
                gen_params: method_gen_params,
                env_gen_params: trait_gen_params.clone(),
                params,
                var_args: false,
                return_ty,
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
            .try_resolve_hlr_annot(&hlr_impl.ty, &gen_params, None, false)
            .ok_or(())?;

        let trait_inst = hlr_impl
            .trait_name
            .as_ref()
            .map(|trait_name| {
                let trait_ = ctxt.traits.resolve_trait_name(trait_name).ok_or(())?;

                let trait_args = hlr_impl
                    .trait_args
                    .iter()
                    .map(|arg| ctxt.tys.try_resolve_hlr_annot(arg, &gen_params, None, false).ok_or(()))
                    .collect::<Result<_, _>>()?;

                let trait_inst = TraitInst {
                    trait_,
                    gen_args: trait_args,
                };
                Ok(trait_inst)
            })
            .transpose()?;

        let impl_ = ctxt.impls.register_impl(ty, gen_params.clone(), trait_inst.clone());
        hlr_meta.impl_ids.insert(idx, impl_);

        for method in &hlr_impl.methods {
            let fn_ = register_function(method, ctxt, Some(ty), trait_inst.clone(), gen_params.clone())?;
            ctxt.impls.register_method(impl_, fn_, &method.name);
        }
    }

    Ok(())
}

fn build_function_mlrs(hlr: &hlr::Program, ctxt: &mut ctxt::Ctxt, hlr_meta: &HlrMetadata) -> Result<(), String> {
    stdlib::define_size_of(ctxt)?;

    for (idx, hlr_fn) in hlr.fns.iter().enumerate() {
        let Some(body) = &hlr_fn.body else {
            continue;
        };

        let target_fn = hlr_meta.fn_ids[&idx];

        h2m::hlr_to_mlr(ctxt, body, target_fn).map_err(|err| err::print_mlr_builder_error(&hlr_fn.name, err, ctxt))?;
    }

    Ok(())
}

fn build_impl_fn_mlrs(hlr: &hlr::Program, ctxt: &mut ctxt::Ctxt, hlr_meta: &HlrMetadata) -> Result<(), String> {
    for (idx, hlr_impl) in hlr.impls.iter().enumerate() {
        let impl_ = hlr_meta.impl_ids[&idx];
        let impl_def = ctxt.impls.get_impl_def(impl_);
        let impl_methods = impl_def.methods.clone();

        for (hlr_method, target_fn) in hlr_impl.methods.iter().zip(impl_methods) {
            let Some(body) = &hlr_method.body else {
                continue;
            };
            h2m::hlr_to_mlr(ctxt, body, target_fn)
                .map_err(|err| err::print_mlr_builder_error(&hlr_method.name, err, ctxt))?;
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

        let subst = ctxt.fns.get_subst_for_fn_spec(&current);

        let fn_specs = ctxt.fns.get_called_specializations(current.fn_).iter().map(|fn_spec| {
            let new_gen_args = fn_spec
                .gen_args
                .iter()
                .map(|&ty| ctxt.tys.substitute_gen_vars(ty, &subst))
                .collect();
            let new_env_gen_args = fn_spec
                .env_gen_args
                .iter()
                .map(|&ty| ctxt.tys.substitute_gen_vars(ty, &subst))
                .collect();

            fns::FnSpecialization {
                fn_: fn_spec.fn_,
                gen_args: new_gen_args,
                env_gen_args: new_env_gen_args,
            }
        });
        open.extend(fn_specs);

        let called_trait_methods = ctxt.fns.get_called_trait_methods(current.fn_).to_vec();
        let trait_fn_specs = called_trait_methods
            .into_iter()
            .map(|trait_method| ctxt.resolve_trait_method_to_fn(&trait_method, &subst));
        open.extend(trait_fn_specs);

        closed.insert(current);
    }

    Ok(closed)
}
