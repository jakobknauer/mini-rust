mod err;
mod impl_check;
mod stdlib;

use std::{
    collections::{HashMap, HashSet, VecDeque},
    path::Path,
};

use crate::{
    ast, ast_lowering, ast_parsing,
    ctxt::{
        self, fns, impls,
        traits::{self, TraitInst},
        ty,
    },
    driver::{
        err::{print_impl_check_error, print_obligation_check_error},
        impl_check::check_trait_impls,
    },
    mlr_lowering,
    obligation_check::check_obligations,
    util::print,
};

#[derive(Default)]
pub struct OutputPaths<'a> {
    pub mlr: Option<&'a Path>,
    pub llvm_ir: Option<&'a Path>,
}

pub fn compile(
    sources: &[String],
    print_pretty: impl Fn(&str),
    print_detail: impl Fn(&str),
    output_paths: &OutputPaths,
) -> Result<(), String> {
    let mut driver = Driver {
        ctxt: ctxt::Ctxt::default(),
        ast_meta: AstMeta::default(),
        sources: sources.to_vec(),
        print_pretty: &print_pretty,
        print_detail: &print_detail,
        output_paths,
    };

    driver.compile()
}

struct Driver<'a> {
    ctxt: ctxt::Ctxt,
    sources: Vec<String>,
    print_pretty: &'a dyn Fn(&str),
    print_detail: &'a dyn Fn(&str),
    output_paths: &'a OutputPaths<'a>,
    ast_meta: AstMeta,
}

#[derive(Default)]
struct AstMeta {
    pub fn_ids: HashMap<usize, fns::Fn>,
    pub struct_ids: HashMap<usize, ty::Struct>,
    pub enum_ids: HashMap<usize, ty::Enum>,
    pub trait_ids: HashMap<usize, traits::Trait>,
    pub impl_ids: HashMap<usize, impls::Impl>,
}

impl<'a> Driver<'a> {
    pub fn compile(&mut self) -> Result<(), String> {
        self.print_pretty("Building AST from source");
        let mut ast = ast::Ast::default();
        for source in &self.sources {
            ast_parsing::parse(source, &mut ast).map_err(|parser_err| err::print_parser_err(&parser_err, source))?;
        }

        self.print_pretty("Building context");
        self.register_tys(&ast).map_err(|_| "Error registering types")?;
        self.define_tys(&ast).map_err(|_| "Error defining types")?;
        self.register_traits(&ast).map_err(|_| "Error registering traits")?;
        self.register_impls(&ast).map_err(|_| "Error registering impls")?;

        self.register_functions(&ast)
            .map_err(|_| "Error registering functions")?;
        self.register_trait_methods(&ast)
            .map_err(|_| "Error registering trait methods")?;
        self.register_impl_methods(&ast)
            .map_err(|_| "Error registering impl methods")?;

        check_trait_impls(&mut self.ctxt).map_err(|err| print_impl_check_error(err, &self.ctxt))?;

        self.print_pretty("Building MLR from AST");
        self.build_function_mlrs(&ast)
            .map_err(|err| format!("Error building MLR: {err}"))?;
        self.build_impl_fn_mlrs(&ast)
            .map_err(|err| format!("Error building MLR for impls: {err}"))?;
        check_obligations(&mut self.ctxt).map_err(|err| print_obligation_check_error(err, &self.ctxt))?;

        ast_lowering::opt::canonicalize_types(&mut self.ctxt).map_err(|_| "Could not infer types")?;

        if let Some(mlr_path) = self.output_paths.mlr {
            self.print_detail(&format!("Saving MLR to {}", mlr_path.display()));
            self.print_functions(mlr_path).map_err(|_| "Error printing MLR")?;
        }

        self.print_pretty("Monomorphizing functions");
        let fn_insts = self
            .monomorphize_functions()
            .map_err(|_| "Error monomorphizing functions")?;

        self.print_pretty("Building LLVM IR from MLR");
        let llvm_ir = mlr_lowering::mlr_to_llvm_ir(&mut self.ctxt, fn_insts.into_iter().collect());

        if let Some(llvm_ir_path) = self.output_paths.llvm_ir {
            self.print_detail(&format!("Saving LLVM IR to {}", llvm_ir_path.display()));
            std::fs::write(llvm_ir_path, &llvm_ir).map_err(|_| "Could not write LLVM IR file")?;
        }

        Ok(())
    }

    fn print_pretty(&self, msg: &str) {
        (self.print_pretty)(msg);
    }

    fn print_detail(&self, msg: &str) {
        (self.print_detail)(msg);
    }

    fn register_tys(&mut self, ast: &ast::Ast) -> Result<(), ()> {
        self.ctxt.tys.register_primitive_tys()?;

        for (idx, struct_) in ast.structs.iter().enumerate() {
            let ty = self.ctxt.tys.register_struct(&struct_.name, &struct_.gen_params)?;
            self.ast_meta.struct_ids.insert(idx, ty);
        }

        for (idx, enum_) in ast.enums.iter().enumerate() {
            let ty = self.ctxt.tys.register_enum(&enum_.name, &enum_.gen_params)?;
            self.ast_meta.enum_ids.insert(idx, ty);

            for variant in &enum_.variants {
                let variant_struct_name = format!("{}::{}", enum_.name, variant.name);
                let variant_ty = self.ctxt.tys.register_struct(&variant_struct_name, &enum_.gen_params)?;

                let enum_def = self.ctxt.tys.get_mut_enum_def(ty).ok_or(())?;

                enum_def.variants.push(ty::EnumVariant {
                    name: variant.name.clone(),
                    struct_: variant_ty,
                });
            }
        }

        Ok(())
    }

    fn define_tys(&mut self, ast: &ast::Ast) -> Result<(), ()> {
        for (idx, struct_) in ast.structs.iter().enumerate() {
            self.set_struct_fields(ast, self.ast_meta.struct_ids[&idx], &struct_.fields)?
        }

        for (idx, ast_enum) in ast.enums.iter().enumerate() {
            let enum_ = self.ast_meta.enum_ids[&idx];
            let variants = self.ctxt.tys.get_enum_def(enum_).ok_or(())?.variants.clone();

            for (ast_variant, variant) in ast_enum.variants.iter().zip(variants) {
                self.set_struct_fields(ast, variant.struct_, &ast_variant.fields)?;
            }
        }

        Ok(())
    }

    fn set_struct_fields<'b>(
        &mut self,
        ast: &ast::Ast,
        struct_: ty::Struct,
        fields: impl IntoIterator<Item = &'b ast::StructField>,
    ) -> Result<(), ()> {
        let gen_params = self.ctxt.tys.get_struct_def(struct_).ok_or(())?.gen_params.clone();

        let fields = fields
            .into_iter()
            .map(|field| {
                Ok(ty::StructField {
                    name: field.name.clone(),
                    ty: self
                        .ctxt
                        .try_resolve_ast_ty_annot(ast, field.ty, &gen_params, None, false)
                        .ok_or(())?,
                })
            })
            .collect::<Result<_, _>>()?;

        let struct_def = self.ctxt.tys.get_mut_struct_def(struct_).ok_or(())?;
        struct_def.fields = fields;

        Ok(())
    }

    fn register_functions(&mut self, ast: &ast::Ast) -> Result<(), ()> {
        stdlib::register_fns(&mut self.ctxt)?;

        for (idx, &function) in ast.free_fns.iter().enumerate() {
            let fn_ = self.register_function(ast, function, None, None, Vec::new())?;
            self.ast_meta.fn_ids.insert(idx, fn_);
        }

        Ok(())
    }

    fn register_function(
        &mut self,
        ast: &ast::Ast,
        ast_fn: ast::Fn,
        associated_ty: Option<ty::Ty>,
        associated_trait_inst: Option<traits::TraitInst>,
        env_gen_params: Vec<ty::GenVar>,
    ) -> Result<fns::Fn, ()> {
        let ast_fn = ast.fn_(ast_fn);
        let gen_params: Vec<_> = ast_fn
            .gen_params
            .iter()
            .map(|gp| self.ctxt.tys.register_gen_var(gp))
            .collect();
        let all_gen_params: Vec<_> = gen_params.iter().chain(&env_gen_params).cloned().collect();

        for constraint in &ast_fn.constraints {
            let subject = gen_params
                .iter()
                .cloned()
                .find(|&gp| self.ctxt.tys.get_gen_var_name(gp) == constraint.subject)
                .ok_or(())?;

            match &constraint.requirement {
                ast::ConstraintRequirement::Trait { trait_name, trait_args } => {
                    let trait_ = self.ctxt.traits.resolve_trait_name(trait_name).ok_or(())?;
                    let trait_args = ast
                        .ty_annot_slice(*trait_args)
                        .iter()
                        .map(|&arg| {
                            self.ctxt
                                .try_resolve_ast_ty_annot(ast, arg, &all_gen_params, associated_ty, false)
                                .ok_or(())
                        })
                        .collect::<Result<_, _>>()?;
                    let trait_inst = TraitInst {
                        trait_,
                        gen_args: trait_args,
                    };
                    self.ctxt.tys.add_implements_trait_constraint(subject, trait_inst);
                }
                &ast::ConstraintRequirement::Callable { params, return_ty } => {
                    let params = ast
                        .ty_annot_slice(params)
                        .iter()
                        .map(|&ty| {
                            self.ctxt
                                .try_resolve_ast_ty_annot(ast, ty, &all_gen_params, associated_ty, false)
                                .ok_or(())
                        })
                        .collect::<Result<_, _>>()?;
                    let return_ty = match return_ty {
                        Some(return_ty) => self
                            .ctxt
                            .try_resolve_ast_ty_annot(ast, return_ty, &all_gen_params, associated_ty, false)
                            .ok_or(())?,
                        None => self.ctxt.tys.unit(),
                    };
                    self.ctxt.tys.add_callable_constraint(subject, params, return_ty);
                }
            }
        }

        let params = ast_fn
            .params
            .iter()
            .enumerate()
            .map(|(idx, param)| self.build_fn_param(ast, param, &all_gen_params, associated_ty, idx == 0))
            .collect::<Result<_, _>>()?;

        let return_ty = match ast_fn.return_ty {
            Some(ty) => self
                .ctxt
                .try_resolve_ast_ty_annot(ast, ty, &all_gen_params, associated_ty, false)
                .ok_or(())?,
            None => self.ctxt.tys.unit(),
        };

        let signature = fns::FnSig {
            name: ast_fn.name.clone(),
            associated_ty,
            associated_trait_inst,
            gen_params,
            env_gen_params,
            params,
            var_args: ast_fn.var_args,
            return_ty,
        };

        self.ctxt.fns.register_fn(signature, associated_ty.is_none())
    }

    fn build_fn_param(
        &mut self,
        ast: &ast::Ast,
        param: &ast::Param,
        gen_params: &[ty::GenVar],
        self_ty: Option<ty::Ty>,
        allow_receiver: bool,
    ) -> Result<fns::FnParam, ()> {
        match param {
            ast::Param::Regular { name, ty } => Ok(fns::FnParam {
                kind: fns::FnParamKind::Regular(name.clone()),
                ty: self
                    .ctxt
                    .try_resolve_ast_ty_annot(ast, *ty, gen_params, self_ty, false)
                    .ok_or(())?,
            }),
            ast::Param::Receiver if allow_receiver => Ok(fns::FnParam {
                kind: fns::FnParamKind::Self_,
                ty: self_ty.ok_or(())?,
            }),
            ast::Param::ReceiverByRef if allow_receiver => Ok(fns::FnParam {
                kind: fns::FnParamKind::SelfByRef,
                ty: self_ty.map(|self_ty| self.ctxt.tys.ref_(self_ty)).ok_or(())?,
            }),
            _ => Err(()),
        }
    }

    fn register_traits(&mut self, ast: &ast::Ast) -> Result<(), ()> {
        for (idx, ast_trait) in ast.traits.iter().enumerate() {
            let trait_gen_params: Vec<_> = ast_trait
                .gen_params
                .iter()
                .map(|gp| self.ctxt.tys.register_gen_var(gp))
                .collect();

            let trait_ = self
                .ctxt
                .traits
                .register_trait(&ast_trait.name, trait_gen_params.clone());
            self.ast_meta.trait_ids.insert(idx, trait_);

            for assoc_ty in &ast_trait.assoc_ty_names {
                self.ctxt.traits.register_assoc_ty(trait_, assoc_ty);
            }
        }

        Ok(())
    }

    fn register_trait_methods(&mut self, ast: &ast::Ast) -> Result<(), ()> {
        for (idx, ast_trait) in ast.traits.iter().enumerate() {
            let trait_ = self.ast_meta.trait_ids[&idx];
            let self_type = self.ctxt.tys.trait_self(trait_);
            let trait_gen_params = self.ctxt.traits.get_trait_def(trait_).gen_params.clone();

            for &mthd in &ast_trait.mthds {
                let mthd = ast.fn_(mthd);
                let mthd_gen_params: Vec<_> = mthd
                    .gen_params
                    .iter()
                    .map(|gp| self.ctxt.tys.register_gen_var(gp))
                    .collect();
                let all_gen_params: Vec<_> = mthd_gen_params.iter().chain(&trait_gen_params).cloned().collect();

                let params = mthd
                    .params
                    .iter()
                    .enumerate()
                    .map(|(idx, param)| self.build_fn_param(ast, param, &all_gen_params, Some(self_type), idx == 0))
                    .collect::<Result<_, _>>()?;

                let return_ty = match mthd.return_ty {
                    Some(ty) => self
                        .ctxt
                        .try_resolve_ast_ty_annot(ast, ty, &all_gen_params, Some(self_type), false)
                        .ok_or(())?,
                    None => self.ctxt.tys.unit(),
                };

                let trait_inst = traits::TraitInst {
                    trait_,
                    gen_args: mthd_gen_params.iter().map(|&gp| self.ctxt.tys.gen_var(gp)).collect(),
                };

                let sig = fns::FnSig {
                    name: mthd.name.clone(),
                    associated_ty: None,
                    associated_trait_inst: Some(trait_inst),
                    gen_params: mthd_gen_params,
                    env_gen_params: trait_gen_params.clone(),
                    params,
                    var_args: false,
                    return_ty,
                };

                self.ctxt.traits.register_mthd(trait_, sig);
            }
        }

        Ok(())
    }

    fn register_impls(&mut self, ast: &ast::Ast) -> Result<(), ()> {
        stdlib::register_impl_for_ptr(&mut self.ctxt)?;

        for (idx, ast_impl) in ast.impls.iter().enumerate() {
            let gen_params: Vec<_> = ast_impl
                .gen_params
                .iter()
                .map(|gp| self.ctxt.tys.register_gen_var(gp))
                .collect();

            let ty = self
                .ctxt
                .try_resolve_ast_ty_annot(ast, ast_impl.ty, &gen_params, None, false)
                .ok_or(())?;

            let trait_inst = ast_impl
                .trait_annot
                .as_ref()
                .map(|trait_annot| {
                    let trait_ = self.ctxt.traits.resolve_trait_name(&trait_annot.name).ok_or(())?;

                    let trait_args = ast
                        .ty_annot_slice(trait_annot.args)
                        .iter()
                        .map(|&arg| {
                            self.ctxt
                                .try_resolve_ast_ty_annot(ast, arg, &gen_params, None, false)
                                .ok_or(())
                        })
                        .collect::<Result<_, _>>()?;

                    let trait_inst = TraitInst {
                        trait_,
                        gen_args: trait_args,
                    };
                    Ok(trait_inst)
                })
                .transpose()?;

            let impl_ = self
                .ctxt
                .impls
                .register_impl(ty, gen_params.clone(), trait_inst.clone());
            self.ast_meta.impl_ids.insert(idx, impl_);

            for assoc_ty in &ast_impl.assoc_tys {
                let ty = self
                    .ctxt
                    .try_resolve_ast_ty_annot(ast, assoc_ty.ty, &gen_params, None, false)
                    .ok_or(())?;
                let assoc_ty_idx = self
                    .ctxt
                    .traits
                    .get_trait_assoc_ty_index(trait_inst.clone().unwrap().trait_, &assoc_ty.name);
                self.ctxt.impls.register_assoc_ty(impl_, assoc_ty_idx, ty);
            }
        }

        Ok(())
    }

    fn register_impl_methods(&mut self, ast: &ast::Ast) -> Result<(), ()> {
        for (idx, ast_impl) in ast.impls.iter().enumerate() {
            let impl_ = self.ast_meta.impl_ids[&idx];
            let impl_def = self.ctxt.impls.get_impl_def(impl_);
            let ty = impl_def.ty;
            let gen_params = impl_def.gen_params.clone();
            let trait_inst = impl_def.trait_inst.clone();

            for &mthd in &ast_impl.mthds {
                let fn_ = self.register_function(ast, mthd, Some(ty), trait_inst.clone(), gen_params.clone())?;
                let mthd_name = &ast.fn_(mthd).name;
                self.ctxt.impls.register_mthd(impl_, fn_, mthd_name);
            }
        }

        Ok(())
    }

    fn build_function_mlrs(&mut self, ast: &ast::Ast) -> Result<(), String> {
        stdlib::define_size_of(&mut self.ctxt)?;
        stdlib::define_impl_for_ptr(&mut self.ctxt)
            .map_err(|err| err::print_mlr_builder_error("offset", err, &self.ctxt))?;

        for (idx, &ast_fn) in ast.free_fns.iter().enumerate() {
            let ast_fn = ast.fn_(ast_fn);
            let Some(body) = &ast_fn.body else {
                continue;
            };

            let target_fn = self.ast_meta.fn_ids[&idx];

            ast_lowering::ast_to_mlr(&mut self.ctxt, ast, body, target_fn)
                .map_err(|err| err::print_mlr_builder_error(&ast_fn.name, err, &self.ctxt))?;
        }

        Ok(())
    }

    fn build_impl_fn_mlrs(&mut self, ast: &ast::Ast) -> Result<(), String> {
        for (idx, ast_impl) in ast.impls.iter().enumerate() {
            let impl_ = self.ast_meta.impl_ids[&idx];
            let impl_def = self.ctxt.impls.get_impl_def(impl_);
            let impl_mthds = impl_def.mthds.clone();

            for (&ast_mthd, target_fn) in ast_impl.mthds.iter().zip(impl_mthds) {
                let ast_mthd = ast.fn_(ast_mthd);
                let Some(body) = &ast_mthd.body else {
                    continue;
                };
                ast_lowering::ast_to_mlr(&mut self.ctxt, ast, body, target_fn)
                    .map_err(|err| err::print_mlr_builder_error(&ast_mthd.name, err, &self.ctxt))?;
            }
        }

        Ok(())
    }

    fn print_functions(&mut self, path: &Path) -> Result<(), ()> {
        let mut file = std::fs::File::create(path).map_err(|_| ())?;

        for fn_ in self.ctxt.fns.get_all_fns() {
            if self.ctxt.fns.is_fn_defined(fn_) {
                print::print_mlr(fn_, &self.ctxt, &mut file).map_err(|_| ())?;
            }
        }

        Ok(())
    }

    fn monomorphize_functions(&mut self) -> Result<HashSet<fns::FnInst>, ()> {
        let mut open = VecDeque::new();
        open.push_back(fns::FnInst {
            fn_: self.ctxt.fns.get_fn_by_name("main").ok_or(())?,
            gen_args: Vec::new(),
            env_gen_args: Vec::new(),
        });

        let mut closed = HashSet::new();

        while let Some(current) = open.pop_front() {
            if closed.contains(&current) {
                continue;
            }

            let subst = self.ctxt.fns.get_subst_for_fn_inst(&current);

            let fn_insts = self.ctxt.fns.get_called_fn_insts(current.fn_).iter().map(|fn_inst| {
                let new_gen_args = fn_inst
                    .gen_args
                    .iter()
                    .map(|&ty| self.ctxt.tys.substitute_gen_vars(ty, &subst))
                    .collect();
                let new_env_gen_args = fn_inst
                    .env_gen_args
                    .iter()
                    .map(|&ty| self.ctxt.tys.substitute_gen_vars(ty, &subst))
                    .collect();

                fns::FnInst {
                    fn_: fn_inst.fn_,
                    gen_args: new_gen_args,
                    env_gen_args: new_env_gen_args,
                }
            });
            open.extend(fn_insts);

            let called_trait_mthd_insts = self.ctxt.fns.get_called_trait_mthd_insts(current.fn_).to_vec();
            let trait_fn_insts = called_trait_mthd_insts
                .into_iter()
                .map(|trait_mthd_inst| self.ctxt.resolve_trait_mthd_to_fn(&trait_mthd_inst, &subst));
            open.extend(trait_fn_insts);

            closed.insert(current);
        }

        Ok(closed)
    }
}
