mod err;
mod impl_check;
mod stdlib;

use std::{
    collections::{HashMap, HashSet, VecDeque},
    path::Path,
};

use crate::{
    ast, ast_lowering,
    ctxt::{self, fns, impls, traits, ty},
    driver::{err::print_impl_check_error, impl_check::check_trait_impls},
    hlr, hlr_lowering, mlr, mlr_lowering, parse, typeck,
    util::print,
};

#[derive(Default)]
pub struct OutputPaths<'a> {
    pub hlr: Option<&'a Path>,
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
        sources: sources.to_vec(),
        print_pretty: &print_pretty,
        print_detail: &print_detail,
        output_paths,

        ctxt: ctxt::Ctxt::default(),
        ast: &ast::Ast::default(),
        ast_meta: AstMeta::default(),
        hlr: &hlr::Hlr::default(),
        mlr: &mlr::Mlr::default(),
    };

    driver.compile()
}

struct Driver<'a, 'ast, 'hlr, 'mlr> {
    sources: Vec<String>,
    print_pretty: &'a dyn Fn(&str),
    print_detail: &'a dyn Fn(&str),
    output_paths: &'a OutputPaths<'a>,

    ctxt: ctxt::Ctxt,
    ast: &'ast ast::Ast<'ast>,
    ast_meta: AstMeta,
    hlr: &'hlr hlr::Hlr<'hlr>,
    mlr: &'mlr mlr::Mlr<'mlr>,
}

#[derive(Default)]
struct AstMeta {
    fn_ids: HashMap<ast::FnId, fns::Fn>,
    struct_ids: HashMap<ast::StructId, ty::Struct>,
    enum_ids: HashMap<ast::EnumId, ty::Enum>,
    trait_ids: HashMap<ast::TraitId, traits::Trait>,
    impl_ids: HashMap<ast::ImplId, impls::Impl>,
}

impl<'a, 'ast, 'hlr, 'mlr> Driver<'a, 'ast, 'hlr, 'mlr> {
    pub fn compile(&mut self) -> Result<(), String> {
        self.print_pretty("Building AST from source");
        for source in &self.sources {
            parse::parse(source, self.ast).map_err(|parser_err| err::print_parser_err(&parser_err, source))?;
        }

        self.print_pretty("Building context");
        self.register_tys().map_err(|_| "Error registering types")?;
        self.define_tys().map_err(|_| "Error defining types")?;
        self.register_traits().map_err(|_| "Error registering traits")?;
        self.register_impls().map_err(|_| "Error registering impls")?;

        self.register_free_fns().map_err(|_| "Error registering functions")?;
        self.register_trait_methods()
            .map_err(|_| "Error registering trait methods")?;
        self.register_impl_methods()
            .map_err(|_| "Error registering impl methods")?;

        check_trait_impls(&mut self.ctxt).map_err(|err| print_impl_check_error(err, &self.ctxt))?;

        self.print_pretty("Lowering AST to HLR");
        let hlr_fns = self.ast_lowering()?;

        self.print_pretty("Type checking");
        let hlr_typings = self.typeck(&hlr_fns)?;

        self.print_pretty("Lowering HLR to MLR");
        let mlr_fns = self.hlr_lowering(&hlr_fns, &hlr_typings);

        if let Some(hlr_path) = self.output_paths.hlr {
            self.print_detail(&format!("Saving HLR to {}", hlr_path.display()));
            self.print_hlr_fns(hlr_path, &hlr_fns, &hlr_typings)
                .map_err(|_| "Error printing HLR")?;
        }

        if let Some(mlr_path) = self.output_paths.mlr {
            self.print_detail(&format!("Saving MLR to {}", mlr_path.display()));
            self.print_mlr_fns(mlr_path, &mlr_fns)
                .map_err(|_| "Error printing MLR")?;
        }

        self.print_pretty("Monomorphizing functions");
        let fn_insts = self
            .monomorphize_functions()
            .map_err(|_| "Error monomorphizing functions")?;

        self.print_pretty("Lowering MLR to LLVM IR");
        let llvm_ir = self.mlr_lowering(mlr_fns, fn_insts);

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

    fn register_tys(&mut self) -> Result<(), ()> {
        self.ctxt.tys.register_primitive_tys()?;

        for struct_ in self.ast.structs().iter() {
            let ty = self.ctxt.tys.register_struct(&struct_.name, &struct_.gen_params)?;
            self.ast_meta.struct_ids.insert(struct_.1, ty);
        }

        for enum_ in self.ast.enums().iter() {
            let ty = self.ctxt.tys.register_enum(&enum_.name, &enum_.gen_params)?;
            self.ast_meta.enum_ids.insert(enum_.1, ty);

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

    fn define_tys(&mut self) -> Result<(), ()> {
        for struct_ in self.ast.structs().iter() {
            self.set_struct_fields(self.ast_meta.struct_ids[&struct_.1], &struct_.fields)?
        }

        for ast_enum in self.ast.enums().iter() {
            let enum_ = self.ast_meta.enum_ids[&ast_enum.1];
            let variants = self.ctxt.tys.get_enum_def(enum_).ok_or(())?.variants.clone();

            for (ast_variant, variant) in ast_enum.variants.iter().zip(variants) {
                self.set_struct_fields(variant.struct_, &ast_variant.fields)?;
            }
        }

        Ok(())
    }

    fn set_struct_fields<'b>(
        &mut self,
        struct_: ty::Struct,
        fields: impl IntoIterator<Item = &'b ast::StructField<'b>>,
    ) -> Result<(), ()> {
        let gen_params = self.ctxt.tys.get_struct_def(struct_).ok_or(())?.gen_params.clone();

        let fields = fields
            .into_iter()
            .map(|field| {
                Ok(ty::StructField {
                    name: field.name.clone(),
                    ty: self
                        .ctxt
                        .try_resolve_ast_ty_annot(field.ty, &gen_params, None)
                        .ok_or(())?,
                })
            })
            .collect::<Result<_, _>>()?;

        let struct_def = self.ctxt.tys.get_mut_struct_def(struct_).ok_or(())?;
        struct_def.fields = fields;

        Ok(())
    }

    fn register_traits(&mut self) -> Result<(), ()> {
        stdlib::register_add_trait(&mut self.ctxt);

        for ast_trait in self.ast.traits().iter() {
            let trait_gen_params: Vec<_> = ast_trait
                .gen_params
                .iter()
                .map(|gp| self.ctxt.tys.register_gen_var(gp))
                .collect();

            let trait_ = self
                .ctxt
                .traits
                .register_trait(&ast_trait.name, trait_gen_params.clone());
            self.ast_meta.trait_ids.insert(ast_trait.1, trait_);

            for assoc_ty in &ast_trait.assoc_ty_names {
                self.ctxt.traits.register_assoc_ty(trait_, assoc_ty);
            }
        }

        Ok(())
    }

    fn register_impls(&mut self) -> Result<(), ()> {
        stdlib::register_impl_for_ptr(&mut self.ctxt)?;

        for ast_impl in self.ast.impls().iter() {
            let gen_params: Vec<_> = ast_impl
                .gen_params
                .iter()
                .map(|gp| self.ctxt.tys.register_gen_var(gp))
                .collect();

            let ty = self
                .ctxt
                .try_resolve_ast_ty_annot(ast_impl.ty, &gen_params, None)
                .ok_or(())?;

            let trait_inst = ast_impl
                .trait_annot
                .as_ref()
                .map(|trait_annot| {
                    let trait_ = self.ctxt.traits.resolve_trait_name(&trait_annot.name).ok_or(())?;

                    let trait_args: Vec<_> = match &trait_annot.args {
                        &Some(args) => args
                            .iter()
                            .map(|&arg| self.ctxt.try_resolve_ast_ty_annot(arg, &gen_params, None).ok_or(()))
                            .collect::<Result<_, _>>()?,
                        None => vec![],
                    };

                    let trait_inst = traits::TraitInst {
                        trait_,
                        gen_args: self.ctxt.tys.ty_slice(&trait_args),
                    };
                    Ok(trait_inst)
                })
                .transpose()?;

            let impl_ = self.ctxt.impls.register_impl(ty, gen_params.clone(), trait_inst);
            self.ast_meta.impl_ids.insert(ast_impl.1, impl_);

            for assoc_ty in &ast_impl.assoc_tys {
                let ty = self
                    .ctxt
                    .try_resolve_ast_ty_annot(assoc_ty.ty, &gen_params, None)
                    .ok_or(())?;
                let assoc_ty_idx = self
                    .ctxt
                    .traits
                    .get_trait_assoc_ty_index(trait_inst.unwrap().trait_, &assoc_ty.name);
                self.ctxt.impls.register_assoc_ty(impl_, assoc_ty_idx, ty);
            }
        }

        Ok(())
    }

    fn register_free_fns(&mut self) -> Result<(), ()> {
        stdlib::register_fns(&mut self.ctxt)?;

        for &function in self.ast.free_fns().iter() {
            let fn_ = self.register_function(function, None, None, Vec::new())?;
            self.ast_meta.fn_ids.insert(function.1, fn_);
        }

        Ok(())
    }

    fn register_function(
        &mut self,
        ast_fn: ast::Fn<'ast>,
        associated_ty: Option<ty::Ty>,
        associated_trait_inst: Option<traits::TraitInst>,
        env_gen_params: Vec<ty::GenVar>,
    ) -> Result<fns::Fn, ()> {
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
                    let trait_args: Vec<_> = trait_args
                        .iter()
                        .map(|&arg| {
                            self.ctxt
                                .try_resolve_ast_ty_annot(arg, &all_gen_params, associated_ty)
                                .ok_or(())
                        })
                        .collect::<Result<_, _>>()?;
                    let trait_inst = traits::TraitInst {
                        trait_,
                        gen_args: self.ctxt.tys.ty_slice(&trait_args),
                    };
                    self.ctxt.tys.add_implements_trait_constraint(subject, trait_inst);
                }
                &ast::ConstraintRequirement::Callable { params, return_ty } => {
                    let params = params
                        .iter()
                        .map(|&ty| {
                            self.ctxt
                                .try_resolve_ast_ty_annot(ty, &all_gen_params, associated_ty)
                                .ok_or(())
                        })
                        .collect::<Result<_, _>>()?;
                    let return_ty = match return_ty {
                        Some(return_ty) => self
                            .ctxt
                            .try_resolve_ast_ty_annot(return_ty, &all_gen_params, associated_ty)
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
            .map(|(idx, param)| self.build_fn_param(param, &all_gen_params, associated_ty, idx == 0))
            .collect::<Result<_, _>>()?;

        let return_ty = match ast_fn.return_ty {
            Some(ty) => self
                .ctxt
                .try_resolve_ast_ty_annot(ty, &all_gen_params, associated_ty)
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
        param: &ast::Param,
        gen_params: &[ty::GenVar],
        self_ty: Option<ty::Ty>,
        allow_receiver: bool,
    ) -> Result<fns::FnParam, ()> {
        match param {
            ast::Param::Regular { name, ty } => Ok(fns::FnParam {
                kind: fns::FnParamKind::Regular(name.clone()),
                ty: self.ctxt.try_resolve_ast_ty_annot(ty, gen_params, self_ty).ok_or(())?,
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

    fn register_trait_methods(&mut self) -> Result<(), ()> {
        for ast_trait in self.ast.traits().iter() {
            let trait_ = self.ast_meta.trait_ids[&ast_trait.1];
            let self_type = self.ctxt.tys.trait_self(trait_);
            let trait_gen_params = self.ctxt.traits.get_trait_def(trait_).gen_params.clone();

            for &mthd in ast_trait.mthds.iter() {
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
                    .map(|(idx, param)| self.build_fn_param(param, &all_gen_params, Some(self_type), idx == 0))
                    .collect::<Result<_, _>>()?;

                let return_ty = match mthd.return_ty {
                    Some(ty) => self
                        .ctxt
                        .try_resolve_ast_ty_annot(ty, &all_gen_params, Some(self_type))
                        .ok_or(())?,
                    None => self.ctxt.tys.unit(),
                };

                let gen_args: Vec<_> = mthd_gen_params.iter().map(|&gp| self.ctxt.tys.gen_var(gp)).collect();
                let trait_inst = traits::TraitInst {
                    trait_,
                    gen_args: self.ctxt.tys.ty_slice(&gen_args),
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

    fn register_impl_methods(&mut self) -> Result<(), ()> {
        for ast_impl in self.ast.impls().iter() {
            let impl_ = self.ast_meta.impl_ids[&ast_impl.1];
            let impl_def = self.ctxt.impls.get_impl_def(impl_);
            let ty = impl_def.ty;
            let gen_params = impl_def.gen_params.clone();
            let trait_inst = impl_def.trait_inst;

            for &mthd in ast_impl.mthds.iter() {
                let fn_ = self.register_function(mthd, Some(ty), trait_inst, gen_params.clone())?;
                self.ctxt.impls.register_mthd(impl_, fn_, &mthd.name);
                self.ast_meta.fn_ids.insert(mthd.1, fn_);
            }
        }

        Ok(())
    }

    fn ast_lowering(&self) -> Result<Vec<hlr::Fn<'hlr>>, String> {
        let mut hlr_fns = Vec::new();

        for &ast_fn in self.ast.free_fns().iter() {
            let Some(body) = ast_fn.body else { continue };
            let target_fn = self.ast_meta.fn_ids[&ast_fn.1];
            let hlr_fn = ast_lowering::ast_to_hlr(&self.ctxt, target_fn, body, self.hlr)
                .map_err(|err| format!("failed to lower {} to HLR: {}", ast_fn.name, err.msg))?;
            hlr_fns.push(hlr_fn);
        }

        for ast_impl in self.ast.impls().iter() {
            for &ast_mthd in ast_impl.mthds.iter() {
                let Some(body) = ast_mthd.body else { continue };
                let target_fn = self.ast_meta.fn_ids[&ast_mthd.1];
                let hlr_fn = ast_lowering::ast_to_hlr(&self.ctxt, target_fn, body, self.hlr)
                    .map_err(|err| format!("failed to lower {} to HLR: {}", ast_mthd.name, err.msg))?;
                hlr_fns.push(hlr_fn);
            }
        }

        Ok(hlr_fns)
    }

    fn typeck(&mut self, hlr_fns: &[hlr::Fn<'hlr>]) -> Result<HashMap<fns::Fn, typeck::HlrTyping>, String> {
        hlr_fns
            .iter()
            .map(|hlr_fn| {
                let fn_name = self.ctxt.fns.get_sig(hlr_fn.fn_).unwrap().name.clone();
                let typing =
                    typeck::typeck(&mut self.ctxt, hlr_fn).map_err(|e| format!("failed to typeck {fn_name}: {e:?}"))?;
                Ok((hlr_fn.fn_, typing))
            })
            .collect()
    }

    fn hlr_lowering(
        &mut self,
        hlr_fns: &[hlr::Fn<'hlr>],
        typings: &HashMap<fns::Fn, typeck::HlrTyping>,
    ) -> Vec<mlr::Fn<'mlr>> {
        hlr_fns
            .iter()
            .filter_map(|hlr_fn| typings.get(&hlr_fn.fn_).map(|typing| (hlr_fn, typing)))
            .flat_map(|(hlr_fn, typing)| hlr_lowering::hlr_to_mlr(&mut self.ctxt, self.mlr, hlr_fn, typing))
            .collect()
    }

    fn print_hlr_fns(
        &self,
        path: &Path,
        hlr_fns: &[hlr::Fn<'hlr>],
        typings: &HashMap<fns::Fn, typeck::HlrTyping>,
    ) -> Result<(), ()> {
        let mut file = std::fs::File::create(path).map_err(|_| ())?;
        for hlr_fn in hlr_fns {
            let typing = typings.get(&hlr_fn.fn_);
            print::print_hlr(hlr_fn, &self.ctxt, typing, &mut file).map_err(|_| ())?;
            use std::io::Write;
            writeln!(file).map_err(|_| ())?;
        }
        Ok(())
    }

    fn print_mlr_fns(&self, path: &Path, mlr_fns: &[mlr::Fn<'mlr>]) -> Result<(), ()> {
        let mut file = std::fs::File::create(path).map_err(|_| ())?;

        let mlr_fn_map: HashMap<fns::Fn, &mlr::Fn<'mlr>> = mlr_fns.iter().map(|f| (f.fn_, f)).collect();
        for fn_ in self.ctxt.fns.get_all_fns() {
            let mlr_fn = mlr_fn_map.get(&fn_).copied();
            print::print_mlr(fn_, mlr_fn, &self.ctxt, &mut file).map_err(|_| ())?;
        }

        Ok(())
    }

    fn monomorphize_functions(&mut self) -> Result<HashSet<fns::FnInst>, ()> {
        let mut open = VecDeque::new();
        open.push_back(fns::FnInst {
            fn_: self.ctxt.fns.get_fn_by_name("main").ok_or(())?,
            gen_args: self.ctxt.tys.ty_slice(&[]),
            env_gen_args: self.ctxt.tys.ty_slice(&[]),
        });

        let mut closed = HashSet::new();

        while let Some(current) = open.pop_front() {
            if closed.contains(&current) {
                continue;
            }

            let subst = self.ctxt.get_subst_for_fn_inst(current);

            let fn_insts = self.ctxt.fns.get_called_fn_insts(current.fn_).iter().map(|fn_inst| {
                let new_gen_args = self.ctxt.tys.substitute_gen_vars_on_slice(fn_inst.gen_args, &subst);
                let new_env_gen_args = self.ctxt.tys.substitute_gen_vars_on_slice(fn_inst.env_gen_args, &subst);
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
                .map(|trait_mthd_inst| self.ctxt.resolve_trait_mthd_to_fn(trait_mthd_inst, &subst));
            open.extend(trait_fn_insts);

            closed.insert(current);
        }

        Ok(closed)
    }

    fn mlr_lowering(&mut self, mlr_fns: Vec<mlr::Fn<'mlr>>, fn_insts: HashSet<fns::FnInst>) -> String {
        mlr_lowering::mlr_to_llvm_ir(&mut self.ctxt, mlr_fns, fn_insts.into_iter().collect())
    }
}
