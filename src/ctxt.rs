pub mod fns;
pub mod impls;
pub mod language_items;
pub mod mlr;
pub mod traits;
pub mod ty;

mod fn_reg;
mod impl_reg;
mod trait_reg;
mod ty_reg;

pub use fn_reg::FnReg;
pub use impl_reg::ImplReg;
pub use trait_reg::TraitReg;
pub use ty_reg::*;

use mlr::Mlr;

use crate::{ast, ctxt::ty::GenVarSubst};

#[derive(Default)]
pub struct Ctxt {
    pub tys: TyReg,
    pub fns: FnReg,
    pub mlr: Mlr,
    pub impls: ImplReg,
    pub traits: TraitReg,
    pub language_items: language_items::LanguageItems,
}

impl Ctxt {
    pub fn get_fn_inst_name(&self, fn_inst: &fns::FnInst) -> String {
        let signature = self.fns.get_sig(fn_inst.fn_).unwrap();

        let assoc_ty = if let Some(assoc_ty) = signature.associated_ty {
            let assoc_ty_name = self.tys.get_string_rep(assoc_ty);
            if let Some(assoc_trait_inst) = &signature.associated_trait_inst {
                let assoc_trait_name = self.traits.get_trait_name(assoc_trait_inst.trait_);
                let assoc_trait_gen_params = if assoc_trait_inst.gen_args.is_empty() {
                    "".to_string()
                } else {
                    format!(
                        "<{}>",
                        assoc_trait_inst
                            .gen_args
                            .iter()
                            .map(|&ty| self.tys.get_string_rep(ty))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                };

                format!(
                    "<{} as {}{}>::",
                    assoc_ty_name, assoc_trait_name, assoc_trait_gen_params
                )
            } else {
                format!("{}::", assoc_ty_name)
            }
        } else {
            "".to_string()
        };

        let env_gen_args = if fn_inst.env_gen_args.is_empty() {
            "".to_string()
        } else {
            format!(
                "{{{}}}",
                fn_inst
                    .env_gen_args
                    .iter()
                    .map(|&ty| self.tys.get_string_rep(ty))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };

        let gen_args = if fn_inst.gen_args.is_empty() {
            "".to_string()
        } else {
            format!(
                "<{}>",
                fn_inst
                    .gen_args
                    .iter()
                    .map(|&ty| self.tys.get_string_rep(ty))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };

        format!("{}{}{}{}", assoc_ty, signature.name, env_gen_args, gen_args)
    }

    pub fn fn_insts_eq(&self, fn_inst1: &fns::FnInst, fn_inst2: &fns::FnInst) -> bool {
        fn_inst1.fn_ == fn_inst2.fn_
            && fn_inst1.gen_args.len() == fn_inst2.gen_args.len()
            && fn_inst1
                .gen_args
                .iter()
                .zip(fn_inst2.gen_args.iter())
                .all(|(a, b)| self.tys.tys_eq(*a, *b))
            && fn_inst1.env_gen_args.len() == fn_inst2.env_gen_args.len()
            && fn_inst1
                .env_gen_args
                .iter()
                .zip(fn_inst2.env_gen_args.iter())
                .all(|(a, b)| self.tys.tys_eq(*a, *b))
    }

    pub fn get_fn_inst_sig(&mut self, fn_inst: &fns::FnInst) -> fns::FnSig {
        let signature = self.fns.get_sig(fn_inst.fn_).unwrap();
        let subst = self.fns.get_subst_for_fn_inst(fn_inst);

        let inst_params = signature
            .params
            .iter()
            .map(|param| fns::FnParam {
                kind: param.kind.clone(),
                ty: self.tys.substitute_gen_vars(param.ty, &subst),
            })
            .collect();

        let inst_return_ty = self.tys.substitute_gen_vars(signature.return_ty, &subst);

        fns::FnSig {
            name: signature.name.clone(),
            // TODO subst associated_ty?
            associated_ty: signature.associated_ty,
            // TODO subst associated_trait_inst?
            associated_trait_inst: signature.associated_trait_inst.clone(),
            gen_params: Vec::new(),
            env_gen_params: Vec::new(),
            params: inst_params,
            var_args: signature.var_args,
            return_ty: inst_return_ty,
        }
    }

    pub fn resolve_trait_mthd_to_fn(
        &mut self,
        trait_mthd_inst: &fns::TraitMthdInst,
        subst: &GenVarSubst,
    ) -> fns::FnInst {
        let trait_mthd_inst = self.subst_trait_mthd_inst(trait_mthd_inst, subst);

        let matching_impl_insts: Vec<_> = self
            .get_impl_insts_for_ty_and_trait_inst(trait_mthd_inst.impl_ty, &trait_mthd_inst.trait_inst)
            .collect();

        // TODO proper error handling
        assert_eq!(matching_impl_insts.len(), 1);
        let [impl_inst] = matching_impl_insts.try_into().unwrap();

        let trait_mthd_name = self
            .traits
            .get_trait_mthd_name(trait_mthd_inst.trait_inst.trait_, trait_mthd_inst.mthd_idx);

        let impl_def = self.impls.get_impl_def(impl_inst.impl_);
        let fn_ = impl_def.mthds_by_name[trait_mthd_name];

        fns::FnInst {
            fn_,
            gen_args: trait_mthd_inst.gen_args,
            env_gen_args: impl_inst.gen_args,
        }
    }

    fn get_impl_insts_for_ty_and_trait(
        &self,
        ty: ty::Ty,
        trait_: traits::Trait,
    ) -> impl Iterator<Item = impls::ImplInst> + use<'_> {
        self.impls.get_impls_for_trait(trait_).filter_map(move |impl_| {
            let impl_def = self.impls.get_impl_def(impl_).clone();

            let gen_args = self
                .tys
                .try_find_instantiation(ty, impl_def.ty, &impl_def.gen_params)
                .ok()?;

            let impl_inst = impls::ImplInst { impl_, gen_args };
            Some(impl_inst)
        })
    }

    fn get_impl_insts_for_ty_and_trait_inst(
        &mut self,
        ty: ty::Ty,
        trait_inst: &traits::TraitInst,
    ) -> impl Iterator<Item = impls::ImplInst> {
        let impl_insts: Vec<_> = self.get_impl_insts_for_ty_and_trait(ty, trait_inst.trait_).collect();

        impl_insts.into_iter().filter(move |impl_inst| {
            let impl_def = self.impls.get_impl_def(impl_inst.impl_).clone();
            let subst = GenVarSubst::new(&impl_def.gen_params, &impl_inst.gen_args).unwrap();

            let inst_impl_trait_inst = self.subst_trait_inst(impl_def.trait_inst.as_ref().unwrap(), &subst);

            inst_impl_trait_inst
                .gen_args
                .iter()
                .zip(trait_inst.gen_args.iter())
                .all(|(gen_arg1, gen_arg2)| self.tys.tys_eq(*gen_arg1, *gen_arg2))
        })
    }

    fn subst_trait_inst(&mut self, trait_inst: &traits::TraitInst, subst: &GenVarSubst) -> traits::TraitInst {
        let mut trait_inst = trait_inst.clone();
        for gen_arg in &mut trait_inst.gen_args {
            *gen_arg = self.tys.substitute_gen_vars(*gen_arg, subst);
        }
        trait_inst
    }

    fn subst_trait_mthd_inst(
        &mut self,
        trait_mthd_inst: &fns::TraitMthdInst,
        subst: &GenVarSubst,
    ) -> fns::TraitMthdInst {
        let mut trait_mthd_inst = trait_mthd_inst.clone();
        trait_mthd_inst.impl_ty = self.tys.substitute_gen_vars(trait_mthd_inst.impl_ty, subst);
        trait_mthd_inst.trait_inst = self.subst_trait_inst(&trait_mthd_inst.trait_inst, subst);
        for gen_arg in &mut trait_mthd_inst.gen_args {
            *gen_arg = self.tys.substitute_gen_vars(*gen_arg, subst);
        }
        trait_mthd_inst
    }

    pub fn ty_implements_trait_inst(&mut self, ty: ty::Ty, trait_inst: &traits::TraitInst) -> bool {
        let ty_def = self.tys.get_ty_def(ty);
        if let Some(&ty::TyDef::GenVar(gen_var)) = ty_def
            && self.tys.implements_trait_inst_constraint_exists(gen_var, trait_inst)
        {
            return true;
        }

        self.get_impl_insts_for_ty_and_trait_inst(ty, trait_inst)
            .next()
            .is_some()
    }

    pub fn ty_implements_trait(&self, ty: ty::Ty, trait_: traits::Trait) -> bool {
        let ty_def = self.tys.get_ty_def(ty);
        if let Some(&ty::TyDef::GenVar(gen_var)) = ty_def
            && self.tys.implements_trait_constraint_exists(gen_var, trait_)
        {
            return true;
        }

        if let Some(&ty::TyDef::TraitSelf(trait_2)) = ty_def
            && trait_2 == trait_
        {
            return true;
        }

        self.get_impl_insts_for_ty_and_trait(ty, trait_).next().is_some()
    }

    pub fn ty_is_callable(&mut self, ty: ty::Ty) -> Option<(Vec<ty::Ty>, ty::Ty, bool)> {
        if let Some((param_tys, return_ty)) = self.tys.try_get_callable_obligation(ty) {
            Some((param_tys, return_ty, false))
        } else if let Some(ty::TyDef::Fn {
            param_tys,
            return_ty,
            var_args,
        }) = self.tys.get_ty_def(ty)
        {
            Some((param_tys.clone(), *return_ty, *var_args))
        } else if let Some(ty::TyDef::GenVar(gen_var)) = self.tys.get_ty_def(ty)
            && let Some((param_tys, return_ty)) = self.tys.try_get_callable_constraint(*gen_var)
        {
            Some((param_tys, return_ty, false))
        } else if let Some(ty::TyDef::Closure { fn_inst, .. }) = self.tys.get_ty_def(ty) {
            let fn_inst = fn_inst.clone();
            let signature = self.get_fn_inst_sig(&fn_inst);
            Some((
                signature.params.iter().skip(1).map(|p| p.ty).collect(),
                signature.return_ty,
                false,
            ))
        } else {
            None
        }
    }

    pub fn try_resolve_ast_ty_annot(
        &mut self,
        ast: &ast::Ast,
        annot: ast::TyAnnot,
        gen_vars: &[ty::GenVar],
        self_ty: Option<ty::Ty>,
        allow_wildcards: bool,
    ) -> Option<ty::Ty> {
        use ast::TyAnnotKind::*;

        match ast.ty_annot(annot) {
            Path(path) => match path.segments.as_slice() {
                [segment] => self.try_resolve_ast_path_segment_to_ty(ast, segment, gen_vars, self_ty, allow_wildcards),
                [base_ty, ast::PathSegment::Ident(_ident)] => {
                    let base_ty =
                        self.try_resolve_ast_path_segment_to_ty(ast, base_ty, gen_vars, self_ty, allow_wildcards)?;

                    let ty = self
                        .resolve_associated_ty_completely(base_ty, _ident)
                        .expect("associated type not found");
                    Some(ty)
                }
                _ => None,
            },
            &Ref(ty_annot) => self
                .try_resolve_ast_ty_annot(ast, ty_annot, gen_vars, self_ty, allow_wildcards)
                .map(|inner| self.tys.ref_(inner)),
            &Ptr(ty_annot) => self
                .try_resolve_ast_ty_annot(ast, ty_annot, gen_vars, self_ty, allow_wildcards)
                .map(|inner| self.tys.ptr(inner)),
            &Fn { param_tys, return_ty } => {
                let param_tys: Vec<ty::Ty> = ast
                    .ty_annot_slice(param_tys)
                    .iter()
                    .map(|&pt| self.try_resolve_ast_ty_annot(ast, pt, gen_vars, self_ty, allow_wildcards))
                    .collect::<Option<Vec<_>>>()?;

                let return_ty = match return_ty {
                    Some(rt) => self.try_resolve_ast_ty_annot(ast, rt, gen_vars, self_ty, allow_wildcards),
                    None => Some(self.tys.unit()),
                }?;

                Some(self.tys.fn_(param_tys, return_ty, false))
            }
            Wildcard => allow_wildcards.then(|| self.tys.undef_ty()),
            &Tuple(ty_annots) => {
                let tys: Vec<ty::Ty> = ast
                    .ty_annot_slice(ty_annots)
                    .iter()
                    .map(|ty_annot| self.try_resolve_ast_ty_annot(ast, *ty_annot, gen_vars, self_ty, allow_wildcards))
                    .collect::<Option<Vec<_>>>()?;
                Some(self.tys.tuple(tys))
            }
        }
    }

    pub fn try_resolve_ast_path_segment_to_ty(
        &mut self,
        ast: &ast::Ast,
        path_segment: &ast::PathSegment,
        gen_vars: &[ty::GenVar],
        self_ty: Option<ty::Ty>,
        allow_wildcards: bool,
    ) -> Option<ty::Ty> {
        match path_segment {
            ast::PathSegment::Ident(ident) => {
                if let Some(&gv) = gen_vars.iter().find(|&&gv| self.tys.get_gen_var_name(gv) == *ident) {
                    let ty = self.tys.gen_var(gv);
                    return Some(ty);
                }

                match *self.tys.get_ty_by_name(ident).ok()? {
                    self::Named::Ty(ty) => Some(ty),
                    self::Named::Struct(struct_) => self.tys.inst_struct(struct_, []).ok(),
                    self::Named::Enum(enum_) => self.tys.inst_enum(enum_, []).ok(),
                }
            }
            ast::PathSegment::Generic(ast::GenPathSegment { ident, gen_args }) => {
                let gen_args: Vec<ty::Ty> = gen_args
                    .iter()
                    .map(|arg_annot| self.try_resolve_ast_ty_annot(ast, *arg_annot, gen_vars, self_ty, allow_wildcards))
                    .collect::<Option<Vec<_>>>()?;

                match *self.tys.get_ty_by_name(ident).ok()? {
                    self::Named::Struct(struct_) => self.tys.inst_struct(struct_, gen_args).ok(),
                    self::Named::Enum(enum_) => self.tys.inst_enum(enum_, gen_args).ok(),
                    self::Named::Ty(..) => None,
                }
            }
            ast::PathSegment::Self_ => Some(self_ty.expect("self type not available")),
        }
    }

    fn resolve_associated_ty_completely(&mut self, base_ty: ty::Ty, ident: &str) -> Option<ty::Ty> {
        let base_ty_def = self.tys.get_ty_def(base_ty);

        if let Some(&ty::TyDef::TraitSelf(trait_)) = base_ty_def {
            let trait_def = self.traits.get_trait_def(trait_);
            let assoc_ty_index = trait_def.assoc_tys.iter().position(|name| name == ident)?;
            let default_trait_inst = traits::TraitInst {
                trait_,
                gen_args: trait_def.gen_params.iter().map(|gp| self.tys.gen_var(*gp)).collect(),
            };
            let ty = self.tys.assoc_ty(base_ty, default_trait_inst, assoc_ty_index);
            return Some(ty);
        }

        let candidate_assoc_tys: Vec<_> = self
            .traits
            .get_trait_assoc_ty_with_name(ident)
            .filter(|&(trait_, _)| self.ty_implements_trait(base_ty, trait_))
            .collect();

        match &candidate_assoc_tys[..] {
            [] => None,
            [(trait_, assoc_ty_idx)] => {
                let impl_insts: Vec<_> = self.get_impl_insts_for_ty_and_trait(base_ty, *trait_).collect();

                let [impl_inst] = &impl_insts[..] else {
                    let trait_inst = traits::TraitInst {
                        trait_: *trait_,
                        gen_args: vec![], // TODO add gen args
                    };
                    return Some(self.tys.assoc_ty(base_ty, trait_inst, *assoc_ty_idx));
                };

                let impl_def = self.impls.get_impl_def(impl_inst.impl_);
                let assoc_ty = *impl_def.assoc_tys.get(assoc_ty_idx).unwrap();

                Some(assoc_ty)
            }
            [_, _, ..] => None,
        }
    }

    pub fn normalize_ty(&mut self, ty: ty::Ty) -> ty::Ty {
        use ty::TyDef::*;

        let ty = self.tys.canonicalize(ty);
        let Some(ty_def) = self.tys.get_ty_def(ty).cloned() else {
            return ty;
        };

        match ty_def {
            Primitive(_) => ty,
            Tuple(items) => {
                let items: Vec<ty::Ty> = items.into_iter().map(|ty| self.normalize_ty(ty)).collect();
                self.tys.tuple(items)
            }
            Struct { struct_, gen_args } => {
                let gen_args: Vec<ty::Ty> = gen_args.into_iter().map(|ty| self.normalize_ty(ty)).collect();
                self.tys.inst_struct(struct_, gen_args).unwrap()
            }
            Enum { enum_, gen_args } => {
                let gen_args: Vec<ty::Ty> = gen_args.into_iter().map(|ty| self.normalize_ty(ty)).collect();
                self.tys.inst_enum(enum_, gen_args).unwrap()
            }
            Fn {
                param_tys,
                return_ty,
                var_args,
            } => {
                let param_tys: Vec<ty::Ty> = param_tys.into_iter().map(|ty| self.normalize_ty(ty)).collect();
                let return_ty = self.normalize_ty(return_ty);
                self.tys.fn_(param_tys, return_ty, var_args)
            }
            Ref(ty) => {
                let ty = self.normalize_ty(ty);
                self.tys.ref_(ty)
            }
            Ptr(ty) => {
                let ty = self.normalize_ty(ty);
                self.tys.ptr(ty)
            }
            Alias(ty) => self.normalize_ty(ty),
            GenVar(_) => ty,
            TraitSelf(_) => ty,
            Closure { .. } => ty,
            AssocTy {
                base_ty,
                trait_inst,
                assoc_ty_idx,
            } => {
                let base_ty = self.normalize_ty(base_ty);
                let trait_inst = traits::TraitInst {
                    trait_: trait_inst.trait_,
                    gen_args: trait_inst.gen_args.iter().map(|ty| self.normalize_ty(*ty)).collect(),
                };

                let impl_insts: Vec<_> = self
                    .get_impl_insts_for_ty_and_trait_inst(base_ty, &trait_inst)
                    .collect();

                let [impl_inst] = &impl_insts[..] else { return ty };

                let impl_def = self.impls.get_impl_def(impl_inst.impl_);
                let assoc_ty = impl_def.assoc_tys[&assoc_ty_idx];

                let subst = GenVarSubst::new(&impl_def.gen_params, &impl_inst.gen_args).unwrap();

                self.tys.substitute_gen_vars(assoc_ty, &subst)
            }
        }
    }
}
