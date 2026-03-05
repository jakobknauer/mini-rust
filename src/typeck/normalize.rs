use crate::ctxt::{fns, traits, ty};

use super::{ExprExtra, MthdResolution};

impl<'ctxt, 'hlr> super::Typeck<'ctxt, 'hlr> {
    pub(super) fn normalize_all(&mut self) {
        self.normalize_hlr_typing();
        self.normalize_closure_fns();
        self.normalize_closure_structs();
    }

    pub(super) fn normalize(&mut self, ty: ty::Ty) -> ty::Ty {
        use ty::TyDef::*;

        let ty_def = self.ctxt.tys.get_ty_def(ty).clone();

        match ty_def {
            InfVar(iv) => match self.type_vars.get(&iv).copied() {
                Some(resolved) => {
                    let normalized = self.normalize(resolved);
                    self.type_vars.insert(iv, normalized); // Path compression
                    normalized
                }
                None => ty,
            },

            Primitive(_) | GenVar(_) | Opaque(_) => ty,

            Closure {
                fn_inst,
                name,
                captures_ty,
            } => {
                let captures_ty = self.normalize(captures_ty);
                let gen_args = self.normalize_slice(fn_inst.gen_args);
                let env_gen_args = self.normalize_slice(fn_inst.env_gen_args);
                let fn_inst = fns::FnInst {
                    fn_: fn_inst.fn_,
                    gen_args: self.ctxt.tys.ty_slice(&gen_args),
                    env_gen_args: self.ctxt.tys.ty_slice(&env_gen_args),
                };
                self.ctxt.tys.closure(fn_inst, name, captures_ty)
            }

            TraitSelf(_) => {
                let self_ty = self
                    .ctxt
                    .fns
                    .get_sig(self.fn_.fn_)
                    .unwrap()
                    .associated_ty
                    .expect("TraitSelf in a function with no associated type");
                self.normalize(self_ty)
            }

            Tuple(items) => {
                let items = self.normalize_slice(items);
                self.ctxt.tys.tuple(&items)
            }

            Struct { struct_, gen_args } => {
                let gen_args = self.normalize_slice(gen_args);
                self.ctxt.tys.inst_struct(struct_, &gen_args).unwrap()
            }

            Enum { enum_, gen_args } => {
                let gen_args = self.normalize_slice(gen_args);
                self.ctxt.tys.inst_enum(enum_, &gen_args).unwrap()
            }

            Fn {
                param_tys,
                return_ty,
                var_args,
            } => {
                let param_tys = self.normalize_slice(param_tys);
                let return_ty = self.normalize(return_ty);
                self.ctxt.tys.fn_(&param_tys, return_ty, var_args)
            }

            Ref(inner) => {
                let inner = self.normalize(inner);
                self.ctxt.tys.ref_(inner)
            }

            Ptr(inner) => {
                let inner = self.normalize(inner);
                self.ctxt.tys.ptr(inner)
            }

            AssocTy {
                base_ty,
                trait_inst,
                assoc_ty_idx,
            } => {
                let base_ty = self.normalize(base_ty);
                let trait_gen_args = self.normalize_slice(trait_inst.gen_args);
                let trait_inst = traits::TraitInst {
                    trait_: trait_inst.trait_,
                    gen_args: self.ctxt.tys.ty_slice(&trait_gen_args),
                };

                let impl_insts: Vec<_> = self
                    .ctxt
                    .get_impl_insts_for_ty_and_trait_inst(base_ty, trait_inst)
                    .collect();
                let [impl_inst] = &impl_insts[..] else {
                    return self.ctxt.tys.assoc_ty(base_ty, trait_inst, assoc_ty_idx);
                };

                let impl_def = self.ctxt.impls.get_impl_def(impl_inst.impl_).clone();
                let assoc_ty = impl_def.assoc_tys[&assoc_ty_idx];
                let impl_gen_args = self.ctxt.tys.get_ty_slice(impl_inst.gen_args).to_vec();
                let subst = ty::GenVarSubst::new(&impl_def.gen_params, &impl_gen_args).unwrap();
                let result = self.ctxt.tys.substitute_gen_vars(assoc_ty, &subst);
                self.normalize(result)
            }
        }
    }

    pub(super) fn normalize_slice(&mut self, slice: ty::TySlice) -> Vec<ty::Ty> {
        let mut tys = self.ctxt.tys.get_ty_slice(slice).to_vec();
        for t in &mut tys {
            *t = self.normalize(*t);
        }
        tys
    }

    fn normalize_hlr_typing(&mut self) {
        let var_ids: Vec<_> = self.typing.var_types.keys().copied().collect();
        for var_id in var_ids {
            let ty = self.typing.var_types[&var_id];
            let normalized = self.normalize(ty);
            self.typing.var_types.insert(var_id, normalized);
        }

        let expr_ids: Vec<_> = self.typing.expr_types.keys().copied().collect();
        for expr_id in expr_ids {
            let ty = self.typing.expr_types[&expr_id];
            let normalized = self.normalize(ty);
            self.typing.expr_types.insert(expr_id, normalized);
        }

        let expr_ids: Vec<_> = self.typing.expr_extra.keys().copied().collect();
        for expr_id in expr_ids {
            let extra = self.typing.expr_extra.remove(&expr_id).unwrap();
            let extra = self.normalize_expr_extra(extra);
            self.typing.expr_extra.insert(expr_id, extra);
        }
    }

    fn normalize_expr_extra(&mut self, extra: ExprExtra) -> ExprExtra {
        match extra {
            ExprExtra::Closure { fn_inst, captured_vars } => ExprExtra::Closure {
                fn_inst: self.normalize_fn_inst(fn_inst),
                captured_vars,
            },
            ExprExtra::ValFn(fn_inst) => ExprExtra::ValFn(self.normalize_fn_inst(fn_inst)),
            ExprExtra::ValMthd(resolution) => ExprExtra::ValMthd(self.normalize_mthd_resolution(resolution)),
            ExprExtra::BinaryOpMthd(resolution) => ExprExtra::BinaryOpMthd(self.normalize_mthd_resolution(resolution)),
            ExprExtra::FieldAccess { .. } | ExprExtra::BinaryPrim(_) | ExprExtra::UnaryPrim(_) => extra,
        }
    }

    fn normalize_fn_inst(&mut self, fn_inst: fns::FnInst) -> fns::FnInst {
        let gen_args = self.normalize_slice(fn_inst.gen_args);
        let env_gen_args = self.normalize_slice(fn_inst.env_gen_args);
        fns::FnInst {
            fn_: fn_inst.fn_,
            gen_args: self.ctxt.tys.ty_slice(&gen_args),
            env_gen_args: self.ctxt.tys.ty_slice(&env_gen_args),
        }
    }

    fn normalize_mthd_resolution(&mut self, resolution: MthdResolution) -> MthdResolution {
        match resolution {
            MthdResolution::Inherent(fn_inst) => MthdResolution::Inherent(self.normalize_fn_inst(fn_inst)),
            MthdResolution::Trait(trait_mthd_inst) => {
                let impl_ty = self.normalize(trait_mthd_inst.impl_ty);
                let gen_args = self.normalize_slice(trait_mthd_inst.gen_args);
                let trait_gen_args = self.normalize_slice(trait_mthd_inst.trait_inst.gen_args);
                let trait_inst = traits::TraitInst {
                    trait_: trait_mthd_inst.trait_inst.trait_,
                    gen_args: self.ctxt.tys.ty_slice(&trait_gen_args),
                };
                MthdResolution::Trait(fns::TraitMthdInst {
                    impl_ty,
                    gen_args: self.ctxt.tys.ty_slice(&gen_args),
                    trait_inst,
                    ..trait_mthd_inst
                })
            }
        }
    }

    fn normalize_closure_fns(&mut self) {
        let fns = self.created_closure_fns.clone();
        for fn_ in fns {
            let sig = self.ctxt.fns.get_sig(fn_).unwrap();

            let return_ty = sig.return_ty;
            let param_tys: Vec<ty::Ty> = sig.params.iter().map(|p| p.ty).collect();

            let return_ty = self.normalize(return_ty);
            let param_tys: Vec<ty::Ty> = param_tys.into_iter().map(|ty| self.normalize(ty)).collect();

            let sig = self.ctxt.fns.get_mut_sig(fn_).unwrap();

            sig.return_ty = return_ty;
            for (param, ty) in sig.params.iter_mut().zip(param_tys) {
                param.ty = ty;
            }
        }
    }

    fn normalize_closure_structs(&mut self) {
        let structs = self.created_closure_structs.clone();
        for struct_ in structs {
            let field_tys: Vec<ty::Ty> = self
                .ctxt
                .tys
                .get_struct_def(struct_)
                .unwrap()
                .fields
                .iter()
                .map(|f| f.ty)
                .collect();

            let field_tys: Vec<ty::Ty> = field_tys.into_iter().map(|ty| self.normalize(ty)).collect();

            let struct_def = self.ctxt.tys.get_mut_struct_def(struct_).unwrap();
            for (field, ty) in struct_def.fields.iter_mut().zip(field_tys) {
                field.ty = ty;
            }
        }
    }
}
