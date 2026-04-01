use std::collections::HashSet;

use crate::{
    ctxt::{fns, ty},
    hlr,
    typeck::{ExprExtra, TypeckError, TypeckResult},
};

struct ClosureBodySignature<'ty> {
    param_tys: Vec<ty::Ty<'ty>>,
    return_ty: ty::Ty<'ty>,
}

struct ClosureNames {
    fn_name: String,
    captures_name: String,
}

struct ClosureGenerics<'ty> {
    env_gen_params: Vec<ty::GenVar<'ty>>,
    env_gen_args: Vec<ty::Ty<'ty>>,
}

struct ScopeSnapshot {
    outer_var_ids: HashSet<hlr::VarId>,
    var_uses_start: usize,
}

struct CapturedVars(Vec<hlr::VarId>);

impl<'a, 'ctxt: 'a> super::Typeck<'a, 'ctxt> {
    pub(super) fn check_closure(
        &mut self,
        expr_id: hlr::ExprId,
        params: hlr::ClosureParams<'ctxt>,
        return_ty: Option<hlr::TyAnnot<'ctxt>>,
        body: hlr::Expr<'ctxt>,
        hint: Option<ty::Ty<'ctxt>>,
    ) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        let scope_snapshot = self.snapshot_scope();
        let body_signature = self.check_closure_body(params, return_ty, body, hint)?;
        let captured_vars = self.create_capture_data(scope_snapshot);

        let names = self.generate_closure_names();
        let ClosureGenerics {
            env_gen_params,
            env_gen_args,
        } = self.collect_closure_generics();

        let captures_ty =
            self.build_closure_captures_ty(&names.captures_name, &env_gen_params, &env_gen_args, &captured_vars);

        self.typing.expr_extra.insert(
            expr_id,
            ExprExtra::Closure {
                captured_vars: captured_vars.0,
            },
        );

        let param_tys = self.ctxt.tys.ty_slice(&body_signature.param_tys);
        let closure_ty = self
            .ctxt
            .tys
            .closure(&names.fn_name, captures_ty, param_tys, body_signature.return_ty);
        Ok(closure_ty)
    }

    pub(super) fn create_closure_fns(&mut self) {
        let closure_exprs: Vec<_> = self
            .typing
            .expr_extra
            .iter()
            .filter_map(|(&expr_id, extra)| {
                if matches!(extra, ExprExtra::Closure { .. }) {
                    Some(expr_id)
                } else {
                    None
                }
            })
            .collect();

        let env_gen_params: Vec<ty::GenVar> = self
            .fn_
            .fn_
            .env_gen_params
            .iter()
            .cloned()
            .chain(self.fn_.fn_.gen_params.iter().cloned())
            .collect();

        for expr_id in closure_exprs {
            let closure_ty = self.typing.expr_types[&expr_id];
            let ty::TyDef::Closure {
                ref name,
                captures_ty,
                param_tys,
                return_ty,
                ref fn_,
            } = *closure_ty.0
            else {
                unreachable!()
            };

            let captures_param = fns::FnParam {
                kind: fns::FnParamKind::Regular("__captures".to_string()),
                ty: captures_ty,
                mutable: false,
            };
            let params: Vec<fns::FnParam> = std::iter::once(captures_param)
                .chain(param_tys.iter().map(|&ty| fns::FnParam {
                    kind: fns::FnParamKind::Regular("__param".to_string()),
                    ty,
                    mutable: false,
                }))
                .collect();

            let closure_fn = self
                .ctxt
                .fns
                .register_fn(
                    fns::FnDecl {
                        id: fns::FnId::default(),
                        name: name.clone(),
                        gen_params: vec![],
                        env_gen_params: env_gen_params.clone(),
                        env_constraints: Vec::new(),
                        params,
                        var_args: false,
                        return_ty,
                        associated_ty: None,
                        associated_trait_inst: None,
                        constraints: self.constraints.clone(),
                    },
                    false,
                )
                .unwrap();

            fn_.set(closure_fn);
        }
    }

    fn snapshot_scope(&self) -> ScopeSnapshot {
        ScopeSnapshot {
            outer_var_ids: self.typing.var_types.keys().cloned().collect(),
            var_uses_start: self.var_uses.len(),
        }
    }

    fn generate_closure_names(&mut self) -> ClosureNames {
        let fn_name = format!("<closure:{}:{}>", self.fn_.fn_.name, self.closure_counter);
        let captures_name = format!("<Captures:{}:{}>", self.fn_.fn_.name, self.closure_counter);
        self.closure_counter += 1;
        ClosureNames { fn_name, captures_name }
    }

    fn collect_closure_generics(&mut self) -> ClosureGenerics<'ctxt> {
        let env_gen_params: Vec<ty::GenVar> = self
            .fn_
            .fn_
            .env_gen_params
            .iter()
            .cloned()
            .chain(self.fn_.fn_.gen_params.iter().cloned())
            .collect();

        let env_gen_args: Vec<ty::Ty<'ctxt>> = env_gen_params.iter().map(|&gv| self.ctxt.tys.gen_var(gv)).collect();

        ClosureGenerics {
            env_gen_params,
            env_gen_args,
        }
    }

    fn try_get_callable_hint_info(&mut self, hint: ty::Ty<'ctxt>) -> Option<(ty::TySlice<'ctxt>, ty::Ty<'ctxt>)> {
        let normalized = self.normalize(hint);
        let constraints = self.constraints.clone();
        if let Some((param_tys, return_ty, _)) = self.ctxt.ty_is_callable(&constraints, normalized) {
            return Some((param_tys, return_ty));
        }
        let obligations = self.pending_obligations.clone();
        obligations.iter().find_map(|(ty, req)| {
            if self.normalize(*ty) == normalized
                && let &ty::ConstraintRequirement::Callable { param_tys, return_ty } = req
            {
                Some((param_tys, return_ty))
            } else {
                None
            }
        })
    }

    fn check_closure_body(
        &mut self,
        params: hlr::ClosureParams<'ctxt>,
        return_ty_annot: Option<hlr::TyAnnot<'ctxt>>,
        body: hlr::Expr<'ctxt>,
        hint: Option<ty::Ty<'ctxt>>,
    ) -> TypeckResult<'ctxt, ClosureBodySignature<'ctxt>> {
        let mut param_tys = Vec::new();
        for hlr::ClosureParam(var_id, annot) in params {
            let ty = self.resolve_optional_ty_annot(*annot)?;
            self.typing.var_types.insert(*var_id, ty);
            param_tys.push(ty);
        }

        let return_ty = self.resolve_optional_ty_annot(return_ty_annot)?;

        if let Some(hint) = hint
            && let Some((hint_param_tys, hint_return_ty)) = self.try_get_callable_hint_info(hint)
        {
            let hint_param_tys = hint_param_tys.to_vec();
            for (ty, hint_ty) in param_tys.iter().zip(hint_param_tys) {
                self.unify(*ty, hint_ty);
            }
            self.unify(return_ty, hint_return_ty);
        }

        self.return_ty_stack.push(return_ty);
        let body_ty = self.check_expr(body, Some(return_ty));
        self.return_ty_stack.pop();
        let body_ty = body_ty?;

        if !self.unify(body_ty, return_ty) {
            return Err(TypeckError::ReturnTypeMismatch {
                expected: return_ty,
                actual: body_ty,
            });
        }

        Ok(ClosureBodySignature { param_tys, return_ty })
    }

    fn create_capture_data(&mut self, scope_snapshot: ScopeSnapshot) -> CapturedVars {
        let mut seen = HashSet::new();

        let captured_vars: Vec<hlr::VarId> = self.var_uses[scope_snapshot.var_uses_start..]
            .iter()
            .filter(|v| scope_snapshot.outer_var_ids.contains(v) && seen.insert(**v))
            .copied()
            .collect();

        CapturedVars(captured_vars)
    }

    fn build_closure_captures_ty(
        &mut self,
        struct_name: &str,
        env_gen_params: &[ty::GenVar<'ctxt>],
        env_gen_args: &[ty::Ty<'ctxt>],
        captured_vars: &CapturedVars,
    ) -> ty::Ty<'ctxt> {
        let captures_struct = self
            .ctxt
            .tys
            .register_struct_with_existing_gen_vars(struct_name, env_gen_params.to_vec())
            .unwrap();

        self.created_closure_structs
            .push((captures_struct, captured_vars.0.to_vec()));

        self.ctxt.tys.inst_struct(captures_struct, env_gen_args).unwrap()
    }
}
