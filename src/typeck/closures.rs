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
    env_gen_params: Vec<ty::GenVar>,
    env_gen_args: Vec<ty::Ty<'ty>>,
}

struct ScopeSnapshot {
    outer_var_ids: HashSet<hlr::VarId>,
    var_uses_start: usize,
}

struct CapturedVars(Vec<hlr::VarId>);

impl<'a, 'f, 'ctxt: 'a + 'hlr, 'hlr: 'ctxt> super::Typeck<'a, 'f, 'ctxt, 'hlr> {
    pub(super) fn check_closure(
        &mut self,
        expr_id: hlr::ExprId,
        params: hlr::ClosureParams<'hlr>,
        return_ty: Option<hlr::TyAnnot<'hlr>>,
        body: hlr::Expr<'hlr>,
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
        let fn_inst = self.build_closure_fn_inst(
            &names.fn_name,
            &env_gen_params,
            &env_gen_args,
            captures_ty,
            &body_signature,
        );

        self.typing.expr_extra.insert(
            expr_id,
            ExprExtra::Closure {
                fn_inst,
                captured_vars: captured_vars.0,
            },
        );

        let closure_ty = self.ctxt.tys.closure(fn_inst, &names.fn_name, captures_ty);
        Ok(closure_ty)
    }

    fn snapshot_scope(&self) -> ScopeSnapshot {
        ScopeSnapshot {
            outer_var_ids: self.typing.var_types.keys().cloned().collect(),
            var_uses_start: self.var_uses.len(),
        }
    }

    fn generate_closure_names(&mut self) -> ClosureNames {
        let outer_sig = self.ctxt.fns.get_sig(self.fn_.fn_).unwrap();
        let fn_name = format!("<closure:{}:{}>", outer_sig.name, self.closure_counter);
        let captures_name = format!("<Captures:{}:{}>", outer_sig.name, self.closure_counter);
        self.closure_counter += 1;
        ClosureNames { fn_name, captures_name }
    }

    fn collect_closure_generics(&mut self) -> ClosureGenerics<'ctxt> {
        let outer_sig = self.ctxt.fns.get_sig(self.fn_.fn_).unwrap();

        let env_gen_params: Vec<ty::GenVar> = outer_sig
            .env_gen_params
            .iter()
            .cloned()
            .chain(outer_sig.gen_params.iter().cloned())
            .collect();

        let _ = outer_sig;

        let env_gen_args: Vec<ty::Ty<'ctxt>> = env_gen_params.iter().map(|&gv| self.ctxt.tys.gen_var(gv)).collect();

        ClosureGenerics {
            env_gen_params,
            env_gen_args,
        }
    }

    fn try_get_callable_hint_info(&mut self, hint: ty::Ty<'ctxt>) -> Option<(ty::TySlice<'ctxt>, ty::Ty<'ctxt>)> {
        let normalized = self.normalize(hint);
        if let Some((param_tys, return_ty, _)) = self.ctxt.ty_is_callable(&self.constraints, normalized) {
            return Some((param_tys, return_ty));
        }
        let obligations = self.pending_obligations.clone();
        obligations.iter().find_map(|(ty, req)| {
            if self.normalize(*ty) == normalized {
                if let &ty::ConstraintRequirement::Callable { param_tys, return_ty } = req {
                    Some((param_tys, return_ty))
                } else {
                    None
                }
            } else {
                None
            }
        })
    }

    fn check_closure_body(
        &mut self,
        params: hlr::ClosureParams<'hlr>,
        return_ty_annot: Option<hlr::TyAnnot<'hlr>>,
        body: hlr::Expr<'hlr>,
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

        let stack_entry = return_ty;
        self.return_ty_stack.push(stack_entry);
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
        env_gen_params: &[ty::GenVar],
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

    fn build_closure_fn_inst(
        &mut self,
        fn_name: &str,
        env_gen_params: &[ty::GenVar],
        env_gen_args: &[ty::Ty<'ctxt>],
        captures_ty: ty::Ty<'ctxt>,
        checked: &ClosureBodySignature<'ctxt>,
    ) -> fns::FnInst<'ctxt> {
        let captures_param = fns::FnParam {
            kind: fns::FnParamKind::Regular("__captures".to_string()),
            ty: captures_ty,
        };
        let regular_params = checked.param_tys.iter().map(|&ty| fns::FnParam {
            kind: fns::FnParamKind::Regular("__param".to_string()),
            ty,
        });
        let fn_params: Vec<fns::FnParam> = std::iter::once(captures_param).chain(regular_params).collect();

        let closure_fn = self
            .ctxt
            .fns
            .register_fn(
                fns::FnSig {
                    name: fn_name.to_string(),
                    gen_params: vec![],
                    env_gen_params: env_gen_params.to_vec(),
                    env_constraints: Vec::new(),
                    params: fn_params,
                    var_args: false,
                    return_ty: checked.return_ty,
                    associated_ty: None,
                    associated_trait_inst: None,
                    constraints: self.constraints.clone(),
                },
                false,
            )
            .unwrap();

        self.created_closure_fns.push(closure_fn);

        let gen_args = self.ctxt.tys.ty_slice(&[]);
        let env_gen_args_slice = self.ctxt.tys.ty_slice(env_gen_args);
        self.ctxt.fns.inst_fn(closure_fn, gen_args, env_gen_args_slice).unwrap()
    }
}
