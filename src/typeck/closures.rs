use std::collections::HashSet;

use crate::{
    ctxt::{fns, ty},
    hlr,
    typeck::{ExprExtra, TypeckError, TypeckResult},
};

struct ClosureBodySignature {
    param_tys: Vec<ty::Ty>,
    return_ty: ty::Ty,
}

struct ClosureNames {
    fn_name: String,
    captures_name: String,
}

struct ClosureGenerics {
    env_gen_params: Vec<ty::GenVar>,
    env_gen_args: Vec<ty::Ty>,
}

struct ScopeSnapshot {
    outer_var_ids: HashSet<hlr::VarId>,
    var_uses_start: usize,
}

struct CaptureData {
    captured_vars: Vec<hlr::VarId>,
    captured_tys: Vec<ty::Ty>,
}

impl<'ctxt, 'hlr> super::Typeck<'ctxt, 'hlr> {
    pub(super) fn infer_closure_ty(
        &mut self,
        expr_id: hlr::ExprId,
        params: hlr::ClosureParams<'hlr>,
        return_ty: Option<hlr::TyAnnot<'hlr>>,
        body: hlr::Expr<'hlr>,
    ) -> TypeckResult<ty::Ty> {
        let scope_snapshot = self.snapshot_scope();
        let body_signature = self.check_closure_body(params, return_ty, body)?;
        let capture_data = self.create_capture_data(scope_snapshot);

        let names = self.generate_closure_names();
        let generics = self.collect_closure_generics();

        let captures_ty = self.build_closure_captures_ty(&names.captures_name, &generics, &capture_data.captured_tys);
        let fn_inst = self.build_closure_fn_inst(&names.fn_name, &generics, captures_ty, &body_signature);

        self.typing.expr_extra.insert(
            expr_id,
            ExprExtra::Closure {
                fn_inst,
                captured_vars: capture_data.captured_vars,
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

    fn collect_closure_generics(&mut self) -> ClosureGenerics {
        let outer_sig = self.ctxt.fns.get_sig(self.fn_.fn_).unwrap();

        let env_gen_params: Vec<ty::GenVar> = outer_sig
            .env_gen_params
            .iter()
            .cloned()
            .chain(outer_sig.gen_params.iter().cloned())
            .collect();

        let env_gen_args: Vec<ty::Ty> = env_gen_params.iter().map(|&gv| self.ctxt.tys.gen_var(gv)).collect();

        ClosureGenerics {
            env_gen_params,
            env_gen_args,
        }
    }

    fn check_closure_body(
        &mut self,
        params: hlr::ClosureParams<'hlr>,
        return_ty_annot: Option<hlr::TyAnnot<'hlr>>,
        body: hlr::Expr<'hlr>,
    ) -> TypeckResult<ClosureBodySignature> {
        let mut param_tys = Vec::new();
        for hlr::ClosureParam(var_id, annot) in params {
            let ty = match annot {
                Some(a) => self.resolve_ty_annot(a)?,
                None => self.new_inf_var(),
            };
            self.typing.var_types.insert(*var_id, ty);
            param_tys.push(ty);
        }

        let return_ty_hint = return_ty_annot.map(|a| self.resolve_ty_annot(a)).transpose()?;
        let stack_entry = return_ty_hint.unwrap_or_else(|| self.new_inf_var());
        self.return_ty_stack.push(stack_entry);
        let body_ty = self.infer_expr_ty(body, return_ty_hint);
        self.return_ty_stack.pop();
        let body_ty = body_ty?;

        let return_ty = if let Some(hint) = return_ty_hint {
            if !self.unify(body_ty, hint) {
                return Err(TypeckError::ReturnTypeMismatch {
                    expected: hint,
                    actual: body_ty,
                });
            }
            hint
        } else {
            body_ty
        };

        Ok(ClosureBodySignature { param_tys, return_ty })
    }

    fn create_capture_data(&mut self, scope_snapshot: ScopeSnapshot) -> CaptureData {
        let mut seen = HashSet::new();

        let captured_vars: Vec<hlr::VarId> = self.var_uses[scope_snapshot.var_uses_start..]
            .iter()
            .filter(|v| scope_snapshot.outer_var_ids.contains(v) && seen.insert(**v))
            .copied()
            .collect();

        let captured_tys = captured_vars
            .iter()
            .map(|var_id| {
                let ty = self.typing.var_types[var_id];
                self.normalize(ty)
            })
            .collect();

        CaptureData {
            captured_vars,
            captured_tys,
        }
    }

    fn build_closure_captures_ty(
        &mut self,
        struct_name: &str,
        generics: &ClosureGenerics,
        captured_tys: &[ty::Ty],
    ) -> ty::Ty {
        let captures_struct = self
            .ctxt
            .tys
            .register_struct_with_existing_gen_vars(struct_name, generics.env_gen_params.clone())
            .unwrap();

        let struct_def = self.ctxt.tys.get_mut_struct_def(captures_struct).unwrap();
        for (i, field_ty) in captured_tys.iter().enumerate() {
            struct_def.fields.push(ty::StructField {
                name: format!("field_{i}"),
                ty: *field_ty,
            });
        }

        self.ctxt
            .tys
            .inst_struct(captures_struct, &generics.env_gen_args)
            .unwrap()
    }

    fn build_closure_fn_inst(
        &mut self,
        fn_name: &str,
        generics: &ClosureGenerics,
        captures_ty: ty::Ty,
        checked: &ClosureBodySignature,
    ) -> fns::FnInst {
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
                    env_gen_params: generics.env_gen_params.clone(),
                    params: fn_params,
                    var_args: false,
                    return_ty: checked.return_ty,
                    associated_ty: None,
                    associated_trait_inst: None,
                },
                false,
            )
            .unwrap();

        let gen_args = self.ctxt.tys.ty_slice(&[]);
        let env_gen_args = self.ctxt.tys.ty_slice(&generics.env_gen_args);
        fns::FnInst {
            fn_: closure_fn,
            gen_args,
            env_gen_args,
        }
    }
}
