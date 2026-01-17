use std::collections::HashMap;

use crate::{
    ctxt::{fns, mlr, ty},
    h2m::H2MResult,
};

impl<'a> super::H2M<'a> {
    pub fn match_param_and_return_ty(
        &mut self,
        param_tys: &[ty::Ty],
        return_ty: ty::Ty,
        expected: Option<ty::Ty>,
    ) -> H2MResult<()> {
        if let Some(expected) = expected
            && let Some((exp_param_tys, exp_return_ty, var_args)) = self.ctxt().ty_is_callable(expected)
            && exp_param_tys.len() == param_tys.len()
            && !var_args
        {
            for (&param_ty, &exp_param_ty) in param_tys.iter().zip(exp_param_tys.iter()) {
                self.tys().unify(param_ty, exp_param_ty).unwrap();
            }
            self.tys().unify(return_ty, exp_return_ty).unwrap();
        }

        Ok(())
    }

    pub fn generate_captures_ty(&mut self) -> H2MResult<ty::Ty> {
        let captures_struct_name = format!(
            "<Captures:{}.{}>",
            self.builder.get_signature().name,
            self.closure_counter
        );

        // The captures type (potentially) depends on all generic variables that the surrounding
        // function depends on
        let surrounding_gen_vars: Vec<_> = self
            .builder
            .get_signature()
            .env_gen_params
            .iter()
            .chain(&self.builder.get_signature().gen_params)
            .cloned()
            .collect();

        // Instantiate the captures struct with the surrounding generic variables (to be replaced
        // later)
        let gen_args: Vec<_> = surrounding_gen_vars
            .iter()
            .map(|&gen_var| self.tys().gen_var(gen_var))
            .collect();

        let captures_struct = self
            .tys()
            .register_struct_with_existing_gen_vars(&captures_struct_name, surrounding_gen_vars)
            .unwrap();

        let captures_ty = self.tys().inst_struct(captures_struct, gen_args)?;

        Ok(captures_ty)
    }

    pub fn generate_closure_fn_sig(
        &mut self,
        param_names: &[String],
        param_tys: &[ty::Ty],
        return_ty: ty::Ty,
        captures_ty: ty::Ty,
    ) -> fns::FnSig {
        let outer_sig = self.builder.get_signature();

        let name = format!(
            "anonymous:{}.{}",
            self.builder.get_signature().name,
            self.closure_counter
        );

        let env_gen_params = outer_sig
            .env_gen_params
            .iter()
            .chain(&outer_sig.gen_params)
            .cloned()
            .collect();

        assert_eq!(param_names.len(), param_tys.len());
        let captures_param = fns::FnParam {
            kind: fns::FnParamKind::Regular("captures".to_string()),
            ty: captures_ty,
        };
        let params = std::iter::once(captures_param)
            .chain(param_names.iter().zip(param_tys).map(|(name, &ty)| fns::FnParam {
                kind: fns::FnParamKind::Regular(name.clone()),
                ty,
            }))
            .collect();

        fns::FnSig {
            name,
            associated_ty: None,
            associated_trait_inst: None,
            gen_params: Vec::new(),
            env_gen_params,
            params,
            var_args: false,
            return_ty,
        }
    }

    pub fn generate_closure_fn_inst(&mut self, signature: fns::FnSig) -> H2MResult<fns::FnInst> {
        let env_gen_args = signature
            .env_gen_params
            .iter()
            .map(|&gen_var| self.tys().gen_var(gen_var))
            .collect();

        let fn_ = self.fns().register_fn(signature, false).unwrap();

        let fn_inst = fns::FnInst {
            fn_,
            gen_args: Vec::new(),
            env_gen_args,
        };

        let target_fn = self.builder.target_fn();
        self.fns().register_fn_call(target_fn, fn_inst.clone());

        Ok(fn_inst)
    }

    pub fn generate_closure_ty(&mut self, fn_inst: fns::FnInst, captures_ty: ty::Ty) -> ty::Ty {
        let closure_name = format!("Closure:{}.{}", self.builder.get_signature().name, self.closure_counter);
        self.closure_counter += 1;
        self.tys().closure(fn_inst, closure_name, captures_ty)
    }

    pub fn fill_captures_fields(
        &mut self,
        closure_place: mlr::Place,
        captured_values: HashMap<mlr::Loc, usize>,
    ) -> H2MResult<()> {
        if captured_values.is_empty() {
            return Ok(());
        }

        let captures_place = self.builder.insert_closure_captures_place(closure_place)?;

        self.builder.start_new_block();
        for (loc, field_index) in captured_values {
            let place = self.builder.insert_loc_place(loc)?;
            let val = self.builder.insert_use_place_val(place)?;

            let field_place = self.builder.insert_field_access_place(captures_place, field_index)?;

            self.builder.insert_assign_stmt(field_place, val)?;
        }
        self.builder.end_and_insert_current_block();

        Ok(())
    }
}
