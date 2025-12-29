use crate::{
    ctxt::{fns, ty},
    h2m::H2MResult,
};

impl<'a> super::H2M<'a> {
    pub fn match_param_and_return_ty(
        &mut self,
        n_params: usize,
        expected: Option<ty::Ty>,
    ) -> H2MResult<(Vec<ty::Ty>, ty::Ty)> {
        let param_tys: Vec<_> = (0..n_params).map(|_| self.tys().new_undefined_ty()).collect();
        let return_ty = self.tys().new_undefined_ty();

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

        Ok((param_tys, return_ty))
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
            name: "captures".to_string(),
            ty: captures_ty,
        };
        let params = std::iter::once(captures_param)
            .chain(
                param_names
                    .iter()
                    .zip(param_tys)
                    .map(|(name, &ty)| fns::FnParam { name: name.clone(), ty }),
            )
            .collect();

        fns::FnSig {
            name,
            associated_ty: None,
            associated_trait: None,
            gen_params: Vec::new(),
            env_gen_params,
            params,
            var_args: false,
            return_ty,
            has_receiver: false,
        }
    }

    pub fn generate_closure_fn_spec(&mut self, signature: fns::FnSig) -> H2MResult<fns::FnSpecialization> {
        let env_gen_args = signature
            .env_gen_params
            .iter()
            .map(|&gen_var| self.tys().register_gen_var_ty(gen_var))
            .collect();

        let fn_ = self.fns().register_fn(signature, false).unwrap();

        let fn_spec = fns::FnSpecialization {
            fn_,
            gen_args: Vec::new(),
            env_gen_args,
        };

        let target_fn = self.builder.target_fn();
        self.fns().specialize_fn(target_fn, fn_spec.clone());

        Ok(fn_spec)
    }

    pub fn generate_closure_ty(&mut self, fn_spec: fns::FnSpecialization, captures_ty: ty::Ty) -> ty::Ty {
        let closure_name = format!("Closure:{}.{}", self.builder.get_signature().name, self.closure_counter);
        self.closure_counter += 1;
        self.tys().register_closure_type(fn_spec, closure_name, captures_ty)
    }
}
