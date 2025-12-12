mod fn_reg;
pub mod fns;
pub mod mlr;
pub mod ty;
mod ty_reg;

pub use fn_reg::FnReg;
pub use ty_reg::*;

use mlr::*;

pub struct Ctxt {
    pub tys: TyReg,
    pub fns: FnReg,
    pub mlr: Mlr,
}

impl Ctxt {
    pub fn new() -> Self {
        Self {
            tys: Default::default(),
            fns: Default::default(),
            mlr: Mlr::new(),
        }
    }

    pub fn get_fn_spec_name(&self, fn_spec: &fns::FnSpecialization) -> String {
        let signature = self.fns.get_sig(&fn_spec.fn_).unwrap();
        if signature.gen_params.is_empty() {
            signature.name.to_string()
        } else {
            format!(
                "{}::<{}>",
                signature.name,
                fn_spec
                    .gen_args
                    .iter()
                    .map(|&ty| self.tys.get_string_rep(ty))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
    }

    pub fn get_specialized_fn_sig(&mut self, fn_spec: &fns::FnSpecialization) -> fns::FnSig {
        let signature = self.fns.get_sig(&fn_spec.fn_).unwrap();
        let substitutions = self.fns.get_substitutions_for_specialization(fn_spec);

        let specialized_params = signature
            .params
            .iter()
            .map(|param| fns::FnParam {
                name: param.name.clone(),
                ty: self.tys.substitute_gen_vars(param.ty, &substitutions),
            })
            .collect();

        let specialized_return_ty = self.tys.substitute_gen_vars(signature.return_ty, &substitutions);

        fns::FnSig {
            name: self.get_fn_spec_name(fn_spec),
            gen_params: Vec::new(),
            params: specialized_params,
            return_ty: specialized_return_ty,
        }
    }
}
