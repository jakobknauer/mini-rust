use std::collections::HashMap;

use crate::ctxt::{
    fns::{Fn, FnMlr, FnSig, FnSpecialization},
    ty::{GenVar, Ty},
};

#[derive(Default)]
pub struct FnReg {
    sigs: Vec<FnSig>,
    fn_names: HashMap<String, Fn>,
    defs: HashMap<Fn, FnMlr>,

    called_specializations: HashMap<Fn, Vec<FnSpecialization>>,
}

impl FnReg {
    pub fn register_fn(&mut self, signature: FnSig) -> Result<Fn, ()> {
        if self.fn_names.contains_key(&signature.name) {
            return Err(());
        }

        let fn_ = Fn(self.sigs.len());

        self.fn_names.insert(signature.name.to_string(), fn_);
        self.sigs.push(signature);
        self.called_specializations.insert(fn_, Vec::new());

        Ok(fn_)
    }

    pub fn get_sig(&self, fn_: &Fn) -> Option<&FnSig> {
        self.sigs.get(fn_.0)
    }

    pub fn get_fn_by_name(&self, name: &str) -> Option<Fn> {
        self.fn_names.get(name).cloned()
    }

    pub fn add_fn_def(&mut self, fn_: Fn, mlr: FnMlr) {
        self.defs.insert(fn_, mlr);
    }

    pub fn is_fn_defined(&self, fn_: &Fn) -> bool {
        self.defs.contains_key(fn_)
    }

    pub fn get_fn_def(&self, fn_: &Fn) -> Option<&FnMlr> {
        self.defs.get(fn_)
    }

    pub fn get_all_fns(&self) -> impl IntoIterator<Item = &Fn> {
        self.fn_names.values()
    }

    pub fn specialize_fn(
        &mut self,
        caller: &Fn,
        callee: &Fn,
        gen_args: impl Into<Vec<Ty>>,
        env_gen_args: impl Into<Vec<Ty>>,
    ) {
        self.called_specializations
            .entry(*caller)
            .or_default()
            .push(FnSpecialization {
                fn_: *callee,
                gen_args: gen_args.into(),
                env_gen_args: env_gen_args.into(),
            });
    }

    pub fn get_called_specializations(&self, caller: &Fn) -> &Vec<FnSpecialization> {
        self.called_specializations.get(caller).unwrap()
    }

    pub fn get_substitutions_for_specialization(&self, fn_specialization: &FnSpecialization) -> HashMap<GenVar, Ty> {
        let sig = self.get_sig(&fn_specialization.fn_).unwrap();
        let gen_param_substitutions = sig
            .gen_params
            .iter()
            .cloned()
            .zip(fn_specialization.gen_args.iter().cloned());
        let env_gen_param_substitutions = sig
            .env_gen_params
            .iter()
            .cloned()
            .zip(fn_specialization.env_gen_args.iter().cloned());
        env_gen_param_substitutions.chain(gen_param_substitutions).collect()
    }
}
