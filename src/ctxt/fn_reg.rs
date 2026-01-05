use std::collections::HashMap;

use crate::ctxt::{
    fns::{Fn, FnMlr, FnSig, FnSpecialization, TraitMethod},
    ty::GenVarSubst,
};

#[derive(Default)]
pub struct FnReg {
    sigs: Vec<FnSig>,
    fn_names: HashMap<String, Fn>,
    defs: HashMap<Fn, FnMlr>,

    called_specializations: HashMap<Fn, Vec<FnSpecialization>>,
    called_trait_methods: HashMap<Fn, Vec<TraitMethod>>,
}

impl FnReg {
    pub fn register_fn(&mut self, signature: FnSig, register_name: bool) -> Result<Fn, ()> {
        let fn_ = Fn(self.sigs.len());

        if register_name {
            if self.fn_names.contains_key(&signature.name) {
                return Err(());
            }
            self.fn_names.insert(signature.name.to_string(), fn_);
        }

        self.sigs.push(signature);
        self.called_specializations.insert(fn_, Vec::new());
        self.called_trait_methods.insert(fn_, Vec::new());

        Ok(fn_)
    }

    pub fn get_sig(&self, fn_: Fn) -> Option<&FnSig> {
        self.sigs.get(fn_.0)
    }

    pub fn get_fn_by_name(&self, name: &str) -> Option<Fn> {
        self.fn_names.get(name).cloned()
    }

    pub fn add_fn_def(&mut self, fn_: Fn, mlr: FnMlr) {
        self.defs.insert(fn_, mlr);
    }

    pub fn is_fn_defined(&self, fn_: Fn) -> bool {
        self.defs.contains_key(&fn_)
    }

    pub fn get_fn_def(&self, fn_: Fn) -> Option<&FnMlr> {
        self.defs.get(&fn_)
    }

    pub fn get_all_fns(&self) -> impl Iterator<Item = Fn> {
        (0..self.sigs.len()).map(Fn)
    }

    pub fn specialize_fn(&mut self, caller: Fn, fn_spec: FnSpecialization) {
        self.called_specializations.entry(caller).or_default().push(fn_spec);
    }

    pub fn specialize_trait_method(&mut self, caller: Fn, trait_method: TraitMethod) {
        self.called_trait_methods.entry(caller).or_default().push(trait_method);
    }

    pub fn get_called_specializations(&self, caller: Fn) -> &Vec<FnSpecialization> {
        self.called_specializations.get(&caller).unwrap()
    }

    pub fn get_called_trait_methods(&self, caller: Fn) -> &Vec<TraitMethod> {
        self.called_trait_methods.get(&caller).unwrap()
    }

    pub fn get_subst_for_fn_spec(&self, fn_specialization: &FnSpecialization) -> GenVarSubst {
        let sig = self.get_sig(fn_specialization.fn_).unwrap();
        let gen_param_subst = GenVarSubst::new(&sig.gen_params, &fn_specialization.gen_args).unwrap();
        let env_gen_param_subst = GenVarSubst::new(&sig.env_gen_params, &fn_specialization.env_gen_args).unwrap();
        GenVarSubst::compose(env_gen_param_subst, gen_param_subst)
    }

    pub fn get_fn_name(&self, method: Fn) -> &str {
        self.get_sig(method)
            .map(|sig| sig.name.as_str())
            .unwrap_or("<unknown fn>")
    }
}
