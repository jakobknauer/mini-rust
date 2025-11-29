use std::collections::{BTreeMap, HashMap};

use crate::ctxt::{
    fns::{Fn, FnMlr, FnSig, FnSpecialization},
    ty::Ty,
};

pub struct FnReg {
    fn_names: BTreeMap<String, Fn>,
    sigs: HashMap<Fn, FnSig>,
    next_fn: Fn,
    defs: HashMap<Fn, FnMlr>,

    called_specializations: HashMap<Fn, Vec<FnSpecialization>>,
}

impl FnReg {
    pub fn new() -> FnReg {
        FnReg {
            fn_names: BTreeMap::new(),
            sigs: HashMap::new(),
            next_fn: Fn(0),
            defs: HashMap::new(),
            called_specializations: HashMap::new(),
        }
    }

    pub fn register_fn(&mut self, signature: FnSig) -> Result<Fn, ()> {
        if self.fn_names.contains_key(&signature.name) {
            return Err(());
        }

        let fn_ = self.next_fn;
        self.next_fn.0 += 1;

        self.fn_names.insert(signature.name.to_string(), fn_);
        self.sigs.insert(fn_, signature);
        self.called_specializations.insert(fn_, Vec::new());

        Ok(fn_)
    }

    pub fn get_sig(&self, fn_: &Fn) -> Option<&FnSig> {
        self.sigs.get(fn_)
    }

    pub fn get_fn_by_name(&self, name: &str) -> Option<Fn> {
        self.fn_names.get(name).cloned()
    }

    pub fn add_fn_def(&mut self, name: &str, mlr: FnMlr) {
        if let Some(&fn_) = self.fn_names.get(name) {
            self.defs.insert(fn_, mlr);
        }
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

    pub fn specialize_fn(&mut self, caller: &Fn, callee: &Fn, gen_args: impl Into<Vec<Ty>>) {
        self.called_specializations
            .entry(*caller)
            .or_default()
            .push(FnSpecialization {
                fn_: *callee,
                gen_args: gen_args.into(),
            });
    }

    pub fn get_called_specializations(&self, caller: &Fn) -> &Vec<FnSpecialization> {
        self.called_specializations.get(caller).unwrap()
    }

    pub fn get_substitutions_for_specialization(&self, fn_specialization: &FnSpecialization) -> HashMap<&str, Ty> {
        let sig = self.get_sig(&fn_specialization.fn_).unwrap();
        sig.gen_params
            .iter()
            .zip(&fn_specialization.gen_args)
            .map(|(gen_param, gen_arg)| (gen_param.name.as_str(), *gen_arg))
            .collect()
    }
}
