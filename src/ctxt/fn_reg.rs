use std::collections::{BTreeMap, HashMap};

use crate::{
    ctxt::{
        fns::{Fn, FnSig, InstantiatedFn},
        ty::Ty,
    },
    mlr::Mlr,
};

pub struct FnReg {
    fn_names: BTreeMap<String, Fn>,
    sigs: HashMap<Fn, FnSig>,
    next_fn: Fn,
    defs: HashMap<Fn, Mlr>,

    instantiated_fns: HashMap<Fn, Vec<InstantiatedFn>>,
}

impl FnReg {
    pub fn new() -> FnReg {
        FnReg {
            fn_names: BTreeMap::new(),
            sigs: HashMap::new(),
            next_fn: Fn(0),
            defs: HashMap::new(),
            instantiated_fns: HashMap::new(),
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
        self.instantiated_fns.insert(fn_, Vec::new());

        Ok(fn_)
    }

    pub fn get_sig(&self, fn_: &Fn) -> Option<&FnSig> {
        self.sigs.get(fn_)
    }

    pub fn get_fn_name(&self, fn_: &Fn) -> Option<&str> {
        self.sigs.get(fn_).map(|sig| sig.name.as_str())
    }

    pub fn get_fn_by_name(&self, name: &str) -> Option<Fn> {
        self.fn_names.get(name).cloned()
    }

    pub fn add_fn_def(&mut self, name: &str, mlr: Mlr) {
        if let Some(&fn_) = self.fn_names.get(name) {
            self.defs.insert(fn_, mlr);
        }
    }

    pub fn is_fn_defined(&self, fn_: &Fn) -> bool {
        self.defs.contains_key(fn_)
    }

    pub fn get_fn_def(&self, fn_: &Fn) -> Option<&Mlr> {
        self.defs.get(fn_)
    }

    pub fn get_all_fns(&self) -> impl IntoIterator<Item = &Fn> {
        self.fn_names.values()
    }

    pub fn add_instantiated_fn(&mut self, caller: &Fn, callee: &Fn, gen_args: Vec<Ty>) {
        self.instantiated_fns
            .entry(*caller)
            .or_default()
            .push(InstantiatedFn { fn_: *callee, gen_args });
    }

    pub fn get_instantiated_fns(&self, caller: &Fn) -> &Vec<InstantiatedFn> {
        self.instantiated_fns.get(caller).unwrap()
    }
}
