use std::collections::HashMap;

use crate::ctxt::{
    fns::{Fn, FnInst, FnInstError, FnSig, TraitMthdInst},
    ty::TySlice,
};

#[derive(Default)]
pub struct FnReg {
    sigs: Vec<FnSig>,
    fn_names: HashMap<String, Fn>,

    called_fn_insts: HashMap<Fn, Vec<FnInst>>,
    called_trait_mthd_insts: HashMap<Fn, Vec<TraitMthdInst>>,
}

impl FnReg {
    pub fn inst_fn(&self, fn_: Fn, gen_args: TySlice, env_gen_args: TySlice) -> Result<FnInst, FnInstError> {
        let sig = self.sigs.get(fn_.0).unwrap();
        if sig.gen_params.len() != gen_args.len {
            return Err(FnInstError::GenArgCountMismatch {
                fn_,
                expected: sig.gen_params.len(),
                actual: gen_args.len,
            });
        }
        if sig.env_gen_params.len() != env_gen_args.len {
            return Err(FnInstError::EnvGenArgCountMismatch {
                fn_,
                expected: sig.env_gen_params.len(),
                actual: env_gen_args.len,
            });
        }
        Ok(FnInst {
            fn_,
            gen_args,
            env_gen_args,
            _private: (),
        })
    }

    pub fn register_fn(&mut self, signature: FnSig, register_name: bool) -> Result<Fn, ()> {
        let fn_ = Fn(self.sigs.len());

        if register_name {
            if self.fn_names.contains_key(&signature.name) {
                return Err(());
            }
            self.fn_names.insert(signature.name.to_string(), fn_);
        }

        self.sigs.push(signature);
        self.called_fn_insts.insert(fn_, Vec::new());
        self.called_trait_mthd_insts.insert(fn_, Vec::new());

        Ok(fn_)
    }

    pub fn get_sig(&self, fn_: Fn) -> Option<&FnSig> {
        self.sigs.get(fn_.0)
    }

    pub fn get_mut_sig(&mut self, fn_: Fn) -> Option<&mut FnSig> {
        self.sigs.get_mut(fn_.0)
    }

    pub fn get_fn_by_name(&self, name: &str) -> Option<Fn> {
        self.fn_names.get(name).cloned()
    }

    pub fn get_all_fns(&self) -> impl Iterator<Item = Fn> {
        (0..self.sigs.len()).map(Fn)
    }

    pub fn register_fn_call(&mut self, caller: Fn, fn_inst: FnInst) {
        self.called_fn_insts.entry(caller).or_default().push(fn_inst);
    }

    pub fn register_trait_mthd_call(&mut self, caller: Fn, trait_mthd_inst: TraitMthdInst) {
        self.called_trait_mthd_insts
            .entry(caller)
            .or_default()
            .push(trait_mthd_inst);
    }

    pub fn get_called_fn_insts(&self, caller: Fn) -> &Vec<FnInst> {
        self.called_fn_insts.get(&caller).unwrap()
    }

    pub fn get_called_trait_mthd_insts(&self, caller: Fn) -> &Vec<TraitMthdInst> {
        self.called_trait_mthd_insts.get(&caller).unwrap()
    }

    pub fn get_fn_name(&self, fn_: Fn) -> &str {
        self.get_sig(fn_).map(|sig| sig.name.as_str()).unwrap_or("<unknown fn>")
    }
}
