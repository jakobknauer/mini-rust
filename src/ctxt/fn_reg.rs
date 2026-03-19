use std::cell::RefCell;
use std::collections::HashMap;

use crate::ctxt::{
    fns::{Fn, FnInst, FnInstError, FnSig, TraitMthdInst},
    ty::TySlice,
};

pub struct FnReg<'fns> {
    arena: &'fns bumpalo::Bump,
    sigs: RefCell<Vec<Fn<'fns>>>,
    fn_names: RefCell<HashMap<String, Fn<'fns>>>,
    called_fn_insts: RefCell<HashMap<Fn<'fns>, Vec<FnInst<'fns>>>>,
    called_trait_mthd_insts: RefCell<HashMap<Fn<'fns>, Vec<TraitMthdInst<'fns>>>>,
}

impl<'fns> FnReg<'fns> {
    pub fn new(arena: &'fns bumpalo::Bump) -> Self {
        Self {
            arena,
            sigs: RefCell::default(),
            fn_names: RefCell::default(),
            called_fn_insts: RefCell::default(),
            called_trait_mthd_insts: RefCell::default(),
        }
    }

    pub fn inst_fn(
        &self,
        fn_: Fn<'fns>,
        gen_args: TySlice<'fns>,
        env_gen_args: TySlice<'fns>,
    ) -> Result<FnInst<'fns>, FnInstError> {
        if fn_.gen_params.len() != gen_args.len() {
            return Err(FnInstError::GenArgCountMismatch {
                expected: fn_.gen_params.len(),
                actual: gen_args.len(),
            });
        }
        if fn_.env_gen_params.len() != env_gen_args.len() {
            return Err(FnInstError::EnvGenArgCountMismatch {
                expected: fn_.env_gen_params.len(),
                actual: env_gen_args.len(),
            });
        }
        Ok(FnInst {
            fn_,
            gen_args,
            env_gen_args,
            _private: (),
            _phantom: std::marker::PhantomData,
        })
    }

    pub fn register_fn(&self, signature: FnSig<'fns>, register_name: bool) -> Result<Fn<'fns>, ()> {
        if register_name && self.fn_names.borrow().contains_key(&signature.name) {
            return Err(());
        }

        let fn_: Fn<'fns> = self.arena.alloc(signature);

        if register_name {
            self.fn_names.borrow_mut().insert(fn_.name.to_string(), fn_);
        }

        self.called_fn_insts.borrow_mut().insert(fn_, Vec::new());
        self.called_trait_mthd_insts.borrow_mut().insert(fn_, Vec::new());
        self.sigs.borrow_mut().push(fn_);

        Ok(fn_)
    }

    pub fn get_fn_by_name(&self, name: &str) -> Option<Fn<'fns>> {
        self.fn_names.borrow().get(name).copied()
    }

    pub fn get_all_fns(&self) -> impl Iterator<Item = Fn<'fns>> {
        self.sigs.borrow().clone().into_iter()
    }

    pub fn register_fn_call(&self, caller: Fn<'fns>, fn_inst: FnInst<'fns>) {
        self.called_fn_insts
            .borrow_mut()
            .entry(caller)
            .or_default()
            .push(fn_inst);
    }

    pub fn register_trait_mthd_call(&self, caller: Fn<'fns>, trait_mthd_inst: TraitMthdInst<'fns>) {
        self.called_trait_mthd_insts
            .borrow_mut()
            .entry(caller)
            .or_default()
            .push(trait_mthd_inst);
    }

    pub fn get_called_fn_insts(&self, caller: Fn<'fns>) -> Vec<FnInst<'fns>> {
        self.called_fn_insts.borrow().get(&caller).cloned().unwrap_or_default()
    }

    pub fn get_called_trait_mthd_insts(&self, caller: Fn<'fns>) -> Vec<TraitMthdInst<'fns>> {
        self.called_trait_mthd_insts
            .borrow()
            .get(&caller)
            .cloned()
            .unwrap_or_default()
    }
}
