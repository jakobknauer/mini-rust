use std::cell::RefCell;
use std::collections::HashMap;

use crate::ctxt::{
    fns::{Fn, FnInst, FnInstError, FnSig, TraitMthdInst},
    ty::{Ty, TySlice},
};

pub struct FnReg<'fns> {
    arena: &'fns bumpalo::Bump,
    sigs: RefCell<Vec<&'fns FnSig<'fns>>>,
    fn_names: RefCell<HashMap<String, Fn>>,
    called_fn_insts: RefCell<HashMap<Fn, Vec<FnInst<'fns>>>>,
    called_trait_mthd_insts: RefCell<HashMap<Fn, Vec<TraitMthdInst<'fns>>>>,
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
        fn_: Fn,
        gen_args: TySlice<'fns>,
        env_gen_args: TySlice<'fns>,
    ) -> Result<FnInst<'fns>, FnInstError> {
        let sig = self.get_sig(fn_);
        if sig.gen_params.len() != gen_args.len() {
            return Err(FnInstError::GenArgCountMismatch {
                fn_,
                expected: sig.gen_params.len(),
                actual: gen_args.len(),
            });
        }
        if sig.env_gen_params.len() != env_gen_args.len() {
            return Err(FnInstError::EnvGenArgCountMismatch {
                fn_,
                expected: sig.env_gen_params.len(),
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

    pub fn register_fn(&self, signature: FnSig<'fns>, register_name: bool) -> Result<Fn, ()> {
        let fn_ = Fn(self.sigs.borrow().len());

        if register_name {
            if self.fn_names.borrow().contains_key(&signature.name) {
                return Err(());
            }
            self.fn_names.borrow_mut().insert(signature.name.to_string(), fn_);
        }

        self.called_fn_insts.borrow_mut().insert(fn_, Vec::new());
        self.called_trait_mthd_insts.borrow_mut().insert(fn_, Vec::new());
        self.sigs.borrow_mut().push(self.arena.alloc(signature));

        Ok(fn_)
    }

    pub fn get_sig(&self, fn_: Fn) -> &'fns FnSig<'fns> {
        self.sigs.borrow()[fn_.0]
    }

    pub fn update_sig_types(&self, fn_: Fn, return_ty: Ty<'fns>, param_tys: &[Ty<'fns>]) {
        let old_sig = self.sigs.borrow()[fn_.0];
        let mut new_sig = old_sig.clone();
        new_sig.return_ty = return_ty;
        for (param, &ty) in new_sig.params.iter_mut().zip(param_tys) {
            param.ty = ty;
        }
        self.sigs.borrow_mut()[fn_.0] = self.arena.alloc(new_sig);
    }

    pub fn get_fn_by_name(&self, name: &str) -> Option<Fn> {
        self.fn_names.borrow().get(name).cloned()
    }

    pub fn get_all_fns(&self) -> impl Iterator<Item = Fn> {
        (0..self.sigs.borrow().len()).map(Fn)
    }

    pub fn register_fn_call(&self, caller: Fn, fn_inst: FnInst<'fns>) {
        self.called_fn_insts
            .borrow_mut()
            .entry(caller)
            .or_default()
            .push(fn_inst);
    }

    pub fn register_trait_mthd_call(&self, caller: Fn, trait_mthd_inst: TraitMthdInst<'fns>) {
        self.called_trait_mthd_insts
            .borrow_mut()
            .entry(caller)
            .or_default()
            .push(trait_mthd_inst);
    }

    pub fn get_called_fn_insts(&self, caller: Fn) -> Vec<FnInst<'fns>> {
        self.called_fn_insts.borrow().get(&caller).cloned().unwrap_or_default()
    }

    pub fn get_called_trait_mthd_insts(&self, caller: Fn) -> Vec<TraitMthdInst<'fns>> {
        self.called_trait_mthd_insts
            .borrow()
            .get(&caller)
            .cloned()
            .unwrap_or_default()
    }

    pub fn get_fn_name(&self, fn_: Fn) -> &'fns str {
        self.get_sig(fn_).name.as_str()
    }
}
