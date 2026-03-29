use std::cell::RefCell;
use std::collections::HashMap;

use crate::ctxt::fns::{Fn, FnDecl, FnId, FnInst, TraitMthdInst};

pub struct FnReg<'fns> {
    arena: &'fns bumpalo::Bump,
    next_id: RefCell<usize>,
    fn_names: RefCell<HashMap<String, Fn<'fns>>>,
    called_fn_insts: RefCell<HashMap<Fn<'fns>, Vec<FnInst<'fns>>>>,
    called_trait_mthd_insts: RefCell<HashMap<Fn<'fns>, Vec<TraitMthdInst<'fns>>>>,
}

impl<'fns> FnReg<'fns> {
    pub fn new(arena: &'fns bumpalo::Bump) -> Self {
        Self {
            arena,
            next_id: RefCell::default(),
            fn_names: RefCell::default(),
            called_fn_insts: RefCell::default(),
            called_trait_mthd_insts: RefCell::default(),
        }
    }

    // TODO create the FnDecl inside the function?
    pub fn register_fn(&self, mut decl: FnDecl<'fns>, register_name: bool) -> Result<Fn<'fns>, ()> {
        if register_name && self.fn_names.borrow().contains_key(&decl.name) {
            return Err(());
        }

        let id = {
            let mut next = self.next_id.borrow_mut();
            let id = FnId(*next);
            *next += 1;
            id
        };
        decl.id = id;
        let fn_: Fn<'fns> = self.arena.alloc(decl);

        if register_name {
            self.fn_names.borrow_mut().insert(fn_.name.to_string(), fn_);
        }

        self.called_fn_insts.borrow_mut().insert(fn_, Vec::new());
        self.called_trait_mthd_insts.borrow_mut().insert(fn_, Vec::new());

        Ok(fn_)
    }

    pub fn get_fn_by_name(&self, name: &str) -> Option<Fn<'fns>> {
        self.fn_names.borrow().get(name).copied()
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
