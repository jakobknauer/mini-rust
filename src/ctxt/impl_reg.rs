use std::collections::HashMap;

use crate::ctxt::{
    fns::Fn,
    impls::{Impl, ImplDef},
    traits::Trait,
    ty::{GenVar, Ty},
};

#[derive(Default)]
pub struct ImplReg {
    impls: Vec<ImplDef>,
}

impl ImplReg {
    pub fn register_impl(&mut self, ty: Ty, gen_params: Vec<GenVar>, trait_: Option<Trait>) -> Impl {
        let impl_ = Impl(self.impls.len());
        let impl_def = ImplDef {
            gen_params,
            ty,
            methods: Vec::new(),
            methods_by_name: HashMap::new(),
            trait_,
        };
        self.impls.push(impl_def);
        impl_
    }

    pub fn register_method(&mut self, impl_: Impl, method: Fn, name: &str) {
        let impl_ = self.impls.get_mut(impl_.0).unwrap();
        impl_.methods.push(method);
        impl_.methods_by_name.insert(name.to_string(), method);
    }

    pub fn get_all_impls(&self) -> impl IntoIterator<Item = (Impl, &ImplDef)> {
        self.impls
            .iter()
            .enumerate()
            .map(|(impl_, impl_def)| (Impl(impl_), impl_def))
    }

    pub fn get_impl_def(&self, impl_: Impl) -> Option<&ImplDef> {
        self.impls.get(impl_.0)
    }
}
