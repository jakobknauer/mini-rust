use crate::ctxt::{
    fns::Fn,
    impls::{Impl, ImplDef},
    ty::Ty,
};

#[derive(Default)]
pub struct ImplReg {
    impls: Vec<ImplDef>,
}

impl ImplReg {
    pub fn register_impl(&mut self, ty: Ty) -> Impl {
        let impl_ = Impl(self.impls.len());
        let impl_def = ImplDef {
            ty,
            methods: Vec::new(),
        };
        self.impls.push(impl_def);
        impl_
    }

    pub fn register_method(&mut self, impl_: Impl, method: Fn) {
        self.impls[impl_.0].methods.push(method);
    }

    pub fn get_impls(&self) -> &Vec<ImplDef> {
        &self.impls
    }
}
