mod fn_reg;
pub mod fns;
mod type_registry;
pub mod types;

pub use fn_reg::FnReg;
pub use type_registry::TypeRegistry;

pub struct Ctxt {
    pub types: TypeRegistry,
    pub fns: FnReg,
}

impl Ctxt {
    pub fn new() -> Self {
        Self {
            types: TypeRegistry::new(),
            fns: FnReg::new(),
        }
    }
}
