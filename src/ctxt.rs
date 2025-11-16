mod fn_reg;
pub mod fns;
pub mod ty;
mod ty_reg;

pub use fn_reg::FnReg;
pub use ty_reg::TyReg;

pub struct Ctxt {
    pub tys: TyReg,
    pub fns: FnReg,
}

impl Ctxt {
    pub fn new() -> Self {
        Self {
            tys: TyReg::new(),
            fns: FnReg::new(),
        }
    }
}
