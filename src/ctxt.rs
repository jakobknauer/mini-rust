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

    pub fn inst_fn_equal(&self, a: &fns::InstantiatedFn, b: &fns::InstantiatedFn) -> bool {
        a.fn_ == b.fn_
            && a.gen_args.len() == b.gen_args.len()
            && a.gen_args
                .iter()
                .zip(b.gen_args.iter())
                .all(|(a_ty, b_ty)| self.tys.tys_equal(a_ty, b_ty))
    }
}
