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

    pub fn get_inst_fn_name(&self, inst_fn: &fns::InstantiatedFn) -> String {
        let signature = self.fns.get_sig(&inst_fn.fn_).unwrap();
        if signature.gen_params.is_empty() {
            signature.name.to_string()
        } else {
            format!(
                "{}::<{}>",
                signature.name,
                inst_fn
                    .gen_args
                    .iter()
                    .map(|ty| self.tys.get_string_rep(ty))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
    }
}
