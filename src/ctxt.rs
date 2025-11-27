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
