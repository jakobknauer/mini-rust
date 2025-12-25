use crate::ctxt::{self, ty};

pub struct ObligationCheckError {
    pub obligation: ty::Obligation,
}

pub fn check_obligations(ctxt: &mut ctxt::Ctxt) -> Result<(), ObligationCheckError> {
    let obligations = ctxt.tys.get_all_obligations().to_vec();

    for obligation in obligations {
        if !ctxt.ty_implements_trait(obligation.ty, obligation.trait_) {
            return Err(ObligationCheckError { obligation });
        }
    }

    Ok(())
}
