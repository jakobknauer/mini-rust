use crate::ctxt::{self, ty::Obligation};

pub struct ObligationCheckError {
    pub obligation: Obligation,
}

pub fn check_obligations(ctxt: &mut ctxt::Ctxt) -> Result<(), ObligationCheckError> {
    let obligations = ctxt.tys.get_all_obligations().to_vec();

    for obligation in obligations {
        match obligation {
            Obligation::ImplementsTrait { ty, trait_, .. } => {
                if !ctxt.ty_implements_trait(ty, trait_) {
                    return Err(ObligationCheckError { obligation });
                }
            }
            Obligation::Callable {
                ty,
                param_tys: ref expected_param_tys,
                return_ty: expected_return_ty,
            } => {
                if let Some((param_tys, return_ty, _)) = ctxt.ty_is_callable(ty)
                    && expected_param_tys.len() == param_tys.len()
                    && expected_param_tys
                        .iter()
                        .zip(param_tys.iter())
                        .all(|(a, b)| ctxt.tys.tys_eq(*a, *b))
                    && ctxt.tys.tys_eq(expected_return_ty, return_ty)
                {
                    continue;
                } else {
                    return Err(ObligationCheckError { obligation });
                }
            }
        }
    }

    Ok(())
}
