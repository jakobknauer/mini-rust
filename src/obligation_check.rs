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
                let ty_def = ctxt.tys.get_ty_def(ty).unwrap();
                match ty_def {
                    &ctxt::ty::TyDef::Fn {
                        ref param_tys,
                        return_ty,
                        var_args,
                    } => {
                        let fulfilled = !var_args
                            && expected_param_tys.len() == param_tys.len()
                            && expected_param_tys
                                .iter()
                                .zip(param_tys)
                                .all(|(&arg, &param)| ctxt.tys.tys_eq(arg, param))
                            && ctxt.tys.tys_eq(expected_return_ty, return_ty);
                        if !fulfilled {
                            return Err(ObligationCheckError { obligation });
                        }
                    }
                    _ => {
                        return Err(ObligationCheckError { obligation });
                    }
                }
            }
        }
    }

    Ok(())
}
