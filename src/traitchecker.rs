use crate::ctxt::{
    self,
    traits::Trait,
    ty::{Obligation, Ty, TyDef},
};

pub struct ObligationCheckError {
    pub obligation: Obligation,
}

pub fn check_obligations(ctxt: &mut ctxt::Ctxt) -> Result<(), ObligationCheckError> {
    let obligations = ctxt.tys.get_all_obligations().to_vec();

    for obligation in obligations {
        if !ty_implements_trait(ctxt, obligation.ty, obligation.trait_) {
            return Err(ObligationCheckError { obligation });
        }
    }

    Ok(())
}

fn ty_implements_trait(ctxt: &mut ctxt::Ctxt, ty: Ty, trait_: Trait) -> bool {
    let ty_def = ctxt.tys.get_ty_def(ty);
    if let Some(&TyDef::GenVar(gen_var)) = ty_def
        && ctxt.tys.constraint_exists(gen_var, trait_)
    {
        return true;
    }

    ctxt.impls
        .get_impls_for_trait(trait_)
        .map(|impl_| ctxt.impls.get_impl_def(impl_))
        .filter_map(|impl_def| {
            ctxt.tys
                .try_find_instantiation(ty, impl_def.ty, &impl_def.gen_params)
                .ok()
        })
        .next()
        .is_some()
}
