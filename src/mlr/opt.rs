use crate::{
    ctxt::{Ctxt, ty::TyDef},
    mlr::Mlr,
};

/// Validate that all types in the MLR are defined,
/// and replace aliases by their canonical types.
pub fn canonicalize_types(mlr: &mut Mlr, ctxt: &Ctxt) -> Result<(), ()> {
    let all_types = mlr
        .loc_tys
        .values_mut()
        .chain(mlr.val_tys.values_mut())
        .chain(mlr.place_tys.values_mut())
        .chain(mlr.op_tys.values_mut());

    for ty in all_types {
        *ty = ctxt.tys.canonicalize(ty);
        let ty_def = ctxt.tys.get_ty_def(ty).ok_or(())?;
        if let TyDef::Undef = ty_def {
            return Err(());
        }
    }

    Ok(())
}
