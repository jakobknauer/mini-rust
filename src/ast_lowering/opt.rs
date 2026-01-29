use crate::ctxt::{self, ty::*};

/// Validate that all types in the MLR are defined,
/// and replace aliases by their canonical types.
pub fn canonicalize_types(ctxt: &mut ctxt::Ctxt) -> Result<(), ()> {
    for ty in ctxt.mlr.get_all_types_mut() {
        *ty = ctxt.tys.canonicalize(*ty);
        if !ctxt.tys.is_ty_defined(*ty) {
            return Err(());
        }
    }

    for ty_slice in ctxt.mlr.get_all_type_slices_mut() {
        *ty_slice = ctxt.tys.canonicalize_ty_slice(*ty_slice);
        let all_defined = iter_ty_slice!(ctxt.tys, *ty_slice, all(|ty| ctxt.tys.is_ty_defined(ty)));
        if !all_defined {
            return Err(());
        }
    }

    Ok(())
}
