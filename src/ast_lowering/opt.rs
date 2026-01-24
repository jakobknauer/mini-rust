use crate::ctxt;

/// Validate that all types in the MLR are defined,
/// and replace aliases by their canonical types.
pub fn canonicalize_types(ctxt: &mut ctxt::Ctxt) -> Result<(), ()> {
    for ty in ctxt.mlr.get_all_types_mut() {
        *ty = ctxt.tys.canonicalize(*ty);
        if !ctxt.tys.is_ty_defined(*ty) {
            return Err(());
        }
    }

    Ok(())
}
