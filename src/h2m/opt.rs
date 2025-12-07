use crate::ctxt;

/// Validate that all types in the MLR are defined,
/// and replace aliases by their canonical types.
pub fn canonicalize_types(ctxt: &mut ctxt::Ctxt) {
    ctxt.mlr.get_all_types_mut().for_each(|ty| {
        *ty = ctxt.tys.canonicalize(*ty);
    });
}
