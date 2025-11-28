use crate::{
    ctxt::{self, fns, ty},
    mlr,
};

/// Validate that all types in the MLR are defined,
/// and replace aliases by their canonical types.
pub fn canonicalize_types(mlr: &mut mlr::Mlr, ctxt: &ctxt::Ctxt) {
    collect_types_in_mlr_mut(mlr).for_each(|ty| {
        *ty = ctxt.tys.canonicalize(ty);
    });
}

fn collect_types_in_mlr_mut(mlr: &mut mlr::Mlr) -> impl Iterator<Item = &mut ty::Ty> {
    mlr.loc_tys
        .values_mut()
        .chain(mlr.val_tys.values_mut())
        .chain(mlr.place_tys.values_mut())
        .chain(mlr.op_tys.values_mut())
        .chain(mlr.vals.values_mut().filter_map(|val_def| match val_def {
            mlr::ValDef::Empty { ty } => Some(ty),
            _ => None,
        }))
        .chain(
            mlr.ops
                .values_mut()
                .filter_map(|op_def| match op_def {
                    mlr::OpDef::Fn(fns::FnSpecialization { gen_args, .. }) => Some(gen_args),
                    _ => None,
                })
                .flatten(),
        )
}
