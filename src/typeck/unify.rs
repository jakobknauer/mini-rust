use crate::ctxt::ty;

impl<'ctxt, 'hlr> super::Typeck<'ctxt, 'hlr> {
    /// Unify two types, constraining any inference variables involved.
    ///
    /// TODO: collect constraints and unify in a single batch at the end, so a failed
    /// unification cannot poison the type registry.
    pub(super) fn unify(&mut self, ty1: ty::Ty, ty2: ty::Ty) -> Result<(), ()> {
        self.ctxt.tys.unify(ty1, ty2).map_err(|_| ())
    }
}
