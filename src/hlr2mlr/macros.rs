macro_rules! assign_to_new_loc {
    ($self:ident, $init:expr) => {{
        let loc: mlr::Loc = $self.ctxt.mlr.insert_loc();
        $self.insert_alloc_stmt(loc)?;

        let init: mlr::Val = $init;
        let init_ty: crate::ctxt::ty::Ty = $self.ctxt.mlr.get_val_ty(&init);
        $self.ctxt.mlr.set_loc_ty(loc, init_ty);

        let place = mlr::PlaceDef::Loc(loc);
        let place: mlr::Place = $self.insert_place(place)?;

        $self.insert_assign_stmt(place, init)?;
        loc
    }};
}

pub(crate) use assign_to_new_loc;
