macro_rules! assign_to_new_loc {
    ($self:ident, $init:expr) => {{
        let loc: mlr::LocId = $self.get_next_loc_id();
        $self.insert_alloc_stmt(loc)?;

        let init: mlr::ValId = $init;
        let init_ty: crate::ctxt::ty::Ty = $self.get_val_ty(&init);
        $self.output.loc_tys.insert(loc, init_ty);

        let place = mlr::Place::Local(loc);
        let place: mlr::PlaceId = $self.insert_place(place)?;

        $self.insert_assign_stmt(place, init)?;
        loc
    }};
}

pub(crate) use assign_to_new_loc;
