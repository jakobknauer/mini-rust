macro_rules! assign_to_new_loc {
    ($self:ident, $init:expr) => {{
        let loc: mlr::LocId = $self.get_next_loc_id();

        let init: mlr::ValId = $init;
        let init_type: crate::ctxt::types::TypeId = $self.get_val_type(&init);
        $self.output.loc_types.insert(loc, init_type);

        let place = mlr::Place::Local(loc);
        let place: mlr::PlaceId = $self.insert_place(place)?;

        $self.insert_assign_stmt(place, init)?;
        loc
    }};
}

pub(crate) use assign_to_new_loc;
