macro_rules! assign_to_new_loc {
    ($self:ident, $init:expr) => {{
        let loc: mlr::LocId = $self.get_next_loc_id();

        let init: mlr::ValId = $init;
        $self
            .output
            .loc_types
            .insert(loc, *$self.output.val_types.get(&init).unwrap());

        let place = mlr::Place::Local(loc);
        let place: mlr::PlaceId = $self.insert_place(place)?;

        let stmt: mlr::StmtId = $self.insert_assign_stmt(place, init)?;
        (loc, stmt)
    }};
}

pub(crate) use assign_to_new_loc;
