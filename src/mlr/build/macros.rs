macro_rules! assign_to_new_loc {
    ($self:ident, $init:expr) => {{
        let loc = $self.get_next_loc_id();
        let place = mlr::Place::Local(loc);
        let place = $self.insert_place(place)?;
        let init = $init;
        let stmt = $self.insert_assign_stmt(place, init)?;
        (loc, stmt)
    }};
}

pub(crate) use assign_to_new_loc;
