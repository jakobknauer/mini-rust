macro_rules! assign_to_new_loc {
    ($self:ident, $init:expr) => {{
        let loc = $self.get_next_loc_id();

        let init = $init;
        let init_type = *$self.output.expr_types.get(&init).unwrap();

        $self.output.loc_types.insert(loc, init_type);
        let place = mlr::Expression::Loc(loc);
        let place = $self.insert_expr(place)?;

        let stmt = $self.insert_assign_stmt(place, init)?;
        (loc, stmt)
    }};
}

pub(crate) use assign_to_new_loc;
