macro_rules! assign_to_new_loc {
    ($self:ident, $init:expr) => {{
        let loc = $self.get_next_loc_id();
        let init = $init;
        let stmt = $self.insert_assign_stmt(loc, init)?;
        (loc, stmt)
    }};
}
