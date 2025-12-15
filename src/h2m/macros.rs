macro_rules! assign_to_fresh_alloc {
    ($self:ident, $init:expr) => {{
        let place = $self.builder.insert_fresh_alloc()?;
        let init: mlr::Val = $init;
        $self.builder.insert_assign_stmt(place, init)?;
        place
    }};
}

pub(crate) use assign_to_fresh_alloc;
