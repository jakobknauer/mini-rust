use crate::ctxt::functions as mr_functions;

pub struct FnGenerator<'a, 'iw, 'mr> {
    gtor: &'a mut super::Generator<'iw, 'mr>,
    fn_id: mr_functions::FnId,
    builder: inkwell::builder::Builder<'iw>,
}

impl<'a, 'iw, 'mr> FnGenerator<'a, 'iw, 'mr> {
    pub fn new(gtor: &'a mut super::Generator<'iw, 'mr>, fn_id: mr_functions::FnId) -> Self {
        let builder = gtor.iw_ctxt.create_builder();
        Self { gtor, fn_id, builder }
    }

    pub fn define_function(&mut self) -> Result<(), ()> {
        self.build_entry_block()
    }

    fn build_entry_block(&mut self) -> Result<(), ()> {
        let mlr = self
            .gtor
            .mr_ctxt
            .function_registry
            .get_function_mlr(&self.fn_id)
            .ok_or(())?;
        let iw_fn = *self.gtor.functions.get(&self.fn_id).unwrap();
        let entry_block = self.gtor.iw_ctxt.append_basic_block(iw_fn, "entry");

        self.builder.position_at_end(entry_block);

        // allocate params
        for (param_index, param_loc) in mlr.param_locs.iter().enumerate() {
            let mr_type = mlr.loc_types.get(param_loc).unwrap();
            let iw_type = self.gtor.get_type_as_basic_type_enum(mr_type).unwrap();
            let param_loc = self
                .builder
                .build_alloca(iw_type, &param_loc.to_string())
                .map_err(|_| ())?;
            self.builder
                .build_store(param_loc, iw_fn.get_nth_param(param_index as u32).unwrap())
                .map_err(|_| ())?;
        }

        Ok(())
    }
}
