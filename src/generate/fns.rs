use std::collections::HashMap;

use inkwell::values::{BasicValue, BasicValueEnum, PointerValue};

use crate::{ctxt::functions as mr_functions, mlr};

pub struct FnGenerator<'a, 'iw, 'mr> {
    gtor: &'a mut super::Generator<'iw, 'mr>,
    fn_id: mr_functions::FnId,
    builder: inkwell::builder::Builder<'iw>,
    mlr: &'a mlr::Mlr,
    locs: HashMap<mlr::LocId, PointerValue<'iw>>,
}

impl<'a, 'iw, 'mr> FnGenerator<'a, 'iw, 'mr> {
    pub fn new(gtor: &'a mut super::Generator<'iw, 'mr>, fn_id: mr_functions::FnId) -> Option<Self> {
        let builder = gtor.iw_ctxt.create_builder();
        let mlr = gtor.mr_ctxt.function_registry.get_function_mlr(&fn_id)?;
        let locs = HashMap::new();

        Some(Self {
            gtor,
            fn_id,
            builder,
            mlr,
            locs,
        })
    }

    pub fn define_function(&mut self) -> Result<(), ()> {
        self.build_entry_block()?;
        self.build_function_body()?;

        Ok(())
    }

    fn get_iw_type_of_loc(&mut self, loc_id: &mlr::LocId) -> Result<inkwell::types::BasicTypeEnum<'iw>, ()> {
        let mr_type = self.mlr.loc_types.get(loc_id).ok_or(())?;
        let iw_type = self.gtor.get_type_as_basic_type_enum(mr_type).ok_or(())?;
        Ok(iw_type)
    }

    fn get_iw_type_and_address_of_loc(
        &mut self,
        loc_id: &mlr::LocId,
    ) -> Result<(inkwell::types::BasicTypeEnum<'iw>, PointerValue<'iw>), ()> {
        let iw_type = self.get_iw_type_of_loc(loc_id)?;
        let address = *self.locs.get(loc_id).ok_or(())?;
        Ok((iw_type, address))
    }

    fn build_entry_block(&mut self) -> Result<(), ()> {
        let iw_fn = *self.gtor.functions.get(&self.fn_id).unwrap();
        let entry_block = self.gtor.iw_ctxt.append_basic_block(iw_fn, "entry");

        self.builder.position_at_end(entry_block);

        // allocate params
        for (param_index, param_loc_id) in self.mlr.param_locs.iter().enumerate() {
            let iw_type = self.get_iw_type_of_loc(param_loc_id)?;
            let param_loc = self
                .builder
                .build_alloca(iw_type, &param_loc_id.to_string())
                .map_err(|_| ())?;
            self.builder
                .build_store(param_loc, iw_fn.get_nth_param(param_index as u32).unwrap())
                .map_err(|_| ())?;
            self.locs.insert(*param_loc_id, param_loc);
        }

        Ok(())
    }

    fn build_function_body(&mut self) -> Result<(), ()> {
        let iw_fn = *self.gtor.functions.get(&self.fn_id).unwrap();
        let body_block = self.gtor.iw_ctxt.append_basic_block(iw_fn, "body");

        self.builder.build_unconditional_branch(body_block).map_err(|_| ())?;
        self.builder.position_at_end(body_block);

        self.build_block(&self.mlr.body)?;

        // return mlr.body.output
        let (iw_type, output_address) = self.get_iw_type_and_address_of_loc(&self.mlr.body.output)?;
        let ret_value = self
            .builder
            .build_load(iw_type, output_address, "ret_value")
            .map_err(|_| ())?;
        self.builder.build_return(Some(&ret_value)).map_err(|_| ())?;

        Ok(())
    }

    fn build_block(&mut self, body: &mlr::Block) -> Result<(), ()> {
        for stmt in &body.statements {
            self.build_statement(stmt)?;
        }
        Ok(())
    }

    fn build_statement(&mut self, _stmt: &mlr::StmtId) -> Result<(), ()> {
        let stmt = self.mlr.statements.get(_stmt).unwrap();
        match stmt {
            mlr::Statement::Assign { loc, value } => {
                if !self.locs.contains_key(loc) {
                    let iw_type = self.get_iw_type_of_loc(loc)?;
                    let address = self.builder.build_alloca(iw_type, &loc.to_string()).map_err(|_| ())?;
                    self.locs.insert(*loc, address);
                }
                let address = *self.locs.get(loc).ok_or(())?;
                let value = self.build_expression(value).map_err(|_| ())?;
                self.builder.build_store(address, value).map_err(|_| ())?;
            }
            mlr::Statement::Return { value } => {
                let (iw_type, address) = self.get_iw_type_and_address_of_loc(value)?;
                let ret_value = self.builder.build_load(iw_type, address, "ret_value").map_err(|_| ())?;
                self.builder.build_return(Some(&ret_value)).map_err(|_| ())?;
            }
        }
        Ok(())
    }

    fn build_expression(&mut self, _expr: &mlr::ExprId) -> Result<BasicValueEnum<'iw>, ()> {
        let expr = self.mlr.expressions.get(_expr).ok_or(())?;
        match expr {
            mlr::Expression::Block(block) => {
                self.build_block(block)?;
                let (iw_type, address) = self.get_iw_type_and_address_of_loc(&block.output)?;
                let ret_value = self
                    .builder
                    .build_load(iw_type, address, "block_value")
                    .map_err(|_| ())?;
                Ok(ret_value)
            }
            _ => {
                // Simply return the integer constant 42 for now
                let int_type = self.gtor.iw_ctxt.i32_type();
                Ok(int_type.const_int(42, false).as_basic_value_enum())
            } //
              // mlr::Expression::Constant(constant) => todo!(),
              // mlr::Expression::Var(loc_id) => todo!(),
              // mlr::Expression::AddressOf(loc_id) => todo!(),
              // mlr::Expression::Call { callable, args } => todo!(),
              // mlr::Expression::Function(fn_id) => todo!(),
              // mlr::Expression::If(_) => todo!(),
              // mlr::Expression::Loop { body } => todo!(),
        }
    }
}
