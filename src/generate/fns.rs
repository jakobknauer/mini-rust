use std::collections::HashMap;

use inkwell::{
    types::{BasicType, FunctionType},
    values::{BasicValue, BasicValueEnum, PointerValue},
};

use crate::{ctxt::functions as mr_functions, ctxt::types as mr_types, mlr};

pub struct FnGenerator<'a, 'iw, 'mr> {
    gtor: &'a mut super::Generator<'iw, 'mr>,
    fn_id: mr_functions::FnId,
    builder: inkwell::builder::Builder<'iw>,
    mlr: &'a mlr::Mlr,
    locs: HashMap<mlr::LocId, PointerValue<'iw>>,
    entry_block: Option<inkwell::basic_block::BasicBlock<'iw>>,
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
            entry_block: None,
        })
    }

    pub fn define_function(&mut self) -> Result<(), ()> {
        let entry_block = self.build_entry_block()?;
        let body_block = self.build_function_body()?;

        self.builder.position_at_end(entry_block);
        self.builder.build_unconditional_branch(body_block).map_err(|_| ())?;

        Ok(())
    }

    fn get_iw_type_of_loc(&mut self, loc_id: &mlr::LocId) -> Result<inkwell::types::BasicTypeEnum<'iw>, ()> {
        let mr_type = self.mlr.loc_types.get(loc_id).ok_or(())?;
        let iw_type = self.gtor.get_type_as_basic_type_enum(mr_type).ok_or(())?;
        Ok(iw_type)
    }

    fn get_iw_type_of_expr(&mut self, expr_id: &mlr::ExprId) -> Result<inkwell::types::BasicTypeEnum<'iw>, ()> {
        let mr_type = self.mlr.expr_types.get(expr_id).ok_or(())?;
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

    fn get_function_type_of_loc(&mut self, loc_id: &mlr::LocId) -> Option<FunctionType<'iw>> {
        let mr_type = self.mlr.loc_types.get(loc_id)?;
        let mr_type = self.gtor.mr_ctxt.type_registry.get_type_by_id(mr_type)?;
        let mr_types::Type::Function {
            return_type,
            param_types,
        } = mr_type
        else {
            return None;
        };
        let return_type = self.gtor.get_type_as_basic_type_enum(return_type)?;
        let param_types: Vec<_> = param_types
            .iter()
            .map(|param| self.gtor.get_type_as_basic_metadata_type_enum(param))
            .collect::<Option<_>>()?;

        Some(return_type.fn_type(&param_types, false))
    }

    fn build_load_from_loc(&mut self, loc_id: &mlr::LocId, name: &str) -> Result<BasicValueEnum<'iw>, ()> {
        let (iw_type, address) = self.get_iw_type_and_address_of_loc(loc_id)?;
        self.builder.build_load(iw_type, address, name).map_err(|_| ())
    }

    fn build_alloca_for_loc(&mut self, loc_id: &mlr::LocId) -> Result<PointerValue<'iw>, ()> {
        let iw_type = self.get_iw_type_of_loc(loc_id)?;
        let name = loc_id.to_string();
        let address = self.build_alloca(iw_type, &name)?;
        self.locs.insert(*loc_id, address);
        Ok(address)
    }

    fn build_alloca(
        &mut self,
        iw_type: inkwell::types::BasicTypeEnum<'iw>,
        name: &str,
    ) -> Result<PointerValue<'iw>, ()> {
        // Remember current block to restore later
        let current_block = self.builder.get_insert_block().ok_or(())?;
        // Position builder at the entry block to ensure allocations are at the start
        let entry_block = self.entry_block.ok_or(())?;
        self.builder.position_at_end(entry_block);
        // Allocate
        let address = self.builder.build_alloca(iw_type, name).map_err(|_| ())?;
        // Restore builder position
        self.builder.position_at_end(current_block);

        Ok(address)
    }

    fn build_entry_block(&mut self) -> Result<inkwell::basic_block::BasicBlock<'iw>, ()> {
        let iw_fn = *self.gtor.functions.get(&self.fn_id).unwrap();

        let entry_block = self.gtor.iw_ctxt.append_basic_block(iw_fn, "entry");
        self.entry_block = Some(entry_block);
        self.builder.position_at_end(entry_block);

        for (param_index, param_loc_id) in self.mlr.param_locs.iter().enumerate() {
            let param_address = self.build_alloca_for_loc(param_loc_id)?;
            self.builder
                .build_store(param_address, iw_fn.get_nth_param(param_index as u32).unwrap())
                .map_err(|_| ())?;
        }

        Ok(entry_block)
    }

    fn build_function_body(&mut self) -> Result<inkwell::basic_block::BasicBlock<'iw>, ()> {
        let iw_fn = *self.gtor.functions.get(&self.fn_id).unwrap();

        let body_block = self.gtor.iw_ctxt.append_basic_block(iw_fn, "body");
        self.builder.position_at_end(body_block);
        let output = self.build_block(&self.mlr.body)?;

        self.builder.build_return(Some(&output)).map_err(|_| ())?;

        Ok(body_block)
    }

    fn build_statement(&mut self, _stmt: &mlr::StmtId) -> Result<(), ()> {
        let stmt = self.mlr.statements.get(_stmt).unwrap();
        match stmt {
            mlr::Statement::Assign { loc, value } => {
                if !self.locs.contains_key(loc) {
                    self.build_alloca_for_loc(loc)?;
                }
                let address = *self.locs.get(loc).ok_or(())?;
                let value = self.build_expression(value).map_err(|_| ())?;
                self.builder.build_store(address, value).map_err(|_| ())?;
            }
            mlr::Statement::Return { value } => {
                let ret_value = self.build_load_from_loc(value, "ret_value")?;
                self.builder.build_return(Some(&ret_value)).map_err(|_| ())?;
            }
        }
        Ok(())
    }

    fn build_expression(&mut self, expr: &mlr::ExprId) -> Result<BasicValueEnum<'iw>, ()> {
        match self.mlr.expressions.get(expr).ok_or(())? {
            mlr::Expression::Block(block) => self.build_block(block),
            mlr::Expression::Constant(constant) => self.build_constant(constant),
            mlr::Expression::Var(loc_id) => self.build_var(loc_id),
            mlr::Expression::Function(fn_id) => self.build_global_function(fn_id),
            mlr::Expression::Call { callable, args } => self.build_call(callable, args),
            mlr::Expression::If(if_) => self.build_if(if_, expr),

            // mlr::Expression::AddressOf(loc_id) => todo!(),
            // mlr::Expression::Loop { body } => todo!(),
            _ => {
                // Simply return the integer constant 42 for now
                let int_type = self.gtor.iw_ctxt.i32_type();
                Ok(int_type.const_int(42, false).as_basic_value_enum())
            }
        }
    }

    fn build_block(&mut self, body: &mlr::Block) -> Result<BasicValueEnum<'iw>, ()> {
        for stmt in &body.statements {
            self.build_statement(stmt)?;
        }
        self.build_load_from_loc(&body.output, "block_output")
    }

    fn build_constant(&mut self, constant: &mlr::Constant) -> Result<BasicValueEnum<'iw>, ()> {
        match constant {
            mlr::Constant::Int(i) => {
                let int_type = self.gtor.iw_ctxt.i32_type();
                Ok(int_type.const_int(*i as u64, false).as_basic_value_enum())
            }
            mlr::Constant::Bool(b) => {
                let bool_type = self.gtor.iw_ctxt.bool_type();
                Ok(bool_type.const_int(*b as u64, false).as_basic_value_enum())
            }
            mlr::Constant::Unit => {
                let unit_type = self.gtor.iw_ctxt.custom_width_int_type(0);
                Ok(unit_type.const_int(0, false).as_basic_value_enum())
            }
        }
    }

    fn build_var(&mut self, loc_id: &mlr::LocId) -> Result<BasicValueEnum<'iw>, ()> {
        self.build_load_from_loc(loc_id, "loaded_var")
    }

    fn build_global_function(&mut self, fn_id: &mr_functions::FnId) -> Result<BasicValueEnum<'iw>, ()> {
        let iw_fn = *self.gtor.functions.get(fn_id).ok_or(())?;
        Ok(iw_fn.as_global_value().as_pointer_value().as_basic_value_enum())
    }

    fn build_call(&mut self, callable: &mlr::LocId, args: &[mlr::LocId]) -> Result<BasicValueEnum<'iw>, ()> {
        let fn_ptr = self
            .build_load_from_loc(callable, "loaded_callable")?
            .into_pointer_value();
        let fn_type = self.get_function_type_of_loc(callable).ok_or(())?;

        let arg_values = args
            .iter()
            .map(|arg_loc| self.build_load_from_loc(arg_loc, "arg_value").unwrap().into())
            .collect::<Vec<_>>();

        let call_site = self
            .builder
            .build_indirect_call(fn_type, fn_ptr, &arg_values, "call_site")
            .map_err(|_| ())?;

        match call_site.try_as_basic_value().left() {
            Some(basic_value) => Ok(basic_value),
            None => Err(()),
        }
    }

    fn build_if(&mut self, if_: &mlr::If, expr_id: &mlr::ExprId) -> Result<BasicValueEnum<'iw>, ()> {
        // Build condition
        let cond_value = self
            .build_load_from_loc(&if_.condition, "if_condition")?
            .into_int_value();

        // Create blocks for then, else, and merge
        let iw_fn = *self.gtor.functions.get(&self.fn_id).unwrap();
        let then_block = self.gtor.iw_ctxt.append_basic_block(iw_fn, "then");
        let else_block = self.gtor.iw_ctxt.append_basic_block(iw_fn, "else");
        let merge_block = self.gtor.iw_ctxt.append_basic_block(iw_fn, "if_merge");

        // Allocate space for the result
        let result_type = self.get_iw_type_of_expr(expr_id)?;
        let result_address = self.build_alloca(result_type, "if_result").map_err(|_| ())?;

        // Build conditional branch
        self.builder
            .build_conditional_branch(cond_value, then_block, else_block)
            .map_err(|_| ())?;

        // Build then block
        self.builder.position_at_end(then_block);
        let then_value = self.build_block(&if_.then_block)?;
        self.builder.build_store(result_address, then_value).map_err(|_| ())?;
        self.builder.build_unconditional_branch(merge_block).map_err(|_| ())?;

        // Build else block
        self.builder.position_at_end(else_block);
        let else_value = self.build_block(&if_.else_block)?;
        self.builder.build_store(result_address, else_value).map_err(|_| ())?;
        self.builder.build_unconditional_branch(merge_block).map_err(|_| ())?;

        // Build merge block
        self.builder.position_at_end(merge_block);
        let result_value = self
            .builder
            .build_load(result_type, result_address, "if_result_value")
            .map_err(|_| ())?;
        Ok(result_value)
    }
}
