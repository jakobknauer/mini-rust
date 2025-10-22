use std::collections::{HashMap, VecDeque};

use inkwell::{
    basic_block::BasicBlock,
    builder::{Builder, BuilderError},
    types::{BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
};

use crate::{ctxt::functions as mr_functions, ctxt::types as mr_types, mlr};

pub struct FnGenerator<'a, 'iw, 'mr> {
    gtor: &'a mut super::Generator<'iw, 'mr>,
    fn_id: mr_functions::FnId,
    iw_fn: FunctionValue<'iw>,
    builder: Builder<'iw>,
    mlr: &'a mlr::Mlr,
    locs: HashMap<mlr::LocId, PointerValue<'iw>>,
    entry_block: Option<BasicBlock<'iw>>,
    after_loop_blocks: VecDeque<BasicBlock<'iw>>,
}

#[derive(Debug)]
pub struct FnGeneratorError;

impl From<BuilderError> for FnGeneratorError {
    fn from(_: BuilderError) -> Self {
        FnGeneratorError
    }
}

pub type FnGeneratorResult<T> = Result<T, FnGeneratorError>;

impl<'a, 'iw, 'mr> FnGenerator<'a, 'iw, 'mr> {
    pub fn new(gtor: &'a mut super::Generator<'iw, 'mr>, fn_id: mr_functions::FnId) -> Option<Self> {
        let builder = gtor.iw_ctxt.create_builder();
        let mlr = gtor.mr_ctxt.function_registry.get_function_mlr(&fn_id)?;
        let locs = HashMap::new();
        let iw_fn = *gtor.functions.get(&fn_id)?;
        let after_loop_blocks = VecDeque::new();

        Some(Self {
            gtor,
            fn_id,
            iw_fn,
            builder,
            mlr,
            locs,
            entry_block: None,
            after_loop_blocks,
        })
    }

    pub fn define_function(&mut self) -> FnGeneratorResult<()> {
        let entry_block = self.build_entry_block()?;
        let body_block = self.build_function_body()?;

        self.builder.position_at_end(entry_block);
        self.builder.build_unconditional_branch(body_block)?;

        Ok(())
    }

    fn get_iw_type_of_loc(&mut self, loc_id: &mlr::LocId) -> FnGeneratorResult<BasicTypeEnum<'iw>> {
        let mr_type = self.mlr.loc_types.get(loc_id).ok_or(FnGeneratorError)?;
        let iw_type = self.gtor.get_type_as_basic_type_enum(mr_type).ok_or(FnGeneratorError)?;
        Ok(iw_type)
    }

    fn get_iw_type_of_expr(&mut self, expr_id: &mlr::ExprId) -> FnGeneratorResult<BasicTypeEnum<'iw>> {
        let mr_type = self.mlr.expr_types.get(expr_id).ok_or(FnGeneratorError)?;
        let iw_type = self.gtor.get_type_as_basic_type_enum(mr_type).ok_or(FnGeneratorError)?;
        Ok(iw_type)
    }

    fn get_iw_type_and_address_of_loc(
        &mut self,
        loc_id: &mlr::LocId,
    ) -> FnGeneratorResult<(BasicTypeEnum<'iw>, PointerValue<'iw>)> {
        let iw_type = self.get_iw_type_of_loc(loc_id)?;
        let address = *self.locs.get(loc_id).ok_or(FnGeneratorError)?;
        Ok((iw_type, address))
    }

    fn get_function_type_of_loc(&mut self, loc_id: &mlr::LocId) -> FnGeneratorResult<FunctionType<'iw>> {
        let mr_type = self.mlr.loc_types.get(loc_id).ok_or(FnGeneratorError)?;
        let mr_type = self
            .gtor
            .mr_ctxt
            .type_registry
            .get_type_by_id(mr_type)
            .ok_or(FnGeneratorError)?;

        let mr_types::Type::Function {
            return_type,
            param_types,
        } = mr_type
        else {
            return Err(FnGeneratorError);
        };

        let return_type = self
            .gtor
            .get_type_as_basic_type_enum(return_type)
            .ok_or(FnGeneratorError)?;

        let param_types: Vec<_> = param_types
            .iter()
            .map(|param| {
                self.gtor
                    .get_type_as_basic_metadata_type_enum(param)
                    .ok_or(FnGeneratorError)
            })
            .collect::<FnGeneratorResult<_>>()?;

        Ok(return_type.fn_type(&param_types, false))
    }

    fn build_load_from_loc(&mut self, loc_id: &mlr::LocId, name: &str) -> FnGeneratorResult<BasicValueEnum<'iw>> {
        let (iw_type, address) = self.get_iw_type_and_address_of_loc(loc_id)?;
        let value = self.builder.build_load(iw_type, address, name)?;
        Ok(value)
    }

    fn build_alloca_for_loc(&mut self, loc_id: &mlr::LocId) -> FnGeneratorResult<PointerValue<'iw>> {
        let iw_type = self.get_iw_type_of_loc(loc_id)?;
        let name = loc_id.to_string();
        let address = self.build_alloca(iw_type, &name)?;
        self.locs.insert(*loc_id, address);
        Ok(address)
    }

    fn build_alloca(&mut self, iw_type: BasicTypeEnum<'iw>, name: &str) -> FnGeneratorResult<PointerValue<'iw>> {
        // Remember current block to restore later
        let current_block = self.builder.get_insert_block().ok_or(FnGeneratorError)?;
        // Position builder at the entry block to ensure allocations are at the start
        let entry_block = self.entry_block.ok_or(FnGeneratorError)?;
        self.builder.position_at_end(entry_block);
        // Allocate
        let address = self.builder.build_alloca(iw_type, name)?;
        // Restore builder position
        self.builder.position_at_end(current_block);

        Ok(address)
    }

    fn build_unit_value(&mut self) -> FnGeneratorResult<BasicValueEnum<'iw>> {
        let mr_unit_type = self
            .gtor
            .mr_ctxt
            .type_registry
            .get_primitive_type_id(mr_types::PrimitiveType::Unit)
            .ok_or(FnGeneratorError)?;
        let iw_type = self.gtor.get_or_define_type(&mr_unit_type).ok_or(FnGeneratorError)?;
        let iw_type: StructType = iw_type.try_into().map_err(|_| FnGeneratorError)?;
        Ok(iw_type.const_named_struct(&[]).as_basic_value_enum())
    }

    fn build_entry_block(&mut self) -> FnGeneratorResult<BasicBlock<'iw>> {
        let entry_block = self.gtor.iw_ctxt.append_basic_block(self.iw_fn, "entry");
        self.entry_block = Some(entry_block);
        self.builder.position_at_end(entry_block);

        for (param_index, param_loc_id) in self.mlr.param_locs.iter().enumerate() {
            let param_address = self.build_alloca_for_loc(param_loc_id)?;
            self.builder
                .build_store(param_address, self.iw_fn.get_nth_param(param_index as u32).unwrap())?;
        }

        Ok(entry_block)
    }

    fn build_function_body(&mut self) -> FnGeneratorResult<BasicBlock<'iw>> {
        let body_block = self.gtor.iw_ctxt.append_basic_block(self.iw_fn, "body");
        self.builder.position_at_end(body_block);
        let output = self.build_block(&self.mlr.body)?;

        self.builder.build_return(Some(&output))?;

        Ok(body_block)
    }

    fn build_statement(&mut self, _stmt: &mlr::StmtId) -> FnGeneratorResult<()> {
        let stmt = self.mlr.statements.get(_stmt).unwrap();
        match stmt {
            mlr::Statement::Assign { loc, value } => {
                if !self.locs.contains_key(loc) {
                    self.build_alloca_for_loc(loc)?;
                }
                let address = *self.locs.get(loc).ok_or(FnGeneratorError)?;
                let value = self.build_expression(value)?;
                self.builder.build_store(address, value)?;
            }
            mlr::Statement::Return { value } => {
                let ret_value = self.build_load_from_loc(value, "ret_value")?;
                self.builder.build_return(Some(&ret_value))?;
            }
            mlr::Statement::Break => {
                let after_loop_block = self.after_loop_blocks.back().ok_or(FnGeneratorError)?;
                self.builder.build_unconditional_branch(*after_loop_block)?;
            }
        }
        Ok(())
    }

    fn build_expression(&mut self, expr: &mlr::ExprId) -> FnGeneratorResult<BasicValueEnum<'iw>> {
        match self.mlr.expressions.get(expr).ok_or(FnGeneratorError)? {
            mlr::Expression::Block(block) => self.build_block(block),
            mlr::Expression::Constant(constant) => self.build_constant(constant),
            mlr::Expression::Var(loc_id) => self.build_var(loc_id),
            mlr::Expression::Function(fn_id) => self.build_global_function(fn_id),
            mlr::Expression::Call { callable, args } => self.build_call(callable, args),
            mlr::Expression::If(if_) => self.build_if(if_, expr),
            mlr::Expression::Loop { body } => self.build_loop(body),
            mlr::Expression::Struct {
                type_id,
                field_initializers,
            } => self.build_struct_expr(type_id, field_initializers),
            // mlr::Expression::AddressOf(loc_id) => todo!(),
            _ => {
                // Simply return the integer constant 42 for now
                let int_type = self.gtor.iw_ctxt.i32_type();
                Ok(int_type.const_int(42, false).as_basic_value_enum())
            }
        }
    }

    fn build_block(&mut self, body: &mlr::Block) -> FnGeneratorResult<BasicValueEnum<'iw>> {
        for stmt in &body.statements {
            self.build_statement(stmt)?;
        }
        self.build_load_from_loc(&body.output, "block_output")
    }

    fn build_constant(&mut self, constant: &mlr::Constant) -> FnGeneratorResult<BasicValueEnum<'iw>> {
        match constant {
            mlr::Constant::Int(i) => {
                let int_type = self.gtor.iw_ctxt.i32_type();
                Ok(int_type.const_int(*i as u64, false).as_basic_value_enum())
            }
            mlr::Constant::Bool(b) => {
                let bool_type = self.gtor.iw_ctxt.bool_type();
                Ok(bool_type.const_int(*b as u64, false).as_basic_value_enum())
            }
            mlr::Constant::Unit => self.build_unit_value(),
        }
    }

    fn build_var(&mut self, loc_id: &mlr::LocId) -> FnGeneratorResult<BasicValueEnum<'iw>> {
        self.build_load_from_loc(loc_id, "loaded_var")
    }

    fn build_global_function(&mut self, fn_id: &mr_functions::FnId) -> FnGeneratorResult<BasicValueEnum<'iw>> {
        let iw_fn = *self.gtor.functions.get(fn_id).ok_or(FnGeneratorError)?;
        Ok(iw_fn.as_global_value().as_pointer_value().as_basic_value_enum())
    }

    fn build_call(&mut self, callable: &mlr::LocId, args: &[mlr::LocId]) -> FnGeneratorResult<BasicValueEnum<'iw>> {
        let fn_ptr = self
            .build_load_from_loc(callable, "loaded_callable")?
            .into_pointer_value();
        let fn_type = self.get_function_type_of_loc(callable)?;

        let arg_values = args
            .iter()
            .map(|arg_loc| self.build_load_from_loc(arg_loc, "arg_value").unwrap().into())
            .collect::<Vec<_>>();

        let call_site = self
            .builder
            .build_indirect_call(fn_type, fn_ptr, &arg_values, "call_site")?;

        call_site.try_as_basic_value().left().ok_or(FnGeneratorError)
    }

    fn build_if(&mut self, if_: &mlr::If, expr_id: &mlr::ExprId) -> FnGeneratorResult<BasicValueEnum<'iw>> {
        // Build condition
        let cond_value = self
            .build_load_from_loc(&if_.condition, "if_condition")?
            .into_int_value();

        // Create blocks for then, else, and merge
        let then_block = self.gtor.iw_ctxt.append_basic_block(self.iw_fn, "then");
        let else_block = self.gtor.iw_ctxt.append_basic_block(self.iw_fn, "else");
        let merge_block = self.gtor.iw_ctxt.append_basic_block(self.iw_fn, "if_merge");

        // Allocate space for the result
        let result_type = self.get_iw_type_of_expr(expr_id)?;
        let result_address = self.build_alloca(result_type, "if_result")?;

        // Build conditional branch
        self.builder
            .build_conditional_branch(cond_value, then_block, else_block)?;

        // Build then block
        self.builder.position_at_end(then_block);
        let then_value = self.build_block(&if_.then_block)?;
        self.builder.build_store(result_address, then_value)?;
        self.builder.build_unconditional_branch(merge_block)?;

        // Build else block
        self.builder.position_at_end(else_block);
        let else_value = self.build_block(&if_.else_block)?;
        self.builder.build_store(result_address, else_value)?;
        self.builder.build_unconditional_branch(merge_block)?;

        // Build merge block
        self.builder.position_at_end(merge_block);
        let result_value = self
            .builder
            .build_load(result_type, result_address, "if_result_value")?;
        Ok(result_value)
    }

    fn build_loop(&mut self, body: &mlr::Block) -> Result<BasicValueEnum<'iw>, FnGeneratorError> {
        let body_block = self.gtor.iw_ctxt.append_basic_block(self.iw_fn, "loop");
        let after_loop = self.gtor.iw_ctxt.append_basic_block(self.iw_fn, "loop_after");

        self.builder.build_unconditional_branch(body_block)?;
        self.after_loop_blocks.push_back(after_loop);
        self.builder.position_at_end(body_block);
        self.build_block(body)?;
        self.builder.build_unconditional_branch(body_block)?;
        self.after_loop_blocks.pop_back();

        self.builder.position_at_end(after_loop);
        self.build_unit_value()
    }

    fn build_struct_expr(
        &mut self,
        type_id: &mr_types::TypeId,
        field_initializers: &[(String, mlr::LocId)],
    ) -> Result<BasicValueEnum<'iw>, FnGeneratorError> {
        let struct_def = self
            .gtor
            .mr_ctxt
            .type_registry
            .get_struct_definition_by_type_id(type_id)
            .ok_or(FnGeneratorError)?;

        let iw_type = self.gtor.get_type_as_basic_type_enum(type_id).ok_or(FnGeneratorError)?;
        let iw_struct_type: StructType<'iw> = iw_type.try_into().map_err(|_| FnGeneratorError)?;

        let mut init_values = vec![None; struct_def.fields.len()];
        for (field_name, init_loc) in field_initializers {
            let init_value = self.build_load_from_loc(init_loc, "field_init")?;
            let field_index = struct_def
                .fields
                .iter()
                .position(|mr_types::StructField { name, .. }| name == field_name)
                .ok_or(FnGeneratorError)?;
            init_values[field_index] = Some(init_value);
        }
        let init_values: Vec<BasicValueEnum<'iw>> = init_values
            .into_iter()
            .map(|opt| opt.ok_or(FnGeneratorError))
            .collect::<Result<_, _>>()?;

        let struct_value_ptr = self.build_alloca(iw_type, "struct_value")?;
        for (index, field_value) in init_values.iter().enumerate() {
            let field_ptr =
                self.builder
                    .build_struct_gep(iw_struct_type, struct_value_ptr, index as u32, "field_ptr")?;
            self.builder.build_store(field_ptr, *field_value)?;
        }
        let struct_value = self
            .builder
            .build_load(iw_type, struct_value_ptr, "loaded_struct_value")?;

        Ok(struct_value)
    }
}
