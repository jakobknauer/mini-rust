use std::collections::{HashMap, VecDeque};

use inkwell::{
    basic_block::BasicBlock,
    builder::{Builder, BuilderError},
    types::{BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
};

use crate::{ctxt::fns as mr_fns, ctxt::types as mr_types, mlr};

pub struct FnGenerator<'a, 'iw, 'mr> {
    gtor: &'a mut super::Generator<'iw, 'mr>,
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
    pub fn new(gtor: &'a mut super::Generator<'iw, 'mr>, fn_: mr_fns::Fn) -> Option<Self> {
        let builder = gtor.iw_ctxt.create_builder();
        let mlr = gtor.mr_ctxt.fns.get_fn_def(&fn_)?;
        let locs = HashMap::new();
        let iw_fn = *gtor.functions.get(&fn_)?;
        let after_loop_blocks = VecDeque::new();

        Some(Self {
            gtor,
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

    fn get_iw_type_of_place(&mut self, place_id: &mlr::PlaceId) -> FnGeneratorResult<BasicTypeEnum<'iw>> {
        let mr_type = self.mlr.place_types.get(place_id).ok_or(FnGeneratorError)?;
        let iw_type = self.gtor.get_type_as_basic_type_enum(mr_type).ok_or(FnGeneratorError)?;
        Ok(iw_type)
    }

    fn get_function_type_of_loc(&mut self, op_id: &mlr::OpId) -> FnGeneratorResult<FunctionType<'iw>> {
        let mr_type = self.mlr.op_types.get(op_id).ok_or(FnGeneratorError)?;
        let mr_type = self
            .gtor
            .mr_ctxt
            .types
            .get_type_by_id(mr_type)
            .ok_or(FnGeneratorError)?;

        let mr_types::Type::Fn {
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
            .types
            .get_primitive_type_id(mr_types::PrimitiveType::Unit)
            .ok_or(FnGeneratorError)?;
        let iw_type = self.gtor.get_or_define_type(&mr_unit_type).ok_or(FnGeneratorError)?;
        let iw_struct_type: StructType = iw_type.try_into().map_err(|_| FnGeneratorError)?;
        Ok(iw_struct_type.const_named_struct(&[]).as_basic_value_enum())
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
        self.build_statement(&self.mlr.body)?;

        Ok(body_block)
    }

    fn build_statement(&mut self, _stmt: &mlr::StmtId) -> FnGeneratorResult<()> {
        use mlr::Stmt::*;

        let stmt = self.mlr.stmts.get(_stmt).unwrap();

        match stmt {
            Alloc { loc } => {
                self.build_alloca_for_loc(loc)?;
            }
            Assign { place, value } => {
                let place = self.build_place(place)?;
                let value = self.build_val(value)?;
                self.builder.build_store(place, value)?;
            }
            Return { value } => {
                let ret_value = self.build_val(value)?;
                self.builder.build_return(Some(&ret_value))?;
            }
            Break => {
                let after_loop_block = self.after_loop_blocks.back().ok_or(FnGeneratorError)?;
                self.builder.build_unconditional_branch(*after_loop_block)?;
            }
            Block(stmt_ids) => self.build_block(stmt_ids)?,
            If(if_) => self.build_if(if_)?,
            Loop { body } => self.build_loop(body)?,
        }

        Ok(())
    }

    fn build_val(&mut self, val: &mlr::ValId) -> FnGeneratorResult<BasicValueEnum<'iw>> {
        use mlr::Val::*;

        match self.mlr.vals.get(val).ok_or(FnGeneratorError)? {
            Use(place_id) => self.build_op(place_id),
            Call { callable, args } => self.build_call(callable, args),
            Empty { type_id } => self.build_empty_val(type_id),
        }
    }

    fn build_place(&mut self, place: &mlr::PlaceId) -> FnGeneratorResult<PointerValue<'iw>> {
        use mlr::Place::*;

        match self.mlr.places.get(place).ok_or(FnGeneratorError)? {
            Local(loc_id) => self.locs.get(loc_id).ok_or(FnGeneratorError).cloned(),
            FieldAccess { base, field_index, .. } => {
                let iw_base_struct_type: StructType<'iw> = self
                    .get_iw_type_of_place(base)?
                    .try_into()
                    .map_err(|_| FnGeneratorError)?;

                let base_address = self.build_place(base)?;

                let field_ptr = self.builder.build_struct_gep(
                    iw_base_struct_type,
                    base_address,
                    *field_index as u32,
                    "field_ptr",
                )?;

                Ok(field_ptr)
            }
            EnumDiscriminant { base, .. } => {
                let iw_base_struct_type: StructType<'iw> = self
                    .get_iw_type_of_place(base)?
                    .try_into()
                    .map_err(|_| FnGeneratorError)?;

                let base_address = self.build_place(base)?;

                let discrim_ptr =
                    self.builder
                        .build_struct_gep(iw_base_struct_type, base_address, 0, "enum_discriminant_ptr")?;

                Ok(discrim_ptr)
            }
            ProjectToVariant { base, .. } => {
                let iw_base_struct_type: StructType<'iw> = self
                    .get_iw_type_of_place(base)?
                    .try_into()
                    .map_err(|_| FnGeneratorError)?;

                let base_address = self.build_place(base)?;

                let variant_ptr =
                    self.builder
                        .build_struct_gep(iw_base_struct_type, base_address, 1, "enum_variant_ptr")?;

                Ok(variant_ptr)
            }
        }
    }

    fn build_block(&mut self, statements: &[mlr::StmtId]) -> FnGeneratorResult<()> {
        for stmt in statements {
            self.build_statement(stmt)?;
        }
        Ok(())
    }

    fn build_constant(&mut self, constant: &mlr::Constant) -> FnGeneratorResult<BasicValueEnum<'iw>> {
        use mlr::Constant::*;

        let value = match constant {
            Int(i) => {
                let int_type = self.gtor.iw_ctxt.i32_type();
                int_type.const_int(*i as u64, false).as_basic_value_enum()
            }
            Bool(b) => {
                let bool_type = self.gtor.iw_ctxt.bool_type();
                bool_type.const_int(*b as u64, false).as_basic_value_enum()
            }
            Unit => self.build_unit_value()?,
        };

        Ok(value)
    }

    fn build_op(&mut self, place_id: &mlr::OpId) -> FnGeneratorResult<BasicValueEnum<'iw>> {
        use mlr::Operand::*;

        let operand = self.mlr.ops.get(place_id).ok_or(FnGeneratorError)?;

        match operand {
            Fn(fn_) => self.build_global_function(fn_),
            Constant(constant) => self.build_constant(constant),
            Copy(place_id) => {
                let place_ptr = self.build_place(place_id)?;
                let iw_type = self.get_iw_type_of_place(place_id)?;
                let value = self.builder.build_load(iw_type, place_ptr, "loaded_place")?;
                Ok(value)
            }
        }
    }

    fn build_global_function(&mut self, fn_: &mr_fns::Fn) -> FnGeneratorResult<BasicValueEnum<'iw>> {
        let iw_fn = *self.gtor.functions.get(fn_).ok_or(FnGeneratorError)?;
        Ok(iw_fn.as_global_value().as_pointer_value().as_basic_value_enum())
    }

    fn build_call(&mut self, callable: &mlr::OpId, args: &[mlr::OpId]) -> FnGeneratorResult<BasicValueEnum<'iw>> {
        let fn_ptr = self.build_op(callable)?.into_pointer_value();
        let fn_type = self.get_function_type_of_loc(callable)?;

        let args = args
            .iter()
            .map(|arg| self.build_op(arg).unwrap().into())
            .collect::<Vec<_>>();

        let call_site = self.builder.build_indirect_call(fn_type, fn_ptr, &args, "call_site")?;

        call_site.try_as_basic_value().left().ok_or(FnGeneratorError)
    }

    fn build_if(&mut self, if_: &mlr::If) -> FnGeneratorResult<()> {
        // Build condition
        let cond_value = self.build_op(&if_.condition)?.into_int_value();

        // Create blocks for then, else, and merge
        let then_block = self.gtor.iw_ctxt.append_basic_block(self.iw_fn, "then");
        let else_block = self.gtor.iw_ctxt.append_basic_block(self.iw_fn, "else");
        let merge_block = self.gtor.iw_ctxt.append_basic_block(self.iw_fn, "if_merge");

        // Build conditional branch
        self.builder
            .build_conditional_branch(cond_value, then_block, else_block)?;

        // Build then block
        self.builder.position_at_end(then_block);
        self.build_statement(&if_.then_block)?;
        self.builder.build_unconditional_branch(merge_block)?;

        // Build else block
        self.builder.position_at_end(else_block);
        self.build_statement(&if_.else_block)?;
        self.builder.build_unconditional_branch(merge_block)?;

        // Build merge block
        self.builder.position_at_end(merge_block);
        Ok(())
    }

    fn build_loop(&mut self, body: &mlr::StmtId) -> FnGeneratorResult<()> {
        let body_block = self.gtor.iw_ctxt.append_basic_block(self.iw_fn, "loop");
        let after_loop = self.gtor.iw_ctxt.append_basic_block(self.iw_fn, "loop_after");

        self.builder.build_unconditional_branch(body_block)?;
        self.after_loop_blocks.push_back(after_loop);
        self.builder.position_at_end(body_block);
        self.build_statement(body)?;
        self.builder.build_unconditional_branch(body_block)?;
        self.after_loop_blocks.pop_back();

        self.builder.position_at_end(after_loop);
        Ok(())
    }

    fn build_empty_val(&mut self, type_id: &mr_types::TypeId) -> Result<BasicValueEnum<'iw>, FnGeneratorError> {
        let iw_type = self.gtor.get_type_as_basic_type_enum(type_id).ok_or(FnGeneratorError)?;
        let struct_value = iw_type.const_zero(); // create a zero value because that's available for BasicValueEnum
        Ok(struct_value)
    }
}
