use std::collections::{HashMap, VecDeque};

use inkwell::{
    basic_block::BasicBlock,
    builder::{Builder, BuilderError},
    types::{BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
};

use crate::{ctxt::fns as mr_fns, ctxt::ty as mr_ty, mlr};

pub struct FnGenerator<'a, 'iw, 'mr> {
    gtor: &'a mut super::Generator<'iw, 'mr>,
    iw_fn: FunctionValue<'iw>,
    builder: Builder<'iw>,
    mlr: &'a mlr::Mlr,
    locs: HashMap<mlr::Loc, PointerValue<'iw>>,
    entry_block: Option<BasicBlock<'iw>>,
    after_loop_blocks: VecDeque<BasicBlock<'iw>>,
    substitutions: HashMap<&'a str, mr_ty::Ty>,
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
    pub fn new(gtor: &'a mut super::Generator<'iw, 'mr>, inst_fn: mr_fns::InstantiatedFn) -> Option<Self> {
        let builder = gtor.iw_ctxt.create_builder();
        let mlr = gtor.mr_ctxt.fns.get_fn_def(&inst_fn.fn_)?;
        let locs = HashMap::new();
        let iw_fn = *gtor.functions.get(&inst_fn)?;
        let after_loop_blocks = VecDeque::new();

        let signature = gtor.mr_ctxt.fns.get_sig(&inst_fn.fn_)?;
        let substitutions = signature.build_substitutions(&inst_fn.gen_args);

        Some(Self {
            gtor,
            iw_fn,
            builder,
            mlr,
            locs,
            entry_block: None,
            after_loop_blocks,
            substitutions,
        })
    }

    pub fn define_fn(&mut self) -> FnGeneratorResult<()> {
        let entry_block = self.build_entry_block()?;
        let body_block = self.build_function_body()?;

        self.builder.position_at_end(entry_block);
        self.builder.build_unconditional_branch(body_block)?;

        Ok(())
    }

    fn get_iw_ty_of_loc(&mut self, loc: &mlr::Loc) -> FnGeneratorResult<BasicTypeEnum<'iw>> {
        let mr_ty = self.mlr.loc_tys.get(loc).ok_or(FnGeneratorError)?;
        let iw_ty = self
            .gtor
            .get_ty_as_basic_type_enum(mr_ty, &self.substitutions)
            .ok_or(FnGeneratorError)?;
        Ok(iw_ty)
    }

    fn get_iw_ty_of_place(&mut self, place: &mlr::Place) -> FnGeneratorResult<BasicTypeEnum<'iw>> {
        let mr_ty = self.mlr.place_tys.get(place).ok_or(FnGeneratorError)?;
        let iw_ty = self
            .gtor
            .get_ty_as_basic_type_enum(mr_ty, &self.substitutions)
            .ok_or(FnGeneratorError)?;
        Ok(iw_ty)
    }

    fn get_fn_ty_of_loc(&mut self, op: &mlr::Op) -> FnGeneratorResult<FunctionType<'iw>> {
        let mr_ty = self.mlr.op_tys.get(op).ok_or(FnGeneratorError)?;
        let mr_ty = self.gtor.mr_ctxt.tys.get_ty_def(mr_ty).ok_or(FnGeneratorError)?;

        let mr_ty::TyDef::Fn { return_ty, param_tys } = mr_ty else {
            return Err(FnGeneratorError);
        };

        let return_ty = self
            .gtor
            .get_ty_as_basic_type_enum(return_ty, &self.substitutions)
            .ok_or(FnGeneratorError)?;

        let param_tys: Vec<_> = param_tys
            .iter()
            .map(|param| {
                self.gtor
                    .get_ty_as_basic_metadata_type_enum(param, &self.substitutions)
                    .ok_or(FnGeneratorError)
            })
            .collect::<FnGeneratorResult<_>>()?;

        Ok(return_ty.fn_type(&param_tys, false))
    }

    fn build_alloca_for_loc(&mut self, loc: &mlr::Loc) -> FnGeneratorResult<PointerValue<'iw>> {
        let iw_ty = self.get_iw_ty_of_loc(loc)?;
        let name = loc.to_string();
        let address = self.build_alloca(iw_ty, &name)?;
        self.locs.insert(*loc, address);
        Ok(address)
    }

    fn build_alloca(&mut self, iw_ty: BasicTypeEnum<'iw>, name: &str) -> FnGeneratorResult<PointerValue<'iw>> {
        // Remember current block to restore later
        let current_block = self.builder.get_insert_block().ok_or(FnGeneratorError)?;
        // Position builder at the entry block to ensure allocations are at the start
        let entry_block = self.entry_block.ok_or(FnGeneratorError)?;
        self.builder.position_at_end(entry_block);
        // Allocate
        let address = self.builder.build_alloca(iw_ty, name)?;
        // Restore builder position
        self.builder.position_at_end(current_block);

        Ok(address)
    }

    fn build_unit_value(&mut self) -> FnGeneratorResult<BasicValueEnum<'iw>> {
        let mr_unit_ty = self
            .gtor
            .mr_ctxt
            .tys
            .get_primitive_ty(mr_ty::Primitive::Unit)
            .ok_or(FnGeneratorError)?;
        let iw_ty = self
            .gtor
            .get_or_define_ty(&mr_unit_ty, &self.substitutions)
            .ok_or(FnGeneratorError)?;
        let iw_struct_type: StructType = iw_ty.try_into().map_err(|_| FnGeneratorError)?;
        Ok(iw_struct_type.const_named_struct(&[]).as_basic_value_enum())
    }

    fn build_entry_block(&mut self) -> FnGeneratorResult<BasicBlock<'iw>> {
        let entry_block = self.gtor.iw_ctxt.append_basic_block(self.iw_fn, "entry");
        self.entry_block = Some(entry_block);
        self.builder.position_at_end(entry_block);

        for (param_index, param_loc) in self.mlr.param_locs.iter().enumerate() {
            let param_address = self.build_alloca_for_loc(param_loc)?;
            self.builder
                .build_store(param_address, self.iw_fn.get_nth_param(param_index as u32).unwrap())?;
        }

        Ok(entry_block)
    }

    fn build_function_body(&mut self) -> FnGeneratorResult<BasicBlock<'iw>> {
        let body_block = self.gtor.iw_ctxt.append_basic_block(self.iw_fn, "body");
        self.builder.position_at_end(body_block);
        self.build_stmt(&self.mlr.body)?;

        Ok(body_block)
    }

    fn build_stmt(&mut self, _stmt: &mlr::Stmt) -> FnGeneratorResult<()> {
        use mlr::StmtDef::*;

        let stmt = self.mlr.stmts.get(_stmt).ok_or(FnGeneratorError)?;

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
            Block(stmts) => self.build_block(stmts)?,
            If(if_) => self.build_if(if_)?,
            Loop { body } => self.build_loop(body)?,
        }

        Ok(())
    }

    fn build_val(&mut self, val: &mlr::Val) -> FnGeneratorResult<BasicValueEnum<'iw>> {
        use mlr::ValDef::*;

        let val = self.mlr.vals.get(val).ok_or(FnGeneratorError)?;

        match val {
            Use(place) => self.build_op(place),
            Call { callable, args } => self.build_call(callable, args),
            Empty { ty } => self.build_empty_val(ty),
            AddrOf(place) => self.build_place(place).map(|ptr| ptr.as_basic_value_enum()),
        }
    }

    fn build_place(&mut self, place: &mlr::Place) -> FnGeneratorResult<PointerValue<'iw>> {
        use mlr::PlaceDef::*;

        let place = self.mlr.places.get(place).ok_or(FnGeneratorError)?;

        match place {
            Loc(loc) => self.locs.get(loc).ok_or(FnGeneratorError).cloned(),
            FieldAccess { base, field_index, .. } => {
                let iw_base_struct_type: StructType<'iw> = self
                    .get_iw_ty_of_place(base)?
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
                    .get_iw_ty_of_place(base)?
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
                    .get_iw_ty_of_place(base)?
                    .try_into()
                    .map_err(|_| FnGeneratorError)?;

                let base_address = self.build_place(base)?;

                let variant_ptr =
                    self.builder
                        .build_struct_gep(iw_base_struct_type, base_address, 1, "enum_variant_ptr")?;

                Ok(variant_ptr)
            }
            Deref(op) => {
                let ptr_value = self.build_op(op)?.into_pointer_value();
                Ok(ptr_value)
            }
        }
    }

    fn build_block(&mut self, stmts: &[mlr::Stmt]) -> FnGeneratorResult<()> {
        for stmt in stmts {
            self.build_stmt(stmt)?;
        }
        Ok(())
    }

    fn build_constant(&mut self, constant: &mlr::Const) -> FnGeneratorResult<BasicValueEnum<'iw>> {
        use mlr::Const::*;

        let value = match constant {
            Int(i) => {
                let int_ty = self.gtor.iw_ctxt.i32_type();
                int_ty.const_int(*i as u64, false).as_basic_value_enum()
            }
            Bool(b) => {
                let bool_ty = self.gtor.iw_ctxt.bool_type();
                bool_ty.const_int(*b as u64, false).as_basic_value_enum()
            }
            Unit => self.build_unit_value()?,
        };

        Ok(value)
    }

    fn build_op(&mut self, op: &mlr::Op) -> FnGeneratorResult<BasicValueEnum<'iw>> {
        use mlr::OpDef::*;

        let op = self.mlr.ops.get(op).ok_or(FnGeneratorError)?;

        match op {
            Fn(inst_fn) => self.build_global_function(inst_fn),
            Const(constant) => self.build_constant(constant),
            Copy(place) => {
                let place_ptr = self.build_place(place)?;
                let iw_ty = self.get_iw_ty_of_place(place)?;
                let value = self.builder.build_load(iw_ty, place_ptr, "loaded_place")?;
                Ok(value)
            }
        }
    }

    fn build_global_function(&mut self, inst_fn: &mr_fns::InstantiatedFn) -> FnGeneratorResult<BasicValueEnum<'iw>> {
        self.gtor
            .functions
            .iter()
            .find_map(|(other, iw_fn)| {
                self.gtor
                    .mr_ctxt
                    .inst_fn_equal(inst_fn, other)
                    .then(|| iw_fn.as_global_value().as_pointer_value().as_basic_value_enum())
            })
            .ok_or(FnGeneratorError)
    }

    fn build_call(&mut self, callable: &mlr::Op, args: &[mlr::Op]) -> FnGeneratorResult<BasicValueEnum<'iw>> {
        let fn_ptr = self.build_op(callable)?.into_pointer_value();
        let fn_ty = self.get_fn_ty_of_loc(callable)?;

        let args = args
            .iter()
            .map(|arg| self.build_op(arg).unwrap().into())
            .collect::<Vec<_>>();

        let call_site = self.builder.build_indirect_call(fn_ty, fn_ptr, &args, "call_site")?;

        call_site.try_as_basic_value().left().ok_or(FnGeneratorError)
    }

    fn build_if(&mut self, if_: &mlr::If) -> FnGeneratorResult<()> {
        // Build condition
        let cond_value = self.build_op(&if_.cond)?.into_int_value();

        // Create blocks for then, else, and merge
        let then_block = self.gtor.iw_ctxt.append_basic_block(self.iw_fn, "then");
        let else_block = self.gtor.iw_ctxt.append_basic_block(self.iw_fn, "else");
        let merge_block = self.gtor.iw_ctxt.append_basic_block(self.iw_fn, "if_merge");

        // Build conditional branch
        self.builder
            .build_conditional_branch(cond_value, then_block, else_block)?;

        // Build then block
        self.builder.position_at_end(then_block);
        self.build_stmt(&if_.then)?;
        self.builder.build_unconditional_branch(merge_block)?;

        // Build else block
        self.builder.position_at_end(else_block);
        self.build_stmt(&if_.else_)?;
        self.builder.build_unconditional_branch(merge_block)?;

        // Build merge block
        self.builder.position_at_end(merge_block);
        Ok(())
    }

    fn build_loop(&mut self, body: &mlr::Stmt) -> FnGeneratorResult<()> {
        let body_block = self.gtor.iw_ctxt.append_basic_block(self.iw_fn, "loop");
        let after_loop = self.gtor.iw_ctxt.append_basic_block(self.iw_fn, "loop_after");

        self.builder.build_unconditional_branch(body_block)?;
        self.after_loop_blocks.push_back(after_loop);
        self.builder.position_at_end(body_block);
        self.build_stmt(body)?;
        self.builder.build_unconditional_branch(body_block)?;
        self.after_loop_blocks.pop_back();

        self.builder.position_at_end(after_loop);
        Ok(())
    }

    fn build_empty_val(&mut self, ty: &mr_ty::Ty) -> Result<BasicValueEnum<'iw>, FnGeneratorError> {
        let iw_ty = self
            .gtor
            .get_ty_as_basic_type_enum(ty, &self.substitutions)
            .ok_or(FnGeneratorError)?;
        let struct_value = iw_ty.const_zero(); // create a zero value because that's available for BasicValueEnum
        Ok(struct_value)
    }
}
