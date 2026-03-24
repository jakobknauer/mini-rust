mod intrinsics;

use std::collections::{HashMap, VecDeque};

use inkwell::{
    basic_block::BasicBlock,
    builder::{Builder, BuilderError},
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
};

use crate::{
    ctxt::{fns as mr_fns, ty as mr_ty},
    mlr,
};

pub struct MlrFnLowerer<'parent, 'iw, 'a, 'ctxt> {
    parent: &'parent mut super::MlrLowerer<'iw, 'a, 'ctxt>,
    mlr_fn: mlr::Fn<'ctxt>,
    iw_fn: FunctionValue<'iw>,
    iw_builder: Builder<'iw>,
    locs: HashMap<mlr::Loc<'ctxt>, PointerValue<'iw>>,
    entry_block: Option<BasicBlock<'iw>>,
    after_loop_blocks: VecDeque<BasicBlock<'iw>>,
    subst: mr_ty::GenVarSubst<'ctxt>,
}

#[derive(Debug)]
pub struct MlrFnLoweringError;

impl From<BuilderError> for MlrFnLoweringError {
    fn from(_: BuilderError) -> Self {
        MlrFnLoweringError
    }
}

pub type MlrFnLoweringResult<T> = Result<T, MlrFnLoweringError>;

impl<'parent, 'iw, 'a, 'ctxt> MlrFnLowerer<'parent, 'iw, 'a, 'ctxt> {
    pub fn new(
        parent: &'parent mut super::MlrLowerer<'iw, 'a, 'ctxt>,
        fn_inst: mr_fns::FnInst<'ctxt>,
        mlr_fn: mlr::Fn<'ctxt>,
    ) -> Self {
        let builder = parent.iw_ctxt.create_builder();
        #[allow(clippy::mutable_key_type)]
        let locs = HashMap::new();
        let iw_fn = parent.get_fn(fn_inst).unwrap();
        let after_loop_blocks = VecDeque::new();
        let subst = parent.mr_ctxt.get_subst_for_fn_inst(fn_inst);

        Self {
            parent,
            mlr_fn,
            iw_fn,
            iw_builder: builder,
            locs,
            entry_block: None,
            after_loop_blocks,
            subst,
        }
    }

    pub fn build_fn(&mut self) -> MlrFnLoweringResult<()> {
        let entry_block = self.build_entry_block()?;
        let body_block = self.build_function_body()?;

        self.iw_builder.position_at_end(entry_block);
        self.iw_builder.build_unconditional_branch(body_block)?;

        Ok(())
    }

    fn get_iw_ty_of_loc(&mut self, loc: mlr::Loc<'ctxt>) -> MlrFnLoweringResult<BasicTypeEnum<'iw>> {
        let iw_ty = self.get_ty_as_basic_type_enum(loc.1).ok_or(MlrFnLoweringError)?;
        Ok(iw_ty)
    }

    fn get_iw_ty_of_place(&mut self, place: mlr::Place<'ctxt>) -> MlrFnLoweringResult<BasicTypeEnum<'iw>> {
        let iw_ty = self.get_ty_as_basic_type_enum(place.1).ok_or(MlrFnLoweringError)?;
        Ok(iw_ty)
    }

    fn get_fn_ty_of_loc(&mut self, op: mlr::Op<'ctxt>) -> MlrFnLoweringResult<FunctionType<'iw>> {
        let mr_ty = self.substitute(op.1);

        let Some((param_tys, return_ty, var_args)) = self.parent.mr_ctxt.ty_is_callable(&[], mr_ty) else {
            return Err(MlrFnLoweringError);
        };

        let return_ty = self.get_ty_as_basic_type_enum(return_ty).ok_or(MlrFnLoweringError)?;

        let param_tys: Vec<_> = param_tys
            .iter()
            .map(|&param| self.get_ty_as_basic_metadata_type_enum(param).ok_or(MlrFnLoweringError))
            .collect::<MlrFnLoweringResult<_>>()?;

        Ok(return_ty.fn_type(&param_tys, var_args))
    }

    fn get_closure_fn_ty(
        &mut self,
        op: mlr::Op<'ctxt>,
        captures_ty: mr_ty::Ty<'ctxt>,
    ) -> MlrFnLoweringResult<FunctionType<'iw>> {
        let mr_ty = self.substitute(op.1);

        let Some((param_tys, return_ty, var_args)) = self.parent.mr_ctxt.ty_is_callable(&[], mr_ty) else {
            return Err(MlrFnLoweringError);
        };

        let return_ty = self.get_ty_as_basic_type_enum(return_ty).ok_or(MlrFnLoweringError)?;

        let captures_ty = self
            .get_ty_as_basic_metadata_type_enum(captures_ty)
            .ok_or(MlrFnLoweringError);

        let param_tys: Vec<_> = std::iter::once(captures_ty)
            .chain(
                param_tys
                    .iter()
                    .map(|&param| self.get_ty_as_basic_metadata_type_enum(param).ok_or(MlrFnLoweringError)),
            )
            .collect::<MlrFnLoweringResult<_>>()?;

        Ok(return_ty.fn_type(&param_tys, var_args))
    }

    fn build_alloca_for_loc(&mut self, loc: mlr::Loc<'ctxt>) -> MlrFnLoweringResult<PointerValue<'iw>> {
        let iw_ty = self.get_iw_ty_of_loc(loc)?;
        let name = loc.to_string();
        let address = self.build_alloca(iw_ty, &name)?;
        self.locs.insert(loc, address);
        Ok(address)
    }

    fn build_alloca(&mut self, iw_ty: BasicTypeEnum<'iw>, name: &str) -> MlrFnLoweringResult<PointerValue<'iw>> {
        // Remember current block to restore later
        let current_block = self.iw_builder.get_insert_block().ok_or(MlrFnLoweringError)?;
        // Position builder at the entry block to ensure allocations are at the start
        let entry_block = self.entry_block.ok_or(MlrFnLoweringError)?;
        self.iw_builder.position_at_end(entry_block);
        // Allocate
        let address = self.iw_builder.build_alloca(iw_ty, name)?;
        // Restore builder position
        self.iw_builder.position_at_end(current_block);

        Ok(address)
    }

    fn build_entry_block(&mut self) -> MlrFnLoweringResult<BasicBlock<'iw>> {
        let entry_block = self.parent.iw_ctxt.append_basic_block(self.iw_fn, "entry");
        self.entry_block = Some(entry_block);
        self.iw_builder.position_at_end(entry_block);

        let param_locs = self.mlr_fn.param_locs.clone();

        for (param_index, &param_loc) in param_locs.iter().enumerate() {
            let param_address = self.build_alloca_for_loc(param_loc)?;
            self.iw_builder
                .build_store(param_address, self.iw_fn.get_nth_param(param_index as u32).unwrap())?;
        }

        Ok(entry_block)
    }

    fn build_function_body(&mut self) -> MlrFnLoweringResult<BasicBlock<'iw>> {
        let body_block = self.parent.iw_ctxt.append_basic_block(self.iw_fn, "body");
        self.iw_builder.position_at_end(body_block);
        self.build_stmt(self.mlr_fn.body)?;
        Ok(body_block)
    }

    fn build_stmt(&mut self, stmt: mlr::Stmt<'ctxt>) -> MlrFnLoweringResult<()> {
        use mlr::StmtDef::*;

        match *stmt {
            Alloc { loc } => {
                self.build_alloca_for_loc(loc)?;
            }
            Assign { place, value } => {
                let place = self.build_place(place)?;
                let value = self.build_val(value)?;
                self.iw_builder.build_store(place, value)?;
            }
            Return { value } => {
                let ret_value = self.build_val(value)?;
                self.iw_builder.build_return(Some(&ret_value))?;
            }
            Break => {
                let after_loop_block = self.after_loop_blocks.back().ok_or(MlrFnLoweringError)?;
                self.iw_builder.build_unconditional_branch(*after_loop_block)?;
            }
            Block(stmts) => self.build_block(stmts)?,
            If(if_) => self.build_if(if_)?,
            Loop { body } => self.build_loop(body)?,
        }

        Ok(())
    }

    fn build_val(&mut self, val: mlr::Val<'ctxt>) -> MlrFnLoweringResult<BasicValueEnum<'iw>> {
        use mlr::ValDef::*;

        match *val {
            Use(place) => self.build_op(place),
            Call { callable, ref args } => {
                let result = self.build_call(callable, &args.clone())?;
                Ok(result)
            }
            AddrOf(place) => self.build_place(place).map(|ptr| ptr.as_basic_value_enum()),
            As { op, .. } => {
                // since the only valid conversion atm is from ref to ptr of the same base type,
                // we can just build the op and return it
                self.build_op(op)
            }
            BinaryPrim { op, lhs, rhs } => self.build_binary_prim(op, lhs, rhs),
            UnaryPrim { op, operand } => self.build_unary_prim(op, operand),
        }
    }

    fn build_place(&mut self, place: mlr::Place<'ctxt>) -> MlrFnLoweringResult<PointerValue<'iw>> {
        use mlr::PlaceDef::*;

        match *place {
            Loc(loc) => self.locs.get(&loc).ok_or(MlrFnLoweringError).cloned(),
            FieldAccess { base, field_index, .. } => {
                let iw_base_struct_type: StructType<'iw> = self
                    .get_iw_ty_of_place(base)?
                    .try_into()
                    .map_err(|_| MlrFnLoweringError)?;

                let base_address = self.build_place(base)?;

                let field_ptr = self.iw_builder.build_struct_gep(
                    iw_base_struct_type,
                    base_address,
                    field_index as u32,
                    "field_ptr",
                )?;

                Ok(field_ptr)
            }
            EnumDiscriminant { base, .. } => {
                let iw_base_struct_type: StructType<'iw> = self
                    .get_iw_ty_of_place(base)?
                    .try_into()
                    .map_err(|_| MlrFnLoweringError)?;

                let base_address = self.build_place(base)?;

                let discrim_ptr =
                    self.iw_builder
                        .build_struct_gep(iw_base_struct_type, base_address, 0, "enum_discriminant_ptr")?;

                Ok(discrim_ptr)
            }
            ProjectToVariant { base, .. } => {
                let iw_base_struct_type: StructType<'iw> = self
                    .get_iw_ty_of_place(base)?
                    .try_into()
                    .map_err(|_| MlrFnLoweringError)?;

                let base_address = self.build_place(base)?;

                let variant_ptr =
                    self.iw_builder
                        .build_struct_gep(iw_base_struct_type, base_address, 1, "enum_variant_ptr")?;

                Ok(variant_ptr)
            }
            Deref(op) => {
                let ptr_value = self.build_op(op)?.into_pointer_value();
                Ok(ptr_value)
            }
            ClosureCaptures(base) => self.build_place(base),
        }
    }

    fn build_block(&mut self, stmts: &[mlr::Stmt<'ctxt>]) -> MlrFnLoweringResult<()> {
        for &stmt in stmts {
            self.build_stmt(stmt)?;
        }
        Ok(())
    }

    fn build_constant(&mut self, constant: &mlr::Const) -> MlrFnLoweringResult<BasicValueEnum<'iw>> {
        use mlr::Const::*;

        let value = match constant {
            &Int(i) => {
                let int_ty = self.parent.iw_ctxt.i32_type();
                int_ty.const_int(i as u64, false).as_basic_value_enum()
            }
            &Bool(b) => {
                let bool_ty = self.parent.iw_ctxt.bool_type();
                bool_ty.const_int(b as u64, false).as_basic_value_enum()
            }
            &CChar(c) => {
                let char_ty = self.parent.iw_ctxt.i8_type();
                char_ty.const_int(c as u64, false).as_basic_value_enum()
            }
            CString(c_string) => self.build_c_string(c_string.clone()),
        };

        Ok(value)
    }

    fn build_op(&mut self, op: mlr::Op<'ctxt>) -> MlrFnLoweringResult<BasicValueEnum<'iw>> {
        use mlr::OpDef::*;

        match *op {
            Fn(fn_inst) => self.build_global_function(fn_inst),
            TraitMthdCall(trait_mthd_inst) => self.build_trait_mthd_inst(trait_mthd_inst),
            Const(ref constant) => self.build_constant(constant),
            Copy(place) => {
                let place_ptr = self.build_place(place)?;
                let iw_ty = self.get_iw_ty_of_place(place)?;
                let value = self.iw_builder.build_load(iw_ty, place_ptr, "loaded_place")?;
                Ok(value)
            }
        }
    }

    fn build_global_function(&mut self, fn_inst: mr_fns::FnInst<'ctxt>) -> MlrFnLoweringResult<BasicValueEnum<'iw>> {
        let substituted_gen_args = self.substitute_slice(fn_inst.gen_args);
        let substituted_env_gen_args = self.substitute_slice(fn_inst.env_gen_args);
        let substituted_fn_inst = fn_inst
            .with_gen_args(substituted_gen_args, substituted_env_gen_args)
            .unwrap();

        let result = self
            .parent
            .get_fn(substituted_fn_inst)
            .map(|fn_value| fn_value.as_global_value().as_pointer_value().as_basic_value_enum())
            .ok_or(MlrFnLoweringError)?;
        Ok(result)
    }

    fn build_trait_mthd_inst(
        &mut self,
        trait_mthd_inst: mr_fns::TraitMthdInst<'ctxt>,
    ) -> MlrFnLoweringResult<BasicValueEnum<'iw>> {
        let fn_inst = self
            .parent
            .mr_ctxt
            .resolve_trait_mthd_to_fn(trait_mthd_inst, &self.subst);
        self.build_global_function(fn_inst)
    }

    fn build_call(
        &mut self,
        callable: mlr::Op<'ctxt>,
        args: &[mlr::Op<'ctxt>],
    ) -> MlrFnLoweringResult<BasicValueEnum<'iw>> {
        let maybe_fn_inst = match *callable {
            mlr::OpDef::Fn(fn_inst) => Some(fn_inst),
            _ => None,
        };

        if let Some(fn_inst) = maybe_fn_inst
            && let Some(result) = self.try_build_intrinsic_call(fn_inst, args)?
        {
            return Ok(result);
        }

        let callable_ty = self.substitute(callable.1);
        let callable_ty = self.parent.mr_ctxt.tys.resolve_opaque_in_ty(callable_ty);
        if let mr_ty::TyDef::Closure {
            ref fn_, captures_ty, ..
        } = *callable_ty.0
        {
            let closure_fn = fn_.get().expect("closure fn not set");
            let fn_inst = {
                let env_gen_args: Vec<_> = closure_fn
                    .env_gen_params
                    .iter()
                    .map(|&gv| self.parent.mr_ctxt.tys.gen_var(gv))
                    .collect();
                let env_gen_args = self.parent.mr_ctxt.tys.ty_slice(&env_gen_args);
                let gen_args = self.parent.mr_ctxt.tys.ty_slice(&[]);
                self.parent
                    .mr_ctxt
                    .fns
                    .inst_fn(closure_fn, gen_args, env_gen_args)
                    .unwrap()
            };
            let fn_ptr = self.build_global_function(fn_inst)?.into_pointer_value();
            let captures: BasicMetadataValueEnum<'iw> = self.build_op(callable)?.into();

            let fn_ty = self.get_closure_fn_ty(callable, captures_ty)?;

            let args = std::iter::once(captures)
                .chain(args.iter().map(|&arg| self.build_op(arg).unwrap().into()))
                .collect::<Vec<_>>();

            let call_site = self
                .iw_builder
                .build_indirect_call(fn_ty, fn_ptr, &args, "closure_call_site")?;
            let output = call_site.try_as_basic_value().unwrap_basic();
            Ok(output)
        } else {
            let fn_ptr = self.build_op(callable)?.into_pointer_value();
            let fn_ty = self.get_fn_ty_of_loc(callable)?;

            let args = args
                .iter()
                .map(|&arg| self.build_op(arg).unwrap().into())
                .collect::<Vec<_>>();

            let call_site = self.iw_builder.build_indirect_call(fn_ty, fn_ptr, &args, "call_site")?;
            let output = call_site.try_as_basic_value().unwrap_basic();
            Ok(output)
        }
    }

    fn build_if(&mut self, if_: mlr::If<'ctxt>) -> MlrFnLoweringResult<()> {
        // Build condition
        let cond_value = self.build_op(if_.cond)?.into_int_value();

        // Create blocks for then, else, and merge
        let then_block = self.parent.iw_ctxt.append_basic_block(self.iw_fn, "then");
        let else_block = self.parent.iw_ctxt.append_basic_block(self.iw_fn, "else");
        let merge_block = self.parent.iw_ctxt.append_basic_block(self.iw_fn, "if_merge");

        // Build conditional branch
        self.iw_builder
            .build_conditional_branch(cond_value, then_block, else_block)?;

        // Build then block
        self.iw_builder.position_at_end(then_block);
        self.build_stmt(if_.then)?;
        self.iw_builder.build_unconditional_branch(merge_block)?;

        // Build else block
        self.iw_builder.position_at_end(else_block);
        self.build_stmt(if_.else_)?;
        self.iw_builder.build_unconditional_branch(merge_block)?;

        // Build merge block
        self.iw_builder.position_at_end(merge_block);
        Ok(())
    }

    fn build_loop(&mut self, body: mlr::Stmt<'ctxt>) -> MlrFnLoweringResult<()> {
        let body_block = self.parent.iw_ctxt.append_basic_block(self.iw_fn, "loop");
        let after_loop = self.parent.iw_ctxt.append_basic_block(self.iw_fn, "loop_after");

        self.iw_builder.build_unconditional_branch(body_block)?;
        self.after_loop_blocks.push_back(after_loop);
        self.iw_builder.position_at_end(body_block);
        self.build_stmt(body)?;
        self.iw_builder.build_unconditional_branch(body_block)?;
        self.after_loop_blocks.pop_back();

        self.iw_builder.position_at_end(after_loop);
        Ok(())
    }

    fn build_c_string(&self, c_string: Vec<u8>) -> BasicValueEnum<'iw> {
        let c_string = String::from_utf8(c_string).unwrap();
        unsafe {
            self.iw_builder
                .build_global_string(&c_string, "c_string_lit")
                .unwrap()
                .as_basic_value_enum()
        }
    }

    fn get_ty_as_basic_type_enum(&mut self, ty: mr_ty::Ty<'ctxt>) -> Option<BasicTypeEnum<'iw>> {
        let ty = self.substitute(ty);
        self.parent.get_ty_as_basic_type_enum(ty)
    }

    fn get_ty_as_basic_metadata_type_enum(&mut self, ty: mr_ty::Ty<'ctxt>) -> Option<BasicMetadataTypeEnum<'iw>> {
        let ty = self.substitute(ty);
        self.parent.get_ty_as_basic_metadata_type_enum(ty)
    }

    fn substitute(&mut self, ty: mr_ty::Ty<'ctxt>) -> mr_ty::Ty<'ctxt> {
        self.parent.mr_ctxt.tys.substitute_gen_vars(ty, &self.subst)
    }

    fn substitute_slice(&mut self, slice: mr_ty::TySlice<'ctxt>) -> mr_ty::TySlice<'ctxt> {
        self.parent.mr_ctxt.tys.substitute_gen_vars_on_slice(slice, &self.subst)
    }
}
