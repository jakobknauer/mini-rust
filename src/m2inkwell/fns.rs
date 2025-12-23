use std::collections::{HashMap, VecDeque};

use inkwell::{
    basic_block::BasicBlock,
    builder::{Builder, BuilderError},
    targets::TargetData,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
};

use crate::ctxt::{fns as mr_fns, mlr, ty as mr_ty};

pub struct M2InkwellFn<'a, 'iw, 'mr> {
    m2iw: &'a mut super::M2Inkwell<'iw, 'mr>,
    specialization: mr_fns::FnSpecialization,
    iw_fn: FunctionValue<'iw>,
    iw_builder: Builder<'iw>,
    locs: HashMap<mlr::Loc, PointerValue<'iw>>,
    entry_block: Option<BasicBlock<'iw>>,
    after_loop_blocks: VecDeque<BasicBlock<'iw>>,
    substitutions: HashMap<mr_ty::GenVar, mr_ty::Ty>,
}

#[derive(Debug)]
pub struct M2InkwellFnError;

impl From<BuilderError> for M2InkwellFnError {
    fn from(_: BuilderError) -> Self {
        M2InkwellFnError
    }
}

pub type M2InkwellFnResult<T> = Result<T, M2InkwellFnError>;

impl<'a, 'iw, 'mr> M2InkwellFn<'a, 'iw, 'mr> {
    pub fn new(m2iw: &'a mut super::M2Inkwell<'iw, 'mr>, specialization: mr_fns::FnSpecialization) -> Option<Self> {
        if !m2iw.mr_ctxt.fns.is_fn_defined(&specialization.fn_) {
            return None;
        }

        let builder = m2iw.iw_ctxt.create_builder();
        let locs = HashMap::new();
        let iw_fn = *m2iw.functions.get(&specialization)?;
        let after_loop_blocks = VecDeque::new();
        let substitutions = m2iw
            .mr_ctxt
            .fns
            .get_substitutions_for_specialization(&specialization)
            .iter()
            .map(|(&k, &v)| (k, v))
            .collect();

        Some(Self {
            m2iw,
            specialization,
            iw_fn,
            iw_builder: builder,
            locs,
            entry_block: None,
            after_loop_blocks,
            substitutions,
        })
    }

    pub fn build_fn(&mut self) -> M2InkwellFnResult<()> {
        let entry_block = self.build_entry_block()?;
        let body_block = self.build_function_body()?;

        self.iw_builder.position_at_end(entry_block);
        self.iw_builder.build_unconditional_branch(body_block)?;

        Ok(())
    }

    fn get_iw_ty_of_loc(&mut self, loc: &mlr::Loc) -> M2InkwellFnResult<BasicTypeEnum<'iw>> {
        let mr_ty = self.m2iw.mr_ctxt.mlr.get_loc_ty(loc);
        let iw_ty = self.get_ty_as_basic_type_enum(mr_ty).ok_or(M2InkwellFnError)?;
        Ok(iw_ty)
    }

    fn get_iw_ty_of_place(&mut self, place: &mlr::Place) -> M2InkwellFnResult<BasicTypeEnum<'iw>> {
        let mr_ty = self.m2iw.mr_ctxt.mlr.get_place_ty(place);
        let iw_ty = self.get_ty_as_basic_type_enum(mr_ty).ok_or(M2InkwellFnError)?;
        Ok(iw_ty)
    }

    fn get_fn_ty_of_loc(&mut self, op: &mlr::Op) -> M2InkwellFnResult<FunctionType<'iw>> {
        let mr_ty = self.m2iw.mr_ctxt.mlr.get_op_ty(op);
        let mr_ty_def = self.m2iw.mr_ctxt.tys.get_ty_def(mr_ty).ok_or(M2InkwellFnError)?;

        let mr_ty::TyDef::Fn {
            return_ty,
            param_tys,
            var_args,
        } = mr_ty_def.clone()
        else {
            return Err(M2InkwellFnError);
        };

        let return_ty = self.get_ty_as_basic_type_enum(return_ty).ok_or(M2InkwellFnError)?;

        let param_tys: Vec<_> = param_tys
            .iter()
            .map(|&param| self.get_ty_as_basic_metadata_type_enum(param).ok_or(M2InkwellFnError))
            .collect::<M2InkwellFnResult<_>>()?;

        Ok(return_ty.fn_type(&param_tys, var_args))
    }

    fn mlr(&self) -> &mr_fns::FnMlr {
        self.m2iw
            .mr_ctxt
            .fns
            .get_fn_def(&self.specialization.fn_)
            .expect("MLR for function should be defined")
    }

    fn build_alloca_for_loc(&mut self, loc: &mlr::Loc) -> M2InkwellFnResult<PointerValue<'iw>> {
        let iw_ty = self.get_iw_ty_of_loc(loc)?;
        let name = loc.to_string();
        let address = self.build_alloca(iw_ty, &name)?;
        self.locs.insert(*loc, address);
        Ok(address)
    }

    fn build_alloca(&mut self, iw_ty: BasicTypeEnum<'iw>, name: &str) -> M2InkwellFnResult<PointerValue<'iw>> {
        // Remember current block to restore later
        let current_block = self.iw_builder.get_insert_block().ok_or(M2InkwellFnError)?;
        // Position builder at the entry block to ensure allocations are at the start
        let entry_block = self.entry_block.ok_or(M2InkwellFnError)?;
        self.iw_builder.position_at_end(entry_block);
        // Allocate
        let address = self.iw_builder.build_alloca(iw_ty, name)?;
        // Restore builder position
        self.iw_builder.position_at_end(current_block);

        Ok(address)
    }

    fn build_unit_value(&mut self) -> M2InkwellFnResult<BasicValueEnum<'iw>> {
        let mr_unit_ty = self.m2iw.mr_ctxt.tys.get_primitive_ty(mr_ty::Primitive::Unit);
        let iw_ty = self.m2iw.get_or_define_ty(mr_unit_ty).ok_or(M2InkwellFnError)?;
        let iw_struct_type: StructType = iw_ty.try_into().map_err(|_| M2InkwellFnError)?;
        Ok(iw_struct_type.const_named_struct(&[]).as_basic_value_enum())
    }

    fn build_entry_block(&mut self) -> M2InkwellFnResult<BasicBlock<'iw>> {
        let entry_block = self.m2iw.iw_ctxt.append_basic_block(self.iw_fn, "entry");
        self.entry_block = Some(entry_block);
        self.iw_builder.position_at_end(entry_block);

        let param_locs = self.mlr().param_locs.clone();

        for (param_index, param_loc) in param_locs.iter().enumerate() {
            let param_address = self.build_alloca_for_loc(param_loc)?;
            self.iw_builder
                .build_store(param_address, self.iw_fn.get_nth_param(param_index as u32).unwrap())?;
        }

        Ok(entry_block)
    }

    fn build_function_body(&mut self) -> M2InkwellFnResult<BasicBlock<'iw>> {
        let body_block = self.m2iw.iw_ctxt.append_basic_block(self.iw_fn, "body");
        self.iw_builder.position_at_end(body_block);
        self.build_stmt(&self.mlr().body.clone())?;

        Ok(body_block)
    }

    fn build_stmt(&mut self, stmt: &mlr::Stmt) -> M2InkwellFnResult<()> {
        use mlr::StmtDef::*;

        let stmt = self.m2iw.mr_ctxt.mlr.get_stmt_def(stmt);

        match *stmt {
            Alloc { loc } => {
                self.build_alloca_for_loc(&loc)?;
            }
            Assign { place, value } => {
                let place = self.build_place(&place)?;
                let value = self.build_val(&value)?;
                self.iw_builder.build_store(place, value)?;
            }
            Return { value } => {
                let ret_value = self.build_val(&value)?;
                self.iw_builder.build_return(Some(&ret_value))?;
            }
            Break => {
                let after_loop_block = self.after_loop_blocks.back().ok_or(M2InkwellFnError)?;
                self.iw_builder.build_unconditional_branch(*after_loop_block)?;
            }
            Block(ref stmts) => self.build_block(&stmts.clone())?,
            If(if_) => self.build_if(&if_)?,
            Loop { body } => self.build_loop(&body)?,
        }

        Ok(())
    }

    fn build_val(&mut self, val: &mlr::Val) -> M2InkwellFnResult<BasicValueEnum<'iw>> {
        use mlr::ValDef::*;

        let val = self.m2iw.mr_ctxt.mlr.get_val_def(val);

        match *val {
            Use(place) => self.build_op(&place),
            Call { callable, ref args } => self.build_call(&callable, &args.clone()),
            AddrOf(place) => self.build_place(&place).map(|ptr| ptr.as_basic_value_enum()),
            As { op, .. } => {
                // since the only valid conversion atm is from ref to ptr of the same base type,
                // we can just build the op and return it
                self.build_op(&op)
            }
            SizeOf(ty) => {
                let ty = self.get_ty_as_basic_type_enum(ty).unwrap();
                let size = TargetData::create("").get_store_size(&ty) as u32;
                let int_ty = self.m2iw.iw_ctxt.i32_type();
                Ok(int_ty.const_int(size as u64, false).as_basic_value_enum())
            }
        }
    }

    fn build_place(&mut self, place: &mlr::Place) -> M2InkwellFnResult<PointerValue<'iw>> {
        use mlr::PlaceDef::*;

        let place = self.m2iw.mr_ctxt.mlr.get_place_def(*place);

        match *place {
            Loc(loc) => self.locs.get(&loc).ok_or(M2InkwellFnError).cloned(),
            FieldAccess { base, field_index, .. } => {
                let iw_base_struct_type: StructType<'iw> = self
                    .get_iw_ty_of_place(&base)?
                    .try_into()
                    .map_err(|_| M2InkwellFnError)?;

                let base_address = self.build_place(&base)?;

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
                    .get_iw_ty_of_place(&base)?
                    .try_into()
                    .map_err(|_| M2InkwellFnError)?;

                let base_address = self.build_place(&base)?;

                let discrim_ptr =
                    self.iw_builder
                        .build_struct_gep(iw_base_struct_type, base_address, 0, "enum_discriminant_ptr")?;

                Ok(discrim_ptr)
            }
            ProjectToVariant { base, .. } => {
                let iw_base_struct_type: StructType<'iw> = self
                    .get_iw_ty_of_place(&base)?
                    .try_into()
                    .map_err(|_| M2InkwellFnError)?;

                let base_address = self.build_place(&base)?;

                let variant_ptr =
                    self.iw_builder
                        .build_struct_gep(iw_base_struct_type, base_address, 1, "enum_variant_ptr")?;

                Ok(variant_ptr)
            }
            Deref(op) => {
                let ptr_value = self.build_op(&op)?.into_pointer_value();
                Ok(ptr_value)
            }
        }
    }

    fn build_block(&mut self, stmts: &[mlr::Stmt]) -> M2InkwellFnResult<()> {
        for stmt in stmts {
            self.build_stmt(stmt)?;
        }
        Ok(())
    }

    fn build_constant(&mut self, constant: &mlr::Const) -> M2InkwellFnResult<BasicValueEnum<'iw>> {
        use mlr::Const::*;

        let value = match *constant {
            Int(i) => {
                let int_ty = self.m2iw.iw_ctxt.i32_type();
                int_ty.const_int(i as u64, false).as_basic_value_enum()
            }
            Bool(b) => {
                let bool_ty = self.m2iw.iw_ctxt.bool_type();
                bool_ty.const_int(b as u64, false).as_basic_value_enum()
            }
            Unit => self.build_unit_value()?,
            CChar(c) => {
                let char_ty = self.m2iw.iw_ctxt.i8_type();
                char_ty.const_int(c as u64, false).as_basic_value_enum()
            }
            CString(ref c_string) => self.build_c_string(c_string.clone()),
        };

        Ok(value)
    }

    fn build_op(&mut self, op: &mlr::Op) -> M2InkwellFnResult<BasicValueEnum<'iw>> {
        use mlr::OpDef::*;

        let op = self.m2iw.mr_ctxt.mlr.get_op_def(op);

        match *op {
            Fn(ref fn_spec) => self.build_global_function(&fn_spec.clone()),
            TraitMethod(ref trait_method) => self.build_trait_method(trait_method.clone()),
            Const(ref constant) => self.build_constant(&constant.clone()),
            Copy(place) => {
                let place_ptr = self.build_place(&place)?;
                let iw_ty = self.get_iw_ty_of_place(&place)?;
                let value = self.iw_builder.build_load(iw_ty, place_ptr, "loaded_place")?;
                Ok(value)
            }
        }
    }

    fn build_global_function(&mut self, fn_spec: &mr_fns::FnSpecialization) -> M2InkwellFnResult<BasicValueEnum<'iw>> {
        let substituted_gen_args = fn_spec
            .gen_args
            .iter()
            .map(|&arg| self.m2iw.mr_ctxt.tys.substitute_gen_vars(arg, &self.substitutions))
            .collect::<Vec<_>>();
        let substituted_env_gen_args = fn_spec
            .env_gen_args
            .iter()
            .map(|&arg| self.m2iw.mr_ctxt.tys.substitute_gen_vars(arg, &self.substitutions))
            .collect::<Vec<_>>();
        let substituted_fn_spec = mr_fns::FnSpecialization {
            fn_: fn_spec.fn_,
            gen_args: substituted_gen_args,
            env_gen_args: substituted_env_gen_args,
        };

        self.m2iw
            .functions
            .get(&substituted_fn_spec)
            .map(|fn_value| fn_value.as_global_value().as_pointer_value().as_basic_value_enum())
            .ok_or(M2InkwellFnError)
    }

    fn build_trait_method(&mut self, trait_method: mr_fns::TraitMethod) -> M2InkwellFnResult<BasicValueEnum<'iw>> {
        let concrete_impl_ty = self
            .m2iw
            .mr_ctxt
            .tys
            .substitute_gen_vars(trait_method.impl_ty, &self.substitutions);
        let fn_spec = self.m2iw.mr_ctxt.get_fn_spec_for_trait_call(
            trait_method.trait_,
            trait_method.method_idx,
            concrete_impl_ty,
        );
        self.build_global_function(&fn_spec)
    }

    fn build_call(&mut self, callable: &mlr::Op, args: &[mlr::Op]) -> M2InkwellFnResult<BasicValueEnum<'iw>> {
        let fn_ptr = self.build_op(callable)?.into_pointer_value();
        let fn_ty = self.get_fn_ty_of_loc(callable)?;

        let args = args
            .iter()
            .map(|arg| self.build_op(arg).unwrap().into())
            .collect::<Vec<_>>();

        let call_site = self.iw_builder.build_indirect_call(fn_ty, fn_ptr, &args, "call_site")?;

        call_site.try_as_basic_value().left().ok_or(M2InkwellFnError)
    }

    fn build_if(&mut self, if_: &mlr::If) -> M2InkwellFnResult<()> {
        // Build condition
        let cond_value = self.build_op(&if_.cond)?.into_int_value();

        // Create blocks for then, else, and merge
        let then_block = self.m2iw.iw_ctxt.append_basic_block(self.iw_fn, "then");
        let else_block = self.m2iw.iw_ctxt.append_basic_block(self.iw_fn, "else");
        let merge_block = self.m2iw.iw_ctxt.append_basic_block(self.iw_fn, "if_merge");

        // Build conditional branch
        self.iw_builder
            .build_conditional_branch(cond_value, then_block, else_block)?;

        // Build then block
        self.iw_builder.position_at_end(then_block);
        self.build_stmt(&if_.then)?;
        self.iw_builder.build_unconditional_branch(merge_block)?;

        // Build else block
        self.iw_builder.position_at_end(else_block);
        self.build_stmt(&if_.else_)?;
        self.iw_builder.build_unconditional_branch(merge_block)?;

        // Build merge block
        self.iw_builder.position_at_end(merge_block);
        Ok(())
    }

    fn build_loop(&mut self, body: &mlr::Stmt) -> M2InkwellFnResult<()> {
        let body_block = self.m2iw.iw_ctxt.append_basic_block(self.iw_fn, "loop");
        let after_loop = self.m2iw.iw_ctxt.append_basic_block(self.iw_fn, "loop_after");

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

    fn get_ty_as_basic_type_enum(&mut self, ty: mr_ty::Ty) -> Option<BasicTypeEnum<'iw>> {
        let ty = self.m2iw.mr_ctxt.tys.substitute_gen_vars(ty, &self.substitutions);
        self.m2iw.get_ty_as_basic_type_enum(ty)
    }

    fn get_ty_as_basic_metadata_type_enum(&mut self, ty: mr_ty::Ty) -> Option<BasicMetadataTypeEnum<'iw>> {
        let ty = self.m2iw.mr_ctxt.tys.substitute_gen_vars(ty, &self.substitutions);
        self.m2iw.get_ty_as_basic_metadata_type_enum(ty)
    }
}
