use inkwell::{
    AddressSpace, IntPredicate,
    targets::TargetData,
    values::{BasicValue, BasicValueEnum, IntValue},
};

use crate::{
    ctxt::{fns as mr_fns, language_items},
    mlr,
};

use super::{MlrFnLowerer, MlrFnLoweringError, MlrFnLoweringResult};

impl<'parent, 'iw, 'a, 'ctxt> MlrFnLowerer<'parent, 'iw, 'a, 'ctxt> {
    pub(super) fn try_build_intrinsic_call(
        &mut self,
        fn_inst: mr_fns::FnInst<'ctxt>,
        args: &[mlr::Op<'ctxt>],
    ) -> MlrFnLoweringResult<Option<BasicValueEnum<'iw>>> {
        let lang = &self.parent.mr_ctxt.language_items;
        if Some(fn_inst.fn_) == lang.size_of {
            return Ok(Some(self.build_size_of_intrinsic(fn_inst)?));
        }
        if Some(fn_inst.fn_) == lang.ptr_offset {
            return Ok(Some(self.build_ptr_offset_intrinsic(fn_inst, args)?));
        }
        Ok(None)
    }

    fn build_size_of_intrinsic(&mut self, fn_inst: mr_fns::FnInst<'ctxt>) -> MlrFnLoweringResult<BasicValueEnum<'iw>> {
        let gen_args = self.substitute_slice(fn_inst.gen_args);
        let ty = gen_args[0];
        let iw_ty = self.get_ty_as_basic_type_enum(ty).ok_or(MlrFnLoweringError)?;
        let size = TargetData::create("").get_store_size(&iw_ty) as u32;
        let int_ty = self.parent.iw_ctxt.i32_type();
        Ok(int_ty.const_int(size as u64, false).as_basic_value_enum())
    }

    pub(super) fn build_binary_prim(
        &mut self,
        op: language_items::BinaryPrimOp,
        lhs: mlr::Op<'ctxt>,
        rhs: mlr::Op<'ctxt>,
    ) -> MlrFnLoweringResult<BasicValueEnum<'iw>> {
        use language_items::BinaryPrimOp::*;

        match op {
            EqUnit => {
                return Ok(self
                    .parent
                    .iw_ctxt
                    .bool_type()
                    .const_int(1, false)
                    .as_basic_value_enum());
            }
            NeUnit => {
                return Ok(self
                    .parent
                    .iw_ctxt
                    .bool_type()
                    .const_int(0, false)
                    .as_basic_value_enum());
            }
            _ => {}
        }

        let (lhs, rhs) = self.build_int_pair(lhs, rhs)?;

        let op = match op {
            AddI32 => self.iw_builder.build_int_add(lhs, rhs, "")?,
            SubI32 => self.iw_builder.build_int_sub(lhs, rhs, "")?,
            MulI32 => self.iw_builder.build_int_mul(lhs, rhs, "")?,
            DivI32 => self.iw_builder.build_int_signed_div(lhs, rhs, "")?,
            RemI32 => self.iw_builder.build_int_signed_rem(lhs, rhs, "")?,
            EqI32 | EqBool => self.iw_builder.build_int_compare(IntPredicate::EQ, lhs, rhs, "")?,
            NeI32 | NeBool => self.iw_builder.build_int_compare(IntPredicate::NE, lhs, rhs, "")?,
            BitOrBool | BitOrI32 => self.iw_builder.build_or(lhs, rhs, "")?,
            BitAndBool | BitAndI32 => self.iw_builder.build_and(lhs, rhs, "")?,
            LtI32 => self.iw_builder.build_int_compare(IntPredicate::SLT, lhs, rhs, "")?,
            GtI32 => self.iw_builder.build_int_compare(IntPredicate::SGT, lhs, rhs, "")?,
            LeI32 => self.iw_builder.build_int_compare(IntPredicate::SLE, lhs, rhs, "")?,
            GeI32 => self.iw_builder.build_int_compare(IntPredicate::SGE, lhs, rhs, "")?,
            EqUnit | NeUnit => unreachable!(),
        };

        Ok(op.as_basic_value_enum())
    }

    pub(super) fn build_unary_prim(
        &mut self,
        op: language_items::UnaryPrimOp,
        operand: mlr::Op<'ctxt>,
    ) -> MlrFnLoweringResult<BasicValueEnum<'iw>> {
        use language_items::UnaryPrimOp::*;
        let operand = self.build_op(operand)?.into_int_value();
        let result = match op {
            NegI32 => self.iw_builder.build_int_neg(operand, "")?,
            NotBool => self.iw_builder.build_not(operand, "")?,
        };
        Ok(result.as_basic_value_enum())
    }

    fn build_int_pair(
        &mut self,
        lhs: mlr::Op<'ctxt>,
        rhs: mlr::Op<'ctxt>,
    ) -> MlrFnLoweringResult<(IntValue<'iw>, IntValue<'iw>)> {
        let lhs = self.build_op(lhs)?.into_int_value();
        let rhs = self.build_op(rhs)?.into_int_value();
        Ok((lhs, rhs))
    }

    fn build_ptr_offset_intrinsic(
        &mut self,
        fn_inst: mr_fns::FnInst<'ctxt>,
        args: &[mlr::Op<'ctxt>],
    ) -> MlrFnLoweringResult<BasicValueEnum<'iw>> {
        let &[ptr, offset] = args else {
            return Err(MlrFnLoweringError);
        };

        let env_gen_args = self.substitute_slice(fn_inst.env_gen_args);
        let pointee_ty = env_gen_args[0];
        let iw_pointee_ty = self.get_ty_as_basic_type_enum(pointee_ty).ok_or(MlrFnLoweringError)?;

        let ptr_ty = self.parent.iw_ctxt.ptr_type(AddressSpace::default());

        let self_ref = self.build_op(ptr)?.into_pointer_value();
        let ptr = self
            .iw_builder
            .build_load(ptr_ty, self_ref, "deref_self")?
            .into_pointer_value();

        let offset = self.build_op(offset)?.into_int_value();

        let result = unsafe { self.iw_builder.build_gep(iw_pointee_ty, ptr, &[offset], "ptr_offset")? };
        Ok(result.as_basic_value_enum())
    }
}
