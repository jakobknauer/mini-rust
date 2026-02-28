use inkwell::{
    AddressSpace,
    targets::TargetData,
    values::{BasicValue, BasicValueEnum},
};

use crate::ctxt::{fns as mr_fns, mlr};

use super::{MlrFnLowerer, MlrLoweringError, MlrLoweringResult};

impl<'a, 'iw, 'mr> MlrFnLowerer<'a, 'iw, 'mr> {
    pub(super) fn try_build_intrinsic_call(
        &mut self,
        fn_inst: mr_fns::FnInst,
        args: &[mlr::Op],
    ) -> MlrLoweringResult<Option<BasicValueEnum<'iw>>> {
        let lang = &self.parent.mr_ctxt.language_items;
        if Some(fn_inst.fn_) == lang.size_of {
            return Ok(Some(self.build_size_of_intrinsic(fn_inst)?));
        }
        if Some(fn_inst.fn_) == lang.ptr_offset {
            return Ok(Some(self.build_ptr_offset_intrinsic(fn_inst, args)?));
        }
        Ok(None)
    }

    fn build_size_of_intrinsic(&mut self, fn_inst: mr_fns::FnInst) -> MlrLoweringResult<BasicValueEnum<'iw>> {
        let gen_args = self.substitute_slice(fn_inst.gen_args);
        let ty = self.parent.mr_ctxt.tys.get_ty_slice(gen_args)[0];
        let iw_ty = self.get_ty_as_basic_type_enum(ty).ok_or(MlrLoweringError)?;
        let size = TargetData::create("").get_store_size(&iw_ty) as u32;
        let int_ty = self.parent.iw_ctxt.i32_type();
        Ok(int_ty.const_int(size as u64, false).as_basic_value_enum())
    }

    fn build_ptr_offset_intrinsic(
        &mut self,
        fn_inst: mr_fns::FnInst,
        args: &[mlr::Op],
    ) -> MlrLoweringResult<BasicValueEnum<'iw>> {
        let &[ptr, offset] = args else {
            return Err(MlrLoweringError);
        };

        let env_gen_args = self.substitute_slice(fn_inst.env_gen_args);
        let pointee_ty = self.parent.mr_ctxt.tys.get_ty_slice(env_gen_args)[0];
        let iw_pointee_ty = self.get_ty_as_basic_type_enum(pointee_ty).ok_or(MlrLoweringError)?;

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
