mod fns;

use std::collections::HashMap;

use inkwell::{
    AddressSpace,
    context::Context as IwContext,
    module::Module,
    targets::TargetData,
    types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::FunctionValue,
};

use crate::{
    ctxt::{self as mr_ctxt, fns as mr_fns, ty as mr_tys},
    mlr,
    mlr_lowering::fns::MlrFnLowerer,
};

#[derive(Debug)]
pub enum MlrLoweringError<'ctxt> {
    FnLowering {
        fn_inst: mr_fns::FnInst<'ctxt>,
        #[allow(unused)]
        error: fns::MlrFnLoweringError,
    },
}

pub fn mlr_to_llvm_ir<'ctxt>(
    mr_ctxt: &mut mr_ctxt::Ctxt<'ctxt>,
    mlr_fns: Vec<mlr::Fn<'ctxt>>,
    fn_insts: Vec<mr_fns::FnInst<'ctxt>>,
) -> Result<String, MlrLoweringError<'ctxt>> {
    let iw_ctxt = IwContext::create();
    let mut generator = MlrLowerer::new(&iw_ctxt, mr_ctxt, mlr_fns, fn_insts);

    generator.set_target_triple();
    generator.declare_functions();
    generator.define_functions()?;

    Ok(generator.iw_module.print_to_string().to_string())
}

struct MlrLowerer<'iw, 'a, 'ctxt> {
    iw_ctxt: &'iw IwContext,
    iw_module: Module<'iw>,

    mr_ctxt: &'a mut mr_ctxt::Ctxt<'ctxt>,
    mlr_fns: HashMap<mr_fns::Fn<'ctxt>, mlr::Fn<'ctxt>>,

    fn_insts: Vec<mr_fns::FnInst<'ctxt>>,

    types: HashMap<mr_tys::Ty<'ctxt>, AnyTypeEnum<'iw>>,
    functions: HashMap<mr_fns::FnInst<'ctxt>, FunctionValue<'iw>>,
    structs: Vec<(mr_tys::Ty<'ctxt>, inkwell::types::StructType<'iw>)>,
    enums: Vec<(mr_tys::Ty<'ctxt>, inkwell::types::StructType<'iw>)>,
}

impl<'iw, 'a, 'ctxt> MlrLowerer<'iw, 'a, 'ctxt> {
    fn new(
        iw_ctxt: &'iw IwContext,
        mr_ctxt: &'a mut mr_ctxt::Ctxt<'ctxt>,
        mlr_fns: Vec<mlr::Fn<'ctxt>>,
        fn_insts: Vec<mr_fns::FnInst<'ctxt>>,
    ) -> Self {
        let iw_module = iw_ctxt.create_module("test");
        #[allow(clippy::mutable_key_type)]
        let mlr_fns = mlr_fns.into_iter().map(|f| (f.fn_, f)).collect();
        MlrLowerer {
            iw_ctxt,
            iw_module,
            fn_insts,
            mr_ctxt,
            mlr_fns,
            types: HashMap::new(),
            functions: HashMap::new(),
            structs: Vec::new(),
            enums: Vec::new(),
        }
    }

    fn set_target_triple(&self) {
        let target_triple = inkwell::targets::TargetTriple::create("x86_64-pc-linux-gnu");
        self.iw_module.set_triple(&target_triple);
    }

    fn declare_functions(&mut self) {
        for fn_inst in self.fn_insts.clone() {
            let (param_tys, return_ty, var_args) = self.mr_ctxt.get_fn_inst_sig(fn_inst);

            let param_types: Vec<_> = param_tys
                .iter()
                .map(|&ty| self.get_ty_as_basic_metadata_type_enum(ty).unwrap())
                .collect();
            let return_type = self.get_ty_as_basic_type_enum(return_ty).unwrap();
            let iw_fn_type = return_type.fn_type(&param_types, var_args);

            let fn_name = fn_inst.to_string();
            let fn_value = self.iw_module.add_function(&fn_name, iw_fn_type, None);
            self.functions.insert(fn_inst, fn_value);
        }
    }

    fn define_functions(&mut self) -> Result<(), MlrLoweringError<'ctxt>> {
        for fn_inst in self.fn_insts.clone() {
            let Some(mlr_fn) = self.mlr_fns.get(&fn_inst.fn_).cloned() else {
                continue;
            };
            let mut fn_gen = MlrFnLowerer::new(self, fn_inst, mlr_fn);
            fn_gen
                .build_fn()
                .map_err(|error| MlrLoweringError::FnLowering { fn_inst, error })?;
        }
        Ok(())
    }

    fn get_or_define_ty(&mut self, ty: mr_tys::Ty<'ctxt>) -> Option<AnyTypeEnum<'iw>> {
        use mr_tys::{Primitive::*, TyDef::*};

        let ty = self.mr_ctxt.normalize_ty(ty);

        if self.types.contains_key(&ty) {
            return self.types.get(&ty).cloned();
        }

        let inkwell_type = match *ty.0 {
            Primitive(ref primitive_type) => match primitive_type {
                Integer32 => self.iw_ctxt.i32_type().as_any_type_enum(),
                Boolean => self.iw_ctxt.bool_type().as_any_type_enum(),
                CVoid => self.iw_ctxt.struct_type(&[], false).as_any_type_enum(),
                CChar => self.iw_ctxt.i8_type().as_any_type_enum(),
            },
            Struct { .. } => self.define_struct(ty),
            Enum { .. } => self.define_enum(ty),
            Tuple(..) => self.define_tuple_ty(ty),
            Fn { .. } | Ref(..) | RefMut(..) | Ptr(..) => {
                self.iw_ctxt.ptr_type(AddressSpace::default()).as_any_type_enum()
            }
            Closure { captures_ty, .. } => self.get_or_define_ty(captures_ty).unwrap(),
            GenVar(gen_var) => unreachable!(
                "generic type variable '{}' should be substituted before this point",
                gen_var.name()
            ),
            TraitSelf(_) => unreachable!("TraitSelf types should not occur in actual functions"),
            AssocTy { .. } => {
                let ty = self.mr_ctxt.normalize_ty(ty);
                self.get_or_define_ty(ty).unwrap()
            }
            Opaque { opaque, gen_args } => {
                let resolved = self
                    .mr_ctxt
                    .tys
                    .get_opaque_resolution(opaque)
                    .expect("opaque type must be resolved before MLR lowering");
                let subst = mr_tys::GenVarSubst::new(&opaque.gen_params, gen_args).unwrap();
                let instantiated = self.mr_ctxt.tys.substitute_gen_vars(resolved, &subst);
                self.get_or_define_ty(instantiated).unwrap()
            }
            InfVar(_) => unreachable!(),
        };

        Some(*self.types.entry(ty).or_insert(inkwell_type))
    }

    fn get_ty_as_basic_type_enum(&mut self, ty: mr_tys::Ty<'ctxt>) -> Option<BasicTypeEnum<'iw>> {
        self.get_or_define_ty(ty)?.try_into().ok()
    }

    fn get_ty_as_basic_metadata_type_enum(&mut self, ty: mr_tys::Ty<'ctxt>) -> Option<BasicMetadataTypeEnum<'iw>> {
        self.get_or_define_ty(ty)?.try_into().ok()
    }

    fn define_struct(&mut self, ty: mr_tys::Ty<'ctxt>) -> AnyTypeEnum<'iw> {
        for &(ty_2, iw_type) in &self.structs {
            if ty == ty_2 {
                return iw_type.as_any_type_enum();
            }
        }

        let iw_struct: inkwell::types::StructType<'_> = self.iw_ctxt.opaque_struct_type(&ty.to_string());
        self.types.insert(ty, iw_struct.as_any_type_enum());
        self.structs.push((ty, iw_struct));

        let mr_field_tys = self.mr_ctxt.tys.get_struct_field_tys(ty).unwrap();
        let iw_field_tys: Vec<BasicTypeEnum> = mr_field_tys
            .into_iter()
            .map(|field_ty| self.get_ty_as_basic_type_enum(field_ty).unwrap())
            .collect();

        iw_struct.set_body(&iw_field_tys, false);
        iw_struct.as_any_type_enum()
    }

    fn define_enum(&mut self, ty: mr_tys::Ty<'ctxt>) -> AnyTypeEnum<'iw> {
        for &(ty_2, iw_type) in &self.enums {
            if ty == ty_2 {
                return iw_type.as_any_type_enum();
            }
        }

        let iw_enum_struct = self.iw_ctxt.opaque_struct_type(&ty.to_string());
        self.types.insert(ty, iw_enum_struct.as_any_type_enum());
        self.enums.push((ty, iw_enum_struct));

        let discrim_bits = 32;
        let discrim_type = self.iw_ctxt.custom_width_int_type(discrim_bits).as_basic_type_enum();

        let mr_variant_tys = self.mr_ctxt.tys.get_enum_variant_tys(ty).unwrap();
        let max_variant_size = mr_variant_tys
            .into_iter()
            .map(|variant_ty| self.get_ty_as_basic_type_enum(variant_ty).unwrap())
            .map(|variant_ty| TargetData::create("").get_store_size(&variant_ty) as u32)
            .max()
            .unwrap_or(0);

        let data_array_type = self.iw_ctxt.i8_type().array_type(max_variant_size).as_basic_type_enum();

        iw_enum_struct.set_body(&[discrim_type, data_array_type], false);
        iw_enum_struct.as_any_type_enum()
    }

    fn define_tuple_ty(&mut self, ty: mr_tys::Ty<'ctxt>) -> AnyTypeEnum<'iw> {
        let iw_field_tys: Vec<BasicTypeEnum> = ty
            .tuple_field_tys()
            .unwrap()
            .iter()
            .map(|&field_ty| self.get_ty_as_basic_type_enum(field_ty).unwrap())
            .collect();
        self.iw_ctxt.struct_type(&iw_field_tys, false).as_any_type_enum()
    }

    fn get_fn(&self, fn_inst: mr_fns::FnInst<'ctxt>) -> Option<FunctionValue<'iw>> {
        for (&fn_inst_2, &fn_value) in &self.functions {
            if fn_inst == fn_inst_2 {
                return Some(fn_value);
            }
        }
        None
    }
}
