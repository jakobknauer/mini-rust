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
    mlr_lowering::fns::MlrFnLowerer,
};

pub fn mlr_to_llvm_ir(mr_ctxt: &mut mr_ctxt::Ctxt, fn_insts: Vec<mr_fns::FnInst>) -> String {
    let iw_ctxt = IwContext::create();
    let mut generator = MlrLowerer::new(&iw_ctxt, mr_ctxt, fn_insts);

    generator.set_target_triple();
    generator.declare_functions();
    generator.define_functions();

    generator.iw_module.print_to_string().to_string()
}

struct MlrLowerer<'iw, 'mr> {
    iw_ctxt: &'iw IwContext,
    iw_module: Module<'iw>,

    mr_ctxt: &'mr mut mr_ctxt::Ctxt,

    fn_insts: Vec<mr_fns::FnInst>,

    types: HashMap<mr_tys::Ty, AnyTypeEnum<'iw>>,
    functions: HashMap<mr_fns::FnInst, FunctionValue<'iw>>,
    structs: Vec<(mr_tys::Ty, inkwell::types::StructType<'iw>)>,
    enums: Vec<(mr_tys::Ty, inkwell::types::StructType<'iw>)>,
}

impl<'iw, 'mr> MlrLowerer<'iw, 'mr> {
    fn new(iw_ctxt: &'iw IwContext, mr_ctxt: &'mr mut mr_ctxt::Ctxt, fn_insts: Vec<mr_fns::FnInst>) -> Self {
        let iw_module = iw_ctxt.create_module("test");
        MlrLowerer {
            iw_ctxt,
            iw_module,
            fn_insts,
            mr_ctxt,
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
            let sig = self.mr_ctxt.get_fn_inst_sig(fn_inst);

            let param_types: Vec<_> = sig
                .params
                .iter()
                .map(|param| self.get_ty_as_basic_metadata_type_enum(param.ty).unwrap())
                .collect();
            let return_type = self.get_ty_as_basic_type_enum(sig.return_ty).unwrap();
            let iw_fn_type = return_type.fn_type(&param_types, sig.var_args);

            let fn_name = self.mr_ctxt.get_fn_inst_name(fn_inst);
            let fn_value = self.iw_module.add_function(&fn_name, iw_fn_type, None);
            self.functions.insert(fn_inst, fn_value);
        }
    }

    fn define_functions(&mut self) {
        for fn_inst in self.fn_insts.clone() {
            let Some(mut fn_gen) = MlrFnLowerer::new(self, fn_inst) else {
                continue;
            };
            if fn_gen.build_fn().is_err() {
                let fn_name = self.mr_ctxt.get_fn_inst_name(fn_inst);
                eprintln!("Failed to define function {fn_name}");
            }
        }
    }

    fn get_or_define_ty(&mut self, ty: mr_tys::Ty) -> Option<AnyTypeEnum<'iw>> {
        use mr_tys::{Primitive::*, TyDef::*};

        if self.types.contains_key(&ty) {
            // TODO real canonicalization, such that the lookup actually considers true type equivalence
            // For now, structs and enums contain separate mechanisms to avoid duplicate definitions
            return self.types.get(&ty).cloned();
        }

        let type_ = self.mr_ctxt.tys.get_ty_def(ty)?;

        let inkwell_type = match *type_ {
            Primitive(ref primitive_type) => match primitive_type {
                Integer32 => self.iw_ctxt.i32_type().as_any_type_enum(),
                Boolean => self.iw_ctxt.bool_type().as_any_type_enum(),
                CVoid => self.iw_ctxt.struct_type(&[], false).as_any_type_enum(),
                CChar => self.iw_ctxt.i8_type().as_any_type_enum(),
            },
            Struct { .. } => self.define_struct(ty),
            Enum { .. } => self.define_enum(ty),
            Tuple(..) => self.define_tuple_ty(ty),
            Fn { .. } | Ref(..) | Ptr(..) => self.iw_ctxt.ptr_type(AddressSpace::default()).as_any_type_enum(),
            Closure { captures_ty, .. } => self.get_or_define_ty(captures_ty).unwrap(),
            Alias(_) => unreachable!("type_ should be canonicalized before this point"),
            GenVar(gen_var) => unreachable!(
                "generic type variable '{}' should be substituted before this point",
                self.mr_ctxt.tys.get_gen_var_name(gen_var)
            ),
            TraitSelf(_) => unreachable!("TraitSelf types should not occur in actual functions"),
            AssocTy { .. } => {
                let ty = self.mr_ctxt.normalize_ty(ty);
                self.get_or_define_ty(ty).unwrap()
            }
        };

        Some(*self.types.entry(ty).or_insert(inkwell_type))
    }

    fn get_ty_as_basic_type_enum(&mut self, ty: mr_tys::Ty) -> Option<BasicTypeEnum<'iw>> {
        self.get_or_define_ty(ty)?.try_into().ok()
    }

    fn get_ty_as_basic_metadata_type_enum(&mut self, ty: mr_tys::Ty) -> Option<BasicMetadataTypeEnum<'iw>> {
        self.get_or_define_ty(ty)?.try_into().ok()
    }

    fn define_struct(&mut self, ty: mr_tys::Ty) -> AnyTypeEnum<'iw> {
        for &(ty_2, iw_type) in &self.structs {
            if self.mr_ctxt.tys.tys_eq(ty, ty_2) {
                return iw_type.as_any_type_enum();
            }
        }

        let name = self.mr_ctxt.tys.get_string_rep(ty);
        let iw_struct: inkwell::types::StructType<'_> = self.iw_ctxt.opaque_struct_type(&name);
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

    fn define_enum(&mut self, ty: mr_tys::Ty) -> AnyTypeEnum<'iw> {
        for &(ty_2, iw_type) in &self.enums {
            if self.mr_ctxt.tys.tys_eq(ty, ty_2) {
                return iw_type.as_any_type_enum();
            }
        }

        let name = self.mr_ctxt.tys.get_string_rep(ty);
        let iw_enum_struct = self.iw_ctxt.opaque_struct_type(&name);
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

    fn define_tuple_ty(&mut self, ty: mr_tys::Ty) -> AnyTypeEnum<'iw> {
        let mr_field_tys = self.mr_ctxt.tys.get_tuple_field_tys(ty).unwrap().to_vec();
        let iw_field_tys: Vec<BasicTypeEnum> = mr_field_tys
            .into_iter()
            .map(|field_ty| self.get_ty_as_basic_type_enum(field_ty).unwrap())
            .collect();
        self.iw_ctxt.struct_type(&iw_field_tys, false).as_any_type_enum()
    }

    fn get_fn(&self, fn_inst: mr_fns::FnInst) -> Option<FunctionValue<'iw>> {
        for (&fn_inst_2, &fn_value) in &self.functions {
            if self.mr_ctxt.fn_insts_eq(fn_inst, fn_inst_2) {
                return Some(fn_value);
            }
        }
        None
    }
}
