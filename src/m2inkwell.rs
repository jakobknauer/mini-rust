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
    m2inkwell::fns::M2InkwellFn,
};

pub fn mlr_to_llvm_ir(mr_ctxt: &mut mr_ctxt::Ctxt, fn_specs: Vec<mr_fns::FnSpecialization>) -> String {
    let iw_ctxt = IwContext::create();
    let mut generator = M2Inkwell::new(&iw_ctxt, mr_ctxt, fn_specs);

    generator.set_target_triple();
    generator.declare_functions();
    generator.define_functions();

    generator.iw_module.print_to_string().to_string()
}

struct M2Inkwell<'iw, 'mr> {
    iw_ctxt: &'iw IwContext,
    iw_module: Module<'iw>,

    mr_ctxt: &'mr mut mr_ctxt::Ctxt,

    fn_specs: Vec<mr_fns::FnSpecialization>,

    types: HashMap<mr_tys::Ty, AnyTypeEnum<'iw>>,
    functions: HashMap<mr_fns::FnSpecialization, FunctionValue<'iw>>,
    structs: Vec<(mr_tys::Ty, inkwell::types::StructType<'iw>)>,
    enums: Vec<(mr_tys::Ty, inkwell::types::StructType<'iw>)>,
}

impl<'iw, 'mr> M2Inkwell<'iw, 'mr> {
    fn new(iw_ctxt: &'iw IwContext, mr_ctxt: &'mr mut mr_ctxt::Ctxt, fn_specs: Vec<mr_fns::FnSpecialization>) -> Self {
        let iw_module = iw_ctxt.create_module("test");
        M2Inkwell {
            iw_ctxt,
            iw_module,
            fn_specs,
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
        for fn_spec in self.fn_specs.clone() {
            let sig = self.mr_ctxt.get_specialized_fn_sig(&fn_spec);

            let param_types: Vec<_> = sig
                .params
                .iter()
                .map(|param| self.get_ty_as_basic_metadata_type_enum(param.ty).unwrap())
                .collect();
            let return_type = self.get_ty_as_basic_type_enum(sig.return_ty).unwrap();
            let iw_fn_type = return_type.fn_type(&param_types, sig.var_args);

            let fn_name = self.mr_ctxt.get_fn_spec_name(&fn_spec);
            let fn_value = self.iw_module.add_function(&fn_name, iw_fn_type, None);
            self.functions.insert(fn_spec, fn_value);
        }
    }

    fn define_functions(&mut self) {
        for fn_spec in self.fn_specs.clone() {
            let Some(mut fn_gen) = M2InkwellFn::new(self, fn_spec.clone()) else {
                continue;
            };
            if fn_gen.build_fn().is_err() {
                let fn_name = self.mr_ctxt.get_fn_spec_name(&fn_spec);
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
                Unit | CVoid => self.iw_ctxt.struct_type(&[], false).as_any_type_enum(),
                CChar => self.iw_ctxt.i8_type().as_any_type_enum(),
            },
            Struct { .. } => self.define_struct(ty),
            Enum { .. } => self.define_enum(ty),
            Fn { .. } | Ref(..) | Ptr(..) => self.iw_ctxt.ptr_type(AddressSpace::default()).as_any_type_enum(),
            Alias(_) => unreachable!("type_ should be canonicalized before this point"),
            GenVar(gen_var) => unreachable!(
                "generic type variable '{}' should be substituted before this point",
                gen_var.0
            ),
            TraitSelf(_) => unreachable!("TraitSelf types should not occur in actual functions"),
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

    fn get_fn(&self, fn_spec: &mr_fns::FnSpecialization) -> Option<FunctionValue<'iw>> {
        for (fn_spec_2, &fn_value) in &self.functions {
            if self.mr_ctxt.fn_specs_eq(fn_spec, fn_spec_2) {
                return Some(fn_value);
            }
        }
        None
    }
}
