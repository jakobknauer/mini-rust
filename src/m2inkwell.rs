mod fns;

use std::collections::HashMap;

use inkwell::{
    AddressSpace,
    context::Context as IwContext,
    module::Module,
    targets::TargetData,
    types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType},
    values::FunctionValue,
};

use crate::{
    ctxt::{self as mr_ctxt, fns as mr_fns, ty as mr_tys},
    m2inkwell::fns::M2InkwellFn,
};

pub fn mlr_to_llvm_ir(ctxt: &mut mr_ctxt::Ctxt, fn_specs: Vec<mr_fns::FnSpecialization>) -> String {
    let iw_ctxt = IwContext::create();
    let mut generator = M2Inkwell::new(&iw_ctxt, ctxt, fn_specs);

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
}

impl<'iw, 'mr> M2Inkwell<'iw, 'mr> {
    pub fn new(
        iw_ctxt: &'iw IwContext,
        mr_ctxt: &'mr mut mr_ctxt::Ctxt,
        fn_specs: Vec<mr_fns::FnSpecialization>,
    ) -> Self {
        let iw_module = iw_ctxt.create_module("test");
        M2Inkwell {
            iw_ctxt,
            iw_module,
            fn_specs,
            mr_ctxt,
            types: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn set_target_triple(&self) {
        let target_triple = inkwell::targets::TargetTriple::create("x86_64-pc-linux-gnu");
        self.iw_module.set_triple(&target_triple);
    }

    fn get_or_define_ty(&mut self, ty: &mr_tys::Ty) -> Option<AnyTypeEnum<'iw>> {
        use mr_tys::{Primitive::*, TyDef::*};

        if self.types.contains_key(ty) {
            return self.types.get(ty).cloned();
        }

        let type_ = self.mr_ctxt.tys.get_ty_def(ty)?;

        let inkwell_type = match *type_ {
            Primitve(ref primitive_type) => match primitive_type {
                Integer32 => self.iw_ctxt.i32_type().as_any_type_enum(),
                Boolean => self.iw_ctxt.bool_type().as_any_type_enum(),
                Unit => self.iw_ctxt.struct_type(&[], false).as_any_type_enum(),
            },
            Struct(struct_) => self.define_struct(ty, &struct_),
            Enum(enum_) => self.define_enum(&enum_),
            Fn { .. } | Ref(..) => self.iw_ctxt.ptr_type(AddressSpace::default()).as_any_type_enum(),
            Alias(_) => unreachable!("type_ should be canonicalized before this point"),
            GenVar(_) => unreachable!("generic type variables should be substituted before this point"),
            InstantiatedStruct { struct_, ref gen_args } => {
                self.define_instantiated_struct(*ty, &struct_, &gen_args.clone())
            }
        };

        if !self.types.contains_key(ty) {
            self.types.insert(*ty, inkwell_type);
        }
        self.types.get(ty).cloned()
    }

    fn get_ty_as_basic_type_enum(&mut self, ty: &mr_tys::Ty) -> Option<BasicTypeEnum<'iw>> {
        self.get_or_define_ty(ty)?.try_into().ok()
    }

    fn get_ty_as_basic_metadata_type_enum(&mut self, ty: &mr_tys::Ty) -> Option<BasicMetadataTypeEnum<'iw>> {
        self.get_or_define_ty(ty)?.try_into().ok()
    }

    fn define_struct(&mut self, ty: &mr_tys::Ty, struct_: &mr_tys::Struct) -> AnyTypeEnum<'iw> {
        let struct_def = self.mr_ctxt.tys.get_struct_def(struct_).unwrap().clone();

        let iw_struct: inkwell::types::StructType<'_> = self.iw_ctxt.opaque_struct_type(&struct_def.name);
        self.types.insert(*ty, iw_struct.as_any_type_enum());

        let field_types: Vec<BasicTypeEnum> = struct_def
            .fields
            .iter()
            .map(|field| self.get_ty_as_basic_type_enum(&field.ty).unwrap())
            .collect();

        iw_struct.set_body(&field_types, false);
        iw_struct.as_any_type_enum()
    }

    fn define_instantiated_struct(
        &mut self,
        ty: mr_tys::Ty,
        struct_: &mr_tys::Struct,
        gen_args: &[mr_tys::Ty],
    ) -> AnyTypeEnum<'iw> {
        let struct_def = self.mr_ctxt.tys.get_struct_def(struct_).unwrap().clone();

        let instantiated_name = format!(
            "{}<{}>",
            struct_def.name,
            gen_args
                .iter()
                .map(|arg| self.mr_ctxt.tys.get_string_rep(arg))
                .collect::<Vec<_>>()
                .join(", ")
        );

        let iw_struct: inkwell::types::StructType<'_> = self.iw_ctxt.opaque_struct_type(&instantiated_name);
        self.types.insert(ty, iw_struct.as_any_type_enum());

        let field_types = self
            .mr_ctxt
            .tys
            .get_instantiated_struct_field_tys(struct_, gen_args)
            .unwrap();
        let field_types: Vec<BasicTypeEnum> = field_types
            .iter()
            .map(|field_ty| self.get_ty_as_basic_type_enum(field_ty).unwrap())
            .collect();

        iw_struct.set_body(&field_types, false);
        iw_struct.as_any_type_enum()
    }

    fn define_enum(&mut self, enum_: &mr_tys::Enum) -> AnyTypeEnum<'iw> {
        let enum_def = self.mr_ctxt.tys.get_enum_def(enum_).unwrap().clone();

        let discrim_bits = 32;
        let discrim_type = self.iw_ctxt.custom_width_int_type(discrim_bits);

        let max_variant_size = enum_def
            .variants
            .iter()
            .map(|variant| {
                let variant_type: StructType = self.get_ty_as_basic_type_enum(&variant.ty).unwrap().try_into().unwrap();
                TargetData::create("").get_store_size(&variant_type)
            })
            .max()
            .unwrap_or(0);

        let data_array_type = self.iw_ctxt.i8_type().array_type(max_variant_size as u32);

        let enum_struct = self.iw_ctxt.opaque_struct_type(&enum_def.name);
        enum_struct.set_body(
            &[discrim_type.as_basic_type_enum(), data_array_type.as_basic_type_enum()],
            false,
        );
        enum_struct.as_any_type_enum()
    }

    fn declare_functions(&mut self) {
        for fn_spec in self.fn_specs.clone() {
            let sig = self.mr_ctxt.get_specialized_fn_sig(&fn_spec);

            let param_types: Vec<_> = sig
                .params
                .iter()
                .map(|param| self.get_ty_as_basic_metadata_type_enum(&param.ty).unwrap())
                .collect();
            let return_type = self.get_ty_as_basic_type_enum(&sig.return_ty).unwrap();
            let iw_fn_type = return_type.fn_type(&param_types, false);

            let fn_name = self.mr_ctxt.get_fn_spec_name(&fn_spec);
            let fn_value = self.iw_module.add_function(&fn_name, iw_fn_type, None);
            self.functions.insert(fn_spec, fn_value);
        }
    }

    pub fn define_functions(&mut self) {
        for fn_spec in self.fn_specs.clone() {
            let Some(mut fn_gen) = M2InkwellFn::new(self, fn_spec.clone()) else {
                continue;
            };
            if fn_gen.define_fn().is_err() {
                let fn_name = self.mr_ctxt.get_fn_spec_name(&fn_spec);
                eprintln!("Failed to define function {fn_name}");
            }
        }
    }
}
