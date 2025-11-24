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
    ctxt::{
        self as mr_ctxt,
        fns::{self as mr_fns, InstantiatedFn},
        ty as mr_tys,
    },
    generate::fns::FnGenerator,
};

pub fn generate_llvm_ir(ctxt: &mr_ctxt::Ctxt, inst: Vec<InstantiatedFn>) -> String {
    let iw_ctxt = IwContext::create();
    let mut generator = Generator::new(&iw_ctxt, ctxt, inst);

    generator.set_target_triple();
    generator.declare_functions();
    generator.define_functions();

    generator.iw_module.print_to_string().to_string()
}

struct Generator<'iw, 'mr> {
    iw_ctxt: &'iw IwContext,
    iw_module: Module<'iw>,

    mr_ctxt: &'mr mr_ctxt::Ctxt,

    inst: Vec<InstantiatedFn>,

    types: HashMap<mr_tys::Ty, AnyTypeEnum<'iw>>,
    functions: HashMap<mr_fns::InstantiatedFn, FunctionValue<'iw>>,
}

impl<'iw, 'mr> Generator<'iw, 'mr> {
    pub fn new(iw_ctxt: &'iw IwContext, mr_ctxt: &'mr mr_ctxt::Ctxt, inst: Vec<InstantiatedFn>) -> Self {
        let iw_module = iw_ctxt.create_module("test");
        Generator {
            iw_ctxt,
            iw_module,
            inst,
            mr_ctxt,
            types: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn set_target_triple(&self) {
        let target_triple = inkwell::targets::TargetTriple::create("x86_64-pc-linux-gnu");
        self.iw_module.set_triple(&target_triple);
    }

    fn get_or_define_ty(
        &mut self,
        ty: &mr_tys::Ty,
        substitutions: &HashMap<String, mr_tys::Ty>,
    ) -> Option<AnyTypeEnum<'iw>> {
        use mr_tys::{Named::*, Primitive::*, TyDef::*};

        if self.types.contains_key(ty) {
            return self.types.get(ty).cloned();
        }

        let type_ = self.mr_ctxt.tys.get_ty_def(ty)?;

        let inkwell_type = match type_ {
            Named(name, named_type) => match named_type {
                Primitve(primitive_type) => match primitive_type {
                    Integer32 => self.iw_ctxt.i32_type().as_any_type_enum(),
                    Boolean => self.iw_ctxt.bool_type().as_any_type_enum(),
                    Unit => self.iw_ctxt.struct_type(&[], false).as_any_type_enum(),
                },
                Struct(struct_) => self.define_struct(name, ty, struct_),
                Enum(enum_) => self.define_enum(name, enum_),
            },
            Fn { .. } | Ref(..) => self.iw_ctxt.ptr_type(AddressSpace::default()).as_any_type_enum(),
            Undef => unreachable!("type_ should not be Undef at this point"),
            Alias(_) => unreachable!("type_ should be canonicalized before this point"),
            GenVar(name) => {
                let substituted_ty = substitutions
                    .get(name)
                    .expect("No substitution found for generic variable");
                let output = self.get_or_define_ty(substituted_ty, substitutions)?;
                return Some(output);
            }
        };

        if !self.types.contains_key(ty) {
            self.types.insert(*ty, inkwell_type);
        }
        self.types.get(ty).cloned()
    }

    fn get_ty_as_basic_type_enum(
        &mut self,
        ty: &mr_tys::Ty,
        substitutions: &HashMap<String, mr_tys::Ty>,
    ) -> Option<BasicTypeEnum<'iw>> {
        self.get_or_define_ty(ty, substitutions)?.try_into().ok()
    }

    fn get_ty_as_basic_metadata_type_enum(
        &mut self,
        ty: &mr_tys::Ty,
        substitutions: &HashMap<String, mr_tys::Ty>,
    ) -> Option<BasicMetadataTypeEnum<'iw>> {
        self.get_or_define_ty(ty, substitutions)?.try_into().ok()
    }

    fn define_struct(&mut self, name: &str, ty: &mr_tys::Ty, struct_: &mr_tys::Struct) -> AnyTypeEnum<'iw> {
        let iw_struct: inkwell::types::StructType<'_> = self.iw_ctxt.opaque_struct_type(name);
        self.types.insert(*ty, iw_struct.as_any_type_enum());

        let struct_def = self.mr_ctxt.tys.get_struct_def(struct_).unwrap();
        let field_types: Vec<BasicTypeEnum> = struct_def
            .fields
            .iter()
            .map(|field| self.get_ty_as_basic_type_enum(&field.ty, &HashMap::new()).unwrap())
            .collect();

        iw_struct.set_body(&field_types, false);
        iw_struct.as_any_type_enum()
    }

    fn define_enum(&mut self, name: &str, enum_: &mr_tys::Enum) -> AnyTypeEnum<'iw> {
        let enum_def = self.mr_ctxt.tys.get_enum_def(enum_).unwrap();

        let discrim_bits = 32;
        let discrim_type = self.iw_ctxt.custom_width_int_type(discrim_bits);

        let max_variant_size = enum_def
            .variants
            .iter()
            .map(|variant| {
                let variant_type: StructType = self
                    .get_ty_as_basic_type_enum(&variant.ty, &HashMap::new())
                    .unwrap()
                    .try_into()
                    .unwrap();

                TargetData::create("").get_store_size(&variant_type)
            })
            .max()
            .unwrap_or(0);

        let data_array_type = self.iw_ctxt.i8_type().array_type(max_variant_size as u32);

        let enum_struct = self.iw_ctxt.opaque_struct_type(name);
        enum_struct.set_body(
            &[discrim_type.as_basic_type_enum(), data_array_type.as_basic_type_enum()],
            false,
        );
        enum_struct.as_any_type_enum()
    }

    fn declare_functions(&mut self) {
        for inst_fn in self.inst.clone() {
            let signature = self.mr_ctxt.fns.get_sig(&inst_fn.fn_).unwrap();
            let substitutions: HashMap<String, mr_tys::Ty> = signature
                .gen_params
                .iter()
                .zip(&inst_fn.gen_args)
                .map(|(gp, ga)| (gp.name.clone(), *ga))
                .collect();
            let param_types: Vec<_> = signature
                .params
                .iter()
                .map(|param| {
                    self.get_or_define_ty(&param.ty, &substitutions)
                        .unwrap()
                        .try_into()
                        .unwrap()
                })
                .collect();
            let return_type: BasicTypeEnum = self
                .get_or_define_ty(&signature.return_ty, &substitutions)
                .unwrap()
                .try_into()
                .unwrap();
            let fn_type = return_type.fn_type(&param_types, false);
            let full_name = if signature.gen_params.is_empty() {
                signature.name.to_string()
            } else {
                format!(
                    "{}<{}>",
                    signature.name,
                    inst_fn
                        .gen_args
                        .iter()
                        .map(|ty| self.mr_ctxt.tys.get_string_rep(ty))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            };
            let fn_value = self.iw_module.add_function(&full_name, fn_type, None);
            self.functions.insert(inst_fn.clone(), fn_value);
        }
    }

    pub fn define_functions(&mut self) {
        for inst_fn in self.inst.clone() {
            let Some(mut fn_gen) = FnGenerator::new(self, inst_fn.clone()) else {
                continue;
            };
            if fn_gen.define_fn().is_err() {
                let fn_name = self.mr_ctxt.fns.get_fn_name(&inst_fn.fn_).unwrap();
                eprintln!("Failed to define function {fn_name}");
            }
        }
    }
}
