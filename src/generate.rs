mod fns;

use std::collections::HashMap;

use inkwell::{
    AddressSpace,
    context::Context as IwContext,
    module::Module,
    types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::FunctionValue,
};

use crate::{
    ctxt::{self as mr_ctxt, functions as mr_functions, types as mr_types},
    generate::fns::FnGenerator,
};

pub fn generate_llvm_ir(ctxt: &mr_ctxt::Ctxt) -> String {
    let iw_ctxt = IwContext::create();
    let mut generator = Generator::new(&iw_ctxt, ctxt);

    generator.define_types();
    generator.declare_functions();
    generator.define_functions();

    let ir = generator.iw_module.print_to_string();
    ir.to_string()
}

struct Generator<'iw, 'mr> {
    iw_ctxt: &'iw IwContext,
    iw_module: Module<'iw>,

    mr_ctxt: &'mr mr_ctxt::Ctxt,

    types: HashMap<mr_types::TypeId, AnyTypeEnum<'iw>>,
    functions: HashMap<mr_functions::FnId, FunctionValue<'iw>>,
}

impl<'iw, 'mr> Generator<'iw, 'mr> {
    pub fn new(iw_ctxt: &'iw IwContext, mr_ctxt: &'mr mr_ctxt::Ctxt) -> Self {
        let iw_module = iw_ctxt.create_module("test");
        Generator {
            iw_ctxt,
            iw_module,
            mr_ctxt,
            types: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn define_types(&mut self) {
        for (type_id, _) in self.mr_ctxt.type_registry.get_all_types() {
            self.get_or_define_type(type_id);
        }
    }

    fn get_or_define_type(&mut self, type_id: &mr_types::TypeId) -> Option<AnyTypeEnum<'iw>> {
        if self.types.contains_key(type_id) {
            return self.types.get(type_id).cloned();
        }

        let type_ = self.mr_ctxt.type_registry.get_type_by_id(type_id)?;

        let inkwell_type = match type_ {
            mr_types::Type::NamedType(name, named_type) => match named_type {
                mr_types::NamedType::Primitve(primitive_type) => match primitive_type {
                    mr_types::PrimitiveType::Integer32 => self.iw_ctxt.i32_type().as_any_type_enum(),
                    mr_types::PrimitiveType::Boolean => self.iw_ctxt.bool_type().as_any_type_enum(),
                    mr_types::PrimitiveType::Unit => self.iw_ctxt.struct_type(&[], false).as_any_type_enum(),
                },
                mr_types::NamedType::Struct(struct_id) => self.define_struct(name, type_id, struct_id),
                mr_types::NamedType::Enum(..) => todo!(),
            },
            mr_types::Type::Function { .. } => self.iw_ctxt.ptr_type(AddressSpace::default()).as_any_type_enum(),
        };

        if !self.types.contains_key(type_id) {
            self.types.insert(*type_id, inkwell_type);
        }
        self.types.get(type_id).cloned()
    }

    fn get_type_as_basic_type_enum(&mut self, type_id: &mr_types::TypeId) -> Option<BasicTypeEnum<'iw>> {
        self.get_or_define_type(type_id)?.try_into().ok()
    }

    fn get_type_as_basic_metadata_type_enum(
        &mut self,
        type_id: &mr_types::TypeId,
    ) -> Option<BasicMetadataTypeEnum<'iw>> {
        self.get_or_define_type(type_id)?.try_into().ok()
    }

    fn define_struct(
        &mut self,
        name: &str,
        type_id: &mr_types::TypeId,
        struct_id: &mr_types::StructId,
    ) -> AnyTypeEnum<'iw> {
        let iw_struct: inkwell::types::StructType<'_> = self.iw_ctxt.opaque_struct_type(name);
        self.types.insert(*type_id, iw_struct.as_any_type_enum());

        let struct_def = self.mr_ctxt.type_registry.get_struct_definition(struct_id).unwrap();
        let field_types: Vec<BasicTypeEnum> = struct_def
            .fields
            .iter()
            .map(|field| self.get_type_as_basic_type_enum(&field.type_id).unwrap())
            .collect();

        iw_struct.set_body(&field_types, false);
        iw_struct.as_any_type_enum()
    }

    fn declare_functions(&mut self) {
        for fn_id in self.mr_ctxt.function_registry.get_all_functions() {
            let signature = self.mr_ctxt.function_registry.get_signature_by_id(fn_id).unwrap();
            let return_type: BasicTypeEnum = self.get_type_as_basic_type_enum(&signature.return_type).unwrap();
            let param_types: Vec<_> = signature
                .parameters
                .iter()
                .map(|param| self.get_type_as_basic_metadata_type_enum(&param.type_).unwrap())
                .collect();
            let fn_type = return_type.fn_type(&param_types, false);
            let fn_value = self.iw_module.add_function(&signature.name, fn_type, None);
            self.functions.insert(*fn_id, fn_value);
        }
    }

    pub fn define_functions(&mut self) {
        for fn_id in self.mr_ctxt.function_registry.get_all_functions() {
            let Some(mut fn_gen) = FnGenerator::new(self, *fn_id) else {
                continue;
            };
            if fn_gen.define_function().is_err() {
                let fn_name = self.mr_ctxt.function_registry.get_function_name_by_id(fn_id).unwrap();
                eprintln!("Failed to define function {fn_name}");
            }
        }
    }
}
