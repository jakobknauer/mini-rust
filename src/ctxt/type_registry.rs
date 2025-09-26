use std::collections::HashMap;

use crate::ctxt::types::*;

pub struct TypeRegistry {
    types: HashMap<TypeId, Type>,
    structs: HashMap<StructId, StructDefinition>,
    enums: HashMap<EnumId, EnumDefinition>,

    named_types: HashMap<String, TypeId>,

    next_type_id: TypeId,
    next_struct_id: StructId,
    next_enum_id: EnumId,
}

impl TypeRegistry {
    pub fn new() -> TypeRegistry {
        TypeRegistry {
            types: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),
            named_types: HashMap::new(),
            next_type_id: TypeId(0),
            next_struct_id: StructId(0),
            next_enum_id: EnumId(0),
        }
    }

    fn register_type(&mut self, type_: Type) -> TypeId {
        let type_id = self.next_type_id;
        self.next_type_id.0 += 1;
        self.types.insert(type_id, type_);
        type_id
    }

    fn register_named_type(&mut self, name: &str, type_: Type) -> Result<TypeId, ()> {
        if self.named_types.contains_key(name) {
            Err(())
        } else {
            let type_id = self.register_type(type_);
            self.named_types.insert(name.to_string(), type_id);
            Ok(type_id)
        }
    }

    pub fn register_primitive_types(&mut self) -> Result<(), ()> {
        self.register_named_type("i32", Type::NamedType(NamedType::Primitve(PrimitiveType::Integer32)))?;
        self.register_named_type("bool", Type::NamedType(NamedType::Primitve(PrimitiveType::Boolean)))?;
        self.register_named_type("()", Type::NamedType(NamedType::Primitve(PrimitiveType::Unit)))?;
        Ok(())
    }

    pub fn register_struct(&mut self, name: &str) -> Result<TypeId, ()> {
        let struct_id = self.next_struct_id;
        self.next_struct_id.0 += 1;

        let type_id = self.register_named_type(name, Type::NamedType(NamedType::Struct(struct_id)))?;
        self.structs.insert(struct_id, StructDefinition { fields: vec![] });
        Ok(type_id)
    }

    pub fn register_enum(&mut self, name: &str) -> Result<TypeId, ()> {
        let enum_id = self.next_enum_id;
        self.next_enum_id.0 += 1;

        let type_id = self.register_named_type(name, Type::NamedType(NamedType::Enum(enum_id)))?;
        self.enums.insert(enum_id, EnumDefinition { variants: vec![] });
        Ok(type_id)
    }

    pub fn get_type_id_by_name(&self, name: &str) -> Option<TypeId> {
        self.named_types.get(name).cloned()
    }

    pub fn get_type_by_name(&self, name: &str) -> Option<Type> {
        let type_id = self.get_type_id_by_name(name)?;
        self.types.get(&type_id).cloned()
    }

    pub fn get_mut_struct_definition_by_name(&mut self, name: &str) -> Option<&mut StructDefinition> {
        let type_ = self.get_type_by_name(name)?;
        if let Type::NamedType(NamedType::Struct(struct_id)) = type_ {
            self.structs.get_mut(&struct_id)
        } else {
            None
        }
    }

    pub fn get_mut_enum_definition_by_name(&mut self, name: &str) -> Option<&mut EnumDefinition> {
        let type_ = self.get_type_by_name(name)?;
        if let Type::NamedType(NamedType::Enum(enum_id)) = type_ {
            self.enums.get_mut(&enum_id)
        } else {
            None
        }
    }

    pub fn get_type_name_by_id(&self, r#type: TypeId) -> Option<&str> {
        self.named_types.iter().find_map(
            |(name, type_id)| {
                if *type_id == r#type { Some(name.as_str()) } else { None }
            },
        )
    }

    pub fn types_equal(&self, t1: TypeId, t2: TypeId) -> bool {
        use Type::*;

        if t1 == t2 {
            return true;
        }

        let t1 = self.types.get(&t1).unwrap();
        let t2 = self.types.get(&t2).unwrap();

        match (t1, t2) {
            (NamedType(left), NamedType(right)) => left == right,
            (
                Function {
                    param_types: p1,
                    return_type: r1,
                },
                Function {
                    param_types: p2,
                    return_type: r2,
                },
            ) => {
                p1.len() == p2.len()
                    && p1.iter().zip(p2).all(|(p1, p2)| self.types_equal(*p1, *p2))
                    && self.types_equal(*r1, *r2)
            }
            _ => false,
        }
    }
}
