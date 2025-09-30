use itertools::Itertools;
use std::collections::HashMap;

use crate::ctxt::types::*;

pub struct TypeRegistry {
    types: HashMap<TypeId, Type>,
    structs: HashMap<StructId, StructDefinition>,
    enums: HashMap<EnumId, EnumDefinition>,

    i32_type: Option<TypeId>,
    bool_type: Option<TypeId>,
    unit_type: Option<TypeId>,

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
            i32_type: None,
            bool_type: None,
            unit_type: None,
            named_types: HashMap::new(),
            next_type_id: TypeId(0),
            next_struct_id: StructId(0),
            next_enum_id: EnumId(0),
        }
    }

    pub fn register_type(&mut self, type_: Type) -> TypeId {
        let type_id = self.next_type_id;
        self.next_type_id.0 += 1;
        self.types.insert(type_id, type_);
        type_id
    }

    fn register_named_type(&mut self, name: &str, type_: NamedType) -> Result<TypeId, ()> {
        if self.named_types.contains_key(name) {
            Err(())
        } else {
            let type_id = self.register_type(Type::NamedType(name.to_string(), type_));
            self.named_types.insert(name.to_string(), type_id);
            Ok(type_id)
        }
    }

    pub fn register_primitive_types(&mut self) -> Result<(), ()> {
        self.i32_type = Some(self.register_named_type("i32", NamedType::Primitve(PrimitiveType::Integer32))?);
        self.bool_type = Some(self.register_named_type("bool", NamedType::Primitve(PrimitiveType::Boolean))?);
        self.unit_type = Some(self.register_named_type("()", NamedType::Primitve(PrimitiveType::Unit))?);
        Ok(())
    }

    pub fn register_struct(&mut self, name: &str) -> Result<TypeId, ()> {
        let struct_id = self.next_struct_id;
        self.next_struct_id.0 += 1;

        let type_id = self.register_named_type(name, NamedType::Struct(struct_id))?;
        self.structs.insert(struct_id, StructDefinition { fields: vec![] });
        Ok(type_id)
    }

    pub fn register_enum(&mut self, name: &str) -> Result<TypeId, ()> {
        let enum_id = self.next_enum_id;
        self.next_enum_id.0 += 1;

        let type_id = self.register_named_type(name, NamedType::Enum(enum_id))?;
        self.enums.insert(enum_id, EnumDefinition { variants: vec![] });
        Ok(type_id)
    }

    pub fn get_type_by_id(&self, id: TypeId) -> Option<&Type> {
        self.types.get(&id)
    }

    pub fn get_type_id_by_name(&self, name: &str) -> Option<TypeId> {
        self.named_types.get(name).cloned()
    }

    pub fn get_type_by_name(&self, name: &str) -> Option<&Type> {
        let type_id = self.get_type_id_by_name(name)?;
        self.types.get(&type_id)
    }

    pub fn get_mut_struct_definition_by_name(&mut self, name: &str) -> Option<&mut StructDefinition> {
        let type_ = self.get_type_by_name(name)?;
        if let Type::NamedType(_, NamedType::Struct(struct_id)) = type_ {
            self.structs.get_mut(&struct_id.clone())
        } else {
            None
        }
    }

    pub fn get_mut_enum_definition_by_name(&mut self, name: &str) -> Option<&mut EnumDefinition> {
        let type_ = self.get_type_by_name(name)?;
        if let Type::NamedType(_, NamedType::Enum(enum_id)) = type_ {
            self.enums.get_mut(&enum_id.clone())
        } else {
            None
        }
    }

    pub fn get_string_rep(&self, type_: TypeId) -> String {
        let Some(type_) = self.types.get(&type_) else {
            return format!("<type id {}>", type_.0).to_string();
        };

        match type_ {
            Type::NamedType(name, _) => name.clone(),
            Type::Function {
                param_types,
                return_type,
            } => {
                let mut param_names = param_types.iter().map(|t| self.get_string_rep(*t));
                let return_name = self.get_string_rep(*return_type);
                format!("fn({}) -> {}", param_names.join(", "), return_name)
            }
        }
    }

    pub fn types_equal(&self, t1: TypeId, t2: TypeId) -> bool {
        use Type::*;

        if t1 == t2 {
            return true;
        }

        let t1 = self.types.get(&t1).unwrap();
        let t2 = self.types.get(&t2).unwrap();

        match (t1, t2) {
            (NamedType(_, left), NamedType(_, right)) => left == right,
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

    pub fn get_primitive_type_id(&self, type_: PrimitiveType) -> Option<TypeId> {
        match type_ {
            PrimitiveType::Integer32 => self.i32_type,
            PrimitiveType::Boolean => self.bool_type,
            PrimitiveType::Unit => self.unit_type,
        }
    }
}
