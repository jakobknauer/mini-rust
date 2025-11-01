use bimap::BiMap;
use itertools::Itertools;
use std::collections::{BTreeMap, HashMap};

use crate::ctxt::types::*;

pub struct TypeRegistry {
    types: BiMap<TypeId, Type>,
    structs: HashMap<StructId, StructDefinition>,
    enums: HashMap<EnumId, EnumDefinition>,

    i32_type: Option<TypeId>,
    bool_type: Option<TypeId>,
    unit_type: Option<TypeId>,

    named_types: BTreeMap<String, TypeId>,

    next_type_id: TypeId,
    next_struct_id: StructId,
    next_enum_id: EnumId,
}

impl TypeRegistry {
    pub fn new() -> TypeRegistry {
        TypeRegistry {
            types: BiMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),

            i32_type: None,
            bool_type: None,
            unit_type: None,

            named_types: BTreeMap::new(),

            next_type_id: TypeId(0),
            next_struct_id: StructId(0),
            next_enum_id: EnumId(0),
        }
    }

    fn register_type(&mut self, type_: Type) -> TypeId {
        if let Some(type_id) = self.types.get_by_right(&type_) {
            *type_id
        } else {
            let type_id = self.next_type_id;
            self.next_type_id.0 += 1;
            self.types.insert(type_id, type_);
            type_id
        }
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

    pub fn register_function_type(&mut self, param_types: impl Into<Vec<TypeId>>, return_type: TypeId) -> TypeId {
        let function_type = Type::Function {
            param_types: param_types.into(),
            return_type,
        };
        self.register_type(function_type)
    }

    pub fn get_type_by_id(&self, id: &TypeId) -> Option<&Type> {
        self.types.get_by_left(id)
    }

    pub fn get_type_id_by_name(&self, name: &str) -> Option<TypeId> {
        self.named_types.get(name).cloned()
    }

    pub fn get_type_by_name(&self, name: &str) -> Option<&Type> {
        let type_id = self.get_type_id_by_name(name)?;
        self.types.get_by_left(&type_id)
    }

    pub fn get_struct_definition(&self, struct_id: &StructId) -> Option<&StructDefinition> {
        self.structs.get(struct_id)
    }

    pub fn get_struct_definition_by_type_id(&self, type_id: &TypeId) -> Option<&StructDefinition> {
        let type_ = self.get_type_by_id(type_id)?;
        if let Type::NamedType(_, NamedType::Struct(struct_id)) = type_ {
            self.structs.get(struct_id)
        } else {
            None
        }
    }

    pub fn get_enum_definition(&self, enum_id: &EnumId) -> Option<&EnumDefinition> {
        self.enums.get(enum_id)
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

    pub fn get_string_rep(&self, type_id: &TypeId) -> String {
        let Some(type_) = self.types.get_by_left(type_id) else {
            return format!("<type id {}>", type_id.0).to_string();
        };

        match type_ {
            Type::NamedType(name, _) => name.clone(),
            Type::Function {
                param_types,
                return_type,
            } => {
                let mut param_names = param_types.iter().map(|pt| self.get_string_rep(pt));
                let return_name = self.get_string_rep(return_type);
                format!("fn({}) -> {}", param_names.join(", "), return_name)
            }
        }
    }

    pub fn get_struct_string_rep(&self, struct_id: &StructId) -> String {
        self.types
            .right_values()
            .filter_map(|type_| {
                if let Type::NamedType(name, NamedType::Struct(sid)) = type_
                    && sid == struct_id
                {
                    Some(name.clone())
                } else {
                    None
                }
            })
            .next()
            .unwrap_or(format!("<struct id {}>", struct_id.0))
    }

    pub fn get_enum_string_rep(&self, enum_id: &EnumId) -> String {
        self.types
            .right_values()
            .filter_map(|type_| {
                if let Type::NamedType(name, NamedType::Enum(eid)) = type_
                    && eid == enum_id
                {
                    Some(name.clone())
                } else {
                    None
                }
            })
            .next()
            .unwrap_or(format!("<enum id {}>", enum_id.0))
    }

    pub fn types_equal(&self, t1: &TypeId, t2: &TypeId) -> bool {
        t1 == t2
    }

    pub fn get_primitive_type_id(&self, type_: PrimitiveType) -> Option<TypeId> {
        match type_ {
            PrimitiveType::Integer32 => self.i32_type,
            PrimitiveType::Boolean => self.bool_type,
            PrimitiveType::Unit => self.unit_type,
        }
    }

    pub fn get_all_types(&self) -> impl IntoIterator<Item = (&TypeId, &Type)> {
        &self.types
    }

    pub fn get_named_type_id(&self, named_type: NamedType) -> Option<TypeId> {
        self.types.iter().find_map(|(type_id, type_)| {
            if let Type::NamedType(_, nt) = type_
                && *nt == named_type
            {
                Some(*type_id)
            } else {
                None
            }
        })
    }

    pub fn get_all_enums(&self) -> impl IntoIterator<Item = (&EnumId, &EnumDefinition)> {
        self.enums.iter()
    }
}
