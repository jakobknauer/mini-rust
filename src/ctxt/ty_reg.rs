use bimap::BiMap;
use itertools::Itertools;
use std::collections::{BTreeMap, HashMap};

use crate::ctxt::ty::*;

pub struct TyReg {
    tys: BiMap<Ty, TyDef>,
    structs: HashMap<StructId, StructDefinition>,
    enums: HashMap<EnumId, EnumDefinition>,

    i32_ty: Option<Ty>,
    bool_ty: Option<Ty>,
    unit_ty: Option<Ty>,

    named_tys: BTreeMap<String, Ty>,

    next_ty: Ty,
    next_struct_id: StructId,
    next_enum_id: EnumId,
}

impl TyReg {
    pub fn new() -> TyReg {
        TyReg {
            tys: BiMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),

            i32_ty: None,
            bool_ty: None,
            unit_ty: None,

            named_tys: BTreeMap::new(),

            next_ty: Ty(0),
            next_struct_id: StructId(0),
            next_enum_id: EnumId(0),
        }
    }

    fn register_ty(&mut self, ty_def: TyDef) -> Ty {
        if let Some(ty) = self.tys.get_by_right(&ty_def) {
            *ty
        } else {
            let ty = self.next_ty;
            self.next_ty.0 += 1;
            self.tys.insert(ty, ty_def);
            ty
        }
    }

    fn register_named_ty(&mut self, name: &str, ty_def: Named) -> Result<Ty, ()> {
        if self.named_tys.contains_key(name) {
            Err(())
        } else {
            let ty = self.register_ty(TyDef::Named(name.to_string(), ty_def));
            self.named_tys.insert(name.to_string(), ty);
            Ok(ty)
        }
    }

    pub fn register_primitive_tys(&mut self) -> Result<(), ()> {
        self.i32_ty = Some(self.register_named_ty("i32", Named::Primitve(Primitive::Integer32))?);
        self.bool_ty = Some(self.register_named_ty("bool", Named::Primitve(Primitive::Boolean))?);
        self.unit_ty = Some(self.register_named_ty("()", Named::Primitve(Primitive::Unit))?);
        Ok(())
    }

    pub fn register_struct(&mut self, name: &str) -> Result<Ty, ()> {
        let struct_id = self.next_struct_id;
        self.next_struct_id.0 += 1;

        let ty = self.register_named_ty(name, Named::Struct(struct_id))?;
        self.structs.insert(struct_id, StructDefinition { fields: vec![] });
        Ok(ty)
    }

    pub fn register_enum(&mut self, name: &str) -> Result<Ty, ()> {
        let enum_id = self.next_enum_id;
        self.next_enum_id.0 += 1;

        let ty = self.register_named_ty(name, Named::Enum(enum_id))?;
        self.enums.insert(enum_id, EnumDefinition { variants: vec![] });
        Ok(ty)
    }

    pub fn register_fn_ty(&mut self, param_tys: impl Into<Vec<Ty>>, return_ty: Ty) -> Ty {
        let fn_ty = TyDef::Fn {
            param_tys: param_tys.into(),
            return_ty,
        };
        self.register_ty(fn_ty)
    }

    pub fn get_ty_def(&self, id: &Ty) -> Option<&TyDef> {
        self.tys.get_by_left(id)
    }

    pub fn get_ty_by_name(&self, name: &str) -> Option<Ty> {
        self.named_tys.get(name).cloned()
    }

    fn get_ty_def_by_name(&self, name: &str) -> Option<&TyDef> {
        let ty = self.get_ty_by_name(name)?;
        self.tys.get_by_left(&ty)
    }

    pub fn get_struct_definition(&self, struct_id: &StructId) -> Option<&StructDefinition> {
        self.structs.get(struct_id)
    }

    pub fn get_enum_definition(&self, enum_id: &EnumId) -> Option<&EnumDefinition> {
        self.enums.get(enum_id)
    }

    pub fn get_mut_struct_definition_by_name(&mut self, name: &str) -> Option<&mut StructDefinition> {
        let ty_def = self.get_ty_def_by_name(name)?;
        if let TyDef::Named(_, Named::Struct(struct_id)) = ty_def {
            self.structs.get_mut(&struct_id.clone())
        } else {
            None
        }
    }

    pub fn get_mut_enum_definition_by_name(&mut self, name: &str) -> Option<&mut EnumDefinition> {
        let ty_def = self.get_ty_def_by_name(name)?;
        if let TyDef::Named(_, Named::Enum(enum_id)) = ty_def {
            self.enums.get_mut(&enum_id.clone())
        } else {
            None
        }
    }

    pub fn get_string_rep(&self, ty: &Ty) -> String {
        let Some(ty_def) = self.tys.get_by_left(ty) else {
            return format!("<type id {}>", ty.0).to_string();
        };

        match ty_def {
            TyDef::Named(name, _) => name.clone(),
            TyDef::Fn { param_tys, return_ty } => {
                let mut param_names = param_tys.iter().map(|pt| self.get_string_rep(pt));
                let return_name = self.get_string_rep(return_ty);
                format!("fn({}) -> {}", param_names.join(", "), return_name)
            }
        }
    }

    pub fn ty_equal(&self, t1: &Ty, t2: &Ty) -> bool {
        t1 == t2
    }

    pub fn get_primitive_ty(&self, primitive: Primitive) -> Option<Ty> {
        match primitive {
            Primitive::Integer32 => self.i32_ty,
            Primitive::Boolean => self.bool_ty,
            Primitive::Unit => self.unit_ty,
        }
    }

    pub fn get_all_tys(&self) -> impl IntoIterator<Item = (&Ty, &TyDef)> {
        &self.tys
    }

    pub fn get_named_ty(&self, named_ty: Named) -> Option<Ty> {
        self.tys.iter().find_map(|(ty, ty_def)| {
            if let TyDef::Named(_, nt) = ty_def
                && *nt == named_ty
            {
                Some(*ty)
            } else {
                None
            }
        })
    }

    pub fn get_ty_by_enum_id(&self, enum_id: &EnumId) -> Option<Ty> {
        self.get_named_ty(Named::Enum(*enum_id))
    }

    pub fn get_all_enums(&self) -> impl IntoIterator<Item = (&EnumId, &EnumDefinition)> {
        self.enums.iter()
    }
}
