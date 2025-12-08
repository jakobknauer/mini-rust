use itertools::Itertools;
use std::{borrow::Borrow, collections::HashMap, hash::Hash};

use crate::{ctxt::ty::*, hlr};

pub struct TyReg {
    tys: Vec<Option<TyDef>>,
    tys_inv: HashMap<TyDef, Ty>,

    i32_ty: Option<Ty>,
    bool_ty: Option<Ty>,
    unit_ty: Option<Ty>,

    structs: Vec<StructDef>,
    enums: Vec<EnumDef>,

    named_tys: HashMap<String, Ty>,
}

pub enum UnificationError {
    FunctionParamCountMismatch,
    TypeMismatch,
}
pub struct NotAStruct(pub Ty);
pub struct NotAnEnum(pub Ty);
pub enum NotAStructField {
    NotAStruct(Ty),
    NotAFieldName(Ty, String),
}
pub enum InstantiationError {
    NotAStruct(Ty),
    GenericArgCountMismatch { ty: Ty, expected: usize, actual: usize },
}

impl TyReg {
    pub fn new() -> TyReg {
        TyReg {
            tys: Vec::new(),
            tys_inv: HashMap::new(),

            i32_ty: None,
            bool_ty: None,
            unit_ty: None,

            structs: Vec::new(),
            enums: Vec::new(),

            named_tys: HashMap::new(),
        }
    }

    fn register_ty(&mut self, ty_def: TyDef) -> Ty {
        if let Some(existing_ty) = self.tys_inv.get(&ty_def) {
            return *existing_ty;
        }

        let ty = Ty(self.tys.len());
        self.tys.push(Some(ty_def.clone()));
        self.tys_inv.insert(ty_def, ty);
        ty
    }

    pub fn new_undefined_ty(&mut self) -> Ty {
        let ty = Ty(self.tys.len());
        self.tys.push(None);
        ty
    }

    fn register_named_ty(&mut self, name: &str, ty_def: TyDef) -> Result<Ty, ()> {
        if self.named_tys.contains_key(name) {
            Err(())
        } else {
            let ty = self.register_ty(ty_def);
            self.named_tys.insert(name.to_string(), ty);
            Ok(ty)
        }
    }

    pub fn register_primitive_tys(&mut self) -> Result<(), ()> {
        self.i32_ty = Some(self.register_named_ty("i32", TyDef::Primitve(Primitive::Integer32))?);
        self.bool_ty = Some(self.register_named_ty("bool", TyDef::Primitve(Primitive::Boolean))?);
        self.unit_ty = Some(self.register_named_ty("()", TyDef::Primitve(Primitive::Unit))?);
        Ok(())
    }

    pub fn register_struct(&mut self, name: &str, gen_param_names: &[String]) -> Result<Ty, ()> {
        let struct_ = Struct(self.structs.len());

        let ty = self.register_named_ty(name, TyDef::Struct(struct_))?;
        let gen_params = gen_param_names
            .iter()
            .map(|gp_name| GenParam {
                name: gp_name.clone(),
                ty: self.register_gen_var_ty(gp_name),
            })
            .collect();

        let struct_def = StructDef {
            name: name.to_string(),
            gen_params,
            fields: vec![],
        };
        self.structs.push(struct_def);

        Ok(ty)
    }

    pub fn register_enum(&mut self, name: &str) -> Result<Ty, ()> {
        let enum_ = Enum(self.enums.len());

        let ty = self.register_named_ty(name, TyDef::Enum(enum_))?;
        self.enums.push(EnumDef {
            name: name.to_string(),
            variants: vec![],
        });
        Ok(ty)
    }

    pub fn register_fn_ty(&mut self, param_tys: impl Into<Vec<Ty>>, return_ty: Ty) -> Ty {
        let fn_ty = TyDef::Fn {
            param_tys: param_tys.into(),
            return_ty,
        };
        self.register_ty(fn_ty)
    }

    pub fn register_ref_ty(&mut self, inner_ty: Ty) -> Ty {
        let ref_ty = TyDef::Ref(inner_ty);
        self.register_ty(ref_ty)
    }

    pub fn register_gen_var_ty(&mut self, name: &str) -> Ty {
        let gen_param_ty = TyDef::GenVar(name.to_string());
        self.register_ty(gen_param_ty)
    }

    pub fn instantiate_struct_ty(
        &mut self,
        struct_ty: Ty,
        gen_args: impl Into<Vec<Ty>>,
    ) -> Result<Ty, InstantiationError> {
        let ty_def = self.get_ty_def(struct_ty).expect("struct type should be registered");
        let &TyDef::Struct(struct_) = ty_def else {
            return Err(InstantiationError::NotAStruct(struct_ty));
        };

        let struct_def = self
            .structs
            .get(struct_.0)
            .expect("struct definition should be registered");

        let gen_args = gen_args.into();

        if struct_def.gen_params.len() != gen_args.len() {
            return Err(InstantiationError::GenericArgCountMismatch {
                ty: struct_ty,
                expected: struct_def.gen_params.len(),
                actual: gen_args.len(),
            });
        }

        let instantiated_ty = TyDef::InstantiatedStruct { struct_, gen_args };

        Ok(self.register_ty(instantiated_ty))
    }

    pub fn get_ty_def(&self, id: Ty) -> Option<&TyDef> {
        let id = self.canonicalize(id);
        self.tys.get(id.0).and_then(|inner| inner.as_ref())
    }

    pub fn get_ty_by_name(&self, name: &str) -> Option<Ty> {
        self.named_tys.get(name).cloned()
    }

    fn get_ty_def_by_name(&self, name: &str) -> Option<&TyDef> {
        let ty = self.get_ty_by_name(name)?;
        self.get_ty_def(ty)
    }

    pub fn get_struct_ty_by_name(&self, name: &str) -> Option<Ty> {
        let ty = self.get_ty_by_name(name)?;
        let ty_def = self.get_ty_def(ty)?;

        if let TyDef::Struct(..) = ty_def { Some(ty) } else { None }
    }

    pub fn get_struct_def(&self, struct_: Struct) -> Option<&StructDef> {
        self.structs.get(struct_.0)
    }

    pub fn get_struct_def_by_name(&self, name: &str) -> Option<&StructDef> {
        let ty_def = self.get_ty_def_by_name(name)?;
        if let TyDef::Struct(struct_) = *ty_def {
            self.structs.get(struct_.0)
        } else {
            None
        }
    }

    pub fn get_mut_struct_def_by_name(&mut self, name: &str) -> Option<&mut StructDef> {
        let ty_def = self.get_ty_def_by_name(name)?;
        if let TyDef::Struct(struct_) = *ty_def {
            self.structs.get_mut(struct_.0)
        } else {
            None
        }
    }

    pub fn get_enum_def(&self, enum_: Enum) -> Option<&EnumDef> {
        self.enums.get(enum_.0)
    }

    pub fn get_enum_def_by_ty(&self, ty: Ty) -> Result<&EnumDef, NotAnEnum> {
        let ty_def = self.get_ty_def(ty).expect("type should be registered");

        let &TyDef::Enum(enum_) = ty_def else {
            return Err(NotAnEnum(ty));
        };

        let enum_def = self.get_enum_def(enum_).expect("enum definition should be registered");

        Ok(enum_def)
    }

    pub fn get_mut_enum_def_by_name(&mut self, name: &str) -> Option<&mut EnumDef> {
        let ty_def = self.get_ty_def_by_name(name)?;
        if let TyDef::Enum(enum_) = *ty_def {
            self.enums.get_mut(enum_.0)
        } else {
            None
        }
    }

    pub fn try_resolve_hlr_annot(&mut self, annot: &hlr::TyAnnot, gen_vars: &Vec<GenParam>) -> Option<Ty> {
        use hlr::TyAnnot::*;

        match annot {
            Named(name) => gen_vars
                .iter()
                .find_map(|gp| (&gp.name == name).then_some(gp.ty))
                .or_else(|| self.get_ty_by_name(name)),
            Reference(ty_annot) => self
                .try_resolve_hlr_annot(ty_annot, gen_vars)
                .map(|inner| self.register_ref_ty(inner)),
            Unit => Some(self.get_primitive_ty(Primitive::Unit)),
            Fn { param_tys, return_ty } => {
                let param_tys: Vec<Ty> = param_tys
                    .iter()
                    .map(|pt| self.try_resolve_hlr_annot(pt, gen_vars))
                    .collect::<Option<Vec<_>>>()?;

                let return_ty = match return_ty {
                    Some(rt) => self.try_resolve_hlr_annot(rt, gen_vars),
                    None => Some(self.get_primitive_ty(Primitive::Unit)),
                }?;

                Some(self.register_fn_ty(param_tys, return_ty))
            }
            Generic(ident) => {
                let base_ty_def = gen_vars
                    .iter()
                    .find_map(|gp| (gp.name == ident.ident).then_some(gp.ty))
                    .or_else(|| self.get_ty_by_name(&ident.ident))
                    .and_then(|ty| self.tys.get(ty.0))?
                    .as_ref()?;

                let &TyDef::Struct(struct_) = base_ty_def else {
                    // Cannot instantiate non-struct types
                    return None;
                };

                let gen_args: Vec<Ty> = ident
                    .gen_args
                    .iter()
                    .map(|arg_annot| self.try_resolve_hlr_annot(arg_annot, gen_vars))
                    .collect::<Option<Vec<_>>>()?;

                Some(self.register_ty(TyDef::InstantiatedStruct { struct_, gen_args }))
            }
        }
    }

    pub fn get_string_rep(&self, ty: Ty) -> String {
        if ty.0 >= self.tys.len() {
            return format!("<unknown type id {}>", ty.0).to_string();
        }

        let Some(ty_def) = self.get_ty_def(ty) else {
            return format!("<undefined type id {}>", ty.0).to_string();
        };

        match *ty_def {
            TyDef::Fn {
                ref param_tys,
                return_ty,
            } => {
                let mut param_names = param_tys.iter().map(|&pt| self.get_string_rep(pt));
                let return_name = self.get_string_rep(return_ty);
                format!("fn({}) -> {}", param_names.join(", "), return_name)
            }
            TyDef::Ref(ty) => format!("&{}", self.get_string_rep(ty)),
            TyDef::Alias(ty) => self.get_string_rep(ty),
            TyDef::GenVar(ref name) => name.clone(),
            TyDef::Primitve(primitive) => match primitive {
                Primitive::Integer32 => "i32".to_string(),
                Primitive::Boolean => "bool".to_string(),
                Primitive::Unit => "()".to_string(),
            },
            TyDef::Struct(struct_) => {
                let struct_def = self
                    .get_struct_def(struct_)
                    .expect("struct definition should be registered");
                if struct_def.gen_params.is_empty() {
                    struct_def.name.clone()
                } else {
                    let gen_param_names = struct_def
                        .gen_params
                        .iter()
                        .map(|gp| gp.name.as_str())
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{}<{}>", struct_def.name, gen_param_names)
                }
            }
            TyDef::Enum(enum_) => {
                let enum_def = self.get_enum_def(enum_).expect("enum definition should be registered");
                enum_def.name.clone()
            }
            TyDef::InstantiatedStruct { struct_, ref gen_args } => {
                let struct_def = self
                    .get_struct_def(struct_)
                    .expect("struct definition should be registered");
                let gen_arg_names = gen_args
                    .iter()
                    .map(|&ga| self.get_string_rep(ga))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}<{}>", struct_def.name, gen_arg_names)
            }
        }
    }

    pub fn unify(&mut self, ty1: Ty, ty2: Ty) -> Result<(), UnificationError> {
        use TyDef::*;

        let ty1 = self.canonicalize(ty1);
        let ty2 = self.canonicalize(ty2);

        if ty1 == ty2 {
            return Ok(());
        }

        let def1 = self.tys.get(ty1.0).expect("ty1 should be registered");
        let def2 = self.tys.get(ty2.0).expect("ty2 should be registered");

        match (def1, def2) {
            (None, _) => {
                *self.tys.get_mut(ty1.0).unwrap() = Some(Alias(ty2));
                Ok(())
            }
            (_, None) => {
                *self.tys.get_mut(ty2.0).unwrap() = Some(Alias(ty1));
                Ok(())
            }

            (Some(def1), Some(def2)) => match (def1, def2) {
                (Alias(_), _) | (_, Alias(_)) => {
                    unreachable!("Types should have been canonicalized");
                }

                (
                    &Fn {
                        param_tys: ref params1,
                        return_ty: ret1,
                    },
                    &Fn {
                        param_tys: ref params2,
                        return_ty: ret2,
                    },
                ) => {
                    if params1.len() != params2.len() {
                        return Err(UnificationError::FunctionParamCountMismatch);
                    }

                    let pairs = params1.clone().into_iter().zip(params2.clone());
                    for (p1, p2) in pairs {
                        self.unify(p1, p2)?;
                    }
                    self.unify(ret1, ret2)
                }

                (&Ref(inner1), &Ref(inner2)) => self.unify(inner1, inner2),

                (Enum(e1), Enum(e2)) if e1 == e2 => Ok(()),
                (Struct(s1), Struct(s2)) if s1 == s2 => Ok(()),

                _ => Err(UnificationError::TypeMismatch),
            },
        }
    }

    pub fn canonicalize(&self, mut ty: Ty) -> Ty {
        while let Some(TyDef::Alias(next_ty)) = self.tys.get(ty.0).expect("current_ty should be registered") {
            ty = *next_ty;
        }
        ty
    }

    pub fn get_primitive_ty(&self, primitive: Primitive) -> Ty {
        match primitive {
            Primitive::Integer32 => self.i32_ty,
            Primitive::Boolean => self.bool_ty,
            Primitive::Unit => self.unit_ty,
        }
        .expect("primitive type should be registered")
    }

    pub fn get_all_enums(&self) -> impl IntoIterator<Item = (Ty, &EnumDef)> {
        self.tys.iter().enumerate().filter_map(|(ty, ty_def_opt)| {
            let TyDef::Enum(enum_) = ty_def_opt.as_ref()? else {
                return None;
            };
            let enum_def = self.enums.get(enum_.0).expect("enum definition should be registered");
            Some((Ty(ty), enum_def))
        })
    }

    pub fn substitute_gen_vars(&mut self, ty: Ty, substitutions: &HashMap<impl Borrow<str> + Eq + Hash, Ty>) -> Ty {
        let ty = self.canonicalize(ty);
        let Some(ty_def) = self.tys.get(ty.0).expect("ty should be registered") else {
            return ty;
        };

        match *ty_def {
            TyDef::GenVar(ref name) => {
                if let Some(replacement_ty) = substitutions.get(name) {
                    *replacement_ty
                } else {
                    ty
                }
            }
            TyDef::Fn {
                ref param_tys,
                return_ty,
            } => {
                let new_param_tys = param_tys
                    .clone()
                    .into_iter()
                    .map(|pt| self.substitute_gen_vars(pt, substitutions))
                    .collect::<Vec<_>>();
                let new_return_ty = self.substitute_gen_vars(return_ty, substitutions);
                self.register_fn_ty(new_param_tys, new_return_ty)
            }
            TyDef::Ref(inner_ty) => {
                let new_inner_ty = self.substitute_gen_vars(inner_ty, substitutions);
                self.register_ref_ty(new_inner_ty)
            }
            TyDef::InstantiatedStruct { struct_, ref gen_args } => {
                let new_gen_args = gen_args
                    .clone()
                    .iter()
                    .map(|&ga| self.substitute_gen_vars(ga, substitutions))
                    .collect::<Vec<_>>();
                self.register_ty(TyDef::InstantiatedStruct {
                    struct_,
                    gen_args: new_gen_args,
                })
            }
            _ => ty,
        }
    }

    pub fn get_struct_field_ty(&mut self, ty: Ty, index: usize) -> Result<Ty, NotAStruct> {
        let ty_def = self.get_ty_def(ty).expect("type should be registered");
        match *ty_def {
            TyDef::Struct(struct_) => {
                let struct_def = self
                    .get_struct_def(struct_)
                    .expect("struct definition should be registered");
                Ok(struct_def.fields[index].ty)
            }
            TyDef::InstantiatedStruct { struct_, ref gen_args } => {
                let struct_def = self
                    .get_struct_def(struct_)
                    .expect("struct definition should be registered");
                let ty = struct_def.fields[index].ty;
                let substitutions: HashMap<String, Ty> = struct_def
                    .gen_params
                    .iter()
                    .map(|gp| gp.name.clone())
                    .zip(gen_args.iter().cloned())
                    .collect();
                Ok(self.substitute_gen_vars(ty, &substitutions))
            }
            _ => Err(NotAStruct(ty)),
        }
    }

    pub fn get_struct_field_names(&mut self, ty: Ty) -> Result<impl IntoIterator<Item = &str>, NotAStruct> {
        let ty_def = self.get_ty_def(ty).expect("type should be registered");
        match *ty_def {
            TyDef::Struct(struct_) | TyDef::InstantiatedStruct { struct_, .. } => {
                let struct_def = self
                    .get_struct_def(struct_)
                    .expect("struct definition should be registered");
                Ok(struct_def.fields.iter().map(|field| field.name.as_str()))
            }
            _ => Err(NotAStruct(ty)),
        }
    }

    pub fn get_instantiated_struct_field_tys(&mut self, struct_: Struct, gen_args: &[Ty]) -> Result<Vec<Ty>, ()> {
        let struct_def = self.get_struct_def(struct_).ok_or(())?.clone();

        if struct_def.gen_params.len() != gen_args.len() {
            return Err(());
        }

        let substitutions: HashMap<String, Ty> = struct_def
            .gen_params
            .iter()
            .map(|gp| gp.name.clone())
            .zip(gen_args.iter().cloned())
            .collect();

        let struct_field_tys: Vec<Ty> = struct_def.fields.iter().map(|field| field.ty).collect();
        // This should reuse the struct_field_tys vector to avoid an extra allocation
        let instantiated_fields: Vec<Ty> = struct_field_tys
            .into_iter()
            .map(|field| self.substitute_gen_vars(field, &substitutions))
            .collect();

        Ok(instantiated_fields)
    }

    pub fn get_struct_field_index_by_name(&self, struct_ty: Ty, field_name: &str) -> Result<usize, NotAStructField> {
        let ty_def = self.get_ty_def(struct_ty).expect("type should be registered");

        match ty_def {
            &TyDef::Struct(struct_) | &TyDef::InstantiatedStruct { struct_, .. } => {
                let struct_def = self
                    .get_struct_def(struct_)
                    .expect("struct definition should be registered");
                struct_def
                    .fields
                    .iter()
                    .position(|field| field.name == field_name)
                    .ok_or_else(|| NotAStructField::NotAFieldName(struct_ty, field_name.to_string()))
            }
            _ => Err(NotAStructField::NotAStruct(struct_ty)),
        }
    }
}
