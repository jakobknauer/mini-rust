use itertools::Itertools;
use std::collections::HashMap;

use crate::{ctxt::ty::*, hlr};

#[derive(Default)]
pub struct TyReg {
    tys: Vec<Option<TyDef>>,

    i32_ty: Option<Ty>,
    bool_ty: Option<Ty>,
    unit_ty: Option<Ty>,

    structs: Vec<StructDef>,
    enums: Vec<EnumDef>,
    gen_var_names: Vec<String>,

    tys_inv: HashMap<TyDef, Ty>,
    named_tys: HashMap<String, Named>,
}

#[derive(Clone, Copy)]
pub enum Named {
    Ty(Ty),
    #[allow(unused)]
    Struct(Struct),
    #[allow(unused)]
    Enum(Enum),
}

pub enum UnificationError {
    FunctionParamCountMismatch,
    TypeMismatch,
}
#[derive(Debug)]
pub struct NotAStruct(pub Ty);
#[derive(Debug)]
pub struct NotAnEnum(pub Ty);
pub enum NotAStructField {
    NotAStruct(Ty),
    NotAFieldName(Ty, String),
}
#[derive(Debug)]
pub enum TyInstError {
    StructGenericArgCountMismatch {
        struct_: Struct,
        expected: usize,
        actual: usize,
    },
    EnumGenericArgCountMismatch {
        enum_: Enum,
        expected: usize,
        actual: usize,
    },
}

impl TyReg {
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
            self.named_tys.insert(name.to_string(), Named::Ty(ty));
            Ok(ty)
        }
    }

    pub fn register_primitive_tys(&mut self) -> Result<(), ()> {
        self.i32_ty = Some(self.register_named_ty("i32", TyDef::Primitve(Primitive::Integer32))?);
        self.bool_ty = Some(self.register_named_ty("bool", TyDef::Primitve(Primitive::Boolean))?);
        self.unit_ty = Some(self.register_named_ty("()", TyDef::Primitve(Primitive::Unit))?);
        Ok(())
    }

    pub fn register_struct(&mut self, name: &str, gen_param_names: &[String]) -> Result<Struct, ()> {
        let struct_ = Struct(self.structs.len());

        let gen_params = gen_param_names
            .iter()
            .map(|gp_name| self.register_gen_var(gp_name))
            .collect();

        let struct_def = StructDef {
            name: name.to_string(),
            gen_params,
            fields: vec![],
        };
        self.structs.push(struct_def);

        self.named_tys.insert(name.to_string(), Named::Struct(struct_));

        Ok(struct_)
    }

    pub fn register_enum(&mut self, name: &str, gen_params: &[String]) -> Result<Enum, ()> {
        let enum_ = Enum(self.enums.len());

        let gen_params = gen_params
            .iter()
            .map(|gp_name| self.register_gen_var(gp_name))
            .collect();

        let enum_def = EnumDef {
            name: name.to_string(),
            gen_params,
            variants: vec![],
        };
        self.enums.push(enum_def);

        self.named_tys.insert(name.to_string(), Named::Enum(enum_));

        Ok(enum_)
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

    pub fn register_gen_var_ty(&mut self, gen_var: GenVar) -> Ty {
        let gen_var_ty = TyDef::GenVar(gen_var);
        self.register_ty(gen_var_ty)
    }

    pub fn register_gen_var(&mut self, name: &str) -> GenVar {
        let gen_var = GenVar(self.gen_var_names.len());
        self.gen_var_names.push(name.to_string());
        gen_var
    }

    pub fn instantiate_struct(&mut self, struct_: Struct, gen_args: impl Into<Vec<Ty>>) -> Result<Ty, TyInstError> {
        let struct_def = self.structs.get(struct_.0).unwrap();
        let gen_args = gen_args.into();

        if struct_def.gen_params.len() != gen_args.len() {
            return Err(TyInstError::StructGenericArgCountMismatch {
                struct_,
                expected: struct_def.gen_params.len(),
                actual: gen_args.len(),
            });
        }

        let struct_ty = TyDef::Struct { struct_, gen_args };
        Ok(self.register_ty(struct_ty))
    }

    pub fn instantiate_enum(&mut self, enum_: Enum, gen_args: impl Into<Vec<Ty>>) -> Result<Ty, TyInstError> {
        let enum_def = self.enums.get(enum_.0).unwrap();
        let gen_args = gen_args.into();

        if enum_def.gen_params.len() != gen_args.len() {
            return Err(TyInstError::EnumGenericArgCountMismatch {
                enum_,
                expected: enum_def.gen_params.len(),
                actual: gen_args.len(),
            });
        }

        let enum_ty = TyDef::Enum { enum_, gen_args };
        Ok(self.register_ty(enum_ty))
    }

    pub fn get_ty_def(&self, id: Ty) -> Option<&TyDef> {
        let id = self.canonicalize(id);
        self.tys.get(id.0).and_then(|inner| inner.as_ref())
    }

    pub fn get_struct_by_name(&self, name: &str) -> Option<Struct> {
        match self.named_tys.get(name) {
            Some(&Named::Struct(struct_)) => Some(struct_),
            _ => None,
        }
    }

    fn get_enum_by_name(&self, name: &str) -> Option<Enum> {
        match self.named_tys.get(name) {
            Some(&Named::Enum(enum_)) => Some(enum_),
            _ => None,
        }
    }

    fn get_struct_def(&self, struct_: Struct) -> Option<&StructDef> {
        self.structs.get(struct_.0)
    }

    pub fn get_struct_def_by_name(&self, name: &str) -> Option<&StructDef> {
        let struct_ = self.get_struct_by_name(name)?;
        self.get_struct_def(struct_)
    }

    pub fn get_mut_struct_def_by_name(&mut self, name: &str) -> Option<&mut StructDef> {
        let struct_ = self.get_struct_by_name(name)?;
        self.structs.get_mut(struct_.0)
    }

    fn get_enum_def(&self, enum_: Enum) -> Option<&EnumDef> {
        self.enums.get(enum_.0)
    }

    pub fn is_enum_ty(&self, base_ty: Ty) -> bool {
        let ty_def = self.get_ty_def(base_ty);
        matches!(ty_def, Some(TyDef::Enum { .. }))
    }

    pub fn get_mut_enum_def_by_name(&mut self, name: &str) -> Option<&mut EnumDef> {
        let enum_ = self.get_enum_by_name(name)?;
        self.enums.get_mut(enum_.0)
    }

    pub fn try_resolve_hlr_annot(&mut self, annot: &hlr::TyAnnot, gen_vars: &Vec<GenVar>) -> Option<Ty> {
        use hlr::TyAnnot::*;

        match annot {
            Named(name) => {
                if let Some(gv) = gen_vars.iter().find(|&&GenVar(gv)| self.gen_var_names[gv] == *name) {
                    let ty = self.register_gen_var_ty(*gv);
                    return Some(ty);
                }

                match *self.named_tys.get(name)? {
                    self::Named::Ty(ty) => Some(ty),
                    self::Named::Struct(struct_) => self.instantiate_struct(struct_, []).ok(),
                    self::Named::Enum(enum_) => self.instantiate_enum(enum_, []).ok(),
                }
            }
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
                let gen_args: Vec<Ty> = ident
                    .gen_args
                    .iter()
                    .map(|arg_annot| self.try_resolve_hlr_annot(arg_annot, gen_vars))
                    .collect::<Option<Vec<_>>>()?;

                match *self.named_tys.get(&ident.ident)? {
                    self::Named::Struct(struct_) => self.instantiate_struct(struct_, gen_args).ok(),
                    self::Named::Enum(enum_) => self.instantiate_enum(enum_, gen_args).ok(),
                    self::Named::Ty(..) => None,
                }
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
            TyDef::GenVar(gen_var) => self.gen_var_names[gen_var.0].clone(),
            TyDef::Primitve(primitive) => match primitive {
                Primitive::Integer32 => "i32".to_string(),
                Primitive::Boolean => "bool".to_string(),
                Primitive::Unit => "()".to_string(),
            },
            TyDef::Struct { struct_, ref gen_args } => {
                let struct_name = self.get_struct_name(struct_);
                let gen_arg_names = gen_args
                    .iter()
                    .map(|&ga| self.get_string_rep(ga))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}<{}>", struct_name, gen_arg_names)
            }
            TyDef::Enum { enum_, ref gen_args } => {
                let enum_name = self.get_enum_name(enum_);
                let gen_arg_names = gen_args
                    .iter()
                    .map(|&ga| self.get_string_rep(ga))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}<{}>", enum_name, gen_arg_names)
            }
        }
    }

    pub fn get_struct_name(&self, struct_: Struct) -> String {
        self.get_struct_def(struct_)
            .map(|sd| sd.name.clone())
            .unwrap_or_else(|| format!("<unknown struct {}>", struct_.0))
    }

    pub fn get_enum_name(&self, enum_: Enum) -> String {
        self.get_enum_def(enum_)
            .map(|ed| ed.name.clone())
            .unwrap_or_else(|| format!("<unknown enum {}>", enum_.0))
    }

    pub fn get_gen_var_name(&self, gen_param: GenVar) -> &str {
        &self.gen_var_names[gen_param.0]
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

                // TODO instantiated structs/enums unification
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

    pub fn get_all_enums(&self) -> impl IntoIterator<Item = (Enum, &EnumDef)> {
        self.enums
            .iter()
            .enumerate()
            .map(|(enum_, enum_def)| (Enum(enum_), enum_def))
    }

    pub fn substitute_gen_vars(&mut self, ty: Ty, substitutions: &HashMap<GenVar, Ty>) -> Ty {
        let ty = self.canonicalize(ty);
        let Some(ty_def) = self.tys.get(ty.0).expect("ty should be registered") else {
            return ty;
        };

        match ty_def {
            TyDef::GenVar(gen_var) => {
                if let Some(replacement_ty) = substitutions.get(gen_var) {
                    *replacement_ty
                } else {
                    ty
                }
            }
            &TyDef::Fn {
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
            &TyDef::Ref(inner_ty) => {
                let new_inner_ty = self.substitute_gen_vars(inner_ty, substitutions);
                self.register_ref_ty(new_inner_ty)
            }
            &TyDef::Struct { struct_, ref gen_args } => {
                let new_gen_args = gen_args
                    .clone()
                    .iter()
                    .map(|&ga| self.substitute_gen_vars(ga, substitutions))
                    .collect::<Vec<_>>();
                self.instantiate_struct(struct_, new_gen_args).unwrap()
            }
            &TyDef::Enum { enum_, ref gen_args } => {
                let new_gen_args = gen_args
                    .clone()
                    .iter()
                    .map(|&ga| self.substitute_gen_vars(ga, substitutions))
                    .collect::<Vec<_>>();
                self.instantiate_enum(enum_, new_gen_args).unwrap()
            }
            _ => ty,
        }
    }

    pub fn get_struct_field_ty(&mut self, ty: Ty, index: usize) -> Result<Ty, NotAStruct> {
        let ty_def = self.get_ty_def(ty).expect("type should be registered");
        let &TyDef::Struct { struct_, ref gen_args } = ty_def else {
            return Err(NotAStruct(ty));
        };

        let struct_def = self
            .get_struct_def(struct_)
            .expect("struct definition should be registered");
        let field_ty = struct_def.fields[index].ty;

        let substitutions = struct_def
            .gen_params
            .iter()
            .cloned()
            .zip(gen_args.iter().cloned())
            .collect();
        let instantiated_field_ty = self.substitute_gen_vars(field_ty, &substitutions);

        Ok(instantiated_field_ty)
    }

    pub fn get_struct_field_tys(&mut self, ty: Ty) -> Result<Vec<Ty>, NotAStruct> {
        let ty_def = self.get_ty_def(ty).expect("type should be registered");
        let &TyDef::Struct { struct_, ref gen_args } = ty_def else {
            return Err(NotAStruct(ty));
        };

        let struct_def = self
            .get_struct_def(struct_)
            .expect("struct definition should be registered");
        let substitutions = struct_def
            .gen_params
            .iter()
            .cloned()
            .zip(gen_args.iter().cloned())
            .collect();
        let fields = struct_def.fields.clone();
        let instantiated_field_tys: Vec<Ty> = fields
            .iter()
            .map(|field| self.substitute_gen_vars(field.ty, &substitutions))
            .collect();
        Ok(instantiated_field_tys)
    }

    pub fn get_enum_variant_ty(&mut self, ty: Ty, variant_index: usize) -> Result<Ty, NotAnEnum> {
        let ty_def = self.get_ty_def(ty).expect("type should be registered");
        let &TyDef::Enum { enum_, ref gen_args } = ty_def else {
            return Err(NotAnEnum(ty));
        };

        let enum_def = self.get_enum_def(enum_).expect("enum definition should be registered");
        let base_variant_struct_ty = enum_def.variants[variant_index].struct_;
        let instantiated_variant_struct_ty = self
            .instantiate_struct(base_variant_struct_ty, gen_args.clone())
            .unwrap();
        Ok(instantiated_variant_struct_ty)
    }

    pub fn get_enum_variant_tys(&mut self, ty: Ty) -> Result<Vec<Ty>, NotAnEnum> {
        let ty_def = self.get_ty_def(ty).expect("type should be registered");
        let &TyDef::Enum { enum_, ref gen_args } = ty_def else {
            return Err(NotAnEnum(ty));
        };

        let enum_def = self.get_enum_def(enum_).expect("enum definition should be registered");
        let base_variant_structs: Vec<Struct> = enum_def.variants.iter().map(|variant| variant.struct_).collect();
        let gen_args = gen_args.clone();
        let instantiated_variant_struct_tys: Vec<Ty> = base_variant_structs
            .into_iter()
            .map(|variant_ty| {
                let gen_args = gen_args.clone();
                self.instantiate_struct(variant_ty, gen_args).unwrap()
            })
            .collect();
        Ok(instantiated_variant_struct_tys)
    }

    pub fn get_struct_field_names(&mut self, ty: Ty) -> Result<impl IntoIterator<Item = &str>, NotAStruct> {
        let ty_def = self.get_ty_def(ty).expect("type should be registered");
        let &TyDef::Struct { struct_, .. } = ty_def else {
            return Err(NotAStruct(ty));
        };

        let struct_def = self
            .get_struct_def(struct_)
            .expect("struct definition should be registered");
        Ok(struct_def.fields.iter().map(|field| field.name.as_str()))
    }

    pub fn get_enum_variant_names(&self, ty: Ty) -> Result<impl IntoIterator<Item = &str>, NotAnEnum> {
        let ty_def = self.get_ty_def(ty).expect("type should be registered");
        let &TyDef::Enum { enum_, .. } = ty_def else {
            return Err(NotAnEnum(ty));
        };

        let enum_def = self.get_enum_def(enum_).expect("enum definition should be registered");
        Ok(enum_def.variants.iter().map(|variant| variant.name.as_str()))
    }

    pub fn get_struct_field_index_by_name(&self, struct_ty: Ty, field_name: &str) -> Result<usize, NotAStructField> {
        let ty_def = self.get_ty_def(struct_ty).expect("type should be registered");
        let &TyDef::Struct { struct_, .. } = ty_def else {
            return Err(NotAStructField::NotAStruct(struct_ty));
        };

        let struct_def = self
            .get_struct_def(struct_)
            .expect("struct definition should be registered");
        struct_def
            .fields
            .iter()
            .position(|field| field.name == field_name)
            .ok_or_else(|| NotAStructField::NotAFieldName(struct_ty, field_name.to_string()))
    }
}
