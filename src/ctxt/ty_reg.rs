use itertools::Itertools;
use std::collections::{BTreeMap, HashMap};

use crate::{
    ctxt::{fns::GenParam, ty::*},
    hlr,
};

pub struct TyReg {
    tys: HashMap<Ty, TyDef>,
    structs: HashMap<Struct, StructDef>,
    enums: HashMap<Enum, EnumDef>,

    i32_ty: Option<Ty>,
    bool_ty: Option<Ty>,
    unit_ty: Option<Ty>,

    named_tys: BTreeMap<String, Ty>,

    next_ty: Ty,
    next_struct: Struct,
    next_enum: Enum,
}

pub enum UnificationError {
    FunctionParamCountMismatch,
    TypeMismatch,
}

impl TyReg {
    pub fn new() -> TyReg {
        TyReg {
            tys: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),

            i32_ty: None,
            bool_ty: None,
            unit_ty: None,

            named_tys: BTreeMap::new(),

            next_ty: Ty(0),
            next_struct: Struct(0),
            next_enum: Enum(0),
        }
    }

    fn register_ty(&mut self, ty_def: TyDef) -> Ty {
        let ty = self.next_ty;
        self.next_ty.0 += 1;
        self.tys.insert(ty, ty_def);
        ty
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
        let struct_ = self.next_struct;
        self.next_struct.0 += 1;

        let ty = self.register_named_ty(name, Named::Struct(struct_))?;
        self.structs.insert(struct_, StructDef { fields: vec![] });
        Ok(ty)
    }

    pub fn register_enum(&mut self, name: &str) -> Result<Ty, ()> {
        let enum_ = self.next_enum;
        self.next_enum.0 += 1;

        let ty = self.register_named_ty(name, Named::Enum(enum_))?;
        self.enums.insert(enum_, EnumDef { variants: vec![] });
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

    pub fn get_ty_def(&self, id: &Ty) -> Option<&TyDef> {
        let id = self.canonicalize(id);
        self.tys.get(&id)
    }

    pub fn get_ty_by_name(&self, name: &str) -> Option<Ty> {
        self.named_tys.get(name).cloned()
    }

    pub fn get_ty_by_hlr_annot(&mut self, annot: &hlr::TyAnnot, gen_vars: &Vec<GenParam>) -> Option<Ty> {
        use hlr::TyAnnot::*;

        match annot {
            Named(name) => gen_vars
                .iter()
                .find_map(|gp| (&gp.name == name).then_some(gp.ty))
                .or_else(|| self.get_ty_by_name(name)),
            Reference(ty_annot) => self
                .get_ty_by_hlr_annot(ty_annot, gen_vars)
                .map(|inner| self.register_ref_ty(inner)),
            Unit => self.get_primitive_ty(Primitive::Unit),
            Fn { param_tys, return_ty } => {
                let param_tys: Vec<Ty> = param_tys
                    .iter()
                    .map(|pt| self.get_ty_by_hlr_annot(pt, gen_vars))
                    .collect::<Option<Vec<_>>>()?;

                let return_ty = match return_ty {
                    Some(rt) => self.get_ty_by_hlr_annot(rt, gen_vars),
                    None => self.get_primitive_ty(Primitive::Unit),
                }?;

                Some(self.register_fn_ty(param_tys, return_ty))
            }
        }
    }

    fn get_ty_def_by_name(&self, name: &str) -> Option<&TyDef> {
        let ty = self.get_ty_by_name(name)?;
        self.tys.get(&ty)
    }

    pub fn get_struct_def(&self, struct_: &Struct) -> Option<&StructDef> {
        self.structs.get(struct_)
    }

    pub fn get_enum_def(&self, enum_: &Enum) -> Option<&EnumDef> {
        self.enums.get(enum_)
    }

    pub fn get_mut_struct_def_by_name(&mut self, name: &str) -> Option<&mut StructDef> {
        let ty_def = self.get_ty_def_by_name(name)?;
        if let TyDef::Named(_, Named::Struct(struct_)) = ty_def {
            self.structs.get_mut(&struct_.clone())
        } else {
            None
        }
    }

    pub fn get_mut_enum_def_by_name(&mut self, name: &str) -> Option<&mut EnumDef> {
        let ty_def = self.get_ty_def_by_name(name)?;
        if let TyDef::Named(_, Named::Enum(enum_)) = ty_def {
            self.enums.get_mut(&enum_.clone())
        } else {
            None
        }
    }

    pub fn get_string_rep(&self, ty: &Ty) -> String {
        let Some(ty_def) = self.tys.get(ty) else {
            return format!("<type id {}>", ty.0).to_string();
        };

        match ty_def {
            TyDef::Named(name, _) => name.clone(),
            TyDef::Fn { param_tys, return_ty } => {
                let mut param_names = param_tys.iter().map(|pt| self.get_string_rep(pt));
                let return_name = self.get_string_rep(return_ty);
                format!("fn({}) -> {}", param_names.join(", "), return_name)
            }
            TyDef::Ref(ty) => format!("&{}", self.get_string_rep(ty)),
            TyDef::Alias(ty) => self.get_string_rep(ty),
            TyDef::Undef => "<undefined>".to_string(),
            TyDef::GenVar(name) => name.clone(),
        }
    }

    pub fn unify(&mut self, t1: &Ty, t2: &Ty) -> Result<(), UnificationError> {
        use TyDef::*;

        let t1 = self.canonicalize(t1);
        let t2 = self.canonicalize(t2);

        if t1 == t2 {
            return Ok(());
        }

        let def1 = self.tys.get(&t1).unwrap().clone();
        let def2 = self.tys.get(&t2).unwrap().clone();

        match (def1, def2) {
            (Alias(_), _) | (_, Alias(_)) => {
                unreachable!("Types should have been canonicalized");
            }

            (Undef, _) => {
                self.tys.insert(t1, Alias(t2));
                Ok(())
            }
            (_, Undef) => {
                self.tys.insert(t2, Alias(t1));
                Ok(())
            }

            (
                Fn {
                    param_tys: params1,
                    return_ty: ret1,
                },
                Fn {
                    param_tys: params2,
                    return_ty: ret2,
                },
            ) => {
                if params1.len() != params2.len() {
                    return Err(UnificationError::FunctionParamCountMismatch);
                }

                for (p1, p2) in params1.iter().zip(params2.iter()) {
                    self.unify(p1, p2)?;
                }
                self.unify(&ret1, &ret2)
            }

            (Ref(inner1), Ref(inner2)) => self.unify(&inner1, &inner2),

            (Named(_, n1), Named(_, n2)) if n1 == n2 => Ok(()),

            _ => Err(UnificationError::TypeMismatch),
        }
    }

    pub fn canonicalize(&self, ty: &Ty) -> Ty {
        let mut current_ty = *ty;
        while let TyDef::Alias(next_ty) = self.tys.get(&current_ty).unwrap() {
            current_ty = *next_ty;
        }
        current_ty
    }

    pub fn get_primitive_ty(&self, primitive: Primitive) -> Option<Ty> {
        match primitive {
            Primitive::Integer32 => self.i32_ty,
            Primitive::Boolean => self.bool_ty,
            Primitive::Unit => self.unit_ty,
        }
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

    pub fn get_ty_of_enum(&self, enum_: &Enum) -> Option<Ty> {
        self.get_named_ty(Named::Enum(*enum_))
    }

    pub fn get_all_enums(&self) -> impl IntoIterator<Item = (&Enum, &EnumDef)> {
        self.enums.iter()
    }

    pub fn get_undef_ty(&mut self) -> Ty {
        self.register_ty(TyDef::Undef)
    }

    pub fn replace_gen_args(&mut self, ty: &Ty, substitutions: &HashMap<String, Ty>) -> Ty {
        let ty = self.canonicalize(ty);
        let ty_def = self.tys.get(&ty).unwrap().clone();

        match ty_def {
            TyDef::GenVar(name) => {
                if let Some(replacement_ty) = substitutions.get(&name) {
                    *replacement_ty
                } else {
                    ty
                }
            }
            TyDef::Fn { param_tys, return_ty } => {
                let new_param_tys = param_tys
                    .iter()
                    .map(|pt| self.replace_gen_args(pt, substitutions))
                    .collect::<Vec<_>>();
                let new_return_ty = self.replace_gen_args(&return_ty, substitutions);
                self.register_fn_ty(new_param_tys, new_return_ty)
            }
            TyDef::Ref(inner_ty) => {
                let new_inner_ty = self.replace_gen_args(&inner_ty, substitutions);
                self.register_ref_ty(new_inner_ty)
            }
            _ => ty,
        }
    }
}
