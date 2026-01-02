use std::collections::HashMap;

use crate::{
    ctxt::{fns, traits::Trait, ty::*},
    hlr,
};

#[derive(Default)]
pub struct TyReg {
    tys: Vec<Option<TyDef>>,

    i32_ty: Option<Ty>,
    bool_ty: Option<Ty>,
    unit_ty: Option<Ty>,
    c_void_ty: Option<Ty>,
    c_char_ty: Option<Ty>,

    structs: Vec<StructDef>,
    enums: Vec<EnumDef>,
    gen_var_names: Vec<String>,

    tys_inv: HashMap<TyDef, Ty>,
    named_tys: HashMap<String, Named>,

    constraints: Vec<Constraint>,
    obligations: Vec<Obligation>,
}

#[derive(Clone, Copy)]
pub enum Named {
    Ty(Ty),
    Struct(Struct),
    Enum(Enum),
}

#[derive(Debug)]
pub enum UnificationError {
    FunctionParamCountMismatch,
    TypeMismatch,
    TupleLengthMismatch,
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
        self.i32_ty = Some(self.register_named_ty("i32", TyDef::Primitive(Primitive::Integer32))?);
        self.bool_ty = Some(self.register_named_ty("bool", TyDef::Primitive(Primitive::Boolean))?);
        self.unit_ty = Some(self.register_named_ty("()", TyDef::Primitive(Primitive::Unit))?);
        self.c_void_ty = Some(self.register_named_ty("c_void", TyDef::Primitive(Primitive::CVoid))?);
        self.c_char_ty = Some(self.register_named_ty("c_char", TyDef::Primitive(Primitive::CChar))?);
        Ok(())
    }

    pub fn register_tuple_ty(&mut self, tys: impl Into<Vec<Ty>>) -> Ty {
        let tuple_ty = TyDef::Tuple(tys.into());
        self.register_ty(tuple_ty)
    }

    pub fn register_struct(&mut self, name: &str, gen_param_names: &[String]) -> Result<Struct, ()> {
        let gen_params: Vec<_> = gen_param_names
            .iter()
            .map(|gp_name| self.register_gen_var(gp_name))
            .collect();

        self.register_struct_with_existing_gen_vars(name, gen_params)
    }

    pub fn register_struct_with_existing_gen_vars(
        &mut self,
        name: &str,
        gen_params: impl Into<Vec<GenVar>>,
    ) -> Result<Struct, ()> {
        let struct_ = Struct(self.structs.len());

        let struct_def = StructDef {
            name: name.to_string(),
            gen_params: gen_params.into(),
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

    pub fn register_fn_ty(&mut self, param_tys: impl Into<Vec<Ty>>, return_ty: Ty, var_args: bool) -> Ty {
        let fn_ty = TyDef::Fn {
            param_tys: param_tys.into(),
            var_args,
            return_ty,
        };
        self.register_ty(fn_ty)
    }

    pub fn register_ref_ty(&mut self, inner_ty: Ty) -> Ty {
        let ref_ty = TyDef::Ref(inner_ty);
        self.register_ty(ref_ty)
    }

    pub fn register_ptr_ty(&mut self, inner_ty: Ty) -> Ty {
        let ptr_ty = TyDef::Ptr(inner_ty);
        self.register_ty(ptr_ty)
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

    pub fn register_trait_self_type(&mut self, trait_: Trait) -> Ty {
        let trait_self = TyDef::TraitSelf(trait_);
        self.register_ty(trait_self)
    }

    pub fn register_closure_ty(
        &mut self,
        fn_spec: fns::FnSpecialization,
        name: impl Into<String>,
        captures_ty: Ty,
    ) -> Ty {
        let closure = TyDef::Closure {
            fn_spec,
            name: name.into(),
            captures_ty,
        };
        self.register_ty(closure)
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

    pub fn get_struct_def(&self, struct_: Struct) -> Option<&StructDef> {
        self.structs.get(struct_.0)
    }

    pub fn get_mut_struct_def(&mut self, struct_: Struct) -> Option<&mut StructDef> {
        self.structs.get_mut(struct_.0)
    }

    pub fn get_enum_def(&self, enum_: Enum) -> Option<&EnumDef> {
        self.enums.get(enum_.0)
    }

    pub fn get_mut_enum_def(&mut self, enum_: Enum) -> Option<&mut EnumDef> {
        self.enums.get_mut(enum_.0)
    }

    pub fn is_enum_ty(&self, base_ty: Ty) -> bool {
        let ty_def = self.get_ty_def(base_ty);
        matches!(ty_def, Some(TyDef::Enum { .. }))
    }

    pub fn is_c_void_ty(&self, base_ty: Ty) -> bool {
        let ty_def = self.get_ty_def(base_ty);
        matches!(ty_def, Some(TyDef::Primitive(Primitive::CVoid)))
    }

    pub fn try_resolve_hlr_annot(
        &mut self,
        annot: &hlr::TyAnnot,
        gen_vars: &[GenVar],
        self_ty: Option<Ty>,
        allow_wildcards: bool,
    ) -> Option<Ty> {
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
            Ref(ty_annot) => self
                .try_resolve_hlr_annot(ty_annot, gen_vars, self_ty, allow_wildcards)
                .map(|inner| self.register_ref_ty(inner)),
            Ptr(ty_annot) => self
                .try_resolve_hlr_annot(ty_annot, gen_vars, self_ty, allow_wildcards)
                .map(|inner| self.register_ptr_ty(inner)),
            Unit => Some(self.get_primitive_ty(Primitive::Unit)),
            Fn { param_tys, return_ty } => {
                let param_tys: Vec<Ty> = param_tys
                    .iter()
                    .map(|pt| self.try_resolve_hlr_annot(pt, gen_vars, self_ty, allow_wildcards))
                    .collect::<Option<Vec<_>>>()?;

                let return_ty = match return_ty {
                    Some(rt) => self.try_resolve_hlr_annot(rt, gen_vars, self_ty, allow_wildcards),
                    None => Some(self.get_primitive_ty(Primitive::Unit)),
                }?;

                Some(self.register_fn_ty(param_tys, return_ty, false))
            }
            Generic(ident) => {
                let gen_args: Vec<Ty> = ident
                    .gen_args
                    .iter()
                    .map(|arg_annot| self.try_resolve_hlr_annot(arg_annot, gen_vars, self_ty, allow_wildcards))
                    .collect::<Option<Vec<_>>>()?;

                match *self.named_tys.get(&ident.ident)? {
                    self::Named::Struct(struct_) => self.instantiate_struct(struct_, gen_args).ok(),
                    self::Named::Enum(enum_) => self.instantiate_enum(enum_, gen_args).ok(),
                    self::Named::Ty(..) => None,
                }
            }
            Wildcard => allow_wildcards.then(|| self.new_undefined_ty()),
            Self_ => Some(self_ty.expect("self type not available")),
            Tuple(ty_annots) => {
                let tys: Vec<Ty> = ty_annots
                    .iter()
                    .map(|ty_annot| self.try_resolve_hlr_annot(ty_annot, gen_vars, self_ty, allow_wildcards))
                    .collect::<Option<Vec<_>>>()?;
                Some(self.register_tuple_ty(tys))
            }
        }
    }

    pub fn get_string_rep(&self, ty: Ty) -> String {
        self.get_string_rep_with_subst(ty, &HashMap::new())
    }

    pub fn get_string_rep_with_subst(&self, ty: Ty, substitutions: &HashMap<GenVar, Ty>) -> String {
        use self::Primitive::*;
        use TyDef::*;

        if ty.0 >= self.tys.len() {
            return format!("<unknown type id {}>", ty.0).to_string();
        }

        let Some(ty_def) = self.get_ty_def(ty) else {
            return format!("<undefined type id {}>", ty.0).to_string();
        };

        match *ty_def {
            Primitive(primitive) => match primitive {
                Integer32 => "i32".to_string(),
                Boolean => "bool".to_string(),
                Unit => "()".to_string(),
                CVoid => "c_void".to_string(),
                CChar => "c_char".to_string(),
            },
            Tuple(ref tys) => match &tys[..] {
                [] => "()".to_string(),
                [ty] => format!("({},)", self.get_string_rep_with_subst(*ty, substitutions)),
                _ => format!(
                    "({})",
                    tys.iter()
                        .map(|&ty| self.get_string_rep_with_subst(ty, substitutions))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            },
            Fn {
                ref param_tys,
                return_ty,
                var_args,
            } => {
                let mut param_names: Vec<_> = param_tys
                    .iter()
                    .map(|&pt| self.get_string_rep_with_subst(pt, substitutions))
                    .collect();
                if var_args {
                    param_names.push("...".to_string());
                }
                let return_name = self.get_string_rep_with_subst(return_ty, substitutions);
                format!("fn({}) -> {}", param_names.join(", "), return_name)
            }
            Ref(ty) => format!("&{}", self.get_string_rep_with_subst(ty, substitutions)),
            Ptr(ty) => format!("*{}", self.get_string_rep_with_subst(ty, substitutions)),
            Alias(ty) => self.get_string_rep_with_subst(ty, substitutions),
            GenVar(gen_var) => substitutions
                .get(&gen_var)
                .map(|&ty| self.get_string_rep_with_subst(ty, substitutions))
                .unwrap_or(self.get_gen_var_name(gen_var).to_string()),
            Struct { struct_, ref gen_args } => {
                let struct_name = self.get_struct_name(struct_);
                if gen_args.is_empty() {
                    return struct_name;
                }
                let gen_arg_names = gen_args
                    .iter()
                    .map(|&ga| self.get_string_rep_with_subst(ga, substitutions))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}<{}>", struct_name, gen_arg_names)
            }
            Enum { enum_, ref gen_args } => {
                let enum_name = self.get_enum_name(enum_);
                if gen_args.is_empty() {
                    return enum_name;
                }
                let gen_arg_names = gen_args
                    .iter()
                    .map(|&ga| self.get_string_rep_with_subst(ga, substitutions))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}<{}>", enum_name, gen_arg_names)
            }
            TraitSelf(_) => "self".to_string(),
            Closure { ref name, .. } => name.clone(),
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
                        var_args: var_args1,
                    },
                    &Fn {
                        param_tys: ref params2,
                        return_ty: ret2,
                        var_args: var_args2,
                    },
                ) => {
                    if params1.len() != params2.len() {
                        return Err(UnificationError::FunctionParamCountMismatch);
                    }

                    if var_args1 != var_args2 {
                        return Err(UnificationError::TypeMismatch);
                    }

                    let pairs = params1.clone().into_iter().zip(params2.clone());
                    for (p1, p2) in pairs {
                        self.unify(p1, p2)?;
                    }
                    self.unify(ret1, ret2)
                }

                (&Ref(inner1), &Ref(inner2)) | (&Ptr(inner1), &Ptr(inner2)) => self.unify(inner1, inner2),

                (
                    &Struct {
                        struct_: struct_1,
                        gen_args: ref gen_args_1,
                    },
                    &Struct {
                        struct_: struct_2,
                        gen_args: ref gen_args_2,
                    },
                ) => {
                    if struct_1 != struct_2 || gen_args_1.len() != gen_args_2.len() {
                        return Err(UnificationError::TypeMismatch);
                    }

                    let pairs = gen_args_1.clone().into_iter().zip(gen_args_2.clone());
                    for (arg1, arg2) in pairs {
                        self.unify(arg1, arg2)?;
                    }
                    Ok(())
                }

                (
                    &Enum {
                        enum_: enum_1,
                        gen_args: ref gen_args_1,
                    },
                    &Enum {
                        enum_: enum_2,
                        gen_args: ref gen_args_2,
                    },
                ) => {
                    if enum_1 != enum_2 || gen_args_1.len() != gen_args_2.len() {
                        return Err(UnificationError::TypeMismatch);
                    }

                    let pairs = gen_args_1.clone().into_iter().zip(gen_args_2.clone());
                    for (arg1, arg2) in pairs {
                        self.unify(arg1, arg2)?;
                    }
                    Ok(())
                }

                (Closure { fn_spec: fn_spec1, .. }, Closure { fn_spec: fn_spec2, .. }) => {
                    if fn_spec1 != fn_spec2 {
                        return Err(UnificationError::TypeMismatch);
                    }

                    let fn_spec1 = fn_spec1.clone();
                    let fn_spec2 = fn_spec2.clone();

                    let gen_args_pairs = fn_spec1.gen_args.iter().zip(&fn_spec2.gen_args);
                    for (arg1, arg2) in gen_args_pairs {
                        self.unify(*arg1, *arg2)?;
                    }
                    let env_gen_args_pairs = fn_spec1.env_gen_args.iter().zip(&fn_spec2.env_gen_args);
                    for (arg1, arg2) in env_gen_args_pairs {
                        self.unify(*arg1, *arg2)?;
                    }

                    Ok(())
                }

                (Tuple(tys1), Tuple(tys2)) => {
                    if tys1.len() != tys2.len() {
                        return Err(UnificationError::TupleLengthMismatch);
                    }

                    let pairs = tys1.clone().into_iter().zip(tys2.clone());
                    for (ty1, ty2) in pairs {
                        self.unify(ty1, ty2)?;
                    }
                    Ok(())
                }
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
            Primitive::CVoid => self.c_void_ty,
            Primitive::CChar => self.c_char_ty,
        }
        .expect("primitive type should be registered")
    }

    pub fn get_all_enums(&self) -> impl Iterator<Item = (Enum, &EnumDef)> {
        self.enums
            .iter()
            .enumerate()
            .map(|(enum_, enum_def)| (Enum(enum_), enum_def))
    }

    pub fn substitute_gen_vars(&mut self, ty: Ty, substitutions: &HashMap<GenVar, Ty>) -> Ty {
        use TyDef::*;

        let ty = self.canonicalize(ty);
        let Some(ty_def) = self.tys.get(ty.0).expect("ty should be registered") else {
            return ty;
        };

        match *ty_def {
            GenVar(gen_var) => {
                if let Some(replacement_ty) = substitutions.get(&gen_var) {
                    *replacement_ty
                } else {
                    ty
                }
            }
            Fn {
                ref param_tys,
                return_ty,
                var_args,
            } => {
                let new_param_tys = param_tys
                    .clone()
                    .into_iter()
                    .map(|pt| self.substitute_gen_vars(pt, substitutions))
                    .collect::<Vec<_>>();
                let new_return_ty = self.substitute_gen_vars(return_ty, substitutions);
                self.register_fn_ty(new_param_tys, new_return_ty, var_args)
            }
            Ref(inner_ty) => {
                let new_inner_ty = self.substitute_gen_vars(inner_ty, substitutions);
                self.register_ref_ty(new_inner_ty)
            }
            Ptr(inner_ty) => {
                let new_inner_ty = self.substitute_gen_vars(inner_ty, substitutions);
                self.register_ptr_ty(new_inner_ty)
            }
            Struct { struct_, ref gen_args } => {
                let new_gen_args = gen_args
                    .clone()
                    .iter()
                    .map(|&ga| self.substitute_gen_vars(ga, substitutions))
                    .collect::<Vec<_>>();
                self.instantiate_struct(struct_, new_gen_args).unwrap()
            }
            Enum { enum_, ref gen_args } => {
                let new_gen_args = gen_args
                    .clone()
                    .iter()
                    .map(|&ga| self.substitute_gen_vars(ga, substitutions))
                    .collect::<Vec<_>>();
                self.instantiate_enum(enum_, new_gen_args).unwrap()
            }
            Primitive(..) => ty,
            Alias(..) => unreachable!("ty should have been canonicalized"),
            TraitSelf(_) => ty,
            Closure {
                ref fn_spec,
                ref name,
                captures_ty,
            } => {
                let name = name.clone();
                let mut new_fn_spec = fn_spec.clone();
                for gen_arg in &mut new_fn_spec.gen_args {
                    *gen_arg = self.substitute_gen_vars(*gen_arg, substitutions);
                }
                for env_gen_arg in &mut new_fn_spec.env_gen_args {
                    *env_gen_arg = self.substitute_gen_vars(*env_gen_arg, substitutions);
                }

                let captures_ty = self.substitute_gen_vars(captures_ty, substitutions);

                self.register_closure_ty(new_fn_spec, name, captures_ty)
            }
            Tuple(ref tys) => {
                let new_tys = tys
                    .clone()
                    .into_iter()
                    .map(|ty| self.substitute_gen_vars(ty, substitutions))
                    .collect::<Vec<_>>();
                self.register_tuple_ty(new_tys)
            }
        }
    }

    pub fn substitute_self_ty(&mut self, ty: Ty, substitute: Ty) -> Ty {
        use TyDef::*;

        let ty = self.canonicalize(ty);
        let Some(ty_def) = self.tys.get(ty.0).expect("ty should be registered") else {
            return ty;
        };

        match *ty_def {
            GenVar(..) => ty,
            Fn {
                ref param_tys,
                return_ty,
                var_args,
            } => {
                let new_param_tys = param_tys
                    .clone()
                    .into_iter()
                    .map(|pt| self.substitute_self_ty(pt, substitute))
                    .collect::<Vec<_>>();
                let new_return_ty = self.substitute_self_ty(return_ty, substitute);
                self.register_fn_ty(new_param_tys, new_return_ty, var_args)
            }
            Ref(inner_ty) => {
                let new_inner_ty = self.substitute_self_ty(inner_ty, substitute);
                self.register_ref_ty(new_inner_ty)
            }
            Ptr(inner_ty) => {
                let new_inner_ty = self.substitute_self_ty(inner_ty, substitute);
                self.register_ptr_ty(new_inner_ty)
            }
            Struct { struct_, ref gen_args } => {
                let new_gen_args = gen_args
                    .clone()
                    .iter()
                    .map(|&ga| self.substitute_self_ty(ga, substitute))
                    .collect::<Vec<_>>();
                self.instantiate_struct(struct_, new_gen_args).unwrap()
            }
            Enum { enum_, ref gen_args } => {
                let new_gen_args = gen_args
                    .clone()
                    .iter()
                    .map(|&ga| self.substitute_self_ty(ga, substitute))
                    .collect::<Vec<_>>();
                self.instantiate_enum(enum_, new_gen_args).unwrap()
            }
            Primitive(..) => ty,
            Alias(..) => unreachable!("ty should have been canonicalized"),
            TraitSelf(_) => substitute,
            Closure {
                ref fn_spec,
                ref name,
                captures_ty,
            } => {
                let name = name.clone();
                let mut new_fn_spec = fn_spec.clone();
                for gen_arg in &mut new_fn_spec.gen_args {
                    *gen_arg = self.substitute_self_ty(*gen_arg, substitute);
                }
                for env_gen_arg in &mut new_fn_spec.env_gen_args {
                    *env_gen_arg = self.substitute_self_ty(*env_gen_arg, substitute);
                }

                self.register_closure_ty(new_fn_spec, name, captures_ty)
            }
            Tuple(ref tys) => {
                let new_tys = tys
                    .clone()
                    .into_iter()
                    .map(|ty| self.substitute_self_ty(ty, substitute))
                    .collect::<Vec<_>>();
                self.register_tuple_ty(new_tys)
            }
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

    pub fn get_tuple_field_tys(&self, ty: Ty) -> Result<&Vec<Ty>, ()> {
        let ty_def = self.get_ty_def(ty).expect("type should be registered");
        let &TyDef::Tuple(ref tys) = ty_def else {
            return Err(());
        };
        Ok(tys)
    }

    pub fn get_struct_field_names(&mut self, ty: Ty) -> Result<impl Iterator<Item = &str>, NotAStruct> {
        let ty_def = self.get_ty_def(ty).expect("type should be registered");
        let &TyDef::Struct { struct_, .. } = ty_def else {
            return Err(NotAStruct(ty));
        };

        let struct_def = self
            .get_struct_def(struct_)
            .expect("struct definition should be registered");
        Ok(struct_def.fields.iter().map(|field| field.name.as_str()))
    }

    pub fn get_enum_variant_names(&self, ty: Ty) -> Result<impl Iterator<Item = &str>, NotAnEnum> {
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

    pub fn tys_eq(&self, ty1: Ty, ty2: Ty) -> bool {
        use TyDef::*;

        let ty1 = self.canonicalize(ty1);
        let ty2 = self.canonicalize(ty2);

        if ty1 == ty2 {
            return true;
        }

        let def1 = self.tys.get(ty1.0).expect("ty1 should be registered");
        let def2 = self.tys.get(ty2.0).expect("ty2 should be registered");

        match (def1, def2) {
            (None, _) | (_, None) => false,
            (Some(def1), Some(def2)) => match (def1, def2) {
                (Alias(_), _) | (_, Alias(_)) => {
                    unreachable!("Types should have been canonicalized");
                }

                (
                    &Fn {
                        param_tys: ref params1,
                        return_ty: ret1,
                        var_args: var_args1,
                    },
                    &Fn {
                        param_tys: ref params2,
                        return_ty: ret2,
                        var_args: var_args2,
                    },
                ) => {
                    params1.len() == params2.len()
                        && params1.iter().zip(params2.iter()).all(|(p1, p2)| self.tys_eq(*p1, *p2))
                        && self.tys_eq(ret1, ret2)
                        && var_args1 == var_args2
                }

                (&Ref(inner1), &Ref(inner2)) | (&Ptr(inner1), &Ptr(inner2)) => self.tys_eq(inner1, inner2),

                (
                    &Struct {
                        struct_: struct_1,
                        gen_args: ref gen_args_1,
                    },
                    &Struct {
                        struct_: struct_2,
                        gen_args: ref gen_args_2,
                    },
                ) => {
                    struct_1 == struct_2
                        && gen_args_1.len() == gen_args_2.len()
                        && gen_args_1
                            .iter()
                            .zip(gen_args_2.iter())
                            .all(|(arg1, arg2)| self.tys_eq(*arg1, *arg2))
                }

                (
                    &Enum {
                        enum_: enum_1,
                        gen_args: ref gen_args_1,
                    },
                    &Enum {
                        enum_: enum_2,
                        gen_args: ref gen_args_2,
                    },
                ) => {
                    enum_1 == enum_2
                        && gen_args_1.len() == gen_args_2.len()
                        && gen_args_1
                            .iter()
                            .zip(gen_args_2.iter())
                            .all(|(arg1, arg2)| self.tys_eq(*arg1, *arg2))
                }

                (Closure { fn_spec: fn_spec1, .. }, Closure { fn_spec: fn_spec2, .. }) => {
                    fn_spec1.fn_ == fn_spec2.fn_
                        && fn_spec1.gen_args.len() == fn_spec2.gen_args.len()
                        && fn_spec1
                            .gen_args
                            .iter()
                            .zip(&fn_spec2.gen_args)
                            .all(|(arg1, arg2)| self.tys_eq(*arg1, *arg2))
                        && fn_spec1.env_gen_args.len() == fn_spec2.env_gen_args.len()
                        && fn_spec1
                            .env_gen_args
                            .iter()
                            .zip(&fn_spec2.env_gen_args)
                            .all(|(arg1, arg2)| self.tys_eq(*arg1, *arg2))
                }

                (Tuple(tys1), Tuple(tys2)) => {
                    tys1.len() == tys2.len() && tys1.iter().zip(tys2).all(|(&ty1, &ty2)| self.tys_eq(ty1, ty2))
                }

                _ => false,
            },
        }
    }

    pub fn try_find_instantiation(&self, target: Ty, generic: Ty, gen_vars: &[GenVar]) -> Result<Vec<Ty>, ()> {
        let mut instantiations = HashMap::new();
        for gen_param in gen_vars {
            instantiations.insert(*gen_param, None);
        }

        if self.try_find_instantiation_internal(target, generic, &mut instantiations) {
            gen_vars
                .iter()
                .map(|gen_var| instantiations[gen_var])
                .collect::<Option<_>>()
                .ok_or(())
        } else {
            Err(())
        }
    }

    fn try_find_instantiation_internal(
        &self,
        target: Ty,
        generic: Ty,
        instantiation: &mut HashMap<GenVar, Option<Ty>>,
    ) -> bool {
        use TyDef::*;

        let Some(target_def) = self.get_ty_def(target) else {
            return false;
        };
        let Some(generic_def) = self.get_ty_def(generic) else {
            return false;
        };

        match (target_def, generic_def) {
            (Alias(_), _) | (_, Alias(_)) => {
                unreachable!("Types should have been canonicalized");
            }

            (_, &GenVar(gen_var)) if instantiation.contains_key(&gen_var) => {
                let substitute = instantiation.get(&gen_var).unwrap();
                if let Some(substitute) = substitute {
                    self.tys_eq(*substitute, target)
                } else {
                    instantiation.insert(gen_var, Some(target));
                    true
                }
            }

            (GenVar(var1), GenVar(var2)) => var1 == var2,

            (&Primitive(a), &Primitive(b)) => a == b,

            (
                &Fn {
                    param_tys: ref param_tys1,
                    return_ty: ret1,
                    var_args: var_args1,
                },
                &Fn {
                    param_tys: ref param_tys2,
                    return_ty: ret2,
                    var_args: var_args2,
                },
            ) => {
                param_tys1.len() == param_tys2.len()
                    && param_tys1
                        .iter()
                        .zip(param_tys2.iter())
                        .all(|(param1, param2)| self.try_find_instantiation_internal(*param1, *param2, instantiation))
                    && self.try_find_instantiation_internal(ret1, ret2, instantiation)
                    && var_args1 == var_args2
            }

            (&Ref(inner1), &Ref(inner2)) | (&Ptr(inner1), &Ptr(inner2)) => {
                self.try_find_instantiation_internal(inner1, inner2, instantiation)
            }

            (
                &Struct {
                    struct_: struct_1,
                    gen_args: ref gen_args_1,
                },
                &Struct {
                    struct_: struct_2,
                    gen_args: ref gen_args_2,
                },
            ) => {
                struct_1 == struct_2
                    && gen_args_1.len() == gen_args_2.len()
                    && gen_args_1
                        .iter()
                        .zip(gen_args_2.iter())
                        .all(|(arg1, arg2)| self.try_find_instantiation_internal(*arg1, *arg2, instantiation))
            }

            (
                &Enum {
                    enum_: enum_1,
                    gen_args: ref gen_args_1,
                },
                &Enum {
                    enum_: enum_2,
                    gen_args: ref gen_args_2,
                },
            ) => {
                enum_1 == enum_2
                    && gen_args_1.len() == gen_args_2.len()
                    && gen_args_1
                        .iter()
                        .zip(gen_args_2.iter())
                        .all(|(arg1, arg2)| self.try_find_instantiation_internal(*arg1, *arg2, instantiation))
            }

            (Closure { fn_spec: fn_spec1, .. }, Closure { fn_spec: fn_spec2, .. }) => {
                fn_spec1.fn_ == fn_spec2.fn_
                    && fn_spec1.gen_args.len() == fn_spec2.gen_args.len()
                    && fn_spec1
                        .gen_args
                        .iter()
                        .zip(&fn_spec2.gen_args)
                        .all(|(arg1, arg2)| self.try_find_instantiation_internal(*arg1, *arg2, instantiation))
                    && fn_spec1.env_gen_args.len() == fn_spec2.env_gen_args.len()
                    && fn_spec1
                        .env_gen_args
                        .iter()
                        .zip(&fn_spec2.env_gen_args)
                        .all(|(arg1, arg2)| self.try_find_instantiation_internal(*arg1, *arg2, instantiation))
            }

            (Tuple(tys1), Tuple(tys2)) => {
                tys1.len() == tys2.len()
                    && tys1
                        .iter()
                        .zip(tys2)
                        .all(|(ty1, ty2)| self.try_find_instantiation_internal(*ty1, *ty2, instantiation))
            }

            (_, _) => false,
        }
    }

    pub fn add_implements_trait_constraint(&mut self, subject: GenVar, trait_: Trait) {
        self.constraints.push(Constraint {
            subject,
            requirement: ConstraintRequirement::Trait(trait_),
        });
    }

    pub fn add_callable_constraint(&mut self, subject: GenVar, params: Vec<Ty>, return_ty: Ty) {
        self.constraints.push(Constraint {
            subject,
            requirement: ConstraintRequirement::Callable {
                param_tys: params,
                return_ty,
            },
        });
    }

    pub fn implements_trait_constraint_exists(&self, gen_var: GenVar, trait_: Trait) -> bool {
        self.constraints
            .iter()
            .any(|c| c.subject == gen_var && c.requirement == ConstraintRequirement::Trait(trait_))
    }

    pub fn try_get_callable_obligation(&self, subject: Ty) -> Option<(Vec<Ty>, Ty)> {
        self.obligations
            .iter()
            .filter_map(|obligation| {
                if let Obligation::Callable {
                    ty,
                    param_tys,
                    return_ty,
                } = obligation
                    && self.tys_eq(*ty, subject)
                {
                    Some((param_tys.clone(), *return_ty))
                } else {
                    None
                }
            })
            .next()
    }

    pub fn try_get_callable_constraint(&self, subject: GenVar) -> Option<(Vec<Ty>, Ty)> {
        self.constraints
            .iter()
            .filter_map(|c| {
                if c.subject == subject {
                    if let ConstraintRequirement::Callable { param_tys, return_ty } = &c.requirement {
                        Some((param_tys.clone(), *return_ty))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .next()
    }

    pub fn get_requirements_for(&self, subject: GenVar) -> impl Iterator<Item = &ConstraintRequirement> {
        self.constraints.iter().filter_map(move |c| {
            if c.subject == subject {
                Some(&c.requirement)
            } else {
                None
            }
        })
    }

    pub fn add_implements_trait_obligation(&mut self, ty: Ty, trait_: Trait) {
        self.obligations.push(Obligation::ImplementsTrait { ty, trait_ });
    }

    pub fn add_callable_obligation(&mut self, ty: Ty, param_tys: Vec<Ty>, return_ty: Ty) {
        self.obligations.push(Obligation::Callable {
            ty,
            param_tys,
            return_ty,
        });
    }

    pub fn get_all_obligations(&self) -> &[Obligation] {
        &self.obligations
    }
}
