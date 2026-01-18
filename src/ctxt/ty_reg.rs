use std::collections::HashMap;

use crate::{
    ctxt::{
        fns,
        traits::{Trait, TraitInst},
        ty::*,
    },
    hlr,
};

#[derive(Default)]
pub struct TyReg {
    tys: Vec<Option<TyDef>>,

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
#[expect(clippy::enum_variant_names)]
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
#[derive(Debug)]
pub struct NotATypeName(pub String);

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

    pub fn undef_ty(&mut self) -> Ty {
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
        self.register_named_ty("i32", TyDef::Primitive(Primitive::Integer32))?;
        self.register_named_ty("bool", TyDef::Primitive(Primitive::Boolean))?;
        self.register_named_ty("c_void", TyDef::Primitive(Primitive::CVoid))?;
        self.register_named_ty("c_char", TyDef::Primitive(Primitive::CChar))?;
        Ok(())
    }

    pub fn primitive(&mut self, primitive: Primitive) -> Ty {
        let ty_def = TyDef::Primitive(primitive);
        self.register_ty(ty_def)
    }

    pub fn tuple(&mut self, tys: impl Into<Vec<Ty>>) -> Ty {
        let tuple_ty = TyDef::Tuple(tys.into());
        self.register_ty(tuple_ty)
    }

    pub fn unit(&mut self) -> Ty {
        self.tuple([])
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

    pub fn fn_(&mut self, param_tys: impl Into<Vec<Ty>>, return_ty: Ty, var_args: bool) -> Ty {
        let fn_ty = TyDef::Fn {
            param_tys: param_tys.into(),
            var_args,
            return_ty,
        };
        self.register_ty(fn_ty)
    }

    pub fn ref_(&mut self, inner_ty: Ty) -> Ty {
        let ref_ty = TyDef::Ref(inner_ty);
        self.register_ty(ref_ty)
    }

    pub fn ptr(&mut self, inner_ty: Ty) -> Ty {
        let ptr_ty = TyDef::Ptr(inner_ty);
        self.register_ty(ptr_ty)
    }

    pub fn gen_var(&mut self, gen_var: GenVar) -> Ty {
        let gen_var_ty = TyDef::GenVar(gen_var);
        self.register_ty(gen_var_ty)
    }

    pub fn register_gen_var(&mut self, name: &str) -> GenVar {
        let gen_var = GenVar(self.gen_var_names.len());
        self.gen_var_names.push(name.to_string());
        gen_var
    }

    pub fn trait_self(&mut self, trait_: Trait) -> Ty {
        let trait_self = TyDef::TraitSelf(trait_);
        self.register_ty(trait_self)
    }

    pub fn closure(&mut self, fn_inst: fns::FnInst, name: impl Into<String>, captures_ty: Ty) -> Ty {
        let closure = TyDef::Closure {
            fn_inst,
            name: name.into(),
            captures_ty,
        };
        self.register_ty(closure)
    }

    pub fn inst_struct(&mut self, struct_: Struct, gen_args: impl Into<Vec<Ty>>) -> Result<Ty, TyInstError> {
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

    pub fn inst_enum(&mut self, enum_: Enum, gen_args: impl Into<Vec<Ty>>) -> Result<Ty, TyInstError> {
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

    pub fn get_enum_by_name(&self, name: &str) -> Option<Enum> {
        match self.named_tys.get(name) {
            Some(&Named::Enum(enum_)) => Some(enum_),
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
            Path(path) => match path.segments.as_slice() {
                [hlr::PathSegment::Ident(ident)] => {
                    if let Some(gv) = gen_vars.iter().find(|&&GenVar(gv)| self.gen_var_names[gv] == *ident) {
                        let ty = self.gen_var(*gv);
                        return Some(ty);
                    }

                    match *self.named_tys.get(ident)? {
                        self::Named::Ty(ty) => Some(ty),
                        self::Named::Struct(struct_) => self.inst_struct(struct_, []).ok(),
                        self::Named::Enum(enum_) => self.inst_enum(enum_, []).ok(),
                    }
                }
                [hlr::PathSegment::Generic(hlr::GenPathSegment { ident, gen_args })] => {
                    let gen_args: Vec<Ty> = gen_args
                        .iter()
                        .map(|arg_annot| self.try_resolve_hlr_annot(arg_annot, gen_vars, self_ty, allow_wildcards))
                        .collect::<Option<Vec<_>>>()?;

                    match *self.named_tys.get(ident)? {
                        self::Named::Struct(struct_) => self.inst_struct(struct_, gen_args).ok(),
                        self::Named::Enum(enum_) => self.inst_enum(enum_, gen_args).ok(),
                        self::Named::Ty(..) => None,
                    }
                }
                [hlr::PathSegment::Self_] => Some(self_ty.expect("self type not available")),
                _ => None,
            },
            Ref(ty_annot) => self
                .try_resolve_hlr_annot(ty_annot, gen_vars, self_ty, allow_wildcards)
                .map(|inner| self.ref_(inner)),
            Ptr(ty_annot) => self
                .try_resolve_hlr_annot(ty_annot, gen_vars, self_ty, allow_wildcards)
                .map(|inner| self.ptr(inner)),
            Fn { param_tys, return_ty } => {
                let param_tys: Vec<Ty> = param_tys
                    .iter()
                    .map(|pt| self.try_resolve_hlr_annot(pt, gen_vars, self_ty, allow_wildcards))
                    .collect::<Option<Vec<_>>>()?;

                let return_ty = match return_ty {
                    Some(rt) => self.try_resolve_hlr_annot(rt, gen_vars, self_ty, allow_wildcards),
                    None => Some(self.unit()),
                }?;

                Some(self.fn_(param_tys, return_ty, false))
            }
            Wildcard => allow_wildcards.then(|| self.undef_ty()),
            Tuple(ty_annots) => {
                let tys: Vec<Ty> = ty_annots
                    .iter()
                    .map(|ty_annot| self.try_resolve_hlr_annot(ty_annot, gen_vars, self_ty, allow_wildcards))
                    .collect::<Option<Vec<_>>>()?;
                Some(self.tuple(tys))
            }
        }
    }

    pub fn get_string_rep(&self, ty: Ty) -> String {
        self.get_string_rep_with_subst(ty, &HashMap::new())
    }

    pub fn get_string_rep_with_subst(&self, ty: Ty, subst: &HashMap<GenVar, Ty>) -> String {
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
                CVoid => "c_void".to_string(),
                CChar => "c_char".to_string(),
            },
            Tuple(ref tys) => match &tys[..] {
                [] => "()".to_string(),
                [ty] => format!("({},)", self.get_string_rep_with_subst(*ty, subst)),
                _ => format!(
                    "({})",
                    tys.iter()
                        .map(|&ty| self.get_string_rep_with_subst(ty, subst))
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
                    .map(|&pt| self.get_string_rep_with_subst(pt, subst))
                    .collect();
                if var_args {
                    param_names.push("...".to_string());
                }
                let return_name = self.get_string_rep_with_subst(return_ty, subst);
                format!("fn({}) -> {}", param_names.join(", "), return_name)
            }
            Ref(ty) => format!("&{}", self.get_string_rep_with_subst(ty, subst)),
            Ptr(ty) => format!("*{}", self.get_string_rep_with_subst(ty, subst)),
            Alias(ty) => self.get_string_rep_with_subst(ty, subst),
            GenVar(gen_var) => subst
                .get(&gen_var)
                .map(|&ty| self.get_string_rep_with_subst(ty, subst))
                .unwrap_or(self.get_gen_var_name(gen_var).to_string()),
            Struct { struct_, ref gen_args } => {
                let struct_name = self.get_struct_name(struct_);
                if gen_args.is_empty() {
                    return struct_name;
                }
                let gen_arg_names = gen_args
                    .iter()
                    .map(|&ga| self.get_string_rep_with_subst(ga, subst))
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
                    .map(|&ga| self.get_string_rep_with_subst(ga, subst))
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

    /// Unify two types
    ///
    /// TODO: Instead of unifying immediately, this function should only gather types to be unified
    /// and then do the unification at the end. Otherwise, the `TyReg` is possibly poisoned once a
    /// unification fails.
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

                (Closure { fn_inst: fn_inst1, .. }, Closure { fn_inst: fn_inst2, .. }) => {
                    if fn_inst1 != fn_inst2 {
                        return Err(UnificationError::TypeMismatch);
                    }

                    let fn_inst1 = fn_inst1.clone();
                    let fn_inst2 = fn_inst2.clone();

                    let gen_args_pairs = fn_inst1.gen_args.iter().zip(&fn_inst2.gen_args);
                    for (arg1, arg2) in gen_args_pairs {
                        self.unify(*arg1, *arg2)?;
                    }
                    let env_gen_args_pairs = fn_inst1.env_gen_args.iter().zip(&fn_inst2.env_gen_args);
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

    #[must_use]
    pub fn substitute_gen_vars(&mut self, ty: Ty, subst: &GenVarSubst) -> Ty {
        use TyDef::*;

        let ty = self.canonicalize(ty);
        let Some(ty_def) = self.tys.get(ty.0).expect("ty should be registered") else {
            return ty;
        };

        match *ty_def {
            GenVar(gen_var) => {
                if let Some(replacement_ty) = subst.get(gen_var) {
                    replacement_ty
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
                    .map(|pt| self.substitute_gen_vars(pt, subst))
                    .collect::<Vec<_>>();
                let new_return_ty = self.substitute_gen_vars(return_ty, subst);
                self.fn_(new_param_tys, new_return_ty, var_args)
            }
            Ref(inner_ty) => {
                let new_inner_ty = self.substitute_gen_vars(inner_ty, subst);
                self.ref_(new_inner_ty)
            }
            Ptr(inner_ty) => {
                let new_inner_ty = self.substitute_gen_vars(inner_ty, subst);
                self.ptr(new_inner_ty)
            }
            Struct { struct_, ref gen_args } => {
                let new_gen_args = gen_args
                    .clone()
                    .iter()
                    .map(|&ga| self.substitute_gen_vars(ga, subst))
                    .collect::<Vec<_>>();
                self.inst_struct(struct_, new_gen_args).unwrap()
            }
            Enum { enum_, ref gen_args } => {
                let new_gen_args = gen_args
                    .clone()
                    .iter()
                    .map(|&ga| self.substitute_gen_vars(ga, subst))
                    .collect::<Vec<_>>();
                self.inst_enum(enum_, new_gen_args).unwrap()
            }
            Primitive(..) => ty,
            Alias(..) => unreachable!("ty should have been canonicalized"),
            TraitSelf(_) => ty,
            Closure {
                ref fn_inst,
                ref name,
                captures_ty,
            } => {
                let name = name.clone();
                let mut new_fn_inst = fn_inst.clone();
                for gen_arg in &mut new_fn_inst.gen_args {
                    *gen_arg = self.substitute_gen_vars(*gen_arg, subst);
                }
                for env_gen_arg in &mut new_fn_inst.env_gen_args {
                    *env_gen_arg = self.substitute_gen_vars(*env_gen_arg, subst);
                }

                let captures_ty = self.substitute_gen_vars(captures_ty, subst);

                self.closure(new_fn_inst, name, captures_ty)
            }
            Tuple(ref tys) => {
                let new_tys = tys
                    .clone()
                    .into_iter()
                    .map(|ty| self.substitute_gen_vars(ty, subst))
                    .collect::<Vec<_>>();
                self.tuple(new_tys)
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
                self.fn_(new_param_tys, new_return_ty, var_args)
            }
            Ref(inner_ty) => {
                let new_inner_ty = self.substitute_self_ty(inner_ty, substitute);
                self.ref_(new_inner_ty)
            }
            Ptr(inner_ty) => {
                let new_inner_ty = self.substitute_self_ty(inner_ty, substitute);
                self.ptr(new_inner_ty)
            }
            Struct { struct_, ref gen_args } => {
                let new_gen_args = gen_args
                    .clone()
                    .iter()
                    .map(|&ga| self.substitute_self_ty(ga, substitute))
                    .collect::<Vec<_>>();
                self.inst_struct(struct_, new_gen_args).unwrap()
            }
            Enum { enum_, ref gen_args } => {
                let new_gen_args = gen_args
                    .clone()
                    .iter()
                    .map(|&ga| self.substitute_self_ty(ga, substitute))
                    .collect::<Vec<_>>();
                self.inst_enum(enum_, new_gen_args).unwrap()
            }
            Primitive(..) => ty,
            Alias(..) => unreachable!("ty should have been canonicalized"),
            TraitSelf(_) => substitute,
            Closure {
                ref fn_inst,
                ref name,
                captures_ty,
            } => {
                let name = name.clone();
                let mut new_fn_inst = fn_inst.clone();
                for gen_arg in &mut new_fn_inst.gen_args {
                    *gen_arg = self.substitute_self_ty(*gen_arg, substitute);
                }
                for env_gen_arg in &mut new_fn_inst.env_gen_args {
                    *env_gen_arg = self.substitute_self_ty(*env_gen_arg, substitute);
                }

                self.closure(new_fn_inst, name, captures_ty)
            }
            Tuple(ref tys) => {
                let new_tys = tys
                    .clone()
                    .into_iter()
                    .map(|ty| self.substitute_self_ty(ty, substitute))
                    .collect::<Vec<_>>();
                self.tuple(new_tys)
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

        let subst = GenVarSubst::new(&struct_def.gen_params, gen_args).unwrap();
        let instantiated_field_ty = self.substitute_gen_vars(field_ty, &subst);

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
        let subst = GenVarSubst::new(&struct_def.gen_params, gen_args).unwrap();

        let field_tys: Vec<Ty> = struct_def.fields.iter().map(|field| field.ty).collect();
        let instantiated_field_tys: Vec<Ty> = field_tys
            .into_iter()
            .map(|field_ty| self.substitute_gen_vars(field_ty, &subst))
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
        let instantiated_variant_struct_ty = self.inst_struct(base_variant_struct_ty, gen_args.clone()).unwrap();
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
                self.inst_struct(variant_ty, gen_args).unwrap()
            })
            .collect();
        Ok(instantiated_variant_struct_tys)
    }

    pub fn get_tuple_field_tys(&self, ty: Ty) -> Result<&Vec<Ty>, ()> {
        let ty_def = self.get_ty_def(ty).expect("type should be registered");
        let TyDef::Tuple(tys) = ty_def else {
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

                (Closure { fn_inst: fn_inst1, .. }, Closure { fn_inst: fn_inst2, .. }) => {
                    fn_inst1.fn_ == fn_inst2.fn_
                        && fn_inst1.gen_args.len() == fn_inst2.gen_args.len()
                        && fn_inst1
                            .gen_args
                            .iter()
                            .zip(&fn_inst2.gen_args)
                            .all(|(arg1, arg2)| self.tys_eq(*arg1, *arg2))
                        && fn_inst1.env_gen_args.len() == fn_inst2.env_gen_args.len()
                        && fn_inst1
                            .env_gen_args
                            .iter()
                            .zip(&fn_inst2.env_gen_args)
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

        let Some(generic_def) = self.get_ty_def(generic) else {
            // TODO should this case panic instead?
            return false;
        };

        if let &GenVar(gen_var) = generic_def
            && instantiation.contains_key(&gen_var)
        {
            let substitute = instantiation.get(&gen_var).unwrap();
            if let Some(substitute) = substitute {
                return self.tys_eq(*substitute, target);
            } else {
                instantiation.insert(gen_var, Some(target));
                return true;
            }
        }

        let Some(target_def) = self.get_ty_def(target) else {
            return false;
        };

        match (target_def, generic_def) {
            (Alias(_), _) | (_, Alias(_)) => {
                unreachable!("Types should have been canonicalized");
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

            (Closure { fn_inst: fn_inst1, .. }, Closure { fn_inst: fn_inst2, .. }) => {
                fn_inst1.fn_ == fn_inst2.fn_
                    && fn_inst1.gen_args.len() == fn_inst2.gen_args.len()
                    && fn_inst1
                        .gen_args
                        .iter()
                        .zip(&fn_inst2.gen_args)
                        .all(|(arg1, arg2)| self.try_find_instantiation_internal(*arg1, *arg2, instantiation))
                    && fn_inst1.env_gen_args.len() == fn_inst2.env_gen_args.len()
                    && fn_inst1
                        .env_gen_args
                        .iter()
                        .zip(&fn_inst2.env_gen_args)
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

    pub fn add_implements_trait_constraint(&mut self, subject: GenVar, trait_inst: TraitInst) {
        self.constraints.push(Constraint {
            subject,
            requirement: ConstraintRequirement::Trait(trait_inst),
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
            .any(|c| c.subject == gen_var &&
                matches!(c.requirement , ConstraintRequirement::Trait(TraitInst { trait_: the_trait_, .. }) if the_trait_ == trait_))
    }

    pub fn implements_trait_inst_constraint_exists(&self, gen_var: GenVar, trait_inst: &TraitInst) -> bool {
        self.constraints.iter().any(|c| {
            if c.subject != gen_var {
                return false;
            }

            match &c.requirement {
                ConstraintRequirement::Trait(trait_inst_2) => {
                    trait_inst.trait_ == trait_inst_2.trait_
                        && trait_inst.gen_args.len() == trait_inst_2.gen_args.len()
                        && trait_inst
                            .gen_args
                            .iter()
                            .zip(&trait_inst_2.gen_args)
                            .all(|(&gen_arg1, &gen_arg2)| self.tys_eq(gen_arg1, gen_arg2))
                }
                _ => false,
            }
        })
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

    pub fn add_implements_trait_inst_obligation(&mut self, ty: Ty, trait_inst: TraitInst) {
        self.obligations
            .push(Obligation::ImplementsTraitInst { ty, trait_inst });
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

    pub fn get_ty_by_name(&self, ty_name: &str) -> Result<&Named, NotATypeName> {
        self.named_tys.get(ty_name).ok_or(NotATypeName(ty_name.to_string()))
    }
}
