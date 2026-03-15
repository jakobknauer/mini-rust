use std::cell::Cell;
use std::collections::{HashMap, HashSet};

use crate::ctxt::{
    fns,
    traits::{self, Trait, TraitInst},
    ty::*,
};

pub struct TyReg<'ty> {
    arena: &'ty bumpalo::Bump,
    next_ty_id: Cell<TyId>,

    structs: Vec<StructDef<'ty>>,
    structs_defined: HashSet<usize>,
    enums: Vec<EnumDef>,
    enums_defined: HashSet<usize>,
    gen_var_names: Vec<String>,

    tys_inv: HashMap<TyDef<'ty>, Ty<'ty>>,
    named_tys: HashMap<String, Named<'ty>>,
    ty_slices_inv: HashSet<&'ty [Ty<'ty>]>,

    next_inf_var: InfVar,
    next_opaque_id: usize,
    opaque_resolutions: HashMap<OpaqueId, Ty<'ty>>,
    opaques: Vec<OpaqueDef<'ty>>,
}

#[derive(Clone, Copy)]
pub enum Named<'ty> {
    Ty(Ty<'ty>),
    Struct(Struct),
    Enum(Enum),
}

#[derive(Debug)]
pub struct NotAStruct<'ty>(#[allow(unused)] pub Ty<'ty>);
#[derive(Debug)]
pub struct NotAnEnum<'ty>(#[allow(unused)] pub Ty<'ty>);
pub enum NotAStructField<'ty> {
    NotAStruct(#[allow(unused)] Ty<'ty>),
    NotAFieldName(#[allow(unused)] Ty<'ty>, #[allow(unused)] String),
}
#[derive(Debug)]
#[allow(clippy::enum_variant_names)]
pub enum TyInstError {
    StructGenericArgCountMismatch {
        #[allow(unused)]
        struct_: Struct,
        #[allow(unused)]
        expected: usize,
        #[allow(unused)]
        actual: usize,
    },
    EnumGenericArgCountMismatch {
        #[allow(unused)]
        enum_: Enum,
        #[allow(unused)]
        expected: usize,
        #[allow(unused)]
        actual: usize,
    },
    OpaqueGenericArgCountMismatch {
        #[allow(unused)]
        opaque: OpaqueId,
        #[allow(unused)]
        expected: usize,
        #[allow(unused)]
        actual: usize,
    },
}
#[derive(Debug)]
pub struct NotATypeName(#[allow(unused)] pub String);

impl<'ty> TyReg<'ty> {
    pub fn new(arena: &'ty bumpalo::Bump) -> Self {
        Self {
            arena,
            next_ty_id: Cell::new(TyId(0)),
            structs: Vec::new(),
            structs_defined: Default::default(),
            enums: Vec::new(),
            enums_defined: Default::default(),
            gen_var_names: Vec::new(),
            tys_inv: Default::default(),
            named_tys: Default::default(),
            ty_slices_inv: Default::default(),
            next_inf_var: Default::default(),
            next_opaque_id: 0,
            opaque_resolutions: Default::default(),
            opaques: Vec::new(),
        }
    }

    fn register_ty(&mut self, ty_def: TyDef<'ty>) -> Ty<'ty> {
        if let Some(existing_ty) = self.tys_inv.get(&ty_def) {
            return *existing_ty;
        }

        let id = self.next_ty_id.get();
        self.next_ty_id.set(TyId(id.0 + 1));
        let ty_ref = self.arena.alloc(ty_def.clone());
        let ty = Ty(ty_ref, id);
        self.tys_inv.insert(ty_def, ty);
        ty
    }

    pub fn ty_slice(&mut self, tys: &[Ty<'ty>]) -> TySlice<'ty> {
        if let Some(&existing) = self.ty_slices_inv.get(tys) {
            return existing;
        }
        let slice = self.arena.alloc_slice_copy(tys);
        self.ty_slices_inv.insert(slice);
        slice
    }

    pub fn get_ty_slice(&self, slice: TySlice<'ty>) -> &[Ty<'ty>] {
        slice
    }

    fn register_named_ty(&mut self, name: &str, ty_def: TyDef<'ty>) -> Result<Ty<'ty>, ()> {
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

    pub fn primitive(&mut self, primitive: Primitive) -> Ty<'ty> {
        let ty_def = TyDef::Primitive(primitive);
        self.register_ty(ty_def)
    }

    pub fn tuple(&mut self, tys: &[Ty<'ty>]) -> Ty<'ty> {
        let tys = self.ty_slice(tys);
        let tuple_ty = TyDef::Tuple(tys);
        self.register_ty(tuple_ty)
    }

    pub fn tuple_from_ty_slice(&mut self, slice: TySlice<'ty>) -> Ty<'ty> {
        let tuple_ty = TyDef::Tuple(slice);
        self.register_ty(tuple_ty)
    }

    pub fn unit(&mut self) -> Ty<'ty> {
        self.tuple(&[])
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

    pub fn fn_(&mut self, param_tys: &[Ty<'ty>], return_ty: Ty<'ty>, var_args: bool) -> Ty<'ty> {
        let param_tys = self.ty_slice(param_tys);
        let fn_ty = TyDef::Fn {
            param_tys,
            var_args,
            return_ty,
        };
        self.register_ty(fn_ty)
    }

    pub fn ref_(&mut self, inner_ty: Ty<'ty>) -> Ty<'ty> {
        let ref_ty = TyDef::Ref(inner_ty);
        self.register_ty(ref_ty)
    }

    pub fn ptr(&mut self, inner_ty: Ty<'ty>) -> Ty<'ty> {
        let ptr_ty = TyDef::Ptr(inner_ty);
        self.register_ty(ptr_ty)
    }

    pub fn gen_var(&mut self, gen_var: GenVar) -> Ty<'ty> {
        let gen_var_ty = TyDef::GenVar(gen_var);
        self.register_ty(gen_var_ty)
    }

    pub fn register_gen_var(&mut self, name: &str) -> GenVar {
        let gen_var = GenVar(self.gen_var_names.len());
        self.gen_var_names.push(name.to_string());
        gen_var
    }

    pub fn trait_self(&mut self, trait_: Trait) -> Ty<'ty> {
        let trait_self = TyDef::TraitSelf(trait_);
        self.register_ty(trait_self)
    }

    pub fn closure(&mut self, fn_inst: fns::FnInst<'ty>, name: impl Into<String>, captures_ty: Ty<'ty>) -> Ty<'ty> {
        let closure = TyDef::Closure {
            fn_inst,
            name: name.into(),
            captures_ty,
        };
        self.register_ty(closure)
    }

    pub fn assoc_ty(&mut self, base_ty: Ty<'ty>, trait_inst: traits::TraitInst<'ty>, assoc_ty_idx: usize) -> Ty<'ty> {
        let assoc_ty = TyDef::AssocTy {
            base_ty,
            trait_inst,
            assoc_ty_idx,
        };
        self.register_ty(assoc_ty)
    }

    pub fn inf_var(&mut self) -> Ty<'ty> {
        let id = self.next_inf_var;
        self.next_inf_var.0 += 1;
        let inf_var = TyDef::InfVar(id);
        self.register_ty(inf_var)
    }

    pub fn opaque(&mut self, gen_params: &[GenVar]) -> (OpaqueId, Ty<'ty>) {
        let id = OpaqueId(self.next_opaque_id);
        self.next_opaque_id += 1;
        let gen_args: Vec<Ty<'ty>> = gen_params.iter().map(|&gv| self.gen_var(gv)).collect();
        let gen_args = self.ty_slice(&gen_args);
        let opaque_def = OpaqueDef {
            gen_params: gen_params.to_vec(),
            constraints: Vec::new(),
        };
        self.opaques.push(opaque_def);
        let ty = self.register_ty(TyDef::Opaque { id, gen_args });
        (id, ty)
    }

    pub fn get_opaque_def(&self, id: OpaqueId) -> &OpaqueDef<'ty> {
        &self.opaques[id.0]
    }

    pub fn set_opaque_resolution(&mut self, id: OpaqueId, ty: Ty<'ty>) {
        self.opaque_resolutions.insert(id, ty);
    }

    pub fn get_opaque_resolution(&self, id: OpaqueId) -> Option<Ty<'ty>> {
        self.opaque_resolutions.get(&id).copied()
    }

    pub fn add_opaque_constraint(&mut self, id: OpaqueId, req: ConstraintRequirement<'ty>) {
        self.opaques[id.0].constraints.push(req);
    }

    pub fn opaque_satisfies_trait_inst(&self, id: OpaqueId, trait_inst: TraitInst<'ty>) -> bool {
        self.opaques[id.0]
            .constraints
            .iter()
            .any(|r| matches!(r, ConstraintRequirement::Trait(ti) if ti.trait_ == trait_inst.trait_))
    }

    pub fn try_get_opaque_callable_constraint(&self, id: OpaqueId) -> Option<(TySlice<'ty>, Ty<'ty>)> {
        self.opaques[id.0].constraints.iter().find_map(|r| {
            if let &ConstraintRequirement::Callable { param_tys, return_ty } = r {
                Some((param_tys, return_ty))
            } else {
                None
            }
        })
    }

    pub fn get_opaque_constraints(&self, id: OpaqueId) -> &[ConstraintRequirement<'ty>] {
        &self.opaques[id.0].constraints
    }

    #[expect(unused)]
    pub fn inst_opaque(&mut self, id: OpaqueId, gen_args: &[Ty<'ty>]) -> Result<Ty<'ty>, TyInstError> {
        let opaque_def = self.opaques.get(id.0).unwrap();
        if opaque_def.gen_params.len() != gen_args.len() {
            return Err(TyInstError::OpaqueGenericArgCountMismatch {
                opaque: id,
                expected: opaque_def.gen_params.len(),
                actual: gen_args.len(),
            });
        }

        let gen_args = self.ty_slice(gen_args);
        self.inst_opaque_from_ty_slice(id, gen_args)
    }

    pub fn inst_opaque_from_ty_slice(&mut self, id: OpaqueId, gen_args: TySlice<'ty>) -> Result<Ty<'ty>, TyInstError> {
        let opaque_def = self.opaques.get(id.0).unwrap();
        if opaque_def.gen_params.len() != gen_args.len() {
            return Err(TyInstError::OpaqueGenericArgCountMismatch {
                opaque: id,
                expected: opaque_def.gen_params.len(),
                actual: gen_args.len(),
            });
        }

        Ok(self.register_ty(TyDef::Opaque { id, gen_args }))
    }

    pub fn resolve_opaque_in_ty(&mut self, ty: Ty<'ty>) -> Ty<'ty> {
        use TyDef::*;

        match *ty.0 {
            Opaque { id, gen_args } => {
                let Some(resolved) = self.opaque_resolutions.get(&id).copied() else {
                    return ty;
                };
                let gen_params = self.opaques[id.0].gen_params.clone();
                let gen_args: Vec<Ty<'ty>> = self.get_ty_slice(gen_args).to_vec();
                let subst = GenVarSubst::new(&gen_params, &gen_args).unwrap();
                let instantiated = self.substitute_gen_vars(resolved, &subst);
                self.resolve_opaque_in_ty(instantiated)
            }
            Primitive(_) | GenVar(_) | TraitSelf(_) | InfVar(_) => ty,
            Ref(inner) => {
                let inner = self.resolve_opaque_in_ty(inner);
                self.ref_(inner)
            }
            Ptr(inner) => {
                let inner = self.resolve_opaque_in_ty(inner);
                self.ptr(inner)
            }
            Tuple(items) => {
                let items: Vec<_> = items.iter().map(|&t| self.resolve_opaque_in_ty(t)).collect();
                self.tuple(&items)
            }
            Struct { struct_, gen_args } => {
                let gen_args: Vec<_> = gen_args.iter().map(|&t| self.resolve_opaque_in_ty(t)).collect();
                self.inst_struct(struct_, &gen_args).unwrap()
            }
            Enum { enum_, gen_args } => {
                let gen_args: Vec<_> = gen_args.iter().map(|&t| self.resolve_opaque_in_ty(t)).collect();
                self.inst_enum(enum_, &gen_args).unwrap()
            }
            Fn {
                param_tys,
                return_ty,
                var_args,
            } => {
                let param_tys: Vec<_> = param_tys.iter().map(|&t| self.resolve_opaque_in_ty(t)).collect();
                let return_ty = self.resolve_opaque_in_ty(return_ty);
                self.fn_(&param_tys, return_ty, var_args)
            }
            Closure {
                fn_inst,
                ref name,
                captures_ty,
            } => {
                let captures_ty = self.resolve_opaque_in_ty(captures_ty);
                // Don't recurse into fn_inst for now
                self.closure(fn_inst, name.as_str(), captures_ty)
            }
            AssocTy {
                base_ty,
                trait_inst,
                assoc_ty_idx,
            } => {
                let base_ty = self.resolve_opaque_in_ty(base_ty);
                self.assoc_ty(base_ty, trait_inst, assoc_ty_idx)
            }
        }
    }

    pub fn inst_struct(&mut self, struct_: Struct, gen_args: &[Ty<'ty>]) -> Result<Ty<'ty>, TyInstError> {
        let struct_def = self.structs.get(struct_.0).unwrap();
        if struct_def.gen_params.len() != gen_args.len() {
            return Err(TyInstError::StructGenericArgCountMismatch {
                struct_,
                expected: struct_def.gen_params.len(),
                actual: gen_args.len(),
            });
        }

        let gen_args = self.ty_slice(gen_args);
        self.inst_struct_from_ty_slice(struct_, gen_args)
    }

    pub fn inst_struct_from_ty_slice(
        &mut self,
        struct_: Struct,
        gen_args: TySlice<'ty>,
    ) -> Result<Ty<'ty>, TyInstError> {
        let struct_def = self.structs.get(struct_.0).unwrap();
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

    pub fn inst_enum(&mut self, enum_: Enum, gen_args: &[Ty<'ty>]) -> Result<Ty<'ty>, TyInstError> {
        let enum_def = self.enums.get(enum_.0).unwrap();

        if enum_def.gen_params.len() != gen_args.len() {
            return Err(TyInstError::EnumGenericArgCountMismatch {
                enum_,
                expected: enum_def.gen_params.len(),
                actual: gen_args.len(),
            });
        }

        let gen_args = self.ty_slice(gen_args);

        let enum_ty = TyDef::Enum { enum_, gen_args };
        Ok(self.register_ty(enum_ty))
    }

    pub fn inst_enum_from_ty_slice(&mut self, enum_: Enum, gen_args: TySlice<'ty>) -> Result<Ty<'ty>, TyInstError> {
        let enum_def = self.enums.get(enum_.0).unwrap();

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

    pub fn get_struct_def(&self, struct_: Struct) -> Option<&StructDef<'ty>> {
        self.structs.get(struct_.0)
    }

    pub fn define_struct_fields(&mut self, struct_: Struct, fields: Vec<StructField<'ty>>) {
        assert!(self.structs_defined.insert(struct_.0), "struct fields already defined");
        self.structs[struct_.0].fields = fields;
    }

    pub fn get_enum_def(&self, enum_: Enum) -> Option<&EnumDef> {
        self.enums.get(enum_.0)
    }

    pub fn define_enum_variants(&mut self, enum_: Enum, variants: Vec<EnumVariant>) {
        assert!(self.enums_defined.insert(enum_.0), "enum variants already defined");
        self.enums[enum_.0].variants = variants;
    }

    pub fn is_c_void_ty(&self, base_ty: Ty<'ty>) -> bool {
        let ty_def = base_ty.0;
        matches!(ty_def, TyDef::Primitive(Primitive::CVoid))
    }

    pub fn get_string_rep(&self, ty: Ty<'ty>) -> String {
        self.get_string_rep_with_subst(ty, &HashMap::new())
    }

    pub fn get_string_rep_with_subst(&self, ty: Ty<'ty>, subst: &HashMap<GenVar, Ty<'ty>>) -> String {
        use self::Primitive::*;
        use TyDef::*;

        match *ty.0 {
            Primitive(primitive) => match primitive {
                Integer32 => "i32".to_string(),
                Boolean => "bool".to_string(),
                CVoid => "c_void".to_string(),
                CChar => "c_char".to_string(),
            },
            Tuple(tys) => match self.get_ty_slice(tys) {
                [] => "()".to_string(),
                [ty] => format!("({},)", self.get_string_rep_with_subst(*ty, subst)),
                tys => format!(
                    "({})",
                    tys.iter()
                        .map(|&ty| self.get_string_rep_with_subst(ty, subst))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            },
            Fn {
                param_tys,
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
            GenVar(gen_var) => subst
                .get(&gen_var)
                .map(|&ty| self.get_string_rep_with_subst(ty, subst))
                .unwrap_or(self.get_gen_var_name(gen_var).to_string()),
            Struct { struct_, gen_args } => {
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
            Enum { enum_, gen_args } => {
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
            AssocTy {
                base_ty,
                trait_inst,
                assoc_ty_idx,
            } => {
                let trait_arg_names = trait_inst
                    .gen_args
                    .iter()
                    .map(|&ga| self.get_string_rep_with_subst(ga, subst))
                    .collect::<Vec<_>>()
                    .join(", ");

                // TODO move this method to ctxt so the trait name can be printed

                format!(
                    "<{} as Trait({})::<{}>>::{}",
                    self.get_string_rep_with_subst(base_ty, subst),
                    trait_inst.trait_.0,
                    trait_arg_names,
                    assoc_ty_idx
                )
            }
            InfVar(id) => format!("inf({})", id.0),
            Opaque { id, gen_args } => {
                if gen_args.is_empty() {
                    return format!("impl({})", id.0);
                }
                let gen_arg_names = gen_args
                    .iter()
                    .map(|&ga| self.get_string_rep_with_subst(ga, subst))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("impl({})<{}>", id.0, gen_arg_names)
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

    #[must_use]
    pub fn substitute(&mut self, ty: Ty<'ty>, gen_vars: &GenVarSubst<'ty>, self_ty: Option<Ty<'ty>>) -> Ty<'ty> {
        use TyDef::*;

        match *ty.0 {
            Primitive(_) | InfVar(_) => ty,
            GenVar(gen_var) => gen_vars.get(gen_var).unwrap_or(ty),
            TraitSelf(_) => self_ty.unwrap_or(ty), // TODO: check actual trait
            Opaque { id, gen_args } => {
                let gen_args = self.substitute_on_slice(gen_args, gen_vars, self_ty);
                self.register_ty(TyDef::Opaque { id, gen_args })
            }
            Fn {
                param_tys,
                return_ty,
                var_args,
            } => {
                let param_tys = self.substitute_on_slice(param_tys, gen_vars, self_ty);
                let return_ty = self.substitute(return_ty, gen_vars, self_ty);
                self.fn_(param_tys, return_ty, var_args)
            }
            Ref(inner_ty) => {
                let new_inner_ty = self.substitute(inner_ty, gen_vars, self_ty);
                self.ref_(new_inner_ty)
            }
            Ptr(inner_ty) => {
                let new_inner_ty = self.substitute(inner_ty, gen_vars, self_ty);
                self.ptr(new_inner_ty)
            }
            Struct { struct_, gen_args } => {
                let gen_args = self.substitute_on_slice(gen_args, gen_vars, self_ty);
                self.inst_struct(struct_, gen_args).unwrap()
            }
            Enum { enum_, gen_args } => {
                let gen_args = self.substitute_on_slice(gen_args, gen_vars, self_ty);
                self.inst_enum(enum_, gen_args).unwrap()
            }
            Closure {
                fn_inst,
                ref name,
                captures_ty,
            } => {
                let name = name.clone();
                let gen_args = self.substitute_on_slice(fn_inst.gen_args, gen_vars, self_ty);
                let env_gen_args = self.substitute_on_slice(fn_inst.env_gen_args, gen_vars, self_ty);
                let fn_inst = fn_inst.with_gen_args(gen_args, env_gen_args).unwrap();
                let captures_ty = self.substitute(captures_ty, gen_vars, self_ty);
                self.closure(fn_inst, name, captures_ty)
            }
            Tuple(items) => {
                let items = self.substitute_on_slice(items, gen_vars, self_ty);
                self.tuple(items)
            }
            AssocTy {
                base_ty,
                trait_inst,
                assoc_ty_idx,
            } => {
                let base_ty = self.substitute(base_ty, gen_vars, self_ty);
                let gen_args = self.substitute_on_slice(trait_inst.gen_args, gen_vars, self_ty);
                let trait_inst = trait_inst.with_gen_args(gen_args).unwrap();
                self.assoc_ty(base_ty, trait_inst, assoc_ty_idx)
            }
        }
    }

    pub fn substitute_on_slice(
        &mut self,
        slice: TySlice<'ty>,
        gen_vars: &GenVarSubst<'ty>,
        self_ty: Option<Ty<'ty>>,
    ) -> TySlice<'ty> {
        let slice: Vec<_> = slice.iter().map(|&ty| self.substitute(ty, gen_vars, self_ty)).collect();
        self.ty_slice(&slice)
    }

    pub fn substitute_gen_vars(&mut self, ty: Ty<'ty>, subst: &GenVarSubst<'ty>) -> Ty<'ty> {
        self.substitute(ty, subst, None)
    }

    pub fn substitute_gen_vars_on_slice(&mut self, slice: TySlice<'ty>, subst: &GenVarSubst<'ty>) -> TySlice<'ty> {
        self.substitute_on_slice(slice, subst, None)
    }

    pub fn get_struct_field_ty(&mut self, ty: Ty<'ty>, index: usize) -> Result<Ty<'ty>, NotAStruct<'ty>> {
        let &TyDef::Struct { struct_, gen_args } = ty.0 else {
            return Err(NotAStruct(ty));
        };

        let struct_def = self
            .get_struct_def(struct_)
            .expect("struct definition should be registered");
        let field_ty = struct_def.fields[index].ty;

        let subst = GenVarSubst::new(&struct_def.gen_params, self.get_ty_slice(gen_args)).unwrap();
        let instantiated_field_ty = self.substitute_gen_vars(field_ty, &subst);

        Ok(instantiated_field_ty)
    }

    pub fn get_struct_field_tys(&mut self, ty: Ty<'ty>) -> Result<Vec<Ty<'ty>>, NotAStruct<'ty>> {
        let &TyDef::Struct { struct_, gen_args } = ty.0 else {
            return Err(NotAStruct(ty));
        };

        let struct_def = self
            .get_struct_def(struct_)
            .expect("struct definition should be registered");
        let subst = GenVarSubst::new(&struct_def.gen_params, self.get_ty_slice(gen_args)).unwrap();

        let field_tys: Vec<Ty<'ty>> = struct_def.fields.iter().map(|field| field.ty).collect();
        let instantiated_field_tys: Vec<Ty<'ty>> = field_tys
            .into_iter()
            .map(|field_ty| self.substitute_gen_vars(field_ty, &subst))
            .collect();

        Ok(instantiated_field_tys)
    }

    pub fn get_enum_variant_ty(&mut self, ty: Ty<'ty>, variant_index: usize) -> Result<Ty<'ty>, NotAnEnum<'ty>> {
        let &TyDef::Enum { enum_, gen_args } = ty.0 else {
            return Err(NotAnEnum(ty));
        };

        let enum_def = self.get_enum_def(enum_).expect("enum definition should be registered");
        let base_variant_struct_ty = enum_def.variants[variant_index].struct_;
        let instantiated_variant_struct_ty = self
            .inst_struct_from_ty_slice(base_variant_struct_ty, gen_args)
            .unwrap();
        Ok(instantiated_variant_struct_ty)
    }

    pub fn get_enum_variant_tys(&mut self, ty: Ty<'ty>) -> Result<Vec<Ty<'ty>>, NotAnEnum<'ty>> {
        let &TyDef::Enum { enum_, gen_args } = ty.0 else {
            return Err(NotAnEnum(ty));
        };

        let enum_def = self.get_enum_def(enum_).expect("enum definition should be registered");
        let base_variant_structs: Vec<Struct> = enum_def.variants.iter().map(|variant| variant.struct_).collect();
        let instantiated_variant_struct_tys: Vec<Ty<'ty>> = base_variant_structs
            .into_iter()
            .map(|variant_ty| self.inst_struct_from_ty_slice(variant_ty, gen_args).unwrap())
            .collect();
        Ok(instantiated_variant_struct_tys)
    }

    pub fn get_tuple_field_tys(&self, ty: Ty<'ty>) -> Result<&[Ty<'ty>], ()> {
        match ty.0 {
            &TyDef::Tuple(tys) => Ok(self.get_ty_slice(tys)),
            _ => Err(()),
        }
    }

    pub fn get_struct_field_index_by_name(
        &self,
        struct_ty: Ty<'ty>,
        field_name: &str,
    ) -> Result<usize, NotAStructField<'ty>> {
        let &TyDef::Struct { struct_, .. } = struct_ty.0 else {
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

    pub fn slices_eq(&self, s1: TySlice<'ty>, s2: TySlice<'ty>) -> bool {
        s1.len() == s2.len() && s1.iter().zip(s2.iter()).all(|(&ty1, &ty2)| self.tys_eq(ty1, ty2))
    }

    pub fn tys_eq(&self, ty1: Ty<'ty>, ty2: Ty<'ty>) -> bool {
        use TyDef::*;

        if ty1 == ty2 {
            return true;
        }

        match (ty1.0, ty2.0) {
            (
                &Fn {
                    param_tys: params1,
                    return_ty: ret1,
                    var_args: var_args1,
                },
                &Fn {
                    param_tys: params2,
                    return_ty: ret2,
                    var_args: var_args2,
                },
            ) => self.slices_eq(params1, params2) && self.tys_eq(ret1, ret2) && var_args1 == var_args2,

            (&Ref(inner1), &Ref(inner2)) | (&Ptr(inner1), &Ptr(inner2)) => self.tys_eq(inner1, inner2),

            (
                &Struct {
                    struct_: struct1,
                    gen_args: gen_args1,
                },
                &Struct {
                    struct_: struct2,
                    gen_args: gen_args2,
                },
            ) => struct1 == struct2 && self.slices_eq(gen_args1, gen_args2),

            (
                &Enum {
                    enum_: enum1,
                    gen_args: gen_args1,
                },
                &Enum {
                    enum_: enum2,
                    gen_args: gen_args2,
                },
            ) => enum1 == enum2 && self.slices_eq(gen_args1, gen_args2),

            (Closure { fn_inst: fn_inst1, .. }, Closure { fn_inst: fn_inst2, .. }) => {
                fn_inst1.fn_ == fn_inst2.fn_
                    && self.slices_eq(fn_inst1.gen_args, fn_inst2.gen_args)
                    && self.slices_eq(fn_inst1.env_gen_args, fn_inst2.env_gen_args)
            }

            (&Tuple(items1), &Tuple(items2)) => self.slices_eq(items1, items2),

            (
                &Opaque {
                    id: id1,
                    gen_args: gen_args1,
                },
                &Opaque {
                    id: id2,
                    gen_args: gen_args2,
                },
            ) => id1 == id2 && self.slices_eq(gen_args1, gen_args2),

            (
                AssocTy {
                    base_ty: base_ty1,
                    trait_inst: trait_inst1,
                    assoc_ty_idx: assoc_ty_idx1,
                },
                AssocTy {
                    base_ty: base_ty2,
                    trait_inst: trait_inst2,
                    assoc_ty_idx: assoc_ty_idx2,
                },
            ) => {
                base_ty1 == base_ty2
                    && trait_inst1.trait_ == trait_inst2.trait_
                    && self.slices_eq(trait_inst1.gen_args, trait_inst2.gen_args)
                    && assoc_ty_idx1 == assoc_ty_idx2
            }

            _ => false,
        }
    }

    pub fn try_find_instantiation(
        &mut self,
        target: Ty<'ty>,
        generic: Ty<'ty>,
        gen_vars: &[GenVar],
    ) -> Result<TySlice<'ty>, ()> {
        let mut instantiations = HashMap::new();
        for gen_param in gen_vars {
            instantiations.insert(*gen_param, None);
        }

        if self.try_find_instantiation_internal(target, generic, &mut instantiations) {
            let inst: Vec<Ty<'ty>> = gen_vars
                .iter()
                .map(|gen_var| instantiations[gen_var])
                .collect::<Option<_>>()
                .ok_or(())?;
            Ok(self.ty_slice(&inst))
        } else {
            Err(())
        }
    }

    fn try_find_instantiation_internal(
        &self,
        target: Ty<'ty>,
        generic: Ty<'ty>,
        instantiation: &mut HashMap<GenVar, Option<Ty<'ty>>>,
    ) -> bool {
        use TyDef::*;

        let generic_def = generic.0;
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

        let target_def = target.0;
        match (target_def, generic_def) {
            (GenVar(var1), GenVar(var2)) => var1 == var2,

            (&Primitive(a), &Primitive(b)) => a == b,

            (
                &Fn {
                    param_tys: params1,
                    return_ty: ret1,
                    var_args: var_args1,
                },
                &Fn {
                    param_tys: params2,
                    return_ty: ret2,
                    var_args: var_args2,
                },
            ) => {
                params1.len() == params2.len()
                    && params1
                        .iter()
                        .zip(params2.iter())
                        .all(|(&ty1, &ty2)| self.try_find_instantiation_internal(ty1, ty2, instantiation))
                    && self.try_find_instantiation_internal(ret1, ret2, instantiation)
                    && var_args1 == var_args2
            }

            (&Ref(inner1), &Ref(inner2)) | (&Ptr(inner1), &Ptr(inner2)) => {
                self.try_find_instantiation_internal(inner1, inner2, instantiation)
            }

            (
                &Struct {
                    struct_: struct1,
                    gen_args: gen_args1,
                },
                &Struct {
                    struct_: struct2,
                    gen_args: gen_args2,
                },
            ) => {
                struct1 == struct2
                    && gen_args1.len() == gen_args2.len()
                    && gen_args1
                        .iter()
                        .zip(gen_args2.iter())
                        .all(|(&ty1, &ty2)| self.try_find_instantiation_internal(ty1, ty2, instantiation))
            }

            (
                &Enum {
                    enum_: enum1,
                    gen_args: gen_args1,
                },
                &Enum {
                    enum_: enum2,
                    gen_args: gen_args2,
                },
            ) => {
                enum1 == enum2
                    && gen_args1.len() == gen_args2.len()
                    && gen_args1
                        .iter()
                        .zip(gen_args2.iter())
                        .all(|(&ty1, &ty2)| self.try_find_instantiation_internal(ty1, ty2, instantiation))
            }

            (Closure { fn_inst: fn_inst1, .. }, Closure { fn_inst: fn_inst2, .. }) => {
                fn_inst1.fn_ == fn_inst2.fn_
                    && fn_inst1.gen_args.len() == fn_inst2.gen_args.len()
                    && fn_inst1
                        .gen_args
                        .iter()
                        .zip(fn_inst2.gen_args.iter())
                        .all(|(&ty1, &ty2)| self.try_find_instantiation_internal(ty1, ty2, instantiation))
                    && fn_inst1.env_gen_args.len() == fn_inst2.env_gen_args.len()
                    && fn_inst1
                        .env_gen_args
                        .iter()
                        .zip(fn_inst2.env_gen_args.iter())
                        .all(|(&ty1, &ty2)| self.try_find_instantiation_internal(ty1, ty2, instantiation))
            }

            (&Tuple(items1), &Tuple(items2)) => {
                items1.len() == items2.len()
                    && items1
                        .iter()
                        .zip(items2.iter())
                        .all(|(&ty1, &ty2)| self.try_find_instantiation_internal(ty1, ty2, instantiation))
            }

            (
                &AssocTy {
                    base_ty: base_ty1,
                    trait_inst: trait_inst1,
                    assoc_ty_idx: assoc_ty_idx1,
                },
                &AssocTy {
                    base_ty: base_ty2,
                    trait_inst: trait_inst2,
                    assoc_ty_idx: assoc_ty_idx2,
                },
            ) => {
                self.try_find_instantiation_internal(base_ty1, base_ty2, instantiation)
                    && trait_inst1.trait_ == trait_inst2.trait_
                    && trait_inst1.gen_args.len() == trait_inst2.gen_args.len()
                    && trait_inst1
                        .gen_args
                        .iter()
                        .zip(trait_inst2.gen_args.iter())
                        .all(|(&ty1, &ty2)| self.try_find_instantiation_internal(ty1, ty2, instantiation))
                    && assoc_ty_idx1 == assoc_ty_idx2
            }

            (
                &Opaque {
                    id: id1,
                    gen_args: gen_args1,
                },
                &Opaque {
                    id: id2,
                    gen_args: gen_args2,
                },
            ) => {
                id1 == id2
                    && gen_args1.len() == gen_args2.len()
                    && gen_args1
                        .iter()
                        .zip(gen_args2.iter())
                        .all(|(&ty1, &ty2)| self.try_find_instantiation_internal(ty1, ty2, instantiation))
            }

            (_, _) => false,
        }
    }

    pub fn get_trait_inst_constraint(
        &self,
        constraints: &[Constraint<'ty>],
        subject: Ty<'ty>,
        trait_: Trait,
    ) -> Option<TraitInst<'ty>> {
        constraints.iter().find_map(|c| {
            if c.subject != subject {
                return None;
            }
            match c.requirement {
                ConstraintRequirement::Trait(trait_inst) if trait_inst.trait_ == trait_ => Some(trait_inst),
                _ => None,
            }
        })
    }

    pub fn implements_trait_constraint_exists(
        &self,
        constraints: &[Constraint<'ty>],
        subject: Ty<'ty>,
        trait_: Trait,
    ) -> bool {
        constraints.iter().any(|c| {
            c.subject == subject
                && matches!(c.requirement,
                    ConstraintRequirement::Trait(TraitInst { trait_: the_trait_, .. }) if the_trait_ == trait_)
        })
    }

    pub fn implements_trait_inst_constraint_exists(
        &self,
        constraints: &[Constraint<'ty>],
        subject: Ty<'ty>,
        trait_inst: TraitInst<'ty>,
    ) -> bool {
        constraints.iter().any(|c| {
            if c.subject != subject {
                return false;
            }

            match &c.requirement {
                ConstraintRequirement::Trait(trait_inst_2) => {
                    trait_inst.trait_ == trait_inst_2.trait_
                        && self.slices_eq(trait_inst.gen_args, trait_inst_2.gen_args)
                }
                _ => false,
            }
        })
    }

    pub fn try_get_callable_constraint(
        &self,
        constraints: &[Constraint<'ty>],
        subject: Ty<'ty>,
    ) -> Option<(TySlice<'ty>, Ty<'ty>)> {
        constraints.iter().find_map(|c| {
            if c.subject == subject {
                if let &ConstraintRequirement::Callable { param_tys, return_ty } = &c.requirement {
                    Some((param_tys, return_ty))
                } else {
                    None
                }
            } else {
                None
            }
        })
    }

    pub fn get_ty_by_name(&self, ty_name: &str) -> Result<&Named<'ty>, NotATypeName> {
        self.named_tys.get(ty_name).ok_or(NotATypeName(ty_name.to_string()))
    }
}
