use std::cell::{Cell, OnceCell, RefCell};
use std::collections::{HashMap, HashSet};

use crate::ctxt::{
    fns,
    traits::{self, Trait, TraitInst},
    ty::*,
};

pub struct TyReg<'ty> {
    arena: &'ty bumpalo::Bump,

    next_ty_id: Cell<TyId>,
    next_gen_var: Cell<usize>,
    next_inf_var: Cell<InfVar>,
    next_struct_id: Cell<StructId>,
    next_enum_id: Cell<EnumId>,
    next_opaque_id: Cell<OpaqueId>,

    tys_inv: RefCell<HashMap<TyDef<'ty>, Ty<'ty>>>,
    ty_slices_inv: RefCell<HashSet<&'ty [Ty<'ty>]>>,
    named_tys: RefCell<HashMap<String, Named<'ty>>>,
}

#[derive(Clone, Copy)]
pub enum Named<'ty> {
    Ty(Ty<'ty>),
    Struct(Struct<'ty>),
    Enum(Enum<'ty>),
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
pub enum TyInstError<'ty> {
    StructGenericArgCountMismatch {
        #[allow(unused)]
        struct_: Struct<'ty>,
        #[allow(unused)]
        expected: usize,
        #[allow(unused)]
        actual: usize,
    },
    EnumGenericArgCountMismatch {
        #[allow(unused)]
        enum_: Enum<'ty>,
        #[allow(unused)]
        expected: usize,
        #[allow(unused)]
        actual: usize,
    },
    OpaqueGenericArgCountMismatch {
        #[allow(unused)]
        opaque: Opaque<'ty>,
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
            next_gen_var: Cell::new(0),
            next_inf_var: Cell::new(Default::default()),
            next_struct_id: Cell::new(StructId(0)),
            next_enum_id: Cell::new(EnumId(0)),
            next_opaque_id: Cell::new(OpaqueId(0)),

            tys_inv: RefCell::new(Default::default()),
            ty_slices_inv: RefCell::new(Default::default()),
            named_tys: Default::default(),
        }
    }

    fn register_ty(&self, ty_def: TyDef<'ty>) -> Ty<'ty> {
        let existing = self.tys_inv.borrow().get(&ty_def).copied();
        if let Some(existing_ty) = existing {
            return existing_ty;
        }

        let id = self.next_ty_id.get();
        self.next_ty_id.set(TyId(id.0 + 1));
        let ty_ref = self.arena.alloc(ty_def.clone());
        let ty = Ty(ty_ref, id);
        self.tys_inv.borrow_mut().insert(ty_def, ty);
        ty
    }

    pub fn ty_slice(&self, tys: &[Ty<'ty>]) -> TySlice<'ty> {
        let existing = self.ty_slices_inv.borrow().get(tys).copied();
        if let Some(existing) = existing {
            return existing;
        }
        let slice = self.arena.alloc_slice_copy(tys);
        self.ty_slices_inv.borrow_mut().insert(slice);
        slice
    }

    fn register_named_ty(&self, name: &str, ty_def: TyDef<'ty>) -> Result<Ty<'ty>, ()> {
        if self.named_tys.borrow().contains_key(name) {
            Err(())
        } else {
            let ty = self.register_ty(ty_def);
            self.named_tys.borrow_mut().insert(name.to_string(), Named::Ty(ty));
            Ok(ty)
        }
    }

    pub fn register_primitive_tys(&self) -> Result<(), ()> {
        self.register_named_ty("i32", TyDef::Primitive(Primitive::Integer32))?;
        self.register_named_ty("bool", TyDef::Primitive(Primitive::Boolean))?;
        self.register_named_ty("c_void", TyDef::Primitive(Primitive::CVoid))?;
        self.register_named_ty("c_char", TyDef::Primitive(Primitive::CChar))?;
        Ok(())
    }

    pub fn primitive(&self, primitive: Primitive) -> Ty<'ty> {
        let ty_def = TyDef::Primitive(primitive);
        self.register_ty(ty_def)
    }

    pub fn tuple(&self, tys: &[Ty<'ty>]) -> Ty<'ty> {
        let tys = self.ty_slice(tys);
        let tuple_ty = TyDef::Tuple(tys);
        self.register_ty(tuple_ty)
    }

    pub fn tuple_from_ty_slice(&self, slice: TySlice<'ty>) -> Ty<'ty> {
        let tuple_ty = TyDef::Tuple(slice);
        self.register_ty(tuple_ty)
    }

    pub fn unit(&self) -> Ty<'ty> {
        self.tuple(&[])
    }

    pub fn register_struct(&self, name: &str, gen_param_names: &[String]) -> Result<Struct<'ty>, ()> {
        let gen_params: Vec<_> = gen_param_names
            .iter()
            .map(|gp_name| self.register_gen_var(gp_name))
            .collect();

        self.register_struct_with_existing_gen_vars(name, gen_params)
    }

    pub fn register_struct_with_existing_gen_vars(
        &self,
        name: &str,
        gen_params: impl Into<Vec<GenVar<'ty>>>,
    ) -> Result<Struct<'ty>, ()> {
        if self.named_tys.borrow().contains_key(name) {
            return Err(());
        }

        let id = self.next_struct_id.get();
        self.next_struct_id.set(StructId(id.0 + 1));

        let struct_ = self.arena.alloc(StructDef {
            name: name.to_string(),
            gen_params: gen_params.into(),
            fields: OnceCell::new(),
            id,
        });

        self.named_tys
            .borrow_mut()
            .insert(name.to_string(), Named::Struct(struct_));

        Ok(struct_)
    }

    pub fn register_enum(&self, name: &str, gen_params: &[String]) -> Result<Enum<'ty>, ()> {
        if self.named_tys.borrow().contains_key(name) {
            return Err(());
        }

        let id = self.next_enum_id.get();
        self.next_enum_id.set(EnumId(id.0 + 1));

        let gen_params = gen_params
            .iter()
            .map(|gp_name| self.register_gen_var(gp_name))
            .collect();

        let enum_ = self.arena.alloc(EnumDef {
            name: name.to_string(),
            gen_params,
            variants: OnceCell::new(),
            id,
        });

        self.named_tys.borrow_mut().insert(name.to_string(), Named::Enum(enum_));

        Ok(enum_)
    }

    pub fn fn_(&self, param_tys: &[Ty<'ty>], return_ty: Ty<'ty>, var_args: bool) -> Ty<'ty> {
        let param_tys = self.ty_slice(param_tys);
        let fn_ty = TyDef::Fn {
            param_tys,
            var_args,
            return_ty,
        };
        self.register_ty(fn_ty)
    }

    pub fn ref_(&self, inner_ty: Ty<'ty>) -> Ty<'ty> {
        let ref_ty = TyDef::Ref(inner_ty);
        self.register_ty(ref_ty)
    }

    pub fn ptr(&self, inner_ty: Ty<'ty>) -> Ty<'ty> {
        let ptr_ty = TyDef::Ptr(inner_ty);
        self.register_ty(ptr_ty)
    }

    pub fn gen_var(&self, gen_var: GenVar<'ty>) -> Ty<'ty> {
        let gen_var_ty = TyDef::GenVar(gen_var);
        self.register_ty(gen_var_ty)
    }

    pub fn register_gen_var(&self, name: &str) -> GenVar<'ty> {
        let idx = self.next_gen_var.get();
        self.next_gen_var.set(idx + 1);
        let name = self.arena.alloc_str(name);
        GenVar(idx, name)
    }

    pub fn trait_self(&self, trait_: Trait) -> Ty<'ty> {
        let trait_self = TyDef::TraitSelf(trait_);
        self.register_ty(trait_self)
    }

    pub fn closure(&self, fn_inst: fns::FnInst<'ty>, name: impl Into<String>, captures_ty: Ty<'ty>) -> Ty<'ty> {
        let closure = TyDef::Closure {
            fn_inst,
            name: name.into(),
            captures_ty,
        };
        self.register_ty(closure)
    }

    pub fn assoc_ty(&self, base_ty: Ty<'ty>, trait_inst: traits::TraitInst<'ty>, assoc_ty_idx: usize) -> Ty<'ty> {
        let assoc_ty = TyDef::AssocTy {
            base_ty,
            trait_inst,
            assoc_ty_idx,
        };
        self.register_ty(assoc_ty)
    }

    pub fn inf_var(&self) -> Ty<'ty> {
        let id = self.next_inf_var.get();
        self.next_inf_var.set(InfVar(id.0 + 1));
        let inf_var = TyDef::InfVar(id);
        self.register_ty(inf_var)
    }

    pub fn opaque(
        &self,
        gen_params: &[GenVar<'ty>],
        constraints: Vec<ConstraintRequirement<'ty>>,
    ) -> (Opaque<'ty>, Ty<'ty>) {
        let id = self.next_opaque_id.get();
        self.next_opaque_id.set(OpaqueId(id.0 + 1));
        let gen_args: Vec<Ty<'ty>> = gen_params.iter().map(|&gv| self.gen_var(gv)).collect();
        let gen_args = self.ty_slice(&gen_args);
        let opaque = self.arena.alloc(OpaqueDef {
            gen_params: gen_params.to_vec(),
            constraints,
            resolution: OnceCell::new(),
            id,
        });
        let ty = self.register_ty(TyDef::Opaque { opaque, gen_args });
        (opaque, ty)
    }

    pub fn set_opaque_resolution(&self, opaque: Opaque<'ty>, ty: Ty<'ty>) {
        opaque.resolution.set(ty).expect("opaque resolution already set");
    }

    pub fn get_opaque_resolution(&self, opaque: Opaque<'ty>) -> Option<Ty<'ty>> {
        opaque.resolution.get().copied()
    }

    pub fn inst_opaque_from_ty_slice(
        &self,
        opaque: Opaque<'ty>,
        gen_args: TySlice<'ty>,
    ) -> Result<Ty<'ty>, TyInstError<'ty>> {
        if opaque.gen_params.len() != gen_args.len() {
            return Err(TyInstError::OpaqueGenericArgCountMismatch {
                opaque,
                expected: opaque.gen_params.len(),
                actual: gen_args.len(),
            });
        }
        Ok(self.register_ty(TyDef::Opaque { opaque, gen_args }))
    }

    pub fn resolve_opaque_in_ty(&self, ty: Ty<'ty>) -> Ty<'ty> {
        use TyDef::*;

        match *ty.0 {
            Opaque { opaque, gen_args } => {
                let Some(resolved) = opaque.resolution.get().copied() else {
                    return ty;
                };
                let subst = GenVarSubst::new(&opaque.gen_params, gen_args).unwrap();
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

    pub fn inst_struct(&self, struct_: Struct<'ty>, gen_args: &[Ty<'ty>]) -> Result<Ty<'ty>, TyInstError<'ty>> {
        if struct_.gen_params.len() != gen_args.len() {
            return Err(TyInstError::StructGenericArgCountMismatch {
                struct_,
                expected: struct_.gen_params.len(),
                actual: gen_args.len(),
            });
        }

        let gen_args = self.ty_slice(gen_args);
        self.inst_struct_from_ty_slice(struct_, gen_args)
    }

    pub fn inst_struct_from_ty_slice(
        &self,
        struct_: Struct<'ty>,
        gen_args: TySlice<'ty>,
    ) -> Result<Ty<'ty>, TyInstError<'ty>> {
        if struct_.gen_params.len() != gen_args.len() {
            return Err(TyInstError::StructGenericArgCountMismatch {
                struct_,
                expected: struct_.gen_params.len(),
                actual: gen_args.len(),
            });
        }

        let struct_ty = TyDef::Struct { struct_, gen_args };
        Ok(self.register_ty(struct_ty))
    }

    pub fn inst_enum(&self, enum_: Enum<'ty>, gen_args: &[Ty<'ty>]) -> Result<Ty<'ty>, TyInstError<'ty>> {
        if enum_.gen_params.len() != gen_args.len() {
            return Err(TyInstError::EnumGenericArgCountMismatch {
                enum_,
                expected: enum_.gen_params.len(),
                actual: gen_args.len(),
            });
        }

        let gen_args = self.ty_slice(gen_args);
        self.inst_enum_from_ty_slice(enum_, gen_args)
    }

    pub fn inst_enum_from_ty_slice(
        &self,
        enum_: Enum<'ty>,
        gen_args: TySlice<'ty>,
    ) -> Result<Ty<'ty>, TyInstError<'ty>> {
        if enum_.gen_params.len() != gen_args.len() {
            return Err(TyInstError::EnumGenericArgCountMismatch {
                enum_,
                expected: enum_.gen_params.len(),
                actual: gen_args.len(),
            });
        }

        let enum_ty = TyDef::Enum { enum_, gen_args };
        Ok(self.register_ty(enum_ty))
    }

    pub fn get_struct_by_name(&self, name: &str) -> Option<Struct<'ty>> {
        match self.named_tys.borrow().get(name).copied() {
            Some(Named::Struct(struct_)) => Some(struct_),
            _ => None,
        }
    }

    pub fn get_enum_by_name(&self, name: &str) -> Option<Enum<'ty>> {
        match self.named_tys.borrow().get(name).copied() {
            Some(Named::Enum(enum_)) => Some(enum_),
            _ => None,
        }
    }

    pub fn define_struct_fields(&self, struct_: Struct<'ty>, fields: Vec<StructField<'ty>>) {
        let fields = self.arena.alloc_slice_clone(&fields);
        struct_.fields.set(fields).expect("struct fields already defined");
    }

    pub fn define_enum_variants(&self, enum_: Enum<'ty>, variants: Vec<EnumVariant<'ty>>) {
        let variants = self.arena.alloc_slice_clone(&variants);
        enum_.variants.set(variants).expect("enum variants already defined");
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
            Tuple(tys) => match tys {
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
                .unwrap_or(gen_var.name().to_string()),
            Struct { struct_, gen_args } => {
                let struct_name = &struct_.name;
                if gen_args.is_empty() {
                    return struct_name.clone();
                }
                let gen_arg_names = gen_args
                    .iter()
                    .map(|&ga| self.get_string_rep_with_subst(ga, subst))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}<{}>", struct_name, gen_arg_names)
            }
            Enum { enum_, gen_args } => {
                if gen_args.is_empty() {
                    return enum_.name.clone();
                }
                let gen_arg_names = gen_args
                    .iter()
                    .map(|&ga| self.get_string_rep_with_subst(ga, subst))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}<{}>", enum_.name, gen_arg_names)
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
            Opaque { opaque, gen_args } => {
                if gen_args.is_empty() {
                    return format!("impl({})", opaque.id.0);
                }
                let gen_arg_names = gen_args
                    .iter()
                    .map(|&ga| self.get_string_rep_with_subst(ga, subst))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("impl({})<{}>", opaque.id.0, gen_arg_names)
            }
        }
    }

    #[must_use]
    pub fn substitute(&self, ty: Ty<'ty>, gen_vars: &GenVarSubst<'ty>, self_ty: Option<Ty<'ty>>) -> Ty<'ty> {
        use TyDef::*;

        match *ty.0 {
            Primitive(_) | InfVar(_) => ty,
            GenVar(gen_var) => gen_vars.get(gen_var).unwrap_or(ty),
            TraitSelf(_) => self_ty.unwrap_or(ty), // TODO: check actual trait
            Opaque { opaque, gen_args } => {
                let gen_args = self.substitute_on_slice(gen_args, gen_vars, self_ty);
                self.register_ty(TyDef::Opaque { opaque, gen_args })
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
        &self,
        slice: TySlice<'ty>,
        gen_vars: &GenVarSubst<'ty>,
        self_ty: Option<Ty<'ty>>,
    ) -> TySlice<'ty> {
        let slice: Vec<_> = slice.iter().map(|&ty| self.substitute(ty, gen_vars, self_ty)).collect();
        self.ty_slice(&slice)
    }

    pub fn substitute_gen_vars(&self, ty: Ty<'ty>, subst: &GenVarSubst<'ty>) -> Ty<'ty> {
        self.substitute(ty, subst, None)
    }

    pub fn substitute_gen_vars_on_slice(&self, slice: TySlice<'ty>, subst: &GenVarSubst<'ty>) -> TySlice<'ty> {
        self.substitute_on_slice(slice, subst, None)
    }

    pub fn get_struct_field_ty(&self, ty: Ty<'ty>, index: usize) -> Result<Ty<'ty>, NotAStruct<'ty>> {
        let &TyDef::Struct { struct_, gen_args } = ty.0 else {
            return Err(NotAStruct(ty));
        };

        let fields = struct_.get_fields();
        let subst = GenVarSubst::new(&struct_.gen_params, gen_args).unwrap();
        let instantiated_field_ty = self.substitute_gen_vars(fields[index].ty, &subst);

        Ok(instantiated_field_ty)
    }

    pub fn get_struct_field_tys(&self, ty: Ty<'ty>) -> Result<Vec<Ty<'ty>>, NotAStruct<'ty>> {
        let &TyDef::Struct { struct_, gen_args } = ty.0 else {
            return Err(NotAStruct(ty));
        };

        let fields = struct_.get_fields();
        let subst = GenVarSubst::new(&struct_.gen_params, gen_args).unwrap();
        let instantiated_field_tys = fields
            .iter()
            .map(|field| self.substitute_gen_vars(field.ty, &subst))
            .collect();

        Ok(instantiated_field_tys)
    }

    pub fn get_enum_variant_ty(&self, ty: Ty<'ty>, variant_index: usize) -> Result<Ty<'ty>, NotAnEnum<'ty>> {
        let &TyDef::Enum { enum_, gen_args } = ty.0 else {
            return Err(NotAnEnum(ty));
        };

        let base_variant_struct_ty = enum_.get_variant(variant_index).struct_;
        let instantiated_variant_struct_ty = self
            .inst_struct_from_ty_slice(base_variant_struct_ty, gen_args)
            .unwrap();
        Ok(instantiated_variant_struct_ty)
    }

    pub fn get_enum_variant_tys(&self, ty: Ty<'ty>) -> Result<Vec<Ty<'ty>>, NotAnEnum<'ty>> {
        let &TyDef::Enum { enum_, gen_args } = ty.0 else {
            return Err(NotAnEnum(ty));
        };

        let base_variant_structs: Vec<Struct<'ty>> =
            enum_.get_variants().iter().map(|variant| variant.struct_).collect();
        let instantiated_variant_struct_tys: Vec<Ty<'ty>> = base_variant_structs
            .into_iter()
            .map(|variant_ty| self.inst_struct_from_ty_slice(variant_ty, gen_args).unwrap())
            .collect();
        Ok(instantiated_variant_struct_tys)
    }

    pub fn get_tuple_field_tys(&self, ty: Ty<'ty>) -> Result<&[Ty<'ty>], ()> {
        match ty.0 {
            &TyDef::Tuple(tys) => Ok(tys),
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

        let fields = struct_.get_fields();
        fields
            .iter()
            .position(|field| field.name == field_name)
            .ok_or_else(|| NotAStructField::NotAFieldName(struct_ty, field_name.to_string()))
    }

    pub fn try_find_instantiation(
        &self,
        target: Ty<'ty>,
        generic: Ty<'ty>,
        gen_vars: &[GenVar<'ty>],
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
        instantiation: &mut HashMap<GenVar<'ty>, Option<Ty<'ty>>>,
    ) -> bool {
        use TyDef::*;

        let generic_def = generic.0;
        if let &GenVar(gen_var) = generic_def
            && instantiation.contains_key(&gen_var)
        {
            let substitute = instantiation.get(&gen_var).unwrap();
            if let Some(substitute) = substitute {
                return *substitute == target;
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
                    opaque: opaque1,
                    gen_args: gen_args1,
                },
                &Opaque {
                    opaque: opaque2,
                    gen_args: gen_args2,
                },
            ) => {
                opaque1 == opaque2
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
                    trait_inst.trait_ == trait_inst_2.trait_ && trait_inst.gen_args == trait_inst_2.gen_args
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

    pub fn get_ty_by_name(&self, ty_name: &str) -> Result<Named<'ty>, NotATypeName> {
        self.named_tys
            .borrow()
            .get(ty_name)
            .copied()
            .ok_or(NotATypeName(ty_name.to_string()))
    }
}
