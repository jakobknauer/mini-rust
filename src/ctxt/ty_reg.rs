use std::cell::{Cell, OnceCell, RefCell};
use std::collections::{HashMap, HashSet};

use crate::ctxt::{
    traits::{self, Trait},
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

    tys_inv: RefCell<HashMap<TyDef<'ty>, Ty<'ty>>>, // TODO the value is just a reference to the key -- can this be improved somehow?
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

#[derive(Debug)]
#[allow(clippy::enum_variant_names)]
#[allow(unused)]
pub enum TyInstError<'ty> {
    StructGenericArgCountMismatch {
        struct_: Struct<'ty>,
        expected: usize,
        actual: usize,
    },
    EnumGenericArgCountMismatch {
        enum_: Enum<'ty>,
        expected: usize,
        actual: usize,
    },
    OpaqueGenericArgCountMismatch {
        opaque: Opaque<'ty>,
        expected: usize,
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

    #[expect(unused)]
    pub fn never(&self) -> Ty<'ty> {
        self.register_ty(TyDef::Never)
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

    pub fn ref_mut(&self, inner_ty: Ty<'ty>) -> Ty<'ty> {
        self.register_ty(TyDef::RefMut(inner_ty))
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

    pub fn trait_self(&self, trait_: Trait<'ty>) -> Ty<'ty> {
        let trait_self = TyDef::TraitSelf(trait_);
        self.register_ty(trait_self)
    }

    pub fn closure(
        &self,
        name: impl Into<String>,
        captures_ty: Ty<'ty>,
        param_tys: TySlice<'ty>,
        return_ty: Ty<'ty>,
    ) -> Ty<'ty> {
        let closure = TyDef::Closure {
            name: name.into(),
            captures_ty,
            param_tys,
            return_ty,
            fn_: ClosureFnCell::new(),
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
            Primitive(_) | GenVar(_) | TraitSelf(_) | InfVar(_) | Never => ty,
            Ref(inner) => {
                let inner = self.resolve_opaque_in_ty(inner);
                self.ref_(inner)
            }
            RefMut(inner) => {
                let inner = self.resolve_opaque_in_ty(inner);
                self.ref_mut(inner)
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
                ref name,
                captures_ty,
                param_tys,
                return_ty,
                ..
            } => {
                let captures_ty = self.resolve_opaque_in_ty(captures_ty);
                let param_tys: Vec<_> = param_tys.iter().map(|&t| self.resolve_opaque_in_ty(t)).collect();
                let return_ty = self.resolve_opaque_in_ty(return_ty);
                self.closure(name.as_str(), captures_ty, self.ty_slice(&param_tys), return_ty)
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

    #[must_use]
    pub fn substitute(&self, ty: Ty<'ty>, gen_vars: &GenVarSubst<'ty>, self_ty: Option<Ty<'ty>>) -> Ty<'ty> {
        use TyDef::*;

        match *ty.0 {
            Primitive(_) | InfVar(_) | Never => ty,
            GenVar(gen_var) => gen_vars.get(gen_var).unwrap_or(ty),
            TraitSelf(_) => match self_ty {
                Some(replacement) => self.substitute(replacement, gen_vars, None),
                None => ty,
            },
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
            RefMut(inner_ty) => {
                let new_inner_ty = self.substitute(inner_ty, gen_vars, self_ty);
                self.ref_mut(new_inner_ty)
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
                ref name,
                captures_ty,
                param_tys,
                return_ty,
                ref fn_,
            } => {
                let name = name.clone();
                let original_fn = fn_.get();
                let captures_ty = self.substitute(captures_ty, gen_vars, self_ty);
                let param_tys = self.substitute_on_slice(param_tys, gen_vars, self_ty);
                let return_ty = self.substitute(return_ty, gen_vars, self_ty);
                let new_ty = self.closure(name, captures_ty, param_tys, return_ty);
                if let Some(fn_val) = original_fn
                    && let TyDef::Closure { fn_: ref new_fn_, .. } = *new_ty.0
                    && new_fn_.get().is_none()
                {
                    new_fn_.set(fn_val);
                }
                new_ty
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

    pub fn get_ty_by_name(&self, ty_name: &str) -> Result<Named<'ty>, NotATypeName> {
        self.named_tys
            .borrow()
            .get(ty_name)
            .copied()
            .ok_or(NotATypeName(ty_name.to_string()))
    }
}
