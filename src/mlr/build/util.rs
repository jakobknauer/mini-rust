use std::collections::HashSet;

use crate::{
    ctxt::{fns, ty},
    hlr,
    mlr::{
        self,
        build::{MlrBuilderError, Result, TyError},
    },
};

impl<'a> mlr::MlrBuilder<'a> {
    pub fn get_next_val(&mut self) -> mlr::Val {
        let val = self.next_val;
        self.next_val.0 += 1;
        val
    }

    pub fn get_next_stmt(&mut self) -> mlr::Stmt {
        let stmt = self.next_stmt;
        self.next_stmt.0 += 1;
        stmt
    }

    pub fn get_next_loc(&mut self) -> mlr::Loc {
        let loc = self.next_loc;
        self.next_loc.0 += 1;
        loc
    }

    pub fn get_next_place(&mut self) -> mlr::Place {
        let place = self.next_place;
        self.next_place.0 += 1;
        place
    }

    pub fn get_next_op(&mut self) -> mlr::Op {
        let op = self.next_op;
        self.next_op.0 += 1;
        op
    }

    pub fn get_loc_ty(&self, loc: &mlr::Loc) -> ty::Ty {
        *self.output.loc_tys.get(loc).expect("type of loc should be known")
    }

    pub fn get_place_ty(&self, place: &mlr::Place) -> ty::Ty {
        *self.output.place_tys.get(place).expect("type of place should be known")
    }

    pub fn get_val_ty(&self, val: &mlr::Val) -> ty::Ty {
        *self.output.val_tys.get(val).expect("type of val should be known")
    }

    pub fn get_op_ty(&self, op: &mlr::Op) -> ty::Ty {
        *self.output.op_tys.get(op).expect("type of op should be known")
    }

    pub fn insert_val(&mut self, val_def: mlr::ValDef) -> Result<mlr::Val> {
        let val = self.get_next_val();
        self.output.vals.insert(val, val_def);

        let ty = self.infer_val_ty(val)?;
        self.output.val_tys.insert(val, ty);

        Ok(val)
    }

    pub fn insert_call_val(&mut self, callable: mlr::Op, args: Vec<mlr::Op>) -> Result<mlr::Val> {
        let val = mlr::ValDef::Call { callable, args };
        self.insert_val(val)
    }

    pub fn insert_empty_val(&mut self, ty: ty::Ty) -> Result<mlr::Val> {
        let val = mlr::ValDef::Empty { ty };
        self.insert_val(val)
    }

    pub fn insert_use_val(&mut self, op: mlr::Op) -> Result<mlr::Val> {
        let val = mlr::ValDef::Use(op);
        self.insert_val(val)
    }

    pub fn insert_use_place_val(&mut self, place: mlr::Place) -> Result<mlr::Val> {
        let op = self.insert_copy_op(place)?;
        self.insert_use_val(op)
    }

    pub fn insert_addr_of_val(&mut self, place: mlr::Place) -> Result<mlr::Val> {
        let val = mlr::ValDef::AddrOf(place);
        self.insert_val(val)
    }

    pub fn insert_if_stmt(&mut self, cond: mlr::Op, then_: mlr::Stmt, else_: mlr::Stmt) -> Result<mlr::Stmt> {
        let if_ = mlr::StmtDef::If(mlr::If {
            cond,
            then: then_,
            else_,
        });
        self.insert_stmt(if_)
    }

    pub fn insert_loop_stmt(&mut self, body: mlr::Stmt) -> Result<mlr::Stmt> {
        let val = mlr::StmtDef::Loop { body };
        self.insert_stmt(val)
    }

    pub fn insert_stmt(&mut self, stmt_def: mlr::StmtDef) -> Result<mlr::Stmt> {
        let stmt = self.get_next_stmt();
        self.output.stmts.insert(stmt, stmt_def);
        self.blocks
            .back_mut()
            .expect("self.blocks should not be empty")
            .push(stmt);
        Ok(stmt)
    }

    pub fn insert_alloc_stmt(&mut self, loc: mlr::Loc) -> Result<mlr::Stmt> {
        let stmt = mlr::StmtDef::Alloc { loc };
        self.insert_stmt(stmt)
    }

    pub fn insert_fresh_alloc(&mut self) -> Result<mlr::Loc> {
        let undef = self.ctxt.tys.get_undef_ty();
        self.insert_alloc_with_ty(undef)
    }

    pub fn insert_alloc_with_ty(&mut self, ty: ty::Ty) -> Result<mlr::Loc> {
        let loc = self.get_next_loc();
        self.output.loc_tys.insert(loc, ty);

        self.insert_alloc_stmt(loc)?;
        Ok(loc)
    }

    pub fn insert_break_stmt(&mut self) -> Result<mlr::Stmt> {
        let stmt = mlr::StmtDef::Break;
        self.insert_stmt(stmt)
    }

    pub fn insert_assign_stmt(&mut self, place: mlr::Place, value: mlr::Val) -> Result<mlr::Stmt> {
        let place_ty = self.get_place_ty(&place);
        let value_ty = self.get_val_ty(&value);

        self.ctxt
            .tys
            .unify(&place_ty, &value_ty)
            .map_err(|_| mlr::build::TyError::AssignStmtTyMismatch {
                place,
                expected: place_ty,
                actual: value_ty,
            })
            .map_err(MlrBuilderError::TyError)?;

        let stmt = mlr::StmtDef::Assign { place, value };
        self.insert_stmt(stmt)
    }

    pub fn insert_assign_to_loc_stmt(&mut self, loc: mlr::Loc, value: mlr::Val) -> Result<mlr::Stmt> {
        let place = mlr::PlaceDef::Loc(loc);
        let place = self.insert_place(place)?;
        self.insert_assign_stmt(place, value)
    }

    pub fn insert_return_stmt(&mut self, value: mlr::Val) -> Result<mlr::Stmt> {
        let return_ty = self
            .ctxt
            .fns
            .get_sig(&self.target_fn)
            .expect("return stmt only valid in function")
            .return_ty;

        let val_ty = self.get_val_ty(&value);

        self.ctxt
            .tys
            .unify(&return_ty, &val_ty)
            .map_err(|_| mlr::build::TyError::ReturnTyMismatch {
                expected: return_ty,
                actual: val_ty,
            })
            .map_err(MlrBuilderError::TyError)?;

        let stmt = mlr::StmtDef::Return { value };
        self.insert_stmt(stmt)
    }

    pub fn insert_place(&mut self, place_def: mlr::PlaceDef) -> Result<mlr::Place> {
        let place = self.get_next_place();
        self.output.places.insert(place, place_def);

        let ty = self.infer_place_ty(&place)?;
        self.output.place_tys.insert(place, ty);

        Ok(place)
    }

    pub fn insert_loc_place(&mut self, loc: mlr::Loc) -> Result<mlr::Place> {
        let place = mlr::PlaceDef::Loc(loc);
        self.insert_place(place)
    }

    pub fn insert_enum_discriminant_place(&mut self, base: mlr::Place) -> Result<mlr::Place> {
        let place = mlr::PlaceDef::EnumDiscriminant { base };
        self.insert_place(place)
    }

    pub fn insert_project_to_variant_place(&mut self, base: mlr::Place, variant_index: usize) -> Result<mlr::Place> {
        let place = mlr::PlaceDef::ProjectToVariant { base, variant_index };
        self.insert_place(place)
    }

    pub fn insert_field_access_place(&mut self, base: mlr::Place, field_index: usize) -> Result<mlr::Place> {
        let place = mlr::PlaceDef::FieldAccess { base, field_index };
        self.insert_place(place)
    }

    pub fn insert_deref_place(&mut self, op: mlr::Op) -> Result<mlr::Place> {
        let place = mlr::PlaceDef::Deref(op);
        self.insert_place(place)
    }

    pub fn insert_op(&mut self, op_def: mlr::OpDef) -> Result<mlr::Op> {
        let op = self.get_next_op();
        self.output.ops.insert(op, op_def);

        let ty = self.infer_op_ty(op)?;
        self.output.op_tys.insert(op, ty);

        Ok(op)
    }

    pub fn insert_fn_op(&mut self, fn_: fns::Fn) -> Result<mlr::Op> {
        self.insert_gen_fn_op(fn_, Vec::new())
    }

    pub fn insert_gen_fn_op(&mut self, fn_: fns::Fn, gen_args: Vec<ty::Ty>) -> Result<mlr::Op> {
        self.ctxt
            .fns
            .add_instantiated_fn(&self.target_fn, &fn_, gen_args.clone());
        let op = mlr::OpDef::Fn(fn_, gen_args);
        self.insert_op(op)
    }

    pub fn insert_int_op(&mut self, int: i64) -> Result<mlr::Op> {
        let val = mlr::OpDef::Const(mlr::Const::Int(int));
        self.insert_op(val)
    }

    pub fn insert_bool_op(&mut self, boolean: bool) -> Result<mlr::Op> {
        let op = mlr::OpDef::Const(mlr::Const::Bool(boolean));
        self.insert_op(op)
    }

    pub fn insert_unit_op(&mut self) -> Result<mlr::Op> {
        let op = mlr::OpDef::Const(mlr::Const::Unit);
        self.insert_op(op)
    }

    pub fn insert_copy_op(&mut self, place: mlr::Place) -> Result<mlr::Op> {
        let op = mlr::OpDef::Copy(place);
        self.insert_op(op)
    }

    pub fn insert_copy_loc_op(&mut self, loc: mlr::Loc) -> Result<mlr::Op> {
        let place = self.insert_loc_place(loc)?;
        self.insert_copy_op(place)
    }

    /// TODO move resolution functionality to an impl block in another submodule,
    /// or create resolver submodule.
    pub fn resolve_name(&mut self, name: &str) -> Result<mlr::Op> {
        if let Some(loc) = self.resolve_name_to_location(name) {
            let place = self.insert_loc_place(loc)?;
            self.insert_copy_op(place)
        } else if let Some(fn_) = self.ctxt.fns.get_fn_by_name(name) {
            self.insert_fn_op(fn_)
        } else {
            Err(MlrBuilderError::UnresolvableSymbol { name: name.to_string() })
        }
    }

    pub fn resolve_name_to_location(&self, name: &str) -> Option<mlr::Loc> {
        self.scopes
            .iter()
            .rev()
            .filter_map(|scope| scope.vars.get(name))
            .next()
            .cloned()
    }

    pub fn try_resolve_enum_variant(&self, variant_name: &str) -> Option<(ty::Ty, usize)> {
        for (enum_, enum_def) in self.ctxt.tys.get_all_enums() {
            for (idx, variant) in enum_def.variants.iter().enumerate() {
                if variant.name == variant_name {
                    let ty = self.ctxt.tys.get_ty_of_enum(enum_).expect("enum type should be known");
                    return Some((ty, idx));
                }
            }
        }
        None
    }

    pub fn build_struct_field_init_stmts(
        &mut self,
        ty: &ty::Ty,
        fields: &[(String, hlr::Expr)],
        base_place: &mlr::Place,
    ) -> Result<()> {
        let field_indices = self.compute_field_indices(ty, fields.iter().map(|(name, _)| name.as_str()))?;

        fields
            .iter()
            .zip(field_indices)
            .try_for_each(|((_, expr), field_index)| {
                let field_place = self.insert_field_access_place(*base_place, field_index)?;
                let field_value = self.lower_to_val(expr)?;
                self.insert_assign_stmt(field_place, field_value)?;
                Ok(())
            })
    }

    pub fn compute_field_indices<'b>(
        &self,
        ty: &ty::Ty,
        field_names: impl IntoIterator<Item = &'b str>,
    ) -> Result<Vec<usize>> {
        let field_names: Vec<&str> = field_names.into_iter().collect();

        let struct_def = self.get_struct_def(ty)?;

        let expected: HashSet<&str> = struct_def.fields.iter().map(|field| field.name.as_str()).collect();
        let actual: HashSet<&str> = field_names.iter().cloned().collect();

        let missing_fields: Vec<&str> = expected.difference(&actual).cloned().collect();
        if !missing_fields.is_empty() {
            return TyError::InitializerMissingFields {
                ty: *ty,
                missing_fields: missing_fields.iter().map(|s| s.to_string()).collect(),
            }
            .into();
        }

        let extra_fields: Vec<&str> = actual.difference(&expected).cloned().collect();
        if !extra_fields.is_empty() {
            return TyError::InitializerExtraFields {
                ty: *ty,
                extra_fields: extra_fields.iter().map(|s| s.to_string()).collect(),
            }
            .into();
        }

        let field_indices = field_names
            .iter()
            .map(|field_name| {
                struct_def
                    .fields
                    .iter()
                    .position(|struct_field| &struct_field.name == field_name)
                    .ok_or(MlrBuilderError::TyError(TyError::NotAStructField {
                        ty: *ty,
                        field_name: field_name.to_string(),
                    }))
            })
            .collect::<Result<_>>()?;

        Ok(field_indices)
    }

    pub fn get_struct_def(&self, ty: &ty::Ty) -> Result<&ty::StructDef> {
        let ty_def = self.ctxt.tys.get_ty_def(ty).expect("type should be registered");

        let &ty::TyDef::Named(_, ty::Named::Struct(struct_)) = ty_def else {
            return TyError::NotAStruct { ty: *ty }.into();
        };

        let struct_def = self
            .ctxt
            .tys
            .get_struct_def(&struct_)
            .expect("struct definition should be registered");

        Ok(struct_def)
    }

    pub fn get_enum_def(&self, ty: &ty::Ty) -> Result<&ty::EnumDef> {
        let ty_def = self.ctxt.tys.get_ty_def(ty).expect("type should be registered");

        let &ty::TyDef::Named(_, ty::Named::Enum(enum_)) = ty_def else {
            return TyError::NotAnEnum { ty: *ty }.into();
        };

        let enum_def = self
            .ctxt
            .tys
            .get_enum_def(&enum_)
            .expect("enum definition should be registered");

        Ok(enum_def)
    }

    pub fn get_signature(&self) -> &fns::FnSig {
        self.ctxt
            .fns
            .get_sig(&self.target_fn)
            .expect("function signature should be registered")
    }

    pub fn resolve_hlr_ty_annot(&mut self, annot: &hlr::TyAnnot) -> Result<ty::Ty> {
        let gen_params = self.get_signature().gen_params.clone();
        let ty = self
            .ctxt
            .tys
            .get_ty_by_hlr_annot(annot, &gen_params)
            .ok_or(MlrBuilderError::TyError(TyError::UnresolvableTyAnnot))?;
        Ok(ty)
    }
}
