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
    pub fn get_next_val_id(&mut self) -> mlr::ValId {
        let id = self.next_val_id;
        self.next_val_id.0 += 1;
        id
    }

    pub fn get_next_stmt_id(&mut self) -> mlr::StmtId {
        let id = self.next_stmt_id;
        self.next_stmt_id.0 += 1;
        id
    }

    pub fn get_next_loc_id(&mut self) -> mlr::LocId {
        let id = self.next_loc_id;
        self.next_loc_id.0 += 1;
        id
    }

    pub fn get_next_place_id(&mut self) -> mlr::PlaceId {
        let id = self.next_place_id;
        self.next_place_id.0 += 1;
        id
    }

    pub fn get_next_op_id(&mut self) -> mlr::OpId {
        let id = self.next_op_id;
        self.next_op_id.0 += 1;
        id
    }

    pub fn get_loc_ty(&self, loc_id: &mlr::LocId) -> ty::Ty {
        *self.output.loc_tys.get(loc_id).expect("type of loc should be known")
    }

    pub fn try_get_loc_ty(&self, loc_id: &mlr::LocId) -> Option<ty::Ty> {
        self.output.loc_tys.get(loc_id).cloned()
    }

    pub fn get_place_ty(&self, place_id: &mlr::PlaceId) -> ty::Ty {
        *self
            .output
            .place_tys
            .get(place_id)
            .expect("type of place should be known")
    }

    pub fn try_get_place_ty(&self, place_id: &mlr::PlaceId) -> Option<ty::Ty> {
        self.output.place_tys.get(place_id).cloned()
    }

    pub fn get_val_ty(&self, val_id: &mlr::ValId) -> ty::Ty {
        *self.output.val_tys.get(val_id).expect("type of val should be known")
    }

    pub fn get_op_ty(&self, op_id: &mlr::OpId) -> ty::Ty {
        *self.output.op_tys.get(op_id).expect("type of op should be known")
    }

    pub fn insert_val(&mut self, val: mlr::Val) -> Result<mlr::ValId> {
        let val_id = self.get_next_val_id();
        self.output.vals.insert(val_id, val);

        let ty = self.infer_val_ty(val_id)?;
        self.output.val_tys.insert(val_id, ty);

        Ok(val_id)
    }

    pub fn insert_call_val(&mut self, callable: mlr::OpId, args: Vec<mlr::OpId>) -> Result<mlr::ValId> {
        let val = mlr::Val::Call { callable, args };
        self.insert_val(val)
    }

    pub fn insert_empty_val(&mut self, ty: ty::Ty) -> Result<mlr::ValId> {
        let val = mlr::Val::Empty { ty };
        self.insert_val(val)
    }

    pub fn insert_use_val(&mut self, op: mlr::OpId) -> Result<mlr::ValId> {
        let val = mlr::Val::Use(op);
        self.insert_val(val)
    }

    pub fn insert_use_place_val(&mut self, place: mlr::PlaceId) -> Result<mlr::ValId> {
        let op = self.insert_copy_op(place)?;
        self.insert_use_val(op)
    }

    pub fn insert_if_stmt(&mut self, cond: mlr::OpId, then_: mlr::StmtId, else_: mlr::StmtId) -> Result<mlr::StmtId> {
        let if_ = mlr::Stmt::If(mlr::If {
            condition: cond,
            then_block: then_,
            else_block: else_,
        });
        self.insert_stmt(if_)
    }

    pub fn insert_loop_stmt(&mut self, body: mlr::StmtId) -> Result<mlr::StmtId> {
        let val = mlr::Stmt::Loop { body };
        self.insert_stmt(val)
    }

    pub fn insert_stmt(&mut self, stmt: mlr::Stmt) -> Result<mlr::StmtId> {
        let stmt_id = self.get_next_stmt_id();
        self.output.stmts.insert(stmt_id, stmt);
        self.blocks
            .back_mut()
            .expect("self.blocks should not be empty")
            .push(stmt_id);
        Ok(stmt_id)
    }

    pub fn insert_alloc_stmt(&mut self, loc_id: mlr::LocId) -> Result<mlr::StmtId> {
        assert!(
            !self.output.allocated_locs.contains(&loc_id),
            "location already allocated"
        );
        self.output.allocated_locs.insert(loc_id);
        let stmt = mlr::Stmt::Alloc { loc: loc_id };
        self.insert_stmt(stmt)
    }

    pub fn insert_fresh_alloc(&mut self) -> Result<mlr::LocId> {
        let loc_id = self.get_next_loc_id();
        self.insert_alloc_stmt(loc_id)?;
        Ok(loc_id)
    }

    pub fn insert_break_stmt(&mut self) -> Result<mlr::StmtId> {
        let stmt = mlr::Stmt::Break;
        self.insert_stmt(stmt)
    }

    pub fn insert_assign_stmt(&mut self, place: mlr::PlaceId, value: mlr::ValId) -> Result<mlr::StmtId> {
        self.assert_place_valid(&place);

        let place_ty = self.try_get_place_ty(&place);
        let value_ty = self.get_val_ty(&value);

        if let Some(place_ty) = place_ty {
            if !self.ctxt.tys.ty_equal(&place_ty, &value_ty) {
                return mlr::build::TyError::AssignStmtTyMismatch {
                    place,
                    expected: place_ty,
                    actual: value_ty,
                }
                .into();
            }
        } else {
            self.output.place_tys.insert(place, value_ty);
            if let mlr::Place::Local(loc_id) = self.output.places.get(&place).expect("place should be known") {
                self.output.loc_tys.insert(*loc_id, value_ty);
            }
        }

        let stmt = mlr::Stmt::Assign { place, value };
        self.insert_stmt(stmt)
    }

    pub fn insert_assign_to_loc_stmt(&mut self, loc_id: mlr::LocId, value: mlr::ValId) -> Result<mlr::StmtId> {
        let place = mlr::Place::Local(loc_id);
        let place_id = self.insert_place(place)?;
        self.insert_assign_stmt(place_id, value)
    }

    pub fn insert_return_stmt(&mut self, value: mlr::ValId) -> Result<mlr::StmtId> {
        let return_ty = self
            .ctxt
            .fns
            .get_signature_by_id(&self.mlr_fn)
            .expect("return stmt only valid in function")
            .return_ty;

        let val_ty = self.get_val_ty(&value);

        if !self.ctxt.tys.ty_equal(&return_ty, &val_ty) {
            return mlr::build::TyError::ReturnTyMismatch {
                expected: return_ty,
                actual: val_ty,
            }
            .into();
        }

        let stmt = mlr::Stmt::Return { value };
        self.insert_stmt(stmt)
    }

    pub fn insert_place(&mut self, place: mlr::Place) -> Result<mlr::PlaceId> {
        let place_id = self.get_next_place_id();
        self.output.places.insert(place_id, place);

        let ty = self.try_infer_place_ty(&place_id)?;
        if let Some(ty) = ty {
            self.output.place_tys.insert(place_id, ty);
        };

        Ok(place_id)
    }

    pub fn insert_loc_place(&mut self, loc_id: mlr::LocId) -> Result<mlr::PlaceId> {
        let place = mlr::Place::Local(loc_id);
        self.insert_place(place)
    }

    pub fn insert_enum_discriminant_place(&mut self, base: mlr::PlaceId) -> Result<mlr::PlaceId> {
        let place = mlr::Place::EnumDiscriminant { base };
        self.insert_place(place)
    }

    pub fn insert_project_to_variant_place(&mut self, base: mlr::PlaceId, variant_idx: usize) -> Result<mlr::PlaceId> {
        let place = mlr::Place::ProjectToVariant {
            base,
            variant_index: variant_idx,
        };
        self.insert_place(place)
    }

    pub fn insert_field_access_place(&mut self, base: mlr::PlaceId, field_index: usize) -> Result<mlr::PlaceId> {
        let place = mlr::Place::FieldAccess { base, field_index };
        self.insert_place(place)
    }

    pub fn insert_op(&mut self, operand: mlr::Operand) -> Result<mlr::OpId> {
        let op_id = self.get_next_op_id();
        self.output.ops.insert(op_id, operand);

        let ty = self.infer_op_ty(op_id)?;
        self.output.op_tys.insert(op_id, ty);

        Ok(op_id)
    }

    pub fn insert_fn_op(&mut self, fn_: fns::Fn) -> Result<mlr::OpId> {
        let op = mlr::Operand::Fn(fn_);
        self.insert_op(op)
    }

    pub fn insert_int_op(&mut self, int: i64) -> Result<mlr::OpId> {
        let val = mlr::Operand::Constant(mlr::Constant::Int(int));
        self.insert_op(val)
    }

    pub fn insert_bool_op(&mut self, boolean: bool) -> Result<mlr::OpId> {
        let op = mlr::Operand::Constant(mlr::Constant::Bool(boolean));
        self.insert_op(op)
    }

    pub fn insert_unit_op(&mut self) -> Result<mlr::OpId> {
        let op = mlr::Operand::Constant(mlr::Constant::Unit);
        self.insert_op(op)
    }

    pub fn insert_copy_op(&mut self, place: mlr::PlaceId) -> Result<mlr::OpId> {
        let op = mlr::Operand::Copy(place);
        self.insert_op(op)
    }

    pub fn insert_copy_loc_op(&mut self, loc_id: mlr::LocId) -> Result<mlr::OpId> {
        let place = self.insert_loc_place(loc_id)?;
        self.insert_copy_op(place)
    }

    /// TODO move resolution functionality to an impl block in another submodule,
    /// or create resolver submodule.
    pub fn resolve_name(&mut self, name: &str) -> Result<mlr::OpId> {
        if let Some(loc_id) = self.resolve_name_to_location(name) {
            let place = self.insert_loc_place(loc_id)?;
            self.insert_copy_op(place)
        } else if let Some(fn_) = self.ctxt.fns.get_fn_by_name(name) {
            self.insert_fn_op(fn_)
        } else {
            Err(MlrBuilderError::UnresolvableSymbol { name: name.to_string() })
        }
    }

    pub fn resolve_name_to_location(&self, name: &str) -> Option<mlr::LocId> {
        self.scopes
            .iter()
            .rev()
            .filter_map(|scope| scope.vars.get(name))
            .next()
            .cloned()
    }

    pub fn try_resolve_enum_variant(&self, variant_name: &str) -> Option<(ty::Ty, usize)> {
        for (enum_id, enum_def) in self.ctxt.tys.get_all_enums() {
            for (idx, variant) in enum_def.variants.iter().enumerate() {
                if variant.name == variant_name {
                    let ty = self
                        .ctxt
                        .tys
                        .get_ty_by_enum_id(enum_id)
                        .expect("enum type should be known");
                    return Some((ty, idx));
                }
            }
        }
        None
    }

    pub fn build_struct_field_init_stmts(
        &mut self,
        ty: &ty::Ty,
        fields: &[(String, hlr::Expression)],
        base_place: &mlr::PlaceId,
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

    pub fn get_struct_def(&self, ty: &ty::Ty) -> Result<&ty::StructDefinition> {
        let ty_def = self.ctxt.tys.get_ty_def(ty).expect("type should be registered");

        let &ty::TyDef::Named(_, ty::Named::Struct(struct_id)) = ty_def else {
            return TyError::NotAStruct { ty: *ty }.into();
        };

        let struct_def = self
            .ctxt
            .tys
            .get_struct_definition(&struct_id)
            .expect("struct definition should be registered");

        Ok(struct_def)
    }

    pub fn get_enum_def(&self, ty: &ty::Ty) -> Result<&ty::EnumDefinition> {
        let ty_def = self.ctxt.tys.get_ty_def(ty).expect("type should be registered");

        let &ty::TyDef::Named(_, ty::Named::Enum(enum_id)) = ty_def else {
            return TyError::NotAnEnum { ty: *ty }.into();
        };

        let enum_def = self
            .ctxt
            .tys
            .get_enum_definition(&enum_id)
            .expect("enum definition should be registered");

        Ok(enum_def)
    }

    pub fn assert_place_valid(&self, place: &mlr::PlaceId) {
        let place = self
            .output
            .places
            .get(place)
            .expect("place should be known for validity check");

        match place {
            mlr::Place::Local(loc_id) => assert!(self.output.allocated_locs.contains(loc_id)),
            mlr::Place::FieldAccess { base, .. } => self.assert_place_valid(base),
            mlr::Place::EnumDiscriminant { base } => self.assert_place_valid(base),
            mlr::Place::ProjectToVariant { base, .. } => self.assert_place_valid(base),
        }
    }
}
