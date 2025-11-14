use std::collections::HashSet;

use crate::{
    ctxt::{functions, types},
    hlr,
    mlr::{
        self,
        build::{MlrBuilderError, Result, TypeError},
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

    pub fn get_loc_type(&self, loc_id: &mlr::LocId) -> types::TypeId {
        *self.output.loc_types.get(loc_id).expect("type of loc should be known")
    }

    pub fn try_get_loc_type(&self, loc_id: &mlr::LocId) -> Option<types::TypeId> {
        self.output.loc_types.get(loc_id).cloned()
    }

    pub fn get_place_type(&self, place_id: &mlr::PlaceId) -> types::TypeId {
        *self
            .output
            .place_types
            .get(place_id)
            .expect("type of place should be known")
    }

    pub fn try_get_place_type(&self, place_id: &mlr::PlaceId) -> Option<types::TypeId> {
        self.output.place_types.get(place_id).cloned()
    }

    pub fn get_val_type(&self, val_id: &mlr::ValId) -> types::TypeId {
        *self.output.val_types.get(val_id).expect("type of val should be known")
    }

    pub fn get_op_type(&self, op_id: &mlr::OpId) -> types::TypeId {
        *self.output.op_types.get(op_id).expect("type of op should be known")
    }

    pub fn insert_val(&mut self, val: mlr::Val) -> Result<mlr::ValId> {
        let val_id = self.get_next_val_id();
        self.output.vals.insert(val_id, val);

        let type_id = self.infer_val_type(val_id)?;
        self.output.val_types.insert(val_id, type_id);

        Ok(val_id)
    }

    pub fn insert_call_val(&mut self, callable: mlr::OpId, args: Vec<mlr::OpId>) -> Result<mlr::ValId> {
        let val = mlr::Val::Call { callable, args };
        self.insert_val(val)
    }

    pub fn insert_if_stmt(
        &mut self,
        condition: mlr::OpId,
        then_block: mlr::StmtId,
        else_block: mlr::StmtId,
    ) -> Result<()> {
        let val = mlr::Stmt::If(mlr::If {
            condition,
            then_block,
            else_block,
        });
        self.insert_stmt(val)
    }

    pub fn insert_new_block_stmt(&mut self, statements: Vec<mlr::StmtId>) -> Result<()> {
        let block = mlr::Stmt::Block(statements);
        self.insert_stmt(block)
    }

    pub fn insert_empty_val(&mut self, type_id: types::TypeId) -> Result<mlr::ValId> {
        let val = mlr::Val::Empty { type_id };
        self.insert_val(val)
    }

    pub fn insert_loop_stmt(&mut self, body: mlr::StmtId) -> Result<()> {
        let val = mlr::Stmt::Loop { body };
        self.insert_stmt(val)
    }

    pub fn insert_use_val(&mut self, op: mlr::OpId) -> Result<mlr::ValId> {
        let val = mlr::Val::Use(op);
        self.insert_val(val)
    }

    pub fn insert_use_place_val(&mut self, place: mlr::PlaceId) -> Result<mlr::ValId> {
        let op = self.insert_copy_op(place)?;
        self.insert_use_val(op)
    }

    pub fn insert_stmt(&mut self, stmt: mlr::Stmt) -> Result<()> {
        let stmt_id = self.get_next_stmt_id();
        self.output.stmts.insert(stmt_id, stmt);
        self.blocks.back_mut().unwrap().push(stmt_id);
        Ok(())
    }

    pub fn insert_alloc_stmt(&mut self, loc_id: mlr::LocId) -> Result<()> {
        assert!(
            !self.output.allocated_locs.contains(&loc_id),
            "location already allocated"
        );
        self.output.allocated_locs.insert(loc_id);
        let stmt = mlr::Stmt::Alloc { loc: loc_id };
        self.insert_stmt(stmt)
    }

    pub fn insert_break_stmt(&mut self) -> Result<()> {
        let stmt = mlr::Stmt::Break;
        self.insert_stmt(stmt)
    }

    pub fn insert_assign_to_loc_stmt(&mut self, loc_id: mlr::LocId, value: mlr::ValId) -> Result<()> {
        let place = mlr::Place::Local(loc_id);
        let place_id = self.insert_place(place)?;
        self.insert_assign_stmt(place_id, value)
    }

    pub fn insert_assign_stmt(&mut self, place: mlr::PlaceId, value: mlr::ValId) -> Result<()> {
        self.assert_place_valid(&place);

        let place_type = self.try_get_place_type(&place);
        let value_type = self.get_val_type(&value);

        if let Some(place_type) = place_type {
            if !self.ctxt.type_registry.types_equal(&place_type, &value_type) {
                return mlr::build::TypeError::AssignStmtTypeMismatch {
                    place,
                    expected: place_type,
                    actual: value_type,
                }
                .into();
            }
        } else {
            self.output.place_types.insert(place, value_type);
            if let mlr::Place::Local(loc_id) = self.output.places.get(&place).expect("place should be known") {
                self.output.loc_types.insert(*loc_id, value_type);
            }
        }

        let stmt = mlr::Stmt::Assign { place, value };
        self.insert_stmt(stmt)
    }

    pub fn insert_return_stmt(&mut self, value: mlr::ValId) -> Result<()> {
        let return_type = self
            .ctxt
            .function_registry
            .get_signature_by_id(&self.fn_id)
            .expect("return stmt only valid in function")
            .return_type;

        let value_type = self.get_val_type(&value);

        if !self.ctxt.type_registry.types_equal(&return_type, &value_type) {
            return mlr::build::TypeError::ReturnTypeMismatch {
                expected: return_type,
                actual: value_type,
            }
            .into();
        }

        let stmt = mlr::Stmt::Return { value };
        self.insert_stmt(stmt)
    }

    pub fn insert_place(&mut self, place: mlr::Place) -> Result<mlr::PlaceId> {
        let place_id = self.get_next_place_id();
        self.output.places.insert(place_id, place);

        let type_id = self.try_infer_place_type(&place_id)?;
        if let Some(type_id) = type_id {
            self.output.place_types.insert(place_id, type_id);
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

    pub fn insert_project_to_variant_place(
        &mut self,
        base: mlr::PlaceId,
        variant_index: usize,
    ) -> Result<mlr::PlaceId> {
        let place = mlr::Place::ProjectToVariant { base, variant_index };
        self.insert_place(place)
    }

    pub fn insert_field_access_place(&mut self, base: mlr::PlaceId, field_index: usize) -> Result<mlr::PlaceId> {
        let place = mlr::Place::FieldAccess { base, field_index };
        self.insert_place(place)
    }

    pub fn insert_op(&mut self, operand: mlr::Operand) -> Result<mlr::OpId> {
        let op_id = self.get_next_op_id();
        self.output.ops.insert(op_id, operand);

        let type_id = self.infer_op_type(op_id)?;
        self.output.op_types.insert(op_id, type_id);

        Ok(op_id)
    }

    pub fn insert_function_op(&mut self, fn_id: functions::FnId) -> Result<mlr::OpId> {
        let op = mlr::Operand::Function(fn_id);
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
        } else if let Some(fn_id) = self.ctxt.function_registry.get_function_by_name(name) {
            self.insert_function_op(fn_id)
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

    pub fn try_resolve_enum_variant(&self, variant_name: &str) -> Option<(types::TypeId, usize)> {
        for (enum_id, enum_def) in self.ctxt.type_registry.get_all_enums() {
            for (idx, variant) in enum_def.variants.iter().enumerate() {
                if variant.name == variant_name {
                    let type_id = self
                        .ctxt
                        .type_registry
                        .get_type_id_by_enum_id(enum_id)
                        .expect("enum type id should be known");
                    return Some((type_id, idx));
                }
            }
        }
        None
    }

    pub fn build_struct_field_init_stmts(
        &mut self,
        type_id: &types::TypeId,
        fields: &[(String, hlr::Expression)],
        base_place: &mlr::PlaceId,
    ) -> Result<()> {
        let field_indices = self.compute_field_indices(type_id, fields.iter().map(|(name, _)| name.as_str()))?;

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
        type_id: &types::TypeId,
        field_names: impl IntoIterator<Item = &'b str>,
    ) -> Result<Vec<usize>> {
        let field_names: Vec<&str> = field_names.into_iter().collect();

        let struct_def = self.get_struct_def(type_id)?;

        let expected: HashSet<&str> = struct_def.fields.iter().map(|field| field.name.as_str()).collect();
        let actual: HashSet<&str> = field_names.iter().cloned().collect();

        let missing_fields: Vec<&str> = expected.difference(&actual).cloned().collect();
        if !missing_fields.is_empty() {
            return TypeError::InitializerMissingFields {
                type_id: *type_id,
                missing_fields: missing_fields.iter().map(|s| s.to_string()).collect(),
            }
            .into();
        }

        let extra_fields: Vec<&str> = actual.difference(&expected).cloned().collect();
        if !extra_fields.is_empty() {
            return TypeError::InitializerExtraFields {
                type_id: *type_id,
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
                    .ok_or(MlrBuilderError::TypeError(TypeError::NotAStructField {
                        type_id: *type_id,
                        field_name: field_name.to_string(),
                    }))
            })
            .collect::<Result<_>>()?;

        Ok(field_indices)
    }

    pub fn get_struct_def(&self, type_id: &types::TypeId) -> Result<&types::StructDefinition> {
        let type_ = self
            .ctxt
            .type_registry
            .get_type_by_id(type_id)
            .expect("type should be registered");

        let &types::Type::NamedType(_, types::NamedType::Struct(struct_id)) = type_ else {
            return TypeError::NotAStruct { type_id: *type_id }.into();
        };

        let struct_def = self
            .ctxt
            .type_registry
            .get_struct_definition(&struct_id)
            .expect("struct definition should be registered");

        Ok(struct_def)
    }

    pub fn get_enum_def(&self, type_id: &types::TypeId) -> Result<&types::EnumDefinition> {
        let type_ = self
            .ctxt
            .type_registry
            .get_type_by_id(type_id)
            .expect("type should be registered");

        let &types::Type::NamedType(_, types::NamedType::Enum(enum_id)) = type_ else {
            return TypeError::NotAnEnum { type_id: *type_id }.into();
        };

        let enum_def = self
            .ctxt
            .type_registry
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
