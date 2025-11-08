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

    pub fn get_loc_type(&self, loc_id: &mlr::LocId) -> types::TypeId {
        *self.output.loc_types.get(loc_id).expect("type of loc should be known")
    }

    pub fn get_place_type(&self, place_id: &mlr::PlaceId) -> types::TypeId {
        *self
            .output
            .place_types
            .get(place_id)
            .expect("type of place should be known")
    }

    pub fn get_val_type(&self, val_id: &mlr::ValId) -> types::TypeId {
        *self.output.val_types.get(val_id).expect("type of val should be known")
    }

    pub fn insert_val(&mut self, val: mlr::Value) -> Result<mlr::ValId> {
        let val_id = self.get_next_val_id();
        self.output.vals.insert(val_id, val);

        let type_id = self.infer_val_type(val_id)?;
        self.output.val_types.insert(val_id, type_id);

        Ok(val_id)
    }

    pub fn insert_function_val(&mut self, fn_id: functions::FnId) -> Result<mlr::ValId> {
        let val = mlr::Value::Function(fn_id);
        self.insert_val(val)
    }

    pub fn insert_call_val(&mut self, callable: mlr::LocId, args: Vec<mlr::LocId>) -> Result<mlr::ValId> {
        let val = mlr::Value::Call { callable, args };
        self.insert_val(val)
    }

    pub fn insert_if_val(
        &mut self,
        condition: mlr::LocId,
        then_block: mlr::ValId,
        else_block: mlr::ValId,
    ) -> Result<mlr::ValId> {
        let val = mlr::Value::If(mlr::If {
            condition,
            then_block,
            else_block,
        });
        self.insert_val(val)
    }

    pub fn insert_new_block_val(&mut self, statements: Vec<mlr::StmtId>, output: mlr::ValId) -> Result<mlr::ValId> {
        let block = mlr::Value::Block { statements, output };
        self.insert_val(block)
    }

    pub fn insert_empty_val(&mut self, type_id: types::TypeId) -> Result<mlr::ValId> {
        let val = mlr::Value::Empty { type_id };
        self.insert_val(val)
    }

    pub fn insert_int_val(&mut self, int: i64) -> Result<mlr::ValId> {
        let val = mlr::Value::Constant(mlr::Constant::Int(int));
        self.insert_val(val)
    }

    pub fn insert_bool_val(&mut self, boolean: bool) -> Result<mlr::ValId> {
        let val = mlr::Value::Constant(mlr::Constant::Bool(boolean));
        self.insert_val(val)
    }

    pub fn insert_unit_val(&mut self) -> Result<mlr::ValId> {
        let val = mlr::Value::Constant(mlr::Constant::Unit);
        self.insert_val(val)
    }

    pub fn insert_use_val(&mut self, place: mlr::PlaceId) -> Result<mlr::ValId> {
        let val = mlr::Value::Use(place);
        self.insert_val(val)
    }

    pub fn insert_loop_val(&mut self, body: mlr::ValId) -> Result<mlr::ValId> {
        let val = mlr::Value::Loop { body };
        self.insert_val(val)
    }

    pub fn insert_stmt(&mut self, stmt: mlr::Statement) -> Result<mlr::StmtId> {
        let stmt_id = self.get_next_stmt_id();
        self.output.stmts.insert(stmt_id, stmt);
        Ok(stmt_id)
    }

    pub fn insert_break_stmt(&mut self) -> Result<mlr::StmtId> {
        let stmt = mlr::Statement::Break;
        self.insert_stmt(stmt)
    }

    pub fn insert_assign_stmt(&mut self, place: mlr::PlaceId, value: mlr::ValId) -> Result<mlr::StmtId> {
        let place_type = self.get_place_type(&place);
        let value_type = self.get_val_type(&value);

        if !self.ctxt.type_registry.types_equal(&place_type, &value_type) {
            return mlr::build::TypeError::AssignStmtTypeMismatch {
                place,
                expected: place_type,
                actual: value_type,
            }
            .into();
        }

        let stmt = mlr::Statement::Assign { place, value };
        self.insert_stmt(stmt)
    }

    pub fn insert_return_stmt(&mut self, value: mlr::LocId) -> Result<mlr::StmtId> {
        let return_type = self
            .ctxt
            .function_registry
            .get_signature_by_id(&self.fn_id)
            .expect("return stmt only valid in function")
            .return_type;

        let value_type = self.get_loc_type(&value);

        if !self.ctxt.type_registry.types_equal(&return_type, &value_type) {
            return mlr::build::TypeError::ReturnTypeMismatch {
                expected: return_type,
                actual: value_type,
            }
            .into();
        }

        let stmt = mlr::Statement::Return { value };
        self.insert_stmt(stmt)
    }

    pub fn insert_place(&mut self, place: mlr::Place) -> Result<mlr::PlaceId> {
        let place_id = self.get_next_place_id();
        self.output.places.insert(place_id, place);

        let type_id = self.infer_place_type(&place_id)?;
        self.output.place_types.insert(place_id, type_id);

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

    /// TODO move resolution functionality to an impl block in another submodule,
    /// or create resolver submodule.
    pub fn resolve_name(&mut self, name: &str) -> Result<mlr::ValId> {
        if let Some(loc_id) = self.resolve_name_to_location(name) {
            let place = self.insert_loc_place(loc_id)?;
            self.insert_use_val(place)
        } else if let Some(fn_id) = self.ctxt.function_registry.get_function_by_name(name) {
            self.insert_function_val(fn_id)
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
    ) -> Result<Vec<mlr::StmtId>> {
        let field_indices = self.compute_field_indices(type_id, fields.iter().map(|(name, _)| name.as_str()))?;

        let field_init_stmts: Vec<_> = fields
            .iter()
            .zip(field_indices)
            .map(|((_, expr), field_index)| {
                let field_place = self.insert_field_access_place(*base_place, field_index)?;
                let field_value = self.lower_to_val(expr)?;
                let field_stmt = self.insert_assign_stmt(field_place, field_value)?;
                Ok(field_stmt)
            })
            .collect::<Result<_>>()?;

        Ok(field_init_stmts)
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
}
