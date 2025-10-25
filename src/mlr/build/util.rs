use std::collections::hash_map::Entry;

use crate::mlr;

use crate::mlr::build::macros::assign_to_new_loc;

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

    pub fn insert_val(&mut self, val: mlr::Value) -> mlr::build::Result<mlr::ValId> {
        let val_id = self.get_next_val_id();
        self.output.vals.insert(val_id, val);

        let type_id = self.infer_type(val_id)?;
        self.output.val_types.insert(val_id, type_id);

        Ok(val_id)
    }

    pub fn insert_stmt(&mut self, stmt: mlr::Statement) -> mlr::build::Result<mlr::StmtId> {
        let stmt_id = self.get_next_stmt_id();
        self.output.statements.insert(stmt_id, stmt);
        Ok(stmt_id)
    }

    /// TODO move resolution functionality to an impl block in another submodule,
    /// or create resolver submodule.
    pub fn resolve_name(&self, name: &str) -> mlr::build::Result<mlr::Value> {
        // Try to resolve as variable
        let loc_id = self.resolve_name_to_location(name);

        if let Some(loc_id) = loc_id {
            Ok(mlr::Value::Use(loc_id))
        } else if let Some(fn_id) = self.ctxt.function_registry.get_function_by_name(name) {
            Ok(mlr::Value::Function(fn_id))
        } else {
            Err(super::MlrBuilderError::UnresolvableSymbol { name: name.to_string() })
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

    pub fn create_unit_value(&mut self) -> mlr::build::Result<mlr::ValId> {
        let val = mlr::Value::Constant(mlr::Constant::Unit);
        self.insert_val(val)
    }

    pub fn create_unit_block(&mut self) -> mlr::build::Result<mlr::Block> {
        let (loc, stmt) = assign_to_new_loc!(self, self.create_unit_value()?);
        Ok(mlr::Block {
            statements: vec![stmt],
            output: loc,
        })
    }

    pub fn insert_assign_stmt(&mut self, place: mlr::Place, value: mlr::ValId) -> mlr::build::Result<mlr::StmtId> {
        let value_type = self
            .output
            .val_types
            .get(&value)
            .expect("type of value should be known");

        let mlr::Place::Local(loc_id) = place;

        match self.output.loc_types.entry(loc_id) {
            Entry::Occupied(occupied_entry) => {
                let loc_type = occupied_entry.get();
                if !self.ctxt.type_registry.types_equal(value_type, loc_type) {
                    return mlr::build::TypeError::ReassignTypeMismatch {
                        loc: loc_id,
                        expected: *loc_type,
                        actual: *value_type,
                    }
                    .into();
                }
            }
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(*value_type);
            }
        };

        let stmt = mlr::Statement::Assign { place, value };
        self.insert_stmt(stmt)
    }

    pub fn insert_return_stmt(&mut self, value: mlr::LocId) -> mlr::build::Result<mlr::StmtId> {
        let stmt = mlr::Statement::Return { value };

        // TODO check type
        let signature = self
            .ctxt
            .function_registry
            .get_signature_by_id(&self.fn_id)
            .expect("return stmt only valid in function");
        let return_type = signature.return_type;

        let value_type = self
            .output
            .loc_types
            .get(&value)
            .expect("type of return value should be known");

        if !self.ctxt.type_registry.types_equal(&return_type, value_type) {
            return mlr::build::TypeError::ReturnTypeMismatch {
                expected: return_type,
                actual: *value_type,
            }
            .into();
        }

        self.insert_stmt(stmt)
    }
}
