use std::collections::hash_map::Entry;

use crate::mlr;

impl<'a> mlr::MlrBuilder<'a> {
    pub fn get_next_expr_id(&mut self) -> mlr::ExprId {
        let id = self.next_expr_id;
        self.next_expr_id.0 += 1;
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

    pub fn insert_expr(&mut self, expr: mlr::Expression) -> mlr::build::Result<mlr::ExprId> {
        let expr_id = self.get_next_expr_id();
        self.output.expressions.insert(expr_id, expr);

        let type_id = self.infer_type(expr_id)?;
        self.output.expr_types.insert(expr_id, type_id);

        Ok(expr_id)
    }

    pub fn insert_stmt(&mut self, stmt: mlr::Statement) -> mlr::build::Result<mlr::StmtId> {
        let stmt_id = self.get_next_stmt_id();
        self.output.statements.insert(stmt_id, stmt);
        Ok(stmt_id)
    }

    /// TODO move resolution functionality to an impl block in another submodule,
    /// or create resolver submodule.
    pub fn resolve_name(&self, name: &str) -> mlr::build::Result<mlr::Expression> {
        // Try to resolve as variable
        let loc_id = self.resolve_name_to_location(name);

        if let Some(loc_id) = loc_id {
            Ok(mlr::Expression::Var(loc_id))
        } else if let Some(fn_id) = self.ctxt.function_registry.get_function_by_name(name) {
            Ok(mlr::Expression::Function(fn_id))
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

    pub fn create_unit_value(&mut self) -> mlr::build::Result<mlr::ExprId> {
        let expr = mlr::Expression::Constant(mlr::Constant::Unit);
        self.insert_expr(expr)
    }

    pub fn create_unit_block(&mut self) -> mlr::build::Result<mlr::Block> {
        let loc = self.get_next_loc_id();
        let init = self.create_unit_value()?;
        let stmt = mlr::Statement::Assign { loc, value: init };
        let stmt = self.insert_stmt(stmt)?;
        Ok(mlr::Block {
            statements: vec![stmt],
            output: loc,
        })
    }

    pub fn insert_assign_stmt(&mut self, loc: mlr::LocId, expr: mlr::ExprId) -> mlr::build::Result<mlr::StmtId> {
        let expr_type = self.output.expr_types.get(&expr).expect("type of expr should be known");

        match self.output.loc_types.entry(loc) {
            Entry::Occupied(occupied_entry) => {
                let loc_type = occupied_entry.get();
                if !self.ctxt.type_registry.types_equal(expr_type, loc_type) {
                    return mlr::build::TypeError::ReassignTypeMismatch {
                        loc,
                        expected: *loc_type,
                        actual: *expr_type,
                    }
                    .into();
                }
            }
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(*expr_type);
            }
        };

        let stmt = mlr::Statement::Assign { loc, value: expr };
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
