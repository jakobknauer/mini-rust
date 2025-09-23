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

    pub fn insert_expr(&mut self, expr: mlr::Expression) -> mlr::ExprId {
        let expr_id = self.get_next_expr_id();
        self.output.expressions.insert(expr_id, expr);
        expr_id
    }

    pub fn insert_stmt(&mut self, stmt: mlr::Statement) -> mlr::StmtId {
        let stmt_id = self.get_next_stmt_id();
        self.output.statements.insert(stmt_id, stmt);
        stmt_id
    }

    /// TODO move resolution functionality to an impl block in another submodule,
    /// or create resolver submodule.
    pub fn resolve_name(&self, name: &str) -> mlr::build::Result<mlr::Expression> {
        // Try to resolve as variable
        let loc_id = self.resolve_name_to_location(name);

        if let Some(loc_id) = loc_id {
            Ok(mlr::Expression::Var(loc_id))
        } else if let Some(fn_id) = self.function_registry.get_function_by_name(name) {
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

    pub fn create_unit_value(&mut self) -> mlr::ExprId {
        let expr = mlr::Expression::Constant(mlr::Constant::Unit);
        self.insert_expr(expr)
    }

    pub fn create_unit_block(&mut self) -> mlr::Block {
        let loc = self.get_next_loc_id();
        let init = self.create_unit_value();
        let stmt = mlr::Statement::Assign { loc: loc, value: init };
        let stmt = self.insert_stmt(stmt);
        mlr::Block {
            statements: vec![stmt],
            output: loc,
        }
    }

    pub fn insert_assign_stmt(&mut self, loc: mlr::LocId, expr: mlr::ExprId) -> mlr::StmtId {
        let stmt = mlr::Statement::Assign { loc, value: expr };
        self.insert_stmt(stmt)
    }
}
