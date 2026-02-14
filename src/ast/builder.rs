use crate::ast::*;

pub struct AstBuilder<'ast> {
    ast: &'ast Ast<'ast>,
}

// General
impl<'ast> AstBuilder<'ast> {
    pub fn new(ast: &'ast Ast<'ast>) -> Self {
        AstBuilder { ast }
    }

    pub fn add_fn(&self, fn_: FnDef<'ast>) -> Fn<'ast> {
        self.ast.fn_(fn_)
    }

    pub fn add_free_fn(&self, fn_: Fn<'ast>) {
        self.ast.free_fns.borrow_mut().push(fn_);
    }

    pub fn add_struct(&self, struct_: Struct<'ast>) {
        let struct_ = self.ast.arena.alloc(struct_);
        self.ast.structs.borrow_mut().push(struct_);
    }

    pub fn add_enum(&self, enum_: Enum<'ast>) {
        let enum_ = self.ast.arena.alloc(enum_);
        self.ast.enums.borrow_mut().push(enum_);
    }

    pub fn add_impl(&self, impl_: Impl<'ast>) {
        let impl_ = self.ast.arena.alloc(impl_);
        self.ast.impls.borrow_mut().push(impl_);
    }

    pub fn add_trait(&self, trait_: Trait<'ast>) {
        let trait_ = self.ast.arena.alloc(trait_);
        self.ast.traits.borrow_mut().push(trait_);
    }
}

// Expressions
impl<'ast> AstBuilder<'ast> {
    pub fn lit(&self, lit: Lit) -> Expr<'ast> {
        self.ast.expr(ExprKind::Lit(lit))
    }

    pub fn path(&self, path: Path<'ast>) -> Expr<'ast> {
        self.ast.expr(ExprKind::Path(path))
    }

    pub fn qualified_path(&self, path: QualifiedPath<'ast>) -> Expr<'ast> {
        self.ast.expr(ExprKind::QualifiedPath(path))
    }

    pub fn tuple(&self, exprs: &[Expr<'ast>]) -> Expr<'ast> {
        let exprs = self.ast.expr_slice(exprs);
        self.ast.expr(ExprKind::Tuple(exprs))
    }

    pub fn binary_op(&self, left: Expr<'ast>, operator: BinaryOperator, right: Expr<'ast>) -> Expr<'ast> {
        self.ast.expr(ExprKind::BinaryOp { left, operator, right })
    }

    pub fn unary_op(&self, operator: UnaryOperator, operand: Expr<'ast>) -> Expr<'ast> {
        self.ast.expr(ExprKind::UnaryOp { operator, operand })
    }

    pub fn assign(&self, target: Expr<'ast>, value: Expr<'ast>) -> Expr<'ast> {
        self.ast.expr(ExprKind::Assign { target, value })
    }

    pub fn call(&self, callee: Expr<'ast>, args: &[Expr<'ast>]) -> Expr<'ast> {
        let args = self.ast.expr_slice(args);
        self.ast.expr(ExprKind::Call { callee, args })
    }

    pub fn mthd_call(&self, obj: Expr<'ast>, mthd: PathSegment<'ast>, args: &[Expr<'ast>]) -> Expr<'ast> {
        let args = self.ast.expr_slice(args);
        self.ast.expr(ExprKind::MthdCall { obj, mthd, args })
    }

    pub fn struct_expr(&self, ty_path: Path<'ast>, fields: Vec<(String, Expr<'ast>)>) -> Expr<'ast> {
        self.ast.expr(ExprKind::Struct { ty_path, fields })
    }

    pub fn field_access(&self, obj: Expr<'ast>, field: FieldDescriptor<'ast>) -> Expr<'ast> {
        self.ast.expr(ExprKind::FieldAccess { obj, field })
    }

    pub fn block(&self, block: Block<'ast>) -> Expr<'ast> {
        self.ast.expr(ExprKind::Block(block))
    }

    pub fn if_(&self, cond: Expr<'ast>, then: Block<'ast>, else_: Option<Block<'ast>>) -> Expr<'ast> {
        self.ast.expr(ExprKind::If { cond, then, else_ })
    }

    pub fn loop_(&self, body: Block<'ast>) -> Expr<'ast> {
        self.ast.expr(ExprKind::Loop { body })
    }

    pub fn while_(&self, cond: Expr<'ast>, body: Block<'ast>) -> Expr<'ast> {
        self.ast.expr(ExprKind::While { cond, body })
    }

    pub fn match_(&self, scrutinee: Expr<'ast>, arms: Vec<MatchArm<'ast>>) -> Expr<'ast> {
        self.ast.expr(ExprKind::Match { scrutinee, arms })
    }

    pub fn deref(&self, base: Expr<'ast>) -> Expr<'ast> {
        self.ast.expr(ExprKind::Deref { base })
    }

    pub fn addr_of(&self, base: Expr<'ast>) -> Expr<'ast> {
        self.ast.expr(ExprKind::AddrOf { base })
    }

    pub fn as_(&self, expr: Expr<'ast>, target_ty: TyAnnot<'ast>) -> Expr<'ast> {
        self.ast.expr(ExprKind::As { expr, target_ty })
    }

    pub fn self_(&self) -> Expr<'ast> {
        self.ast.expr(ExprKind::Self_)
    }

    pub fn closure(
        &self,
        params: Vec<ClosureParam<'ast>>,
        return_ty: Option<TyAnnot<'ast>>,
        body: Block<'ast>,
    ) -> Expr<'ast> {
        self.ast.expr(ExprKind::Closure {
            params,
            return_ty,
            body,
        })
    }
}

// Statements
impl<'ast> AstBuilder<'ast> {
    pub fn let_stmt(&self, name: String, ty_annot: Option<TyAnnot<'ast>>, value: Expr<'ast>) -> Stmt<'ast> {
        self.ast.stmt(StmtKind::Let { name, ty_annot, value })
    }

    pub fn expr_stmt(&self, expr: Expr<'ast>) -> Stmt<'ast> {
        self.ast.stmt(StmtKind::Expr(expr))
    }

    pub fn return_stmt(&self, expr: Option<Expr<'ast>>) -> Stmt<'ast> {
        self.ast.stmt(StmtKind::Return(expr))
    }

    pub fn break_stmt(&self) -> Stmt<'ast> {
        self.ast.stmt(StmtKind::Break)
    }

    pub fn stmt_slice(&self, stmts: &[Stmt<'ast>]) -> StmtSlice<'ast> {
        self.ast.stmt_slice(stmts)
    }
}

// Type annotations
impl<'ast> AstBuilder<'ast> {
    pub fn ty_annot_slice(&self, ty_annots: &[TyAnnot<'ast>]) -> TyAnnotSlice<'ast> {
        self.ast.ty_annot_slice(ty_annots)
    }

    pub fn path_annot(&self, path: Path<'ast>) -> TyAnnot<'ast> {
        self.ast.ty_annot(TyAnnotKind::Path(path))
    }

    pub fn tuple_annot(&self, ty_annots: &[TyAnnot<'ast>]) -> TyAnnot<'ast> {
        let ty_annots = self.ast.ty_annot_slice(ty_annots);
        self.ast.ty_annot(TyAnnotKind::Tuple(ty_annots))
    }

    pub fn ref_annot(&self, ty_annot: TyAnnot<'ast>) -> TyAnnot<'ast> {
        self.ast.ty_annot(TyAnnotKind::Ref(ty_annot))
    }

    pub fn ptr_annot(&self, ty_annot: TyAnnot<'ast>) -> TyAnnot<'ast> {
        self.ast.ty_annot(TyAnnotKind::Ptr(ty_annot))
    }

    pub fn fn_annot(&self, param_tys: &[TyAnnot<'ast>], return_ty: Option<TyAnnot<'ast>>) -> TyAnnot<'ast> {
        let param_tys = self.ast.ty_annot_slice(param_tys);
        self.ast.ty_annot(TyAnnotKind::Fn { param_tys, return_ty })
    }

    pub fn wildcard_annot(&self) -> TyAnnot<'ast> {
        self.ast.ty_annot(TyAnnotKind::Wildcard)
    }
}
