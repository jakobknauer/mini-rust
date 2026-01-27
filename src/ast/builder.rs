use crate::ast::*;

pub struct AstBuilder<'a> {
    ast: &'a mut Ast,
}

// General
impl<'a> AstBuilder<'a> {
    pub fn new(ast: &'a mut Ast) -> Self {
        AstBuilder { ast }
    }

    pub fn ast(&self) -> &Ast {
        self.ast
    }

    pub fn add_fn(&mut self, fn_: FnDef) -> Fn {
        self.ast.new_fn(fn_)
    }

    pub fn add_free_fn(&mut self, fn_: Fn) {
        self.ast.free_fns.push(fn_);
    }

    pub fn add_struct(&mut self, struct_: Struct) {
        self.ast.structs.push(struct_);
    }

    pub fn add_enum(&mut self, enum_: Enum) {
        self.ast.enums.push(enum_);
    }

    pub fn add_impl(&mut self, impl_: Impl) {
        self.ast.impls.push(impl_);
    }

    pub fn add_trait(&mut self, trait_: Trait) {
        self.ast.traits.push(trait_);
    }
}

// Expressions
impl<'a> AstBuilder<'a> {
    pub fn lit(&mut self, lit: Lit) -> Expr {
        self.ast.new_expr(ExprKind::Lit(lit))
    }

    pub fn path(&mut self, path: Path) -> Expr {
        self.ast.new_expr(ExprKind::Path(path))
    }

    pub fn qualified_path(&mut self, path: QualifiedPath) -> Expr {
        self.ast.new_expr(ExprKind::QualifiedPath(path))
    }

    pub fn tuple(&mut self, exprs: &[Expr]) -> Expr {
        let exprs = self.ast.new_expr_slice(exprs);
        self.ast.new_expr(ExprKind::Tuple(exprs))
    }

    pub fn binary_op(&mut self, left: Expr, operator: BinaryOperator, right: Expr) -> Expr {
        self.ast.new_expr(ExprKind::BinaryOp { left, operator, right })
    }

    pub fn unary_op(&mut self, operator: UnaryOperator, operand: Expr) -> Expr {
        self.ast.new_expr(ExprKind::UnaryOp { operator, operand })
    }

    pub fn assign(&mut self, target: Expr, value: Expr) -> Expr {
        self.ast.new_expr(ExprKind::Assign { target, value })
    }

    pub fn call(&mut self, callee: Expr, args: &[Expr]) -> Expr {
        let args = self.ast.new_expr_slice(args);
        self.ast.new_expr(ExprKind::Call { callee, args })
    }

    pub fn mthd_call(&mut self, obj: Expr, mthd: PathSegment, args: &[Expr]) -> Expr {
        let args = self.ast.new_expr_slice(args);
        self.ast.new_expr(ExprKind::MthdCall { obj, mthd, args })
    }

    pub fn struct_expr(&mut self, ty_path: Path, fields: Vec<(String, Expr)>) -> Expr {
        self.ast.new_expr(ExprKind::Struct { ty_path, fields })
    }

    pub fn field_access(&mut self, obj: Expr, field: FieldDescriptor) -> Expr {
        self.ast.new_expr(ExprKind::FieldAccess { obj, field })
    }

    pub fn block(&mut self, block: Block) -> Expr {
        self.ast.new_expr(ExprKind::Block(block))
    }

    pub fn if_(&mut self, cond: Expr, then: Block, else_: Option<Block>) -> Expr {
        self.ast.new_expr(ExprKind::If { cond, then, else_ })
    }

    pub fn loop_(&mut self, body: Block) -> Expr {
        self.ast.new_expr(ExprKind::Loop { body })
    }

    pub fn while_(&mut self, cond: Expr, body: Block) -> Expr {
        self.ast.new_expr(ExprKind::While { cond, body })
    }

    pub fn match_(&mut self, scrutinee: Expr, arms: Vec<MatchArm>) -> Expr {
        self.ast.new_expr(ExprKind::Match { scrutinee, arms })
    }

    pub fn deref(&mut self, base: Expr) -> Expr {
        self.ast.new_expr(ExprKind::Deref { base })
    }

    pub fn addr_of(&mut self, base: Expr) -> Expr {
        self.ast.new_expr(ExprKind::AddrOf { base })
    }

    pub fn as_(&mut self, expr: Expr, target_ty: TyAnnot) -> Expr {
        self.ast.new_expr(ExprKind::As { expr, target_ty })
    }

    pub fn self_(&mut self) -> Expr {
        self.ast.new_expr(ExprKind::Self_)
    }

    pub fn closure(&mut self, params: Vec<ClosureParam>, return_ty: Option<TyAnnot>, body: Block) -> Expr {
        self.ast.new_expr(ExprKind::Closure {
            params,
            return_ty,
            body,
        })
    }
}

// Type annotations
impl<'a> AstBuilder<'a> {
    pub fn ty_annot_slice(&mut self, ty_annots: &[TyAnnot]) -> TyAnnotSlice {
        self.ast.new_ty_annot_slice(ty_annots)
    }

    pub fn path_annot(&mut self, path: Path) -> TyAnnot {
        self.ast.new_ty_annot(TyAnnotKind::Path(path))
    }

    pub fn tuple_annot(&mut self, ty_annots: &[TyAnnot]) -> TyAnnot {
        let ty_annots = self.ast.new_ty_annot_slice(ty_annots);
        self.ast.new_ty_annot(TyAnnotKind::Tuple(ty_annots))
    }

    pub fn ref_annot(&mut self, ty_annot: TyAnnot) -> TyAnnot {
        self.ast.new_ty_annot(TyAnnotKind::Ref(ty_annot))
    }

    pub fn ptr_annot(&mut self, ty_annot: TyAnnot) -> TyAnnot {
        self.ast.new_ty_annot(TyAnnotKind::Ptr(ty_annot))
    }

    pub fn fn_annot(&mut self, param_tys: &[TyAnnot], return_ty: Option<TyAnnot>) -> TyAnnot {
        let param_tys = self.ast.new_ty_annot_slice(param_tys);
        self.ast.new_ty_annot(TyAnnotKind::Fn { param_tys, return_ty })
    }

    pub fn wildcard_annot(&mut self) -> TyAnnot {
        self.ast.new_ty_annot(TyAnnotKind::Wildcard)
    }
}
