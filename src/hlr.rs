mod expr;
mod stmt;
mod ty_annot;

use std::cell::Cell;

use crate::ctxt::fns;

pub use expr::*;
pub use stmt::*;
pub use ty_annot::*;

pub struct Hlr<'hlr> {
    arena: &'hlr bumpalo::Bump,

    next_var_id: Cell<VarId>,
    next_expr_id: Cell<ExprId>,
}

impl<'hlr> Hlr<'hlr> {
    pub fn new(arena: &'hlr bumpalo::Bump) -> Self {
        Self {
            arena,
            next_var_id: Cell::new(VarId(0)),
            next_expr_id: Cell::new(ExprId(0)),
        }
    }
}

pub struct Fn<'hlr> {
    pub fn_: fns::Fn<'hlr>,
    pub body: Expr<'hlr>,
    pub param_var_ids: Vec<VarId>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Default)]
pub struct VarId(usize);

impl std::fmt::Display for VarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "v{}", self.0)
    }
}

impl<'hlr> Hlr<'hlr> {
    pub fn var_id(&self) -> VarId {
        let id = self.next_var_id.get();
        self.next_var_id.set(VarId(id.0 + 1));
        id
    }

    pub fn expr(&self, expr: ExprDef<'hlr>) -> Expr<'hlr> {
        Expr(self.arena.alloc(expr), {
            let id = self.next_expr_id.get();
            self.next_expr_id.set(ExprId(id.0 + 1));
            id
        })
    }

    pub fn stmt(&self, stmt: StmtDef<'hlr>) -> Stmt<'hlr> {
        self.arena.alloc(stmt)
    }

    pub fn ty_annot(&self, annot: TyAnnotDef<'hlr>) -> TyAnnot<'hlr> {
        self.arena.alloc(annot)
    }

    pub fn expr_slice(&self, exprs: &[Expr<'hlr>]) -> ExprSlice<'hlr> {
        self.arena.alloc_slice_copy(exprs)
    }

    pub fn stmt_slice(&self, stmts: &[Stmt<'hlr>]) -> StmtSlice<'hlr> {
        self.arena.alloc_slice_copy(stmts)
    }

    pub fn ty_annot_slice(&self, ty_annots: &[TyAnnot<'hlr>]) -> TyAnnotSlice<'hlr> {
        self.arena.alloc_slice_copy(ty_annots)
    }

    pub fn struct_expr_field_slice(
        &'hlr self,
        fields: impl IntoIterator<Item = (FieldSpec, Expr<'hlr>), IntoIter: ExactSizeIterator>,
    ) -> StructFields<'hlr> {
        self.arena.alloc_slice_fill_iter(fields)
    }

    pub fn closure_params(
        &'hlr self,
        params: impl IntoIterator<Item = ClosureParam<'hlr>, IntoIter: ExactSizeIterator>,
    ) -> ClosureParams<'hlr> {
        self.arena.alloc_slice_fill_iter(params)
    }

    pub fn match_arms(
        &'hlr self,
        arms: impl IntoIterator<Item = MatchArm<'hlr>, IntoIter: ExactSizeIterator>,
    ) -> &'hlr [MatchArm<'hlr>] {
        self.arena.alloc_slice_fill_iter(arms)
    }

    pub fn variant_pattern_fields(
        &'hlr self,
        fields: impl IntoIterator<Item = VariantPatternField, IntoIter: ExactSizeIterator>,
    ) -> &'hlr [VariantPatternField] {
        self.arena.alloc_slice_fill_iter(fields)
    }

    pub fn pattern(&'hlr self, pattern: PatternKind<'hlr>) -> Pattern<'hlr> {
        self.arena.alloc(pattern)
    }
}
