mod expr;
mod stmt;
mod ty_annot;

use std::{cell::RefCell, marker::PhantomData};

use crate::ctxt::fns;

pub use expr::*;
pub use stmt::*;
pub use ty_annot::*;

#[derive(Default)]
pub struct Hlr<'hlr> {
    arena: bumpalo::Bump,
    _marker: PhantomData<&'hlr ()>,

    next_var_id: RefCell<VarId>,
    next_expr_id: RefCell<ExprId>,
}

pub struct Fn<'hlr> {
    pub fn_: fns::Fn,
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
        self.next_var_id.replace_with(|VarId(id)| VarId(*id + 1))
    }

    pub fn expr(&'hlr self, expr: ExprDef<'hlr>) -> Expr<'hlr> {
        Expr(
            self.arena.alloc(expr),
            self.next_expr_id.replace_with(|ExprId(id)| ExprId(*id + 1)),
        )
    }

    pub fn stmt(&'hlr self, stmt: StmtDef<'hlr>) -> Stmt<'hlr> {
        self.arena.alloc(stmt)
    }

    pub fn ty_annot(&'hlr self, annot: TyAnnotDef<'hlr>) -> TyAnnot<'hlr> {
        self.arena.alloc(annot)
    }

    pub fn expr_slice(&'hlr self, exprs: &[Expr<'hlr>]) -> ExprSlice<'hlr> {
        self.arena.alloc_slice_copy(exprs)
    }

    pub fn stmt_slice(&'hlr self, stmts: &[Stmt<'hlr>]) -> StmtSlice<'hlr> {
        self.arena.alloc_slice_copy(stmts)
    }

    pub fn ty_annot_slice(&'hlr self, ty_annots: &[TyAnnot<'hlr>]) -> TyAnnotSlice<'hlr> {
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
}
