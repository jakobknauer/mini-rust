#![allow(unused)]

mod expr;
mod stmt;
mod ty_annot;

use std::{cell::RefCell, marker::PhantomData};

use bumpalo::Bump;

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

pub struct FnHlr<'hlr> {
    pub fn_: fns::Fn,
    pub body: Expr<'hlr>,
    pub param_var_ids: Vec<VarId>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Default)]
pub struct VarId(usize);

impl<'hlr> Hlr<'hlr> {
    pub fn new() -> Self {
        Self {
            arena: Bump::new(),
            _marker: PhantomData,

            next_var_id: RefCell::new(VarId(0)),
            next_expr_id: RefCell::new(ExprId(0)),
        }
    }

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

    pub fn struct_expr_field_slice(&'hlr self, fields: &[(FieldSpec, Expr<'hlr>)]) -> StructFields<'hlr> {
        self.arena.alloc_slice_clone(fields)
    }

    pub fn closure_params(&'hlr self, params: &[ClosureParam<'hlr>]) -> ClosureParams<'hlr> {
        self.arena.alloc_slice_clone(params)
    }

    pub fn match_arms(&'hlr self, arms: &[MatchArm<'hlr>]) -> &'hlr [MatchArm<'hlr>] {
        self.arena.alloc_slice_clone(arms)
    }

    pub fn variant_pattern_fields(&'hlr self, fields: &[VariantPatternField]) -> &'hlr [VariantPatternField] {
        self.arena.alloc_slice_clone(fields)
    }
}
