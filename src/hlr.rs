#![allow(unused)]

use std::marker::PhantomData;

use crate::{
    ast::{self},
    ctxt::{
        fns::Fn,
        traits,
        ty::{Enum, GenVar, Struct, Ty},
    },
};

#[derive(Default)]
pub struct Hlr<'hlr> {
    exprs: Vec<ExprDef<'hlr>>,
    stmts: Vec<StmtDef<'hlr>>,

    ty_annots: Vec<TyAnnotDef<'hlr>>,
    ty_annot_slices: Vec<TyAnnot<'hlr>>,
}

impl<'hlr> Hlr<'hlr> {
    pub fn new_expr(&mut self, expr: ExprDef<'hlr>) -> Expr<'hlr> {
        self.exprs.push(expr);
        Expr(self.exprs.len() - 1, PhantomData)
    }

    pub fn expr(&self, expr: Expr<'hlr>) -> &ExprDef<'hlr> {
        &self.exprs[expr.0]
    }

    pub fn new_stmt(&mut self, stmt: StmtDef<'hlr>) -> Stmt<'hlr> {
        self.stmts.push(stmt);
        Stmt(self.stmts.len() - 1, PhantomData)
    }

    pub fn stmt(&self, stmt: Stmt<'hlr>) -> &StmtDef<'hlr> {
        &self.stmts[stmt.0]
    }

    pub fn new_ty_annot(&mut self, annot: TyAnnotDef<'hlr>) -> TyAnnot<'hlr> {
        self.ty_annots.push(annot);
        TyAnnot(self.ty_annots.len() - 1, PhantomData)
    }

    pub fn ty_annot(&self, ty_annot: TyAnnot<'hlr>) -> &TyAnnotDef<'hlr> {
        &self.ty_annots[ty_annot.0]
    }

    pub fn new_ty_annot_slice(&mut self, ty_annots: &[TyAnnot<'hlr>]) -> TyAnnotSlice<'hlr> {
        self.ty_annot_slices.extend_from_slice(ty_annots);
        TyAnnotSlice(
            self.ty_annot_slices.len() - ty_annots.len(),
            ty_annots.len(),
            PhantomData,
        )
    }

    pub fn ty_annot_slice(&self, TyAnnotSlice(start, len, _): TyAnnotSlice<'hlr>) -> &[TyAnnot<'hlr>] {
        &self.ty_annot_slices[start..start + len]
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Expr<'hlr>(usize, PhantomData<&'hlr Hlr<'hlr>>);

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Stmt<'hlr>(usize, PhantomData<&'hlr Hlr<'hlr>>);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TyAnnot<'hlr>(usize, PhantomData<&'hlr Hlr<'hlr>>);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TyAnnotSlice<'hlr>(usize, usize, PhantomData<&'hlr Hlr<'hlr>>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Val<'hlr> {
    Var(VarId),
    Fn(Fn, Option<TyAnnotSlice<'hlr>>),
    Struct(Struct, Option<TyAnnotSlice<'hlr>>),
    Variant(Enum, usize, Option<TyAnnotSlice<'hlr>>),
    Mthd(TyAnnot<'hlr>, String, Option<TyAnnotSlice<'hlr>>),
    TraitMthd(TyAnnot<'hlr>, TyAnnot<'hlr>, String),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct VarId(pub usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprDef<'hlr> {
    Lit(Lit),
    Val(Val<'hlr>),

    BinaryOp {
        left: Expr<'hlr>,
        right: Expr<'hlr>,
        operator: ast::BinaryOperator,
    },

    UnaryOp {
        operand: Expr<'hlr>,
        operator: ast::UnaryOperator,
    },

    Call {
        callee: Expr<'hlr>,
        args: Vec<Expr<'hlr>>,
    },

    MthdCall {
        receiver: Expr<'hlr>,
        mthd_name: String,
        gen_args: Option<TyAnnotSlice<'hlr>>,
        args: Vec<Expr<'hlr>>,
    },

    Struct {
        constructor: Val<'hlr>,
        fields: Vec<(FieldSpec, Expr<'hlr>)>,
    },

    FieldAccess {
        base: Expr<'hlr>,
        field: FieldSpec,
    },

    Tuple(Vec<Expr<'hlr>>),

    Assign {
        target: Expr<'hlr>,
        value: Expr<'hlr>,
    },

    Deref(Expr<'hlr>),
    AddrOf(Expr<'hlr>),

    As {
        expr: Expr<'hlr>,
        ty: TyAnnot<'hlr>,
    },

    Closure {
        params: Vec<(VarId, Option<TyAnnot<'hlr>>)>,
        body: Expr<'hlr>,
    },

    If {
        cond: Expr<'hlr>,
        then: Expr<'hlr>,
        else_: Option<Expr<'hlr>>,
    },

    Loop {
        body: Expr<'hlr>,
    },

    Match {
        scrutinee: Expr<'hlr>,
        arms: Vec<MatchArm<'hlr>>,
    },

    Block {
        stmts: Vec<Stmt<'hlr>>,
        trailing: Expr<'hlr>,
    },

    QualifiedMthd {
        ty: TyAnnot<'hlr>,
        trait_: Option<traits::Trait>,
        trait_args: Option<TyAnnotSlice<'hlr>>,
        mthd_name: String,
        args: Option<TyAnnotSlice<'hlr>>,
    },
}

pub enum StmtDef<'hlr> {
    Expr(Expr<'hlr>),

    Let {
        var: VarId,
        ty: Option<TyAnnot<'hlr>>,
        init: Expr<'hlr>,
    },

    Break,
    Return(Option<Expr<'hlr>>),
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
    CChar(u8),
    CString(Vec<u8>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FieldSpec {
    Name(String),
    Index(usize),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MatchArm<'hlr> {
    pub pattern: Pattern<'hlr>,
    pub body: Expr<'hlr>,
}

pub type Pattern<'hlr> = VariantPattern<'hlr>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariantPattern<'hlr> {
    pub variant: Val<'hlr>,
    pub fields: Vec<VariantPatternField>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariantPatternField {
    pub field_index: usize,
    pub binding: VarId,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TyAnnotDef<'hlr> {
    Struct(Struct, Option<TyAnnotSlice<'hlr>>),
    Enum(Enum, Option<TyAnnotSlice<'hlr>>),
    Ty(Ty),
    GenVar(GenVar),

    AssocTy {
        base: TyAnnot<'hlr>,
        trait_: Option<TyAnnot<'hlr>>,
        name: String,
    },

    Ref(TyAnnot<'hlr>),
    Ptr(TyAnnot<'hlr>),

    Fn {
        params: TyAnnotSlice<'hlr>,
        ret: Option<TyAnnot<'hlr>>,
    },

    Tuple(TyAnnotSlice<'hlr>),

    Infer,
    Self_,
}
