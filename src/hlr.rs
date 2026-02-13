#![allow(unused)]

use crate::{
    ast,
    ctxt::{
        fns::Fn,
        traits,
        ty::{Enum, GenVar, Struct, Ty},
    },
};

#[derive(Default)]
pub struct Hlr {
    exprs: Vec<ExprDef>,
    stmts: Vec<StmtDef>,

    ty_annots: Vec<TyAnnotDef>,
    ty_annot_slices: Vec<TyAnnot>,
}

impl Hlr {
    pub fn new_expr(&mut self, expr: ExprDef) -> Expr {
        self.exprs.push(expr);
        Expr(self.exprs.len() - 1)
    }

    pub fn expr(&self, expr: Expr) -> &ExprDef {
        &self.exprs[expr.0]
    }

    pub fn new_stmt(&mut self, stmt: StmtDef) -> Stmt {
        self.stmts.push(stmt);
        Stmt(self.stmts.len() - 1)
    }

    pub fn stmt(&self, stmt: Stmt) -> &StmtDef {
        &self.stmts[stmt.0]
    }

    pub fn new_ty_annot(&mut self, annot: TyAnnotDef) -> TyAnnot {
        self.ty_annots.push(annot);
        TyAnnot(self.ty_annots.len() - 1)
    }

    pub fn ty_annot(&self, ty_annot: TyAnnot) -> &TyAnnotDef {
        &self.ty_annots[ty_annot.0]
    }

    pub fn new_ty_annot_slice(&mut self, ty_annots: &[TyAnnot]) -> TyAnnotSlice {
        self.ty_annot_slices.extend_from_slice(ty_annots);
        TyAnnotSlice(self.ty_annot_slices.len() - ty_annots.len(), ty_annots.len())
    }

    pub fn ty_annot_slice(&self, TyAnnotSlice(start, len): TyAnnotSlice) -> &[TyAnnot] {
        &self.ty_annot_slices[start..start + len]
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Val {
    Var(VarId),
    Fn(Fn, Option<TyAnnotSlice>),
    Struct(Struct, Option<TyAnnotSlice>),
    Variant(Enum, usize, Option<TyAnnotSlice>),
    Mthd(TyAnnot, String, Option<TyAnnotSlice>),
    TraitMthd(TyAnnot, TyAnnot, String),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct VarId(pub usize);

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Expr(usize);

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Stmt(usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprDef {
    Lit(Lit),
    Val(Val),
    BinaryOp {
        left: Expr,
        right: Expr,
        operator: ast::BinaryOperator,
    },
    UnaryOp {
        operand: Expr,
        operator: ast::UnaryOperator,
    },
    Call {
        callee: Expr,
        args: Vec<Expr>,
    },
    MthdCall {
        receiver: Expr,
        mthd_name: String,
        gen_args: Option<TyAnnotSlice>,
        args: Vec<Expr>,
    },
    Struct {
        constructor: Val,
        fields: Vec<(FieldSpec, Expr)>,
    },
    FieldAccess {
        base: Expr,
        field: FieldSpec,
    },
    Tuple(Vec<Expr>),
    Assign {
        target: Expr,
        value: Expr,
    },
    Deref(Expr),
    AddrOf(Expr),
    As {
        expr: Expr,
        ty: TyAnnot,
    },
    Closure {
        params: Vec<(VarId, Option<TyAnnot>)>,
        body: Expr,
    },
    If {
        cond: Expr,
        then: Expr,
        else_: Option<Expr>,
    },
    Loop {
        body: Expr,
    },
    Match {
        scrutinee: Expr,
        arms: Vec<MatchArm>,
    },
    Block {
        stmts: Vec<Stmt>,
        trailing: Expr,
    },
    QualifiedMthd {
        ty: TyAnnot,
        trait_: Option<traits::Trait>,
        trait_args: Option<TyAnnotSlice>,
        mthd_name: String,
        args: Option<TyAnnotSlice>,
    },
}

pub enum StmtDef {
    Expr(Expr),
    Let {
        var: VarId,
        ty: Option<TyAnnot>,
        init: Expr,
    },
    Break,
    Return(Option<Expr>),
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
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
}

pub type Pattern = VariantPattern;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariantPattern {
    pub variant: Val,
    pub fields: Vec<VariantPatternField>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariantPatternField {
    pub field_index: usize,
    pub binding: VarId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TyAnnot(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TyAnnotSlice(usize, usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TyAnnotDef {
    Struct(Struct, Option<TyAnnotSlice>),
    Enum(Enum, Option<TyAnnotSlice>),
    Ty(Ty),
    GenVar(GenVar),
    AssocTy {
        base: TyAnnot,
        trait_: Option<TyAnnot>,
        name: String,
    },
    Ref(TyAnnot),
    Ptr(TyAnnot),
    Fn {
        params: TyAnnotSlice,
        ret: Option<TyAnnot>,
    },
    Tuple(TyAnnotSlice),
    Infer,
    Self_,
}
