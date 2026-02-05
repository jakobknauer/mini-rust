#![allow(unused)]

use crate::{
    ast,
    ctxt::{
        fns::Fn,
        ty::{Enum, GenVar, Struct, Ty},
    },
};

pub struct Hlr {
    exprs: Vec<ExprDef>,
    stmts: Vec<StmtDef>,
    ty_annots: Vec<TyAnnotDef>,
}

impl Hlr {
    pub fn new() -> Self {
        Self {
            exprs: Vec::new(),
            stmts: Vec::new(),
            ty_annots: Vec::new(),
        }
    }

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
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Def {
    Var(VarId),
    Fn(Fn),
    Struct(Struct),
    Enum(Enum),
    Variant(Enum, usize),
    Mthd(TyAnnot, usize),
    TraitMthd(TyAnnot, TyAnnot, String),
    GenVar(GenVar),
    Ty(Ty),
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
    Def(Def), // variable or function reference
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
    MethodCall {
        // not yet resolved
        receiver: Expr,
        method_name: String,
        args: Vec<Expr>,
    },
    /// Struct literal or enum variant constructor
    Construct {
        def: Def, // StructId or VariantId
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
    Block {
        stmts: Vec<Stmt>,
        trailing: Expr,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TyAnnot(usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TyAnnotDef {
    Struct(Struct, Option<Vec<TyAnnot>>),
    Enum(Enum, Option<Vec<TyAnnot>>),
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
        params: Vec<TyAnnot>,
        ret: Option<TyAnnot>,
    },
    Tuple(Vec<TyAnnot>),
    Infer,
    Self_,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Path {
    pub segments: Vec<PathSegment>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PathSegment {
    Resolved(Def),
    Ident(String),
    Generic(GenPathSegment),
    Self_,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GenPathSegment {
    pub ident: String,
    pub gen_args: Vec<TyAnnot>,
}
