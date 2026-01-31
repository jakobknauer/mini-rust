use crate::ctxt::{
    fns::Fn,
    ty::{Enum, Struct},
};

pub struct Hlr {
    exprs: Vec<ExprDef>,
    stmts: Vec<StmtDef>,
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
}

type VarId = usize;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Def {
    Var(VarId),
    Fn(Fn),
    Struct(Struct),
    Variant(Enum, usize),
    Mthd(TyAnnot, String),
    TraitMthd(TyAnnot, TyAnnot, String),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Expr(usize);

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Stmt(usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprDef {
    Lit(Lit),
    Def(Def), // variable or function reference
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
    Int(i32),
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
pub enum TyAnnot {}
