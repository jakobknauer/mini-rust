#![allow(unused)]

use std::marker::PhantomData;

use bumpalo::Bump;

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
    arena: bumpalo::Bump,
    _marker: PhantomData<&'hlr ()>,
}

impl<'hlr> Hlr<'hlr> {
    pub fn new() -> Self {
        Self {
            arena: Bump::new(),
            _marker: PhantomData,
        }
    }

    pub fn expr(&'hlr self, expr: ExprDef<'hlr>) -> Expr<'hlr> {
        self.arena.alloc(expr)
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
        self.arena.alloc_slice_copy(params)
    }

    pub fn match_arms(&'hlr self, arms: &[MatchArm<'hlr>]) -> &'hlr [MatchArm<'hlr>] {
        self.arena.alloc_slice_clone(arms)
    }

    pub fn variant_pattern_fields(
        &'hlr self,
        fields: &[VariantPatternField<'hlr>],
    ) -> &'hlr [VariantPatternField<'hlr>] {
        self.arena.alloc_slice_clone(fields)
    }
}

pub type Expr<'hlr> = &'hlr ExprDef<'hlr>;
pub type Stmt<'hlr> = &'hlr StmtDef<'hlr>;
pub type TyAnnot<'hlr> = &'hlr TyAnnotDef<'hlr>;

pub type ExprSlice<'hlr> = &'hlr [Expr<'hlr>];
pub type StmtSlice<'hlr> = &'hlr [Stmt<'hlr>];
pub type TyAnnotSlice<'hlr> = &'hlr [TyAnnot<'hlr>];

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Val<'hlr> {
    Var(VarId<'hlr>),
    Fn(Fn, Option<TyAnnotSlice<'hlr>>),
    Struct(Struct, Option<TyAnnotSlice<'hlr>>),
    Variant(Enum, usize, Option<TyAnnotSlice<'hlr>>),
    Mthd(TyAnnot<'hlr>, String, Option<TyAnnotSlice<'hlr>>),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct VarId<'hlr>(pub usize, PhantomData<&'hlr ()>);

impl<'hlr> VarId<'hlr> {
    pub fn new() -> Self {
        VarId(0, PhantomData)
    }

    pub fn get_and_increment(&mut self) -> VarId<'hlr> {
        let id = self.0;
        self.0 += 1;
        VarId(id, PhantomData)
    }
}

#[derive(Clone, Debug)]
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
        args: ExprSlice<'hlr>,
    },

    MthdCall {
        receiver: Expr<'hlr>,
        mthd_name: String,
        gen_args: Option<TyAnnotSlice<'hlr>>,
        args: ExprSlice<'hlr>,
    },

    Struct {
        constructor: Val<'hlr>,
        fields: StructFields<'hlr>,
    },

    FieldAccess {
        base: Expr<'hlr>,
        field: FieldSpec,
    },

    Tuple(ExprSlice<'hlr>),

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
        params: ClosureParams<'hlr>,
        return_ty: Option<TyAnnot<'hlr>>,
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
        arms: &'hlr [MatchArm<'hlr>],
    },

    Block {
        stmts: StmtSlice<'hlr>,
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

#[derive(Clone, Debug)]
pub enum StmtDef<'hlr> {
    Expr(Expr<'hlr>),

    Let {
        var: VarId<'hlr>,
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

pub type StructFields<'hlr> = &'hlr [(FieldSpec, Expr<'hlr>)];

#[derive(Clone, Copy, Debug)]
pub struct ClosureParam<'hlr>(pub VarId<'hlr>, pub Option<TyAnnot<'hlr>>);

pub type ClosureParams<'hlr> = &'hlr [ClosureParam<'hlr>];

#[derive(Clone, Debug)]
pub struct MatchArm<'hlr> {
    pub pattern: Pattern<'hlr>,
    pub body: Expr<'hlr>,
}

pub type Pattern<'hlr> = VariantPattern<'hlr>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariantPattern<'hlr> {
    pub variant: Val<'hlr>,
    pub fields: &'hlr [VariantPatternField<'hlr>],
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariantPatternField<'hlr> {
    pub field_index: usize,
    pub binding: VarId<'hlr>,
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
