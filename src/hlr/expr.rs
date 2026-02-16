use crate::{
    ast,
    ctxt::{fns, traits, ty},
    hlr::{StmtSlice, TyAnnot, TyAnnotSlice, VarId},
};

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Default)]
pub struct ExprId(pub(super) usize);

#[derive(Debug, Clone, Copy)]
pub struct Expr<'hlr>(pub &'hlr ExprDef<'hlr>, pub ExprId);

pub type ExprSlice<'hlr> = &'hlr [Expr<'hlr>];

#[derive(Debug)]
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

#[derive(Debug, Clone)]
pub enum Val<'hlr> {
    Var(VarId),
    Fn(fns::Fn, Option<TyAnnotSlice<'hlr>>),
    Struct(ty::Struct, Option<TyAnnotSlice<'hlr>>),
    Variant(ty::Enum, usize, Option<TyAnnotSlice<'hlr>>),
    Mthd(TyAnnot<'hlr>, String, Option<TyAnnotSlice<'hlr>>),
}

#[derive(Debug)]
pub enum Lit {
    Int(i64),
    Bool(bool),
    CChar(u8),
    CString(Vec<u8>),
}

#[derive(Clone, Debug)]
pub enum FieldSpec {
    Name(String),
    Index(usize),
}

pub type StructFields<'hlr> = &'hlr [(FieldSpec, Expr<'hlr>)];

#[derive(Clone, Debug)]
pub struct ClosureParam<'hlr>(pub VarId, pub Option<TyAnnot<'hlr>>);

pub type ClosureParams<'hlr> = &'hlr [ClosureParam<'hlr>];

#[derive(Clone, Debug)]
pub struct MatchArm<'hlr> {
    pub pattern: Pattern<'hlr>,
    pub body: Expr<'hlr>,
}

pub type Pattern<'hlr> = VariantPattern<'hlr>;

#[derive(Clone, Debug)]
pub struct VariantPattern<'hlr> {
    pub variant: Val<'hlr>,
    pub fields: &'hlr [VariantPatternField],
}

#[derive(Clone, Debug)]
pub struct VariantPatternField {
    pub field_index: usize,
    pub binding: VarId,
}
