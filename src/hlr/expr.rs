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
        operator: BinaryOperator,
    },

    UnaryOp {
        operand: Expr<'hlr>,
        operator: UnaryOperator,
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
    AddrOfMut(Expr<'hlr>),

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
        trait_: Option<traits::Trait<'hlr>>,
        trait_args: Option<TyAnnotSlice<'hlr>>,
        mthd_name: String,
        args: Option<TyAnnotSlice<'hlr>>,
    },
}

#[derive(Debug)]
pub enum Val<'hlr> {
    Var(VarId),
    Fn(fns::Fn<'hlr>, Option<TyAnnotSlice<'hlr>>),
    Struct(ty::Struct<'hlr>, Option<TyAnnotSlice<'hlr>>),
    Variant(ty::Enum<'hlr>, usize, Option<TyAnnotSlice<'hlr>>),
    Mthd(TyAnnot<'hlr>, String, Option<TyAnnotSlice<'hlr>>),
}

#[derive(Debug)]
pub enum Lit {
    Int(i64),
    Bool(bool),
    CChar(u8),
    CString(Vec<u8>),
}

#[derive(Debug)]
pub enum FieldSpec {
    Name(String),
    Index(usize),
}

pub type StructFields<'hlr> = &'hlr [(FieldSpec, Expr<'hlr>)];

#[derive(Debug)]
pub struct ClosureParam<'hlr>(pub VarId, pub Option<TyAnnot<'hlr>>);

pub type ClosureParams<'hlr> = &'hlr [ClosureParam<'hlr>];

#[derive(Debug)]
pub struct MatchArm<'hlr> {
    pub pattern: Pattern<'hlr>,
    pub body: Expr<'hlr>,
}

pub type Pattern<'hlr> = &'hlr PatternKind<'hlr>;

#[derive(Debug)]
pub enum PatternKind<'hlr> {
    Ref(Pattern<'hlr>),
    RefMut(Pattern<'hlr>),
    Variant(VariantPattern<'hlr>),
    Struct(StructPattern<'hlr>),
    Tuple(&'hlr [Pattern<'hlr>]),
    Lit(Lit),
    Identifier { var_id: VarId, mutable: bool },
    Wildcard,
}

#[derive(Debug)]
pub struct VariantPattern<'hlr> {
    pub variant: Val<'hlr>,
    pub fields: &'hlr [PatternField<'hlr>],
}

#[derive(Debug)]
pub struct StructPattern<'hlr> {
    pub constructor: Val<'hlr>,
    pub fields: &'hlr [PatternField<'hlr>],
}

#[derive(Debug)]
pub struct PatternField<'hlr> {
    pub field_index: usize,
    pub pattern: Pattern<'hlr>,
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Remainder,
    BitOr,
    BitAnd,
    LogicalAnd,
    LogicalOr,
}

impl From<ast::BinaryOperator> for BinaryOperator {
    fn from(op: ast::BinaryOperator) -> Self {
        match op {
            ast::BinaryOperator::Add => Self::Add,
            ast::BinaryOperator::Subtract => Self::Subtract,
            ast::BinaryOperator::Multiply => Self::Multiply,
            ast::BinaryOperator::Divide => Self::Divide,
            ast::BinaryOperator::Equal => Self::Equal,
            ast::BinaryOperator::NotEqual => Self::NotEqual,
            ast::BinaryOperator::LessThan => Self::LessThan,
            ast::BinaryOperator::GreaterThan => Self::GreaterThan,
            ast::BinaryOperator::LessThanOrEqual => Self::LessThanOrEqual,
            ast::BinaryOperator::GreaterThanOrEqual => Self::GreaterThanOrEqual,
            ast::BinaryOperator::Remainder => Self::Remainder,
            ast::BinaryOperator::BitOr => Self::BitOr,
            ast::BinaryOperator::BitAnd => Self::BitAnd,
            ast::BinaryOperator::LogicalAnd => Self::LogicalAnd,
            ast::BinaryOperator::LogicalOr => Self::LogicalOr,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOperator {
    Not,
    Negative,
}

impl From<ast::UnaryOperator> for UnaryOperator {
    fn from(op: ast::UnaryOperator) -> Self {
        match op {
            ast::UnaryOperator::Not => Self::Not,
            ast::UnaryOperator::Negative => Self::Negative,
        }
    }
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Add => "+",
            Self::Subtract => "-",
            Self::Multiply => "*",
            Self::Divide => "/",
            Self::Remainder => "%",
            Self::Equal => "==",
            Self::NotEqual => "!=",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::LessThanOrEqual => "<=",
            Self::GreaterThanOrEqual => ">=",
            Self::BitOr => "|",
            Self::BitAnd => "&",
            Self::LogicalAnd => "&&",
            Self::LogicalOr => "||",
        };
        write!(f, "{}", s)
    }
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Not => "!",
            Self::Negative => "-",
        };
        write!(f, "{}", s)
    }
}
