#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub fns: Vec<Fn>,
    pub structs: Vec<Struct>,
    pub enums: Vec<Enum>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            fns: Vec::new(),
            structs: Vec::new(),
            enums: Vec::new(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Fn {
    pub name: String,
    pub gen_params: Vec<String>,
    pub params: Vec<Param>,
    pub return_ty: Option<TyAnnot>,
    pub body: Block,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Param {
    pub name: String,
    pub ty: TyAnnot,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Struct {
    pub name: String,
    pub gen_params: Vec<String>,
    pub fields: Vec<StructField>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructField {
    pub name: String,
    pub ty: TyAnnot,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Enum {
    pub name: String,
    pub gen_params: Vec<String>,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: String,
    pub fields: Vec<StructField>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub return_expr: Option<Box<Expr>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    Let {
        name: String,
        ty_annot: Option<TyAnnot>,
        value: Expr,
    },
    Expr(Expr),
    Return(Option<Expr>),
    Break,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Lit(Lit),
    Ident(String),
    GenQualIdent(GenQualIdent),
    BinaryOp {
        left: Box<Expr>,
        operator: BinaryOperator,
        right: Box<Expr>,
    },
    Assign {
        target: Box<Expr>,
        value: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
    Struct {
        name: String,
        fields: Vec<(String, Expr)>,
    },
    If {
        condition: Box<Expr>,
        then_block: Block,
        else_block: Option<Block>,
    },
    Loop {
        body: Block,
    },
    Block(Block),
    FieldAccess {
        base: Box<Expr>,
        name: String,
    },
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    Deref {
        base: Box<Expr>,
    },
    AddrOf {
        base: Box<Expr>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct GenQualIdent {
    pub ident: String,
    pub gen_args: Vec<TyAnnot>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
}

#[derive(Debug, PartialEq, Eq)]
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
}

#[derive(Debug, PartialEq, Eq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub value: Box<Expr>,
}

type Pattern = StructPattern;

#[derive(Debug, PartialEq, Eq)]
pub struct StructPattern {
    pub variant: String,
    pub fields: Vec<StructPatternField>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructPatternField {
    pub field_name: String,
    pub binding_name: String,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TyAnnot {
    Named(String),
    Generic(GenQualIdent),
    Reference(Box<TyAnnot>),
    Unit,
    Fn {
        param_tys: Vec<TyAnnot>,
        return_ty: Option<Box<TyAnnot>>,
    },
}
