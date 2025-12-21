#[derive(Debug, PartialEq, Eq, Default)]
pub struct Program {
    pub fns: Vec<Fn>,
    pub structs: Vec<Struct>,
    pub enums: Vec<Enum>,
    pub impls: Vec<Impl>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Fn {
    pub name: String,
    pub gen_params: Vec<String>,
    pub params: Vec<Param>,
    pub var_args: bool,
    pub return_ty: Option<TyAnnot>,
    pub body: Option<Block>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Param {
    pub name: String,
    pub ty: TyAnnot,
    pub is_receiver: bool,
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
pub struct Impl {
    pub gen_params: Vec<String>,
    pub ty: TyAnnot,
    pub methods: Vec<Fn>,
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
    Ident(Ident),
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
    MethodCall {
        obj: Box<Expr>,
        method: Ident,
        arguments: Vec<Expr>,
    },
    Struct {
        name: Ident,
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
        obj: Box<Expr>,
        field: Ident,
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
    As {
        expr: Box<Expr>,
        target_ty: TyAnnot,
    },
    Self_,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Ident {
    pub ident: String,
    pub gen_args: Vec<TyAnnot>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
    CChar(u8),
    CString(Vec<u8>),
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
    Generic(Ident),
    Ref(Box<TyAnnot>),
    Ptr(Box<TyAnnot>),
    Unit,
    Fn {
        param_tys: Vec<TyAnnot>,
        return_ty: Option<Box<TyAnnot>>,
    },
    Self_,
}
