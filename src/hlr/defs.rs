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
    pub parameters: Vec<Parameter>,
    pub return_type: Option<String>,
    pub body: Block,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Parameter {
    pub name: String,
    pub param_type: String,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructField {
    pub name: String,
    pub field_type: String,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Enum {
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: String,
    pub fields: Vec<StructField>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub return_expression: Option<Box<Expression>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Let {
        name: String,
        var_type: Option<String>,
        value: Expression,
    },
    Expression(Expression),
    Return(Option<Expression>),
    Break,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Literal(Literal),
    Variable(String),
    BinaryOp {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    Assignment {
        target: Box<Expression>,
        value: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
    StructExpr {
        struct_name: String,
        fields: Vec<(String, Expression)>,
    },
    If {
        condition: Box<Expression>,
        then_block: Block,
        else_block: Option<Block>,
    },
    Loop {
        body: Block,
    },
    Block(Block),
    FieldAccess {
        base: Box<Expression>,
        field_name: String,
    },
    Match {
        scrutinee: Box<Expression>,
        arms: Vec<MatchArm>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Literal {
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
    pub value: Box<Expression>,
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
