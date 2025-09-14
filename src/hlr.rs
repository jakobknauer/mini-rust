pub struct Program {
    pub functions: Vec<Function>,
    pub structs: Vec<Struct>,
}

pub struct Function {
    pub name: String,
    pub return_type: Option<String>,
    pub parameters: Vec<Parameter>,
    pub body: Block,
}

pub struct Parameter {
    pub name: String,
    pub param_type: String,
}

pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>,
}

pub struct StructField {
    pub name: String,
    pub field_type: String,
}

pub struct Block {
    pub statements: Vec<Statement>,
    pub return_expression: Option<Box<Expression>>,
}

pub enum Statement {
    Let {
        name: String,
        var_type: Option<String>,
        value: Expression,
    },
    Expression(Expression),
    Return(Expression),
    Break,
}

pub enum Expression {
    Literal(Literal),
    Variable(String),
    BinaryOp {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    Assignment {
        target: String,
        value: Box<Expression>,
    },
    FunctionCall {
        name: String,
        arguments: Vec<Expression>,
    },
    StructInit {
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
}

pub enum Literal {
    Int(i64),
    Bool(bool),
}

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
}
