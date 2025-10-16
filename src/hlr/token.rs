#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    NumLiteral(String),
    BoolLiteral(bool),

    LBrace,             // {
    RBrace,             // }
    LParen,             // (
    RParen,             // )
    Semicolon,          // ;
    Comma,              // ,
    Equal,              // =
    EqualEqual,         // ==
    BangEqual,          // !=
    Dot,                // .
    Colon,              // :
    Ampersand,          // &
    AmpersandAmpersand, // &&
    Pipe,               // |
    PipePipe,           // ||
    Arrow,              // ->
    Plus,               // +
    Minus,              // -
    Asterisk,           // *
    Slash,              // /
    Percent,            // %
    Smaller,            // <
    Greater,            // >
    SmallerEqual,       // <=
    GreaterEqual,       // >=
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Break,
    Else,
    Enum,
    Fn,
    If,
    Let,
    Loop,
    Return,
    Struct,
}
