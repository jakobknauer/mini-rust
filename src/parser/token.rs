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
    AmpersandAmpersand, // &&
    PipePipe,           // ||
    Arrow,              // ->
    Plus,               // +
    Minus,              // -
    Asterisk,           // *
    Slash,              // /
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Let,
    Fn,
    Struct,
    Return,
    Break,
    If,
    Else,
    Loop,
}
