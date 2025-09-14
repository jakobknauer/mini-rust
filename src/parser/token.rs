#[derive(Debug, PartialEq, Clone)]
pub(super) enum Token {
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
    Dot,                // .
    Colon,              // :
    AmpersandAmpersand, // &&
    PipePipe,           // ||
}

#[derive(Debug, PartialEq, Clone)]
pub(super) enum Keyword {
    Let,
    Fn,
    Struct,
    Return,
    Break,
}
