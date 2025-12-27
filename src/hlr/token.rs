#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    NumLiteral(String),
    BoolLiteral(bool),
    CCharLiteral(u8),
    CStringLiteral(Vec<u8>),

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
    ColonColon,         // ::
    Ampersand,          // &
    AmpersandAmpersand, // &&
    Pipe,               // |
    PipePipe,           // ||
    Arrow,              // ->
    BoldArrow,          // =>
    Plus,               // +
    Minus,              // -
    Asterisk,           // *
    Slash,              // /
    Percent,            // %
    Smaller,            // <
    Greater,            // >
    SmallerEqual,       // <=
    GreaterEqual,       // >=
    Dots,               // ...
    Underscore,         // _
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    As,
    Break,
    CallableTrait,
    Else,
    Enum,
    Fn,
    For,
    If,
    Impl,
    Let,
    Loop,
    Match,
    Return,
    Self_,
    Struct,
    Trait,
    Where,
}
