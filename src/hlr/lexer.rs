use crate::hlr::token::{Keyword, Token};

pub fn get_tokens(input: &str) -> Result<Vec<Token>, LexerError> {
    Lexer::new(input).collect()
}

#[derive(Debug, Clone)]
pub struct LexerError {
    pub position: usize,
}

struct Lexer<'a> {
    input: &'a str,
    position: usize,
}

const ONE_CHAR_TOKENS: &[(char, Token)] = &[
    ('{', Token::LBrace),
    ('}', Token::RBrace),
    ('(', Token::LParen),
    (')', Token::RParen),
    (';', Token::Semicolon),
    (',', Token::Comma),
    ('=', Token::Equal),
    ('.', Token::Dot),
    (':', Token::Colon),
    ('+', Token::Plus),
    ('-', Token::Minus),
    ('*', Token::Asterisk),
    ('/', Token::Slash),
];

const TWO_CHAR_TOKENS: &[(char, char, Token)] = &[
    ('=', '=', Token::EqualEqual),
    ('!', '=', Token::BangEqual),
    ('&', '&', Token::AmpersandAmpersand),
    ('|', '|', Token::PipePipe),
    ('-', '>', Token::Arrow),
];

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Lexer { input, position: 0 }
    }

    fn get_current_char(&self) -> Option<char> {
        self.input.chars().nth(self.position)
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.get_current_char()
            && c.is_whitespace()
        {
            self.position += 1;
        }
    }

    fn try_parse_identifier_or_keyword(&mut self) -> Option<Token> {
        if !(self.get_current_char()?.is_alphabetic() || self.get_current_char()? == '_') {
            return None;
        }

        let start = self.position;
        while let Some(next) = self.get_current_char()
            && (next.is_alphanumeric() || next == '_')
        {
            self.position += 1;
        }

        let identifier = &self
            .input
            .chars()
            .skip(start)
            .take(self.position - start)
            .collect::<String>();
        let token = try_into_keyword_token(identifier).unwrap_or_else(|| Token::Identifier(identifier.to_string()));

        Some(token)
    }

    fn try_parse_number(&mut self) -> Option<Token> {
        if !self.get_current_char()?.is_ascii_digit() {
            return None;
        }

        let start = self.position;

        while let Some(next) = self.get_current_char()
            && next.is_ascii_digit()
        {
            self.position += 1;
        }

        let number_str = &self
            .input
            .chars()
            .skip(start)
            .take(self.position - start)
            .collect::<String>();

        Some(Token::NumLiteral(number_str.to_string()))
    }

    fn try_parse_two_char_token(&mut self) -> Option<Token> {
        TWO_CHAR_TOKENS.iter().find_map(|&(s1, s2, ref token)| {
            if self.get_current_char() == Some(s1) && self.input.chars().nth(self.position + 1) == Some(s2) {
                self.position += 2;
                Some(token.clone())
            } else {
                None
            }
        })
    }

    fn try_parse_one_char_token(&mut self) -> Option<Token> {
        ONE_CHAR_TOKENS.iter().find_map(|&(s, ref token)| {
            if self.get_current_char() == Some(s) {
                self.position += 1;
                Some(token.clone())
            } else {
                None
            }
        })
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Result<Token, LexerError>> {
        self.skip_whitespace();
        if self.position >= self.input.len() {
            return None;
        }

        let result = self
            .try_parse_two_char_token()
            .or_else(|| self.try_parse_one_char_token())
            .or_else(|| self.try_parse_identifier_or_keyword())
            .or_else(|| self.try_parse_number())
            .ok_or(LexerError {
                position: self.position,
            });

        Some(result)
    }
}

fn try_into_keyword_token(keyword: &str) -> Option<Token> {
    match keyword {
        "let" => Some(Token::Keyword(Keyword::Let)),
        "fn" => Some(Token::Keyword(Keyword::Fn)),
        "struct" => Some(Token::Keyword(Keyword::Struct)),
        "return" => Some(Token::Keyword(Keyword::Return)),
        "break" => Some(Token::Keyword(Keyword::Break)),
        "true" => Some(Token::BoolLiteral(true)),
        "false" => Some(Token::BoolLiteral(false)),
        "if" => Some(Token::Keyword(Keyword::If)),
        "else" => Some(Token::Keyword(Keyword::Else)),
        "loop" => Some(Token::Keyword(Keyword::Loop)),
        "enum" => Some(Token::Keyword(Keyword::Enum)),
        _ => None,
    }
}

mod test {
    #[test]
    fn test_empty() {
        use super::get_tokens;
        let input = "";
        let tokens = get_tokens(input);
        assert!(tokens.unwrap().is_empty());
    }

    #[test]
    fn test_whitespace_only() {
        use super::get_tokens;
        let input = "   \n\t  ";
        let tokens = get_tokens(input);
        assert!(tokens.unwrap().is_empty());
    }

    #[test]
    fn test_word_starting_with_digit() {
        use super::get_tokens;
        let input = "1abc";
        let tokens = get_tokens(input);
        assert_eq!(
            tokens.unwrap(),
            vec![
                super::Token::NumLiteral("1".to_string()),
                super::Token::Identifier("abc".to_string())
            ]
        );
    }

    #[test]
    fn test_ident_with_digits() {
        use super::get_tokens;
        let input = "var_123";
        let tokens = get_tokens(input);
        assert_eq!(tokens.unwrap(), vec![super::Token::Identifier(input.to_string())]);
    }

    #[test]
    fn test_sample_program() {
        use super::{Keyword, Token, get_tokens};

        let input = r#"
            struct Point {
                x: i32,
                y: i32,
            }

            fn main() {
                let p = Point { x: 10, y: 20 };
                let b = true && p.x == p.y;
                return;
            }
        "#;

        let expected_tokens = vec![
            Token::Keyword(Keyword::Struct),
            Token::Identifier("Point".to_string()),
            Token::LBrace,
            Token::Identifier("x".to_string()),
            Token::Colon,
            Token::Identifier("i32".to_string()),
            Token::Comma,
            Token::Identifier("y".to_string()),
            Token::Colon,
            Token::Identifier("i32".to_string()),
            Token::Comma,
            Token::RBrace,
            Token::Keyword(Keyword::Fn),
            Token::Identifier("main".to_string()),
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::Keyword(Keyword::Let),
            Token::Identifier("p".to_string()),
            Token::Equal,
            Token::Identifier("Point".to_string()),
            Token::LBrace,
            Token::Identifier("x".to_string()),
            Token::Colon,
            Token::NumLiteral("10".to_string()),
            Token::Comma,
            Token::Identifier("y".to_string()),
            Token::Colon,
            Token::NumLiteral("20".to_string()),
            Token::RBrace,
            Token::Semicolon,
            Token::Keyword(Keyword::Let),
            Token::Identifier("b".to_string()),
            Token::Equal,
            Token::BoolLiteral(true),
            Token::AmpersandAmpersand,
            Token::Identifier("p".to_string()),
            Token::Dot,
            Token::Identifier("x".to_string()),
            Token::EqualEqual,
            Token::Identifier("p".to_string()),
            Token::Dot,
            Token::Identifier("y".to_string()),
            Token::Semicolon,
            Token::Keyword(Keyword::Return),
            Token::Semicolon,
            Token::RBrace,
        ];

        let tokens = get_tokens(input);
        assert_eq!(tokens.unwrap(), expected_tokens);
    }
}
