use crate::hlr::{defs::*, lexer, token::Keyword};

pub use crate::hlr::{lexer::LexerError, token::Token};

pub fn build_program(input: &str) -> Result<Program, ParserError> {
    let tokens = lexer::get_tokens(input).map_err(ParserError::LexerError)?;
    let mut parser = HlrParser::new(&tokens[..]);
    parser.parse_program()
}

fn build_expr(input: &str) -> Result<Expression, ParserError> {
    let tokens = lexer::get_tokens(input).map_err(ParserError::LexerError)?;
    let mut parser = HlrParser::new(&tokens[..]);
    parser.parse_expression()
}

fn build_block(input: &str) -> Result<Block, ParserError> {
    let tokens = lexer::get_tokens(input).map_err(ParserError::LexerError)?;
    let mut parser = HlrParser::new(&tokens[..]);
    parser.parse_block()
}

#[derive(Debug)]
pub enum ParserError {
    LexerError(LexerError),
    UnexpectedToken(Token),
    UndelimitedStatement,
    InvalidLiteral,
    UnexpectedEOF,
}

struct HlrParser<'a> {
    input: &'a [Token],
    position: usize,
}

impl<'a> HlrParser<'a> {
    fn new(input: &'a [Token]) -> Self {
        HlrParser { input, position: 0 }
    }

    fn current(&self) -> Option<&Token> {
        self.input.get(self.position)
    }

    fn expect_keyword(&mut self, keyword: Keyword) -> Result<(), ParserError> {
        let current = self.current().ok_or(ParserError::UnexpectedEOF)?;
        match current {
            Token::Keyword(k) if *k == keyword => {
                self.position += 1;
                Ok(())
            }
            token => Err(ParserError::UnexpectedToken(token.clone())),
        }
    }

    fn expect_identifier(&mut self) -> Result<String, ParserError> {
        let current = self.current().ok_or(ParserError::UnexpectedEOF)?;
        match current {
            Token::Identifier(name) => {
                let name = name.to_string();
                self.position += 1;
                Ok(name)
            }
            token => Err(ParserError::UnexpectedToken(token.clone())),
        }
    }

    fn expect_token(&mut self, token: Token) -> Result<(), ParserError> {
        let current = self.current().ok_or(ParserError::UnexpectedEOF)?;
        if *current == token {
            self.position += 1;
            Ok(())
        } else {
            Err(ParserError::UnexpectedToken(current.clone()))
        }
    }

    fn advance_if(&mut self, token: Token) -> bool {
        if let Some(t) = self.current()
            && *t == token
        {
            self.position += 1;
            true
        } else {
            false
        }
    }

    fn parse_program(&mut self) -> Result<Program, ParserError> {
        let mut program = Program::new();

        while let Some(token) = self.current() {
            match token {
                Token::Keyword(Keyword::Fn) => program.functions.push(self.parse_function()?),
                Token::Keyword(Keyword::Struct) => program.structs.push(self.parse_struct()?),
                Token::Keyword(Keyword::Enum) => program.enums.push(self.parse_enum()?),
                token => return Err(ParserError::UnexpectedToken(token.clone())),
            }
        }

        Ok(program)
    }

    fn parse_function(&mut self) -> Result<Function, ParserError> {
        self.expect_keyword(Keyword::Fn)?;
        let name = self.expect_identifier()?;

        self.expect_token(Token::LParen)?;
        let parameters = self.parse_function_parameters()?;
        self.expect_token(Token::RParen)?;

        let return_type = self.parse_function_return_type()?;
        let body = self.parse_block()?;

        Ok(Function {
            name,
            return_type,
            parameters,
            body,
        })
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Parameter>, ParserError> {
        let mut parameters = Vec::new();
        while let Some(Token::Identifier(_)) = self.current() {
            parameters.push(self.parse_function_parameter()?);

            if !self.advance_if(Token::Comma) {
                break;
            }
        }
        Ok(parameters)
    }

    fn parse_function_parameter(&mut self) -> Result<Parameter, ParserError> {
        let name = self.expect_identifier()?;
        self.expect_token(Token::Colon)?;
        let param_type = self.expect_identifier()?;
        Ok(Parameter { name, param_type })
    }

    fn parse_function_return_type(&mut self) -> Result<Option<String>, ParserError> {
        if self.advance_if(Token::Arrow) {
            let return_type = self.expect_identifier()?;
            Ok(Some(return_type))
        } else {
            Ok(None)
        }
    }

    fn parse_struct(&mut self) -> Result<Struct, ParserError> {
        self.expect_keyword(Keyword::Struct)?;
        let name = self.expect_identifier()?;

        self.expect_token(Token::LBrace)?;
        let fields = self.parse_struct_fields()?;
        self.expect_token(Token::RBrace)?;

        Ok(Struct { name, fields })
    }

    fn parse_struct_fields(&mut self) -> Result<Vec<StructField>, ParserError> {
        let mut fields = Vec::new();
        while let Some(Token::Identifier(_)) = self.current() {
            fields.push(self.parse_struct_field()?);

            if !self.advance_if(Token::Comma) {
                break;
            }
        }
        Ok(fields)
    }

    fn parse_struct_field(&mut self) -> Result<StructField, ParserError> {
        let name = self.expect_identifier()?;
        self.expect_token(Token::Colon)?;
        let field_type = self.expect_identifier()?;
        Ok(StructField { name, field_type })
    }

    fn parse_enum(&mut self) -> Result<Enum, ParserError> {
        self.expect_keyword(Keyword::Enum)?;
        let name = self.expect_identifier()?;

        self.expect_token(Token::LBrace)?;
        let variants = self.parse_enum_variants()?;
        self.expect_token(Token::RBrace)?;

        Ok(Enum { name, variants })
    }

    fn parse_enum_variants(&mut self) -> Result<Vec<EnumVariant>, ParserError> {
        let mut variants = Vec::new();
        while let Some(Token::Identifier(_)) = self.current() {
            variants.push(self.parse_enum_variant()?);

            if !self.advance_if(Token::Comma) {
                break;
            }
        }
        Ok(variants)
    }

    fn parse_enum_variant(&mut self) -> Result<EnumVariant, ParserError> {
        let name = self.expect_identifier()?;
        Ok(EnumVariant { name })
    }

    /// TODO allow loop/if/block expression statements without delimiting semicolon
    fn parse_block(&mut self) -> Result<Block, ParserError> {
        let mut statements = Vec::new();
        let mut return_expression = None;

        self.expect_token(Token::LBrace)?;

        loop {
            while self.advance_if(Token::Semicolon) {}

            if let Some(Token::RBrace) = self.current() {
                break;
            }

            let statement = self.parse_statement()?;

            if self.advance_if(Token::Semicolon) {
                statements.push(statement);
            } else {
                match statement {
                    Statement::Expression(expr) => {
                        return_expression = Some(Box::new(expr));
                        break;
                    }
                    _ => return Err(ParserError::UndelimitedStatement),
                }
            }
        }

        self.expect_token(Token::RBrace)?;

        Ok(Block {
            statements,
            return_expression,
        })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.current() {
            Some(Token::Keyword(Keyword::Let)) => self.parse_let_statement(),
            Some(Token::Keyword(Keyword::Return)) => self.parse_return_statement(),
            Some(Token::Keyword(Keyword::Break)) => self.parse_break_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        self.expect_keyword(Keyword::Let)?;
        let name = self.expect_identifier()?;
        let var_type = if self.advance_if(Token::Colon) {
            Some(self.expect_identifier()?)
        } else {
            None
        };
        self.expect_token(Token::Equal)?;
        let value = self.parse_expression()?;
        Ok(Statement::Let { name, var_type, value })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        self.expect_keyword(Keyword::Return)?;
        if let Some(Token::Semicolon) = self.current() {
            Ok(Statement::Return(None))
        } else {
            let expr = self.parse_expression()?;
            Ok(Statement::Return(Some(expr)))
        }
    }

    fn parse_break_statement(&mut self) -> Result<Statement, ParserError> {
        self.expect_keyword(Keyword::Break)?;
        Ok(Statement::Break)
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expr = self.parse_expression()?;
        Ok(Statement::Expression(expr))
    }

    fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        self.parse_assignment_expression()
    }

    fn parse_assignment_expression(&mut self) -> Result<Expression, ParserError> {
        let target = self.parse_equality_expression()?;
        if self.advance_if(Token::Equal) {
            let value = self.parse_sum_expression()?;
            Ok(Expression::Assignment {
                target: Box::new(target),
                value: Box::new(value),
            })
        } else {
            Ok(target)
        }
    }

    fn parse_equality_expression(&mut self) -> Result<Expression, ParserError> {
        let mut acc = self.parse_sum_expression()?;

        while let Some(op @ (Token::EqualEqual | Token::BangEqual)) = self.current() {
            let operator = match op {
                Token::EqualEqual => BinaryOperator::Equal,
                Token::BangEqual => BinaryOperator::NotEqual,
                _ => unreachable!(),
            };
            self.position += 1; // consume operator
            let right = self.parse_sum_expression()?;
            acc = Expression::BinaryOp {
                left: Box::new(acc),
                operator,
                right: Box::new(right),
            };
        }

        Ok(acc)
    }

    fn parse_sum_expression(&mut self) -> Result<Expression, ParserError> {
        let mut acc = self.parse_product_expression()?;

        while let Some(op @ (Token::Plus | Token::Minus)) = self.current() {
            let operator = match op {
                Token::Plus => BinaryOperator::Add,
                Token::Minus => BinaryOperator::Subtract,
                _ => unreachable!(),
            };
            self.position += 1; // consume operator
            let right = self.parse_product_expression()?;
            acc = Expression::BinaryOp {
                left: Box::new(acc),
                operator,
                right: Box::new(right),
            };
        }

        Ok(acc)
    }

    fn parse_product_expression(&mut self) -> Result<Expression, ParserError> {
        let mut acc = self.parse_function_call()?;

        while let Some(op @ (Token::Asterisk | Token::Slash)) = self.current() {
            let operator = match op {
                Token::Asterisk => BinaryOperator::Multiply,
                Token::Slash => BinaryOperator::Divide,
                _ => unreachable!(),
            };
            self.position += 1; // consume operator
            let right = self.parse_function_call()?;
            acc = Expression::BinaryOp {
                left: Box::new(acc),
                operator,
                right: Box::new(right),
            };
        }

        Ok(acc)
    }

    fn parse_function_call(&mut self) -> Result<Expression, ParserError> {
        let mut acc = self.parse_atomic_expression()?;

        while self.advance_if(Token::LParen) {
            let mut arguments = Vec::new();
            while self.current() != Some(&Token::RParen) {
                arguments.push(self.parse_expression()?);
                if !self.advance_if(Token::Comma) {
                    break;
                }
            }
            self.expect_token(Token::RParen)?;
            acc = Expression::FunctionCall {
                function: Box::new(acc),
                arguments,
            };
        }

        Ok(acc)
    }

    fn parse_atomic_expression(&mut self) -> Result<Expression, ParserError> {
        let current = self.current().ok_or(ParserError::UnexpectedEOF)?;

        match current {
            Token::NumLiteral(value) => {
                let value = value.parse().map_err(|_| ParserError::InvalidLiteral)?;
                self.position += 1;
                Ok(Expression::Literal(Literal::Int(value)))
            }
            Token::BoolLiteral(b) => {
                let value = *b;
                self.position += 1;
                Ok(Expression::Literal(Literal::Bool(value)))
            }
            Token::Identifier(ident) => {
                let ident = ident.clone();
                self.position += 1;

                // if self.advance_if(Token::LBrace) {
                //     let mut fields = Vec::new();
                //     while let Some(Token::Identifier(field_name)) = self.current() {
                //         let field_name = field_name.clone();
                //         self.position += 1; // consume LBrace
                //         self.expect_token(Token::Colon)?;
                //         let field_value = self.parse_expression()?;
                //         fields.push((field_name, field_value));
                //         if !self.advance_if(Token::Comma) {
                //             break;
                //         }
                //     }
                //     self.expect_token(Token::RBrace)?;
                //     Ok(Expression::StructInit {
                //         struct_name: ident,
                //         fields,
                //     })
                // } else {
                Ok(Expression::Variable(ident))
                // }
            }
            Token::LParen => {
                self.position += 1;
                let expr = self.parse_expression()?;
                self.expect_token(Token::RParen)?;
                Ok(expr)
            }
            Token::LBrace => {
                let block = self.parse_block()?;
                Ok(Expression::Block(block))
            }
            Token::Keyword(Keyword::If) => {
                self.position += 1;
                let condition = self.parse_expression()?;
                let then_block = self.parse_block()?;
                let else_block = if self.advance_if(Token::Keyword(Keyword::Else)) {
                    Some(self.parse_block()?)
                } else {
                    None
                };
                Ok(Expression::If {
                    condition: Box::new(condition),
                    then_block,
                    else_block,
                })
            }
            Token::Keyword(Keyword::Loop) => {
                self.position += 1;
                let body = self.parse_block()?;
                Ok(Expression::Loop { body })
            }
            token => Err(ParserError::UnexpectedToken(token.clone())),
        }
    }
}

mod tests {
    use super::*;

    mod program {
        use super::*;

        fn parse_and_compare(input: &str, expected: Program) {
            let parsed = build_program(input).expect("Failed to parse HLR");
            assert_eq!(parsed, expected);
        }

        #[test]
        fn test_empty_program_items() {
            let input = r#"
                fn empty() {
                }

                struct Empty {
                }

                enum Empty {
                }
                "#;

            let expected = Program {
                functions: vec![Function {
                    name: "empty".to_string(),
                    return_type: None,
                    parameters: vec![],
                    body: Block {
                        statements: vec![],
                        return_expression: None,
                    },
                }],
                structs: vec![Struct {
                    name: "Empty".to_string(),
                    fields: vec![],
                }],
                enums: vec![Enum {
                    name: "Empty".to_string(),
                    variants: vec![],
                }],
            };

            parse_and_compare(input, expected);
        }

        #[test]
        fn test_parse_simple_function() {
            let input = r#"
                fn add(a: int, b: int) -> int {
                    return a + b;
                }
                "#;

            let expected = Program {
                functions: vec![Function {
                    name: "add".to_string(),
                    return_type: Some("int".to_string()),
                    parameters: vec![
                        Parameter {
                            name: "a".to_string(),
                            param_type: "int".to_string(),
                        },
                        Parameter {
                            name: "b".to_string(),
                            param_type: "int".to_string(),
                        },
                    ],
                    body: Block {
                        statements: vec![Statement::Return(Some(Expression::BinaryOp {
                            left: Box::new(Expression::Variable("a".to_string())),
                            operator: BinaryOperator::Add,
                            right: Box::new(Expression::Variable("b".to_string())),
                        }))],
                        return_expression: None,
                    },
                }],
                structs: vec![],
                enums: vec![],
            };

            parse_and_compare(input, expected);
        }

        #[test]
        fn test_parse_simple_struct() {
            let input = r#"
                struct Point {
                    x: int,
                    y: int,
                }"#;

            let expected = Program {
                functions: vec![],
                structs: vec![Struct {
                    name: "Point".to_string(),
                    fields: vec![
                        StructField {
                            name: "x".to_string(),
                            field_type: "int".to_string(),
                        },
                        StructField {
                            name: "y".to_string(),
                            field_type: "int".to_string(),
                        },
                    ],
                }],
                enums: vec![],
            };

            parse_and_compare(input, expected);
        }

        #[test]
        fn test_parse_simple_enum() {
            let input = r#"
                enum State {
                    Off,
                    On,
                    Unknown,
                }"#;

            let expected = Program {
                functions: vec![],
                structs: vec![],
                enums: vec![Enum {
                    name: "State".to_string(),
                    variants: vec![
                        EnumVariant {
                            name: "Off".to_string(),
                        },
                        EnumVariant { name: "On".to_string() },
                        EnumVariant {
                            name: "Unknown".to_string(),
                        },
                    ],
                }],
            };

            parse_and_compare(input, expected);
        }
    }

    mod block {
        use super::*;

        fn parse_and_compare(input: &str, expected: Block) {
            let parsed = build_block(input).expect("Failed to parse HLR");
            assert_eq!(parsed, expected);
        }

        #[test]
        fn test_empty_block() {
            let input = "{}";

            let expected = Block {
                statements: vec![],
                return_expression: None,
            };

            parse_and_compare(input, expected);
        }

        #[test]
        fn test_control_structures() {
            let input = r#"{
                let result = 1;
                loop {
                    if n == 1 {
                        break;
                    };
                    result = result * n;
                    n = n - 1;
                };
                result
            }"#;

            let expected = Block {
                statements: vec![
                    Statement::Let {
                        name: "result".to_string(),
                        var_type: None,
                        value: Expression::Literal(Literal::Int(1)),
                    },
                    Statement::Expression(Expression::Loop {
                        body: Block {
                            statements: vec![
                                Statement::Expression(Expression::If {
                                    condition: Box::new(Expression::BinaryOp {
                                        left: Box::new(Expression::Variable("n".to_string())),
                                        operator: BinaryOperator::Equal,
                                        right: Box::new(Expression::Literal(Literal::Int(1))),
                                    }),
                                    then_block: Block {
                                        statements: vec![Statement::Break],
                                        return_expression: None,
                                    },
                                    else_block: None,
                                }),
                                Statement::Expression(Expression::Assignment {
                                    target: Box::new(Expression::Variable("result".to_string())),
                                    value: Box::new(Expression::BinaryOp {
                                        left: Box::new(Expression::Variable("result".to_string())),
                                        operator: BinaryOperator::Multiply,
                                        right: Box::new(Expression::Variable("n".to_string())),
                                    }),
                                }),
                                Statement::Expression(Expression::Assignment {
                                    target: Box::new(Expression::Variable("n".to_string())),
                                    value: Box::new(Expression::BinaryOp {
                                        left: Box::new(Expression::Variable("n".to_string())),
                                        operator: BinaryOperator::Subtract,
                                        right: Box::new(Expression::Literal(Literal::Int(1))),
                                    }),
                                }),
                            ],
                            return_expression: None,
                        },
                    }),
                ],
                return_expression: Some(Box::new(Expression::Variable("result".to_string()))),
            };

            parse_and_compare(input, expected);
        }
    }

    mod expression {
        use super::*;

        fn parse_and_compare(input: &str, expected: Expression) {
            let parsed = build_expr(input).expect("Failed to parse HLR");
            assert_eq!(parsed, expected);
        }

        #[test]
        fn test_parse_arithmetic_expression() {
            let input = r#"1 + 2 * a - arctan2(x, y)"#;

            let expected = Expression::BinaryOp {
                left: Box::new(Expression::BinaryOp {
                    left: Box::new(Expression::Literal(Literal::Int(1))),
                    operator: BinaryOperator::Add,
                    right: Box::new(Expression::BinaryOp {
                        left: Box::new(Expression::Literal(Literal::Int(2))),
                        operator: BinaryOperator::Multiply {},
                        right: Box::new(Expression::Variable("a".to_string())),
                    }),
                }),
                operator: BinaryOperator::Subtract,
                right: Box::new(Expression::FunctionCall {
                    function: Box::new(Expression::Variable("arctan2".to_string())),
                    arguments: vec![
                        Expression::Variable("x".to_string()),
                        Expression::Variable("y".to_string()),
                    ],
                }),
            };
            parse_and_compare(input, expected);
        }

        #[ignore]
        #[test]
        fn parse_struct_initializer() {
            let input = r#"
                Circle {
                    center: Point { x: 1, y: 2 },
                    radius: 3
                }"#;

            let expected = Expression::StructInit {
                struct_name: "Circle".to_string(),
                fields: vec![
                    (
                        "center".to_string(),
                        Expression::StructInit {
                            struct_name: "Point".to_string(),
                            fields: vec![
                                ("x".to_string(), Expression::Literal(Literal::Int(1))),
                                ("y".to_string(), Expression::Literal(Literal::Int(2))),
                            ],
                        },
                    ),
                    ("radius".to_string(), Expression::Literal(Literal::Int(3))),
                ],
            };

            parse_and_compare(input, expected);
        }

        #[test]
        fn test_parse_int_literal() {
            let input = "42";
            let expected = Expression::Literal(Literal::Int(42));
            parse_and_compare(input, expected);
        }

        #[test]
        fn test_parse_if_expression() {
            let input = r#"if condition {
                42
            } else {
                0
            }"#;

            let expected = Expression::If {
                condition: Box::new(Expression::Variable("condition".to_string())),
                then_block: Block {
                    statements: vec![],
                    return_expression: Some(Box::new(Expression::Literal(Literal::Int(42)))),
                },
                else_block: Some(Block {
                    statements: vec![],
                    return_expression: Some(Box::new(Expression::Literal(Literal::Int(0)))),
                }),
            };

            parse_and_compare(input, expected);
        }
    }
}
