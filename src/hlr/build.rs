#[macro_use]
mod macros;

use crate::hlr::{defs::*, lexer, token::Keyword};

pub use crate::hlr::{lexer::LexerError, token::Token};

pub fn build_program(input: &str) -> Result<Program, ParserError> {
    let tokens = lexer::get_tokens(input).map_err(ParserError::LexerError)?;
    let mut parser = HlrParser::new(&tokens[..]);
    parser.parse_program()
}

#[cfg(test)]
fn build_expr(input: &str) -> Result<Expression, ParserError> {
    let tokens = lexer::get_tokens(input).map_err(ParserError::LexerError)?;
    let mut parser = HlrParser::new(&tokens[..]);
    parser.parse_expression(true)
}

#[cfg(test)]
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

#[derive(PartialEq, Eq)]
enum StatementKind {
    /// A statement that is an expression but is delimited by a closing brace, e.g. if, loop,
    /// match, or a block. This is allowed as the last statement in a block, but is also allowed
    /// to be followed by other statements without requireing a semicolon in between.
    BlockExpression,
    /// A statement that is an expression but is not delimited by a semicolon, e.g. `1 + a`.
    /// This is only allowed as the final statement in a block, representing the return value.
    UndelimitedExpression,
    /// A statement that is explicitly delimited by a semicolon. If the last statement in a block
    /// is of this kind, it does not represent the return value of the block.
    ExplicitelyDelimited,
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
                Token::Keyword(Keyword::Fn) => program.fns.push(self.parse_function()?),
                Token::Keyword(Keyword::Struct) => program.structs.push(self.parse_struct()?),
                Token::Keyword(Keyword::Enum) => program.enums.push(self.parse_enum()?),
                token => return Err(ParserError::UnexpectedToken(token.clone())),
            }
        }

        Ok(program)
    }

    fn parse_function(&mut self) -> Result<Fn, ParserError> {
        self.expect_keyword(Keyword::Fn)?;
        let name = self.expect_identifier()?;

        self.expect_token(Token::LParen)?;
        let parameters = self.parse_function_parameters()?;
        self.expect_token(Token::RParen)?;

        let return_type = self.parse_function_return_type()?;
        let body = self.parse_block()?;

        Ok(Fn {
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
        self.expect_token(Token::LBrace)?;
        let fields = self.parse_struct_fields()?;
        self.expect_token(Token::RBrace)?;
        Ok(EnumVariant { name, fields })
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

            let (statement, statement_kind) = self.parse_statement()?;

            if statement_kind == StatementKind::UndelimitedExpression {
                // expect that we are at the end of the block
                if self.current() != Some(&Token::RBrace) {
                    return Err(ParserError::UndelimitedStatement);
                }
            }

            if self.current() == Some(&Token::RBrace) {
                if statement_kind == StatementKind::UndelimitedExpression
                    || statement_kind == StatementKind::BlockExpression
                {
                    let Statement::Expression(expr) = statement else {
                        unreachable!();
                    };
                    return_expression = Some(Box::new(expr));
                } else {
                    statements.push(statement);
                }
                break;
            } else {
                statements.push(statement);
            }
        }

        self.expect_token(Token::RBrace)?;

        Ok(Block {
            statements,
            return_expression,
        })
    }

    fn parse_statement(&mut self) -> Result<(Statement, StatementKind), ParserError> {
        match self.current() {
            Some(Token::Keyword(Keyword::Let)) => {
                let stmt = self.parse_let_statement()?;
                self.expect_token(Token::Semicolon)?;
                Ok((stmt, StatementKind::ExplicitelyDelimited))
            }
            Some(Token::Keyword(Keyword::Return)) => {
                let stmt = self.parse_return_statement()?;
                self.expect_token(Token::Semicolon)?;
                Ok((stmt, StatementKind::ExplicitelyDelimited))
            }
            Some(Token::Keyword(Keyword::Break)) => {
                let stmt = self.parse_break_statement()?;
                self.expect_token(Token::Semicolon)?;
                Ok((stmt, StatementKind::ExplicitelyDelimited))
            }
            Some(Token::LBrace) => {
                let expr = self.parse_block()?;
                let stmt = Statement::Expression(Expression::Block(expr));
                Ok((stmt, StatementKind::BlockExpression))
            }
            Some(Token::Keyword(Keyword::If)) => {
                let expr = self.parse_if_expr()?;
                Ok((Statement::Expression(expr), StatementKind::BlockExpression))
            }
            Some(Token::Keyword(Keyword::Loop)) => {
                let expr = self.parse_loop()?;
                Ok((Statement::Expression(expr), StatementKind::BlockExpression))
            }
            Some(Token::Keyword(Keyword::Match)) => {
                let expr = self.parse_match()?;
                Ok((Statement::Expression(expr), StatementKind::BlockExpression))
            }
            _ => {
                let stmt = self.parse_expression_statement()?;
                let semicolon = self.advance_if(Token::Semicolon);
                let statement_kind = if semicolon {
                    StatementKind::ExplicitelyDelimited
                } else {
                    StatementKind::UndelimitedExpression
                };
                Ok((stmt, statement_kind))
            }
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
        let value = self.parse_expression(true)?;
        Ok(Statement::Let { name, var_type, value })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        self.expect_keyword(Keyword::Return)?;
        if let Some(Token::Semicolon) = self.current() {
            Ok(Statement::Return(None))
        } else {
            let expr = self.parse_expression(true)?;
            Ok(Statement::Return(Some(expr)))
        }
    }

    fn parse_break_statement(&mut self) -> Result<Statement, ParserError> {
        self.expect_keyword(Keyword::Break)?;
        Ok(Statement::Break)
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expr = self.parse_expression(true)?;
        Ok(Statement::Expression(expr))
    }

    fn parse_expression(&mut self, allow_top_level_struct_expr: bool) -> Result<Expression, ParserError> {
        self.parse_assignment_expression(allow_top_level_struct_expr)
    }

    fn parse_assignment_expression(&mut self, allow_top_level_struct_expr: bool) -> Result<Expression, ParserError> {
        let target = self.parse_disjunction(allow_top_level_struct_expr)?;
        if self.advance_if(Token::Equal) {
            let value = self.parse_disjunction(allow_top_level_struct_expr)?;
            Ok(Expression::Assignment {
                target: Box::new(target),
                value: Box::new(value),
            })
        } else {
            Ok(target)
        }
    }

    parse_left_associative!(parse_disjunction, parse_conjunction, [
        Token::Pipe => BinaryOperator::BitOr
    ]);

    parse_left_associative!(parse_conjunction, parse_equality_expression, [
        Token::Ampersand => BinaryOperator::BitAnd
    ]);

    parse_left_associative!(parse_equality_expression, parse_comparison, [
        Token::EqualEqual => BinaryOperator::Equal,
        Token::BangEqual => BinaryOperator::NotEqual
    ]);

    parse_left_associative!(parse_comparison, parse_sum_expression, [
        Token::Smaller => BinaryOperator::LessThan,
        Token::Greater => BinaryOperator::GreaterThan,
        Token::SmallerEqual => BinaryOperator::LessThanOrEqual,
        Token::GreaterEqual => BinaryOperator::GreaterThanOrEqual
    ]);

    parse_left_associative!(parse_sum_expression, parse_product_expression, [
        Token::Plus => BinaryOperator::Add,
        Token::Minus => BinaryOperator::Subtract
    ]);

    parse_left_associative!(parse_product_expression, parse_function_call_and_field_access, [
        Token::Asterisk => BinaryOperator::Multiply,
        Token::Slash => BinaryOperator::Divide,
        Token::Percent => BinaryOperator::Remainder
    ]);

    fn parse_function_call_and_field_access(
        &mut self,
        allow_top_level_struct_expr: bool,
    ) -> Result<Expression, ParserError> {
        let mut acc = self.parse_primary_expression(allow_top_level_struct_expr)?;

        loop {
            if self.advance_if(Token::LParen) {
                // function call
                let mut arguments = Vec::new();
                while self.current() != Some(&Token::RParen) {
                    let argument = self.parse_expression(true)?; // Allow top-level struct expression in argument
                    arguments.push(argument);
                    if !self.advance_if(Token::Comma) {
                        break;
                    }
                }
                self.expect_token(Token::RParen)?;
                acc = Expression::Call {
                    callee: Box::new(acc),
                    arguments,
                };
            } else if self.advance_if(Token::Dot) {
                // field access
                let field_name = self.expect_identifier()?;
                acc = Expression::FieldAccess {
                    base: Box::new(acc),
                    field_name,
                };
            } else {
                break;
            }
        }

        Ok(acc)
    }

    fn parse_primary_expression(&mut self, allow_top_level_struct_expr: bool) -> Result<Expression, ParserError> {
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

                if allow_top_level_struct_expr && self.advance_if(Token::LBrace) {
                    // parse struct expr
                    let mut fields = Vec::new();
                    while let Some(Token::Identifier(field_name)) = self.current() {
                        let field_name = field_name.clone();
                        self.position += 1; // consume field name
                        self.expect_token(Token::Colon)?;
                        let field_value = self.parse_expression(true)?;
                        fields.push((field_name, field_value));
                        if !self.advance_if(Token::Comma) {
                            break;
                        }
                    }
                    self.expect_token(Token::RBrace)?;
                    Ok(Expression::StructExpr {
                        struct_name: ident,
                        fields,
                    })
                } else {
                    Ok(Expression::Variable(ident))
                }
            }
            Token::LParen => {
                self.position += 1;
                let expr = self.parse_expression(true)?; // Allow top-level struct expression in parens
                self.expect_token(Token::RParen)?;
                Ok(expr)
            }
            Token::LBrace => {
                let block = self.parse_block()?;
                Ok(Expression::Block(block))
            }
            Token::Keyword(Keyword::If) => self.parse_if_expr(),
            Token::Keyword(Keyword::Loop) => self.parse_loop(),
            Token::Keyword(Keyword::Match) => self.parse_match(),

            token => Err(ParserError::UnexpectedToken(token.clone())),
        }
    }

    fn parse_if_expr(&mut self) -> Result<Expression, ParserError> {
        self.expect_keyword(Keyword::If)?;
        let condition = self.parse_expression(false)?; // Don't allow top-level struct in if condition
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

    fn parse_loop(&mut self) -> Result<Expression, ParserError> {
        self.expect_keyword(Keyword::Loop)?;
        let body = self.parse_block()?;
        Ok(Expression::Loop { body })
    }

    fn parse_match(&mut self) -> Result<Expression, ParserError> {
        self.expect_keyword(Keyword::Match)?;
        let scrutinee = self.parse_expression(false)?; // Don't allow top-level struct in match scrutinee

        let mut arms = Vec::new();
        self.expect_token(Token::LBrace)?;

        while Some(&Token::RBrace) != self.current() {
            let pattern = self.parse_struct_pattern()?;
            self.expect_token(Token::BoldArrow)?;
            let value = self.parse_expression(true)?; // Allow top-level struct expr in match arm body

            arms.push(MatchArm {
                pattern,
                value: Box::new(value),
            });

            if !self.advance_if(Token::Comma) {
                break;
            }
        }

        self.expect_token(Token::RBrace)?;

        Ok(Expression::Match {
            scrutinee: Box::new(scrutinee),
            arms,
        })
    }

    fn parse_struct_pattern(&mut self) -> Result<StructPattern, ParserError> {
        let variant = self.expect_identifier()?;

        self.expect_token(Token::LBrace)?;

        let mut fields = Vec::new();
        while let Some(Token::Identifier(field_name)) = self.current() {
            let field_name = field_name.clone();
            self.position += 1; // consume field name
            self.expect_token(Token::Colon)?;
            let binding_name = self.expect_identifier()?;

            fields.push(StructPatternField {
                field_name,
                binding_name,
            });

            if !self.advance_if(Token::Comma) {
                break;
            }
        }

        self.expect_token(Token::RBrace)?;

        Ok(StructPattern { variant, fields })
    }
}

#[cfg(test)]
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
                fns: vec![Fn {
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
                fns: vec![Fn {
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
                fns: vec![],
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
                    Off {},
                    On {},
                    Unknown {},
                }"#;

            let expected = Program {
                fns: vec![],
                structs: vec![],
                enums: vec![Enum {
                    name: "State".to_string(),
                    variants: vec![
                        EnumVariant {
                            name: "Off".to_string(),
                            fields: vec![],
                        },
                        EnumVariant {
                            name: "On".to_string(),
                            fields: vec![],
                        },
                        EnumVariant {
                            name: "Unknown".to_string(),
                            fields: vec![],
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
                right: Box::new(Expression::Call {
                    callee: Box::new(Expression::Variable("arctan2".to_string())),
                    arguments: vec![
                        Expression::Variable("x".to_string()),
                        Expression::Variable("y".to_string()),
                    ],
                }),
            };
            parse_and_compare(input, expected);
        }

        #[test]
        fn parse_struct_expr() {
            let input = r#"
                Circle {
                    center: Point { x: 1, y: 2 },
                    radius: 3
                }"#;

            let expected = Expression::StructExpr {
                struct_name: "Circle".to_string(),
                fields: vec![
                    (
                        "center".to_string(),
                        Expression::StructExpr {
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

        #[test]
        fn test_parse_function_call_and_field_access() {
            let input = "obj.method(arg1, arg2).field";
            let expected = Expression::FieldAccess {
                base: Box::new(Expression::Call {
                    callee: Box::new(Expression::FieldAccess {
                        base: Box::new(Expression::Variable("obj".to_string())),
                        field_name: "method".to_string(),
                    }),
                    arguments: vec![
                        Expression::Variable("arg1".to_string()),
                        Expression::Variable("arg2".to_string()),
                    ],
                }),
                field_name: "field".to_string(),
            };
            parse_and_compare(input, expected);
        }

        #[test]
        fn test_parse_match_expression() {
            let input = r#"match value {
                Some { inner: a } => a,
                None {} => 0,
            }"#;

            let expected = Expression::Match {
                scrutinee: Box::new(Expression::Variable("value".to_string())),
                arms: vec![
                    MatchArm {
                        pattern: StructPattern {
                            variant: "Some".to_string(),
                            fields: vec![StructPatternField {
                                field_name: "inner".to_string(),
                                binding_name: "a".to_string(),
                            }],
                        },
                        value: Box::new(Expression::Variable("a".to_string())),
                    },
                    MatchArm {
                        pattern: StructPattern {
                            variant: "None".to_string(),
                            fields: vec![],
                        },
                        value: Box::new(Expression::Literal(Literal::Int(0))),
                    },
                ],
            };

            parse_and_compare(input, expected);
        }
    }
}
