mod lexer;
mod token;

#[macro_use]
mod macros;

use crate::{ast::*, ast_parsing::token::Keyword};

pub use lexer::LexerErr;
pub use token::Token;

pub fn parse(input: &str, output: &mut Ast) -> Result<(), ParserErr> {
    let tokens = lexer::get_tokens(input)?;
    let mut parser = AstParser::new(&tokens[..], output);
    parser.parse()
}

#[cfg(test)]
fn parse_block(input: &str) -> Result<Block, ParserErr> {
    let tokens = lexer::get_tokens(input)?;
    let mut ast = Ast::default();
    let mut parser = AstParser::new(&tokens[..], &mut ast);
    parser.parse_block()
}

#[derive(Debug)]
pub enum ParserErr {
    LexerErr(LexerErr),
    UnexpectedToken(Token),
    UndelimitedStmt,
    InvalidLiteral,
    UnexpectedEOF,
    TraitMthdWithBody,
    ExpectedTraitName,
    UnexpectedReceiverArg,
}

impl From<LexerErr> for ParserErr {
    fn from(err: LexerErr) -> Self {
        ParserErr::LexerErr(err)
    }
}

struct AstParser<'a> {
    input: &'a [Token],
    position: usize,
    ast: &'a mut Ast,
}

#[derive(PartialEq, Eq)]
enum StmtKind {
    /// A statement that is an expression but is delimited by a closing brace, e.g. if, loop,
    /// match, or a block. This is allowed as the last statement in a block, but is also allowed
    /// to be followed by other statements without requireing a semicolon in between.
    BlockExpr,
    /// A statement that is an expression but is not delimited by a semicolon, e.g. `1 + a`.
    /// This is only allowed as the final statement in a block, representing the return value.
    UndelimitedExpr,
    /// A statement that is explicitly delimited by a semicolon. If the last statement in a block
    /// is of this kind, it does not represent the return value of the block.
    ExplicitelyDelimited,
}

impl<'a> AstParser<'a> {
    fn new(input: &'a [Token], ast: &'a mut Ast) -> Self {
        AstParser {
            input,
            position: 0,
            ast,
        }
    }

    fn current(&self) -> Option<&Token> {
        self.input.get(self.position)
    }

    fn next(&self) -> Option<&Token> {
        self.input.get(self.position + 1)
    }

    fn expect_keyword(&mut self, keyword: Keyword) -> Result<(), ParserErr> {
        let current = self.current().ok_or(ParserErr::UnexpectedEOF)?;
        match current {
            Token::Keyword(k) if *k == keyword => {
                self.position += 1;
                Ok(())
            }
            token => Err(ParserErr::UnexpectedToken(token.clone())),
        }
    }

    fn expect_identifier(&mut self) -> Result<String, ParserErr> {
        let current = self.current().ok_or(ParserErr::UnexpectedEOF)?;
        match current {
            Token::Identifier(name) => {
                let name = name.to_string();
                self.position += 1;
                Ok(name)
            }
            token => Err(ParserErr::UnexpectedToken(token.clone())),
        }
    }

    fn expect_token(&mut self, token: Token) -> Result<(), ParserErr> {
        let current = self.current().ok_or(ParserErr::UnexpectedEOF)?;
        if *current == token {
            self.position += 1;
            Ok(())
        } else {
            Err(ParserErr::UnexpectedToken(current.clone()))
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

    fn advance_if_turbofish(&mut self) -> bool {
        if self.current() == Some(&Token::ColonColon) && self.next() == Some(&Token::Smaller) {
            self.position += 2;
            true
        } else {
            false
        }
    }

    fn parse(&mut self) -> Result<(), ParserErr> {
        while let Some(token) = self.current() {
            match token {
                Token::Keyword(Keyword::Fn) => {
                    let fn_ = self.parse_function(true)?;
                    self.ast.fns.push(fn_);
                }
                Token::Keyword(Keyword::Struct) => {
                    let struct_ = self.parse_struct()?;
                    self.ast.structs.push(struct_);
                }
                Token::Keyword(Keyword::Enum) => {
                    let enum_ = self.parse_enum()?;
                    self.ast.enums.push(enum_);
                }
                Token::Keyword(Keyword::Impl) => {
                    let impl_ = self.parse_impl()?;
                    self.ast.impls.push(impl_);
                }
                Token::Keyword(Keyword::Trait) => {
                    let trait_ = self.parse_trait()?;
                    self.ast.traits.push(trait_);
                }
                token => return Err(ParserErr::UnexpectedToken(token.clone())),
            }
        }

        Ok(())
    }

    fn parse_function(&mut self, allow_receiver_param: bool) -> Result<Fn, ParserErr> {
        self.expect_keyword(Keyword::Fn)?;
        let name = self.expect_identifier()?;

        let gen_params = self.parse_gen_params()?;

        self.expect_token(Token::LParen)?;
        let (params, var_args) = self.parse_fn_params(allow_receiver_param)?;
        self.expect_token(Token::RParen)?;

        let return_ty = self.parse_function_return_type()?;

        let constraints = if self.advance_if(Token::Keyword(Keyword::Where)) {
            self.parse_function_constraints()?
        } else {
            Vec::new()
        };

        let body = if self.current() == Some(&Token::LBrace) {
            Some(self.parse_block()?)
        } else {
            self.expect_token(Token::Semicolon)?;
            None
        };

        Ok(Fn {
            name,
            gen_params,
            params,
            var_args,
            return_ty,
            constraints,
            body,
        })
    }

    fn parse_fn_params(&mut self, allow_receiver: bool) -> Result<(Vec<Param>, bool), ParserErr> {
        let mut params = Vec::new();
        let mut first = true;
        while !matches!(self.current(), Some(&Token::RParen | &Token::Dots)) {
            params.push(self.parse_fn_param(first && allow_receiver)?);
            first = false;

            if !self.advance_if(Token::Comma) {
                return Ok((params, false));
            }
        }

        let var_args = self.advance_if(Token::Dots);
        Ok((params, var_args))
    }

    fn parse_fn_param(&mut self, allow_receiver: bool) -> Result<Param, ParserErr> {
        if self.current() == Some(&Token::Keyword(Keyword::Self_)) {
            self.position += 1;
            if !allow_receiver {
                return Err(ParserErr::UnexpectedReceiverArg);
            }
            Ok(Param::Receiver)
        } else if self.current() == Some(&Token::Ampersand) {
            self.position += 1;
            self.expect_token(Token::Keyword(Keyword::Self_))?;
            if !allow_receiver {
                return Err(ParserErr::UnexpectedReceiverArg);
            }
            Ok(Param::ReceiverByRef)
        } else {
            let name = self.expect_identifier()?;
            self.expect_token(Token::Colon)?;
            let ty = self.parse_ty_annot()?;
            Ok(Param::Regular { name, ty })
        }
    }

    fn parse_function_return_type(&mut self) -> Result<Option<TyAnnot>, ParserErr> {
        if self.advance_if(Token::Arrow) {
            let return_type = self.parse_ty_annot()?;
            Ok(Some(return_type))
        } else {
            Ok(None)
        }
    }

    fn parse_function_constraints(&mut self) -> Result<Vec<Constraint>, ParserErr> {
        let mut constraints = Vec::new();
        while let Some(Token::Identifier(_)) = self.current() {
            constraints.push(self.parse_constraint()?);
            if !self.advance_if(Token::Comma) {
                break;
            }
        }
        Ok(constraints)
    }

    fn parse_constraint(&mut self) -> Result<Constraint, ParserErr> {
        let subject = self.expect_identifier()?;
        self.expect_token(Token::Colon)?;
        let requirement = self.parse_constraint_requirement()?;
        Ok(Constraint { subject, requirement })
    }

    fn parse_constraint_requirement(&mut self) -> Result<ConstraintRequirement, ParserErr> {
        let requirement = match self.current() {
            Some(Token::Keyword(Keyword::FnTrait)) => {
                self.position += 1;
                self.expect_token(Token::LParen)?;

                let mut params = Vec::new();
                while self.current() != Some(&Token::RParen) {
                    let param = self.parse_ty_annot()?;
                    params.push(param);
                    if !self.advance_if(Token::Comma) {
                        break;
                    }
                }
                self.expect_token(Token::RParen)?;

                let return_ty = if self.advance_if(Token::Arrow) {
                    Some(self.parse_ty_annot()?)
                } else {
                    None
                };

                ConstraintRequirement::Callable { params, return_ty }
            }
            Some(Token::Identifier(trait_)) => {
                let trait_name = trait_.clone();
                self.position += 1;

                let trait_args = if self.advance_if(Token::Smaller) {
                    let mut gen_args = Vec::new();
                    while self.current() != Some(&Token::Greater) {
                        gen_args.push(self.parse_ty_annot()?);
                        if !self.advance_if(Token::Comma) {
                            break;
                        }
                    }
                    self.expect_token(Token::Greater)?;
                    gen_args
                } else {
                    vec![]
                };

                ConstraintRequirement::Trait { trait_name, trait_args }
            }
            _ => return Err(ParserErr::UnexpectedToken(self.current().unwrap().clone())),
        };
        Ok(requirement)
    }

    fn parse_struct(&mut self) -> Result<Struct, ParserErr> {
        self.expect_keyword(Keyword::Struct)?;
        let name = self.expect_identifier()?;
        let gen_params = self.parse_gen_params()?;

        self.expect_token(Token::LBrace)?;
        let fields = self.parse_struct_fields()?;
        self.expect_token(Token::RBrace)?;

        Ok(Struct {
            name,
            gen_params,
            fields,
        })
    }

    fn parse_struct_fields(&mut self) -> Result<Vec<StructField>, ParserErr> {
        let mut fields = Vec::new();
        while let Some(Token::Identifier(_)) = self.current() {
            fields.push(self.parse_struct_field()?);

            if !self.advance_if(Token::Comma) {
                break;
            }
        }
        Ok(fields)
    }

    fn parse_struct_field(&mut self) -> Result<StructField, ParserErr> {
        let name = self.expect_identifier()?;
        self.expect_token(Token::Colon)?;
        let ty = self.parse_ty_annot()?;
        Ok(StructField { name, ty })
    }

    fn parse_enum(&mut self) -> Result<Enum, ParserErr> {
        self.expect_keyword(Keyword::Enum)?;
        let name = self.expect_identifier()?;
        let gen_params = self.parse_gen_params()?;

        self.expect_token(Token::LBrace)?;
        let variants = self.parse_enum_variants()?;
        self.expect_token(Token::RBrace)?;

        Ok(Enum {
            name,
            gen_params,
            variants,
        })
    }

    fn parse_enum_variants(&mut self) -> Result<Vec<EnumVariant>, ParserErr> {
        let mut variants = Vec::new();
        while let Some(Token::Identifier(_)) = self.current() {
            variants.push(self.parse_enum_variant()?);

            if !self.advance_if(Token::Comma) {
                break;
            }
        }
        Ok(variants)
    }

    fn parse_enum_variant(&mut self) -> Result<EnumVariant, ParserErr> {
        let name = self.expect_identifier()?;
        let fields = if self.advance_if(Token::LBrace) {
            let fields = self.parse_struct_fields()?;
            self.expect_token(Token::RBrace)?;
            fields
        } else {
            vec![]
        };
        Ok(EnumVariant { name, fields })
    }

    fn parse_impl(&mut self) -> Result<Impl, ParserErr> {
        self.expect_keyword(Keyword::Impl)?;
        let gen_params = self.parse_gen_params()?;
        let ty = self.parse_ty_annot()?;

        let (trait_annot, ty) = if self.advance_if(Token::Keyword(Keyword::For)) {
            let ty2 = self.parse_ty_annot()?;
            match ty {
                TyAnnot::Path(path) => match path.segments.as_slice() {
                    [PathSegment::Ident(ident)] => (
                        Some(TraitAnnot {
                            name: ident.clone(),
                            args: vec![],
                        }),
                        ty2,
                    ),
                    [PathSegment::Generic(GenPathSegment { ident, gen_args })] => (
                        Some(TraitAnnot {
                            name: ident.clone(),
                            args: gen_args.clone(),
                        }),
                        ty2,
                    ),
                    _ => return Err(ParserErr::ExpectedTraitName),
                },
                _ => return Err(ParserErr::ExpectedTraitName),
            }
        } else {
            (None, ty)
        };

        let mut mthds = Vec::new();
        let mut assoc_tys = Vec::new();

        self.expect_token(Token::LBrace)?;
        loop {
            match self.current() {
                Some(Token::Keyword(Keyword::Type)) => {
                    self.position += 1;
                    let name = self.expect_identifier()?;
                    self.expect_token(Token::Equal)?;
                    let ty = self.parse_ty_annot()?;
                    self.expect_token(Token::Semicolon)?;

                    assoc_tys.push(AssocTy { name, ty });
                }
                Some(Token::Keyword(Keyword::Fn)) => {
                    let mthd = self.parse_function(true)?;
                    mthds.push(mthd);
                }
                Some(Token::RBrace) => break,
                Some(token) => return Err(ParserErr::UnexpectedToken(token.clone())),
                None => return Err(ParserErr::UnexpectedEOF),
            }
        }
        self.expect_token(Token::RBrace)?;

        Ok(Impl {
            gen_params,
            trait_annot,
            ty,
            mthds,
            assoc_tys,
        })
    }

    fn parse_trait(&mut self) -> Result<Trait, ParserErr> {
        self.expect_keyword(Keyword::Trait)?;
        let name = self.expect_identifier()?;

        let gen_params = self.parse_gen_params()?;

        let mut mthds = Vec::new();
        let mut assoc_ty_names = Vec::new();

        self.expect_token(Token::LBrace)?;
        loop {
            match self.current() {
                Some(Token::Keyword(Keyword::Type)) => {
                    self.position += 1;
                    assoc_ty_names.push(self.expect_identifier()?);
                    self.expect_token(Token::Semicolon)?;
                }
                Some(Token::Keyword(Keyword::Fn)) => {
                    let mthd = self.parse_function(true)?;
                    if mthd.body.is_some() {
                        return Err(ParserErr::TraitMthdWithBody);
                    }
                    mthds.push(mthd);
                }
                Some(Token::RBrace) => break,
                Some(token) => return Err(ParserErr::UnexpectedToken(token.clone())),
                None => return Err(ParserErr::UnexpectedEOF),
            }
        }
        self.expect_token(Token::RBrace)?;

        Ok(Trait {
            name,
            gen_params,
            mthds,
            assoc_ty_names,
        })
    }

    fn parse_gen_params(&mut self) -> Result<Vec<String>, ParserErr> {
        if self.current() != Some(&Token::Smaller) {
            return Ok(Vec::new());
        }
        self.expect_token(Token::Smaller)?;

        let mut params = Vec::new();
        while let Some(Token::Identifier(_)) = self.current() {
            let arg = self.expect_identifier()?;
            params.push(arg);

            if !self.advance_if(Token::Comma) {
                break;
            }
        }
        self.expect_token(Token::Greater)?;
        Ok(params)
    }

    /// TODO allow loop/if/block expression statements without delimiting semicolon
    fn parse_block(&mut self) -> Result<Block, ParserErr> {
        let mut stmts = Vec::new();
        let mut return_expr = None;

        self.expect_token(Token::LBrace)?;

        loop {
            while self.advance_if(Token::Semicolon) {}

            if let Some(Token::RBrace) = self.current() {
                break;
            }

            let (stmt, stmt_kind) = self.parse_stmt()?;

            if stmt_kind == StmtKind::UndelimitedExpr {
                // expect that we are at the end of the block
                if self.current() != Some(&Token::RBrace) {
                    return Err(ParserErr::UndelimitedStmt);
                }
            }

            if self.current() == Some(&Token::RBrace) {
                if stmt_kind == StmtKind::UndelimitedExpr || stmt_kind == StmtKind::BlockExpr {
                    let Stmt::Expr(expr) = stmt else {
                        unreachable!();
                    };
                    return_expr = Some(expr);
                } else {
                    stmts.push(stmt);
                }
                break;
            } else {
                stmts.push(stmt);
            }
        }

        self.expect_token(Token::RBrace)?;

        Ok(Block { stmts, return_expr })
    }

    fn parse_stmt(&mut self) -> Result<(Stmt, StmtKind), ParserErr> {
        match self.current() {
            Some(Token::Keyword(Keyword::Let)) => {
                let stmt = self.parse_let_stmt()?;
                self.expect_token(Token::Semicolon)?;
                Ok((stmt, StmtKind::ExplicitelyDelimited))
            }
            Some(Token::Keyword(Keyword::Return)) => {
                let stmt = self.parse_return_stmt()?;
                self.expect_token(Token::Semicolon)?;
                Ok((stmt, StmtKind::ExplicitelyDelimited))
            }
            Some(Token::Keyword(Keyword::Break)) => {
                let stmt = self.parse_break_stmt()?;
                self.expect_token(Token::Semicolon)?;
                Ok((stmt, StmtKind::ExplicitelyDelimited))
            }
            Some(Token::LBrace) => {
                let block = self.parse_block()?;
                let expr = self.ast.new_expr(ExprKind::Block(block));
                let stmt = Stmt::Expr(expr);
                Ok((stmt, StmtKind::BlockExpr))
            }
            Some(Token::Keyword(Keyword::If)) => {
                let expr = self.parse_if_expr()?;
                Ok((Stmt::Expr(expr), StmtKind::BlockExpr))
            }
            Some(Token::Keyword(Keyword::Loop)) => {
                let expr = self.parse_loop()?;
                Ok((Stmt::Expr(expr), StmtKind::BlockExpr))
            }
            Some(Token::Keyword(Keyword::While)) => {
                let expr = self.parse_while()?;
                Ok((Stmt::Expr(expr), StmtKind::BlockExpr))
            }
            Some(Token::Keyword(Keyword::Match)) => {
                let expr = self.parse_match()?;
                Ok((Stmt::Expr(expr), StmtKind::BlockExpr))
            }
            _ => {
                let stmt = self.parse_expr_stmt()?;
                let semicolon = self.advance_if(Token::Semicolon);
                let stmt_kind = if semicolon {
                    StmtKind::ExplicitelyDelimited
                } else {
                    StmtKind::UndelimitedExpr
                };
                Ok((stmt, stmt_kind))
            }
        }
    }

    fn parse_let_stmt(&mut self) -> Result<Stmt, ParserErr> {
        self.expect_keyword(Keyword::Let)?;
        let name = self.expect_identifier()?;
        let ty_annot = if self.advance_if(Token::Colon) {
            Some(self.parse_ty_annot()?)
        } else {
            None
        };
        self.expect_token(Token::Equal)?;
        let value = self.parse_expr(true)?;
        Ok(Stmt::Let { name, ty_annot, value })
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, ParserErr> {
        self.expect_keyword(Keyword::Return)?;
        if let Some(Token::Semicolon) = self.current() {
            Ok(Stmt::Return(None))
        } else {
            let expr = self.parse_expr(true)?;
            Ok(Stmt::Return(Some(expr)))
        }
    }

    fn parse_break_stmt(&mut self) -> Result<Stmt, ParserErr> {
        self.expect_keyword(Keyword::Break)?;
        Ok(Stmt::Break)
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ParserErr> {
        let expr = self.parse_expr(true)?;
        Ok(Stmt::Expr(expr))
    }

    fn parse_expr(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr, ParserErr> {
        self.parse_assign_expr(allow_top_level_struct_expr)
    }

    fn parse_assign_expr(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr, ParserErr> {
        let target = self.parse_conversion_expr(allow_top_level_struct_expr)?;
        if self.advance_if(Token::Equal) {
            let value = self.parse_conversion_expr(allow_top_level_struct_expr)?;
            let expr = ExprKind::Assign { target, value };
            Ok(self.ast.new_expr(expr))
        } else {
            Ok(target)
        }
    }

    fn parse_conversion_expr(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr, ParserErr> {
        let mut expr = self.parse_disjunction(allow_top_level_struct_expr)?;
        while self.advance_if(Token::Keyword(Keyword::As)) {
            let ty_annot = self.parse_ty_annot()?;
            let expr_kind = ExprKind::As {
                expr,
                target_ty: ty_annot,
            };
            expr = self.ast.new_expr(expr_kind);
        }
        Ok(expr)
    }

    parse_left_associative!(parse_disjunction, parse_conjunction, [
        Token::Pipe => BinaryOperator::BitOr,
        Token::PipePipe => BinaryOperator::LogicalOr
    ]);

    parse_left_associative!(parse_conjunction, parse_equality_expr, [
        Token::Ampersand => BinaryOperator::BitAnd,
        Token::AmpersandAmpersand => BinaryOperator::LogicalAnd
    ]);

    parse_left_associative!(parse_equality_expr, parse_comparison, [
        Token::EqualEqual => BinaryOperator::Equal,
        Token::BangEqual => BinaryOperator::NotEqual
    ]);

    parse_left_associative!(parse_comparison, parse_sum, [
        Token::Smaller => BinaryOperator::LessThan,
        Token::Greater => BinaryOperator::GreaterThan,
        Token::SmallerEqual => BinaryOperator::LessThanOrEqual,
        Token::GreaterEqual => BinaryOperator::GreaterThanOrEqual
    ]);

    parse_left_associative!(parse_sum, parse_product, [
        Token::Plus => BinaryOperator::Add,
        Token::Minus => BinaryOperator::Subtract
    ]);

    parse_left_associative!(parse_product, parse_unary_expr, [
        Token::Asterisk => BinaryOperator::Multiply,
        Token::Slash => BinaryOperator::Divide,
        Token::Percent => BinaryOperator::Remainder
    ]);

    fn parse_unary_expr(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr, ParserErr> {
        let expr = if self.advance_if(Token::Asterisk) {
            let base = self.parse_unary_expr(allow_top_level_struct_expr)?;
            ExprKind::Deref { base }
        } else if self.advance_if(Token::Ampersand) {
            let base = self.parse_unary_expr(allow_top_level_struct_expr)?;
            ExprKind::AddrOf { base }
        } else if self.advance_if(Token::AmpersandAmpersand) {
            let base = self.parse_unary_expr(allow_top_level_struct_expr)?;
            ExprKind::AddrOf {
                base: self.ast.new_expr(ExprKind::AddrOf { base }),
            }
        } else if self.advance_if(Token::Bang) {
            let base = self.parse_unary_expr(allow_top_level_struct_expr)?;
            ExprKind::UnaryOp {
                operand: base,
                operator: UnaryOperator::Not,
            }
        } else if self.advance_if(Token::Minus) {
            let base = self.parse_unary_expr(allow_top_level_struct_expr)?;
            ExprKind::UnaryOp {
                operand: base,
                operator: UnaryOperator::Negative,
            }
        } else {
            return self.parse_function_call_and_field_access(allow_top_level_struct_expr);
        };

        Ok(self.ast.new_expr(expr))
    }

    fn parse_function_call_and_field_access(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr, ParserErr> {
        let mut acc = self.parse_primary_expr(allow_top_level_struct_expr)?;

        loop {
            if self.advance_if(Token::LParen) {
                // function call
                let mut arguments = Vec::new();
                while self.current() != Some(&Token::RParen) {
                    let argument = self.parse_expr(true)?; // Allow top-level struct expression in argument
                    arguments.push(argument);
                    if !self.advance_if(Token::Comma) {
                        break;
                    }
                }
                self.expect_token(Token::RParen)?;
                let args = self.ast.new_expr_slice(arguments);
                let new_acc = ExprKind::Call { callee: acc, args };
                acc = self.ast.new_expr(new_acc);
            } else if self.advance_if(Token::Dot) {
                // field access or method call
                if let Some(Token::NumLiteral(n)) = self.current() {
                    let index = n.parse().unwrap();
                    self.position += 1;
                    // indexed field access
                    let new_acc = ExprKind::FieldAccess {
                        obj: acc,
                        field: FieldDescriptor::Indexed(index),
                    };
                    acc = self.ast.new_expr(new_acc);
                } else {
                    let member = self.parse_path_segment(true)?;
                    if self.advance_if(Token::LParen) {
                        // method call
                        let mut args = Vec::new();
                        while self.current() != Some(&Token::RParen) {
                            let argument = self.parse_expr(true)?; // Allow top-level struct expression in argument
                            args.push(argument);
                            if !self.advance_if(Token::Comma) {
                                break;
                            }
                        }
                        self.expect_token(Token::RParen)?;
                        let args = self.ast.new_expr_slice(args);
                        let new_acc = ExprKind::MthdCall {
                            obj: acc,
                            mthd: member,
                            args,
                        };
                        acc = self.ast.new_expr(new_acc);
                    } else {
                        // named field access
                        let new_acc = ExprKind::FieldAccess {
                            obj: acc,
                            field: FieldDescriptor::Named(member),
                        };
                        acc = self.ast.new_expr(new_acc);
                    }
                }
            } else {
                break;
            }
        }

        Ok(acc)
    }

    fn parse_primary_expr(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr, ParserErr> {
        let current = self.current().ok_or(ParserErr::UnexpectedEOF)?;

        let expr = match current {
            Token::NumLiteral(value) => {
                let value = value.parse().map_err(|_| ParserErr::InvalidLiteral)?;
                self.position += 1;
                ExprKind::Lit(Lit::Int(value))
            }
            Token::BoolLiteral(b) => {
                let value = *b;
                self.position += 1;
                ExprKind::Lit(Lit::Bool(value))
            }
            Token::CCharLiteral(c) => {
                let value = *c;
                self.position += 1;
                ExprKind::Lit(Lit::CChar(value))
            }
            Token::CStringLiteral(s) => {
                let value = s.clone();
                self.position += 1;
                ExprKind::Lit(Lit::CString(value))
            }
            Token::Identifier(..) => {
                let path = self.parse_path(true)?;

                if allow_top_level_struct_expr && self.advance_if(Token::LBrace) {
                    // parse struct expr
                    let mut fields = Vec::new();
                    while let Some(Token::Identifier(field_name)) = self.current() {
                        let field_name = field_name.clone();
                        self.position += 1; // consume field name
                        self.expect_token(Token::Colon)?;
                        let field_value = self.parse_expr(true)?;
                        fields.push((field_name, field_value));
                        if !self.advance_if(Token::Comma) {
                            break;
                        }
                    }
                    self.expect_token(Token::RBrace)?;
                    ExprKind::Struct { ty_path: path, fields }
                } else {
                    ExprKind::Path(path)
                }
            }
            Token::LParen => {
                self.position += 1;

                let mut inner_exprs = Vec::new();
                let mut trailing_comma = true;
                while self.current() != Some(&Token::RParen) {
                    let inner_expr = self.parse_expr(true)?; // Allow top-level struct expression
                    inner_exprs.push(inner_expr);
                    if !self.advance_if(Token::Comma) {
                        trailing_comma = false;
                        break;
                    }
                }
                self.expect_token(Token::RParen)?;

                if inner_exprs.len() == 1 && !trailing_comma {
                    return Ok(inner_exprs.remove(0));
                } else {
                    let inner_exprs = self.ast.new_expr_slice(inner_exprs);
                    ExprKind::Tuple(inner_exprs)
                }
            }
            Token::LBrace => {
                let block = self.parse_block()?;
                ExprKind::Block(block)
            }
            Token::Keyword(Keyword::If) => return self.parse_if_expr(),
            Token::Keyword(Keyword::Loop) => return self.parse_loop(),
            Token::Keyword(Keyword::While) => return self.parse_while(),
            Token::Keyword(Keyword::Match) => return self.parse_match(),
            Token::Keyword(Keyword::Self_) => {
                self.position += 1;
                ExprKind::Self_
            }
            Token::Pipe => {
                self.position += 1;

                let mut params = Vec::new();
                while self.current() != Some(&Token::Pipe) {
                    let param = self.parse_closure_param()?;
                    params.push(param);
                    if !self.advance_if(Token::Comma) {
                        break;
                    }
                }
                self.expect_token(Token::Pipe)?;

                let return_ty = if self.advance_if(Token::Arrow) {
                    Some(self.parse_ty_annot()?)
                } else {
                    None
                };

                let body = self.parse_closure_body(return_ty.is_some())?;

                ExprKind::Closure {
                    params,
                    return_ty,
                    body,
                }
            }
            Token::PipePipe => {
                self.position += 1;

                let return_ty = if self.advance_if(Token::Arrow) {
                    Some(self.parse_ty_annot()?)
                } else {
                    None
                };

                let body = self.parse_closure_body(return_ty.is_some())?;

                ExprKind::Closure {
                    params: vec![],
                    return_ty,
                    body,
                }
            }
            Token::Smaller => {
                self.position += 1;
                let ty = self.parse_ty_annot()?;

                let trait_ = if self.advance_if(Token::Keyword(Keyword::As)) {
                    Some(self.parse_trait_annot()?)
                } else {
                    None
                };

                self.expect_token(Token::Greater)?;
                self.expect_token(Token::ColonColon)?;
                let path = self.parse_path(true)?;

                let qual_path = QualifiedPath { ty, trait_, path };

                ExprKind::QualifiedPath(qual_path)
            }

            token => return Err(ParserErr::UnexpectedToken(token.clone())),
        };

        Ok(self.ast.new_expr(expr))
    }

    fn parse_closure_param(&mut self) -> Result<ClosureParam, ParserErr> {
        let name = self.expect_identifier()?;
        let ty = if self.advance_if(Token::Colon) {
            Some(self.parse_ty_annot()?)
        } else {
            None
        };
        Ok(ClosureParam { name, ty })
    }

    fn parse_closure_body(&mut self, force_block: bool) -> Result<Block, ParserErr> {
        if self.current() == Some(&Token::LBrace) || force_block {
            self.parse_block()
        } else {
            let return_expr = self.parse_expr(true)?;
            let block = Block {
                return_expr: Some(return_expr),
                stmts: Vec::new(),
            };
            Ok(block)
        }
    }

    fn parse_path(&mut self, in_expression: bool) -> Result<Path, ParserErr> {
        let mut segments = Vec::new();

        loop {
            let segment = self.parse_path_segment(in_expression)?;
            segments.push(segment);

            if !self.advance_if(Token::ColonColon) {
                break;
            }
        }

        Ok(Path { segments })
    }

    fn parse_path_segment(&mut self, in_expression: bool) -> Result<PathSegment, ParserErr> {
        if self.advance_if(Token::Keyword(Keyword::SelfTy)) {
            return Ok(PathSegment::Self_);
        }

        let ident = self.expect_identifier()?;

        let segment = if (!in_expression && self.advance_if(Token::Smaller)) || self.advance_if_turbofish() {
            let mut gen_args = Vec::new();
            while self.current() != Some(&Token::Greater) {
                let gen_arg = self.parse_ty_annot()?;
                gen_args.push(gen_arg);
                if !self.advance_if(Token::Comma) {
                    break;
                }
            }
            self.expect_token(Token::Greater)?;
            PathSegment::Generic(GenPathSegment { ident, gen_args })
        } else {
            PathSegment::Ident(ident)
        };

        Ok(segment)
    }

    fn parse_if_expr(&mut self) -> Result<Expr, ParserErr> {
        self.expect_keyword(Keyword::If)?;
        let condition = self.parse_expr(false)?; // Don't allow top-level struct in if condition
        let then_block = self.parse_block()?;
        let else_block = if self.advance_if(Token::Keyword(Keyword::Else)) {
            Some(self.parse_block()?)
        } else {
            None
        };
        let expr = ExprKind::If {
            cond: condition,
            then: then_block,
            else_: else_block,
        };
        Ok(self.ast.new_expr(expr))
    }

    fn parse_loop(&mut self) -> Result<Expr, ParserErr> {
        self.expect_keyword(Keyword::Loop)?;
        let body = self.parse_block()?;
        let expr = ExprKind::Loop { body };
        Ok(self.ast.new_expr(expr))
    }

    fn parse_while(&mut self) -> Result<Expr, ParserErr> {
        self.expect_keyword(Keyword::While)?;
        let condition = self.parse_expr(false)?; // Don't allow top-level struct in while condition
        let body = self.parse_block()?;
        let expr = ExprKind::While { condition, body };
        Ok(self.ast.new_expr(expr))
    }

    fn parse_match(&mut self) -> Result<Expr, ParserErr> {
        self.expect_keyword(Keyword::Match)?;
        let scrutinee = self.parse_expr(false)?; // Don't allow top-level struct in match scrutinee

        let mut arms = Vec::new();
        self.expect_token(Token::LBrace)?;

        while Some(&Token::RBrace) != self.current() {
            let pattern = self.parse_struct_pattern()?;
            self.expect_token(Token::BoldArrow)?;
            let value = self.parse_expr(true)?; // Allow top-level struct expr in match arm body

            arms.push(MatchArm { pattern, value });

            if !self.advance_if(Token::Comma) {
                break;
            }
        }

        self.expect_token(Token::RBrace)?;

        let expr = ExprKind::Match { scrutinee, arms };
        Ok(self.ast.new_expr(expr))
    }

    fn parse_struct_pattern(&mut self) -> Result<StructPattern, ParserErr> {
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

    fn parse_ty_annot(&mut self) -> Result<TyAnnot, ParserErr> {
        let current = self.current().ok_or(ParserErr::UnexpectedEOF)?;
        match current {
            Token::Identifier(_) | Token::Keyword(Keyword::SelfTy) => {
                let path = self.parse_path(false)?;
                Ok(TyAnnot::Path(path))
            }
            Token::Ampersand => {
                self.position += 1;
                let inner_ty = self.parse_ty_annot()?;
                Ok(TyAnnot::Ref(Box::new(inner_ty)))
            }
            Token::AmpersandAmpersand => {
                // Two levels of reference
                self.position += 1;
                let inner_ty = self.parse_ty_annot()?;
                Ok(TyAnnot::Ref(Box::new(TyAnnot::Ref(Box::new(inner_ty)))))
            }
            Token::Asterisk => {
                self.position += 1;
                let inner_ty = self.parse_ty_annot()?;
                Ok(TyAnnot::Ptr(Box::new(inner_ty)))
            }
            Token::LParen => {
                self.position += 1;

                let mut inner_tys = Vec::new();
                let mut trailing_comma = true;
                while self.current() != Some(&Token::RParen) {
                    let inner_ty = self.parse_ty_annot()?;
                    inner_tys.push(inner_ty);
                    if !self.advance_if(Token::Comma) {
                        trailing_comma = false;
                        break;
                    }
                }
                self.expect_token(Token::RParen)?;

                if inner_tys.len() == 1 && !trailing_comma {
                    Ok(inner_tys.remove(0))
                } else {
                    Ok(TyAnnot::Tuple(inner_tys))
                }
            }
            Token::Keyword(Keyword::Fn) => {
                self.position += 1;
                self.expect_token(Token::LParen)?;
                let mut param_tys = Vec::new();
                while self.current() != Some(&Token::RParen) {
                    let param_type = self.parse_ty_annot()?;
                    param_tys.push(param_type);
                    if !self.advance_if(Token::Comma) {
                        break;
                    }
                }
                self.expect_token(Token::RParen)?;

                let return_ty = if self.advance_if(Token::Arrow) {
                    Some(Box::new(self.parse_ty_annot()?))
                } else {
                    None
                };

                Ok(TyAnnot::Fn { param_tys, return_ty })
            }
            Token::Underscore => {
                self.position += 1;
                Ok(TyAnnot::Wildcard)
            }
            token => Err(ParserErr::UnexpectedToken(token.clone())),
        }
    }

    fn parse_trait_annot(&mut self) -> Result<TraitAnnot, ParserErr> {
        let ty_annot = self.parse_ty_annot()?;
        match ty_annot {
            TyAnnot::Path(path) => match path.segments.as_slice() {
                [PathSegment::Ident(ident)] => Ok(TraitAnnot {
                    name: ident.clone(),
                    args: vec![],
                }),
                [PathSegment::Generic(GenPathSegment { ident, gen_args })] => Ok(TraitAnnot {
                    name: ident.clone(),
                    args: gen_args.clone(),
                }),
                _ => Err(ParserErr::ExpectedTraitName),
            },
            _ => Err(ParserErr::ExpectedTraitName),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn str_to_path(path: &str) -> Path {
        Path {
            segments: vec![PathSegment::Ident(path.to_string())],
        }
    }

    mod ast {
        use super::*;
        use pretty_assertions::assert_eq;

        fn parse_and_compare(input: &str, expected: Ast) {
            let mut ast = Ast::default();
            parse(input, &mut ast).expect("Failed to parse AST");
            assert_eq!(ast, expected);
        }

        #[test]
        fn test_empty_ast_items() {
            let input = r#"
                fn empty() {
                }

                struct Empty {
                }

                enum Empty {
                }

                impl Empty {
                }

                trait Empty {
                }
                "#;

            let expected = Ast {
                fns: vec![Fn {
                    name: "empty".to_string(),
                    gen_params: vec![],
                    params: vec![],
                    var_args: false,
                    return_ty: None,
                    constraints: vec![],
                    body: Some(Block {
                        stmts: vec![],
                        return_expr: None,
                    }),
                }],
                structs: vec![Struct {
                    name: "Empty".to_string(),
                    gen_params: vec![],
                    fields: vec![],
                }],
                enums: vec![Enum {
                    name: "Empty".to_string(),
                    gen_params: vec![],
                    variants: vec![],
                }],
                impls: vec![Impl {
                    gen_params: vec![],
                    ty: TyAnnot::Path(str_to_path("Empty")),
                    trait_annot: None,
                    mthds: vec![],
                    assoc_tys: vec![],
                }],
                traits: vec![Trait {
                    name: "Empty".to_string(),
                    gen_params: vec![],
                    mthds: vec![],
                    assoc_ty_names: vec![],
                }],
                ..Default::default()
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

            let expected = Ast {
                structs: vec![Struct {
                    name: "Point".to_string(),
                    gen_params: vec![],
                    fields: vec![
                        StructField {
                            name: "x".to_string(),
                            ty: TyAnnot::Path(str_to_path("int")),
                        },
                        StructField {
                            name: "y".to_string(),
                            ty: TyAnnot::Path(str_to_path("int")),
                        },
                    ],
                }],
                ..Default::default()
            };

            parse_and_compare(input, expected);
        }

        #[test]
        fn test_parse_simple_enum() {
            let input = r#"
                enum State {
                    Off {},
                    On {},
                    Unknown,
                }"#;

            let expected = Ast {
                enums: vec![Enum {
                    name: "State".to_string(),
                    gen_params: vec![],
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
                ..Default::default()
            };

            parse_and_compare(input, expected);
        }
    }

    mod block {
        use super::*;
        use pretty_assertions::assert_eq;

        fn parse_and_compare(input: &str, expected: Block) {
            let parsed = parse_block(input).expect("Failed to parse AST");
            assert_eq!(parsed, expected);
        }

        #[test]
        fn test_empty_block() {
            let input = "{}";

            let expected = Block {
                stmts: vec![],
                return_expr: None,
            };

            parse_and_compare(input, expected);
        }
    }
}
