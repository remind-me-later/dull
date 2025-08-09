use std::{iter::Peekable, mem, vec};

use crate::{
    error::{ParseError, ParseResult, Span},
    lex::{
        SpannedToken, Token, binary_number::BinaryNumber, decimal_number::DecimalNumber,
        keyword::Keyword, symbol::Symbol,
    },
    parse::{
        expression::{
            Expr, binary_op::BinaryOp, expr_inner::ExprInner, function::Function, lvalue::LValue,
            memory_area::MemoryArea, unary_op::UnaryOp,
        },
        line::Line,
        statement::{
            Assignment, BeepOptionalParams, DimInner, LCursorClause, LPrintInner, LPrintable,
            LetInner, LineInner, PrintInner, PrintSeparator, Printable, Statement, UsingClause,
        },
    },
};

pub mod expression;
pub mod line;
pub mod program;
pub mod statement;

pub struct Parser<I: Clone>
where
    I: Iterator<Item = SpannedToken>,
{
    tokens: Peekable<I>,
}

impl<I: Clone> Parser<I>
where
    I: Iterator<Item = SpannedToken>,
{
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    /// Helper method to peek at the token without the span
    fn peek_token(&mut self) -> &Token {
        self.tokens
            .peek()
            .map(|spanned| spanned.token())
            .unwrap_or(&Token::Symbol(Symbol::Eof))
    }

    fn peek_mut_token(&mut self) -> Option<&mut Token> {
        self.tokens.peek_mut().map(|spanned| spanned.token_mut())
    }

    /// Helper method to get the next token with span
    fn next_spanned(&mut self) -> Option<SpannedToken> {
        self.tokens.next()
    }

    /// Helper method to check if next token matches and consume it if so
    fn next_if_token_eq(&mut self, expected: &Token) -> Option<SpannedToken> {
        if self.peek_token() == expected {
            self.next_spanned()
        } else {
            None
        }
    }

    /// Helper method to conditionally consume token based on predicate  
    fn next_if_token(&mut self, predicate: impl Fn(&Token) -> bool) -> Option<SpannedToken> {
        if predicate(self.peek_token()) {
            self.next_spanned()
        } else {
            None
        }
    }

    /// Helper method to get the current span for error reporting
    fn current_span(&mut self) -> Span {
        self.tokens
            .peek()
            .map(|token| *token.span())
            .unwrap_or_else(|| Span::single(0)) // Default span if no tokens left
    }

    /// Helper method to expect a specific token and return an error if not found
    fn expect_token(&mut self, expected: &Token, context: &str) -> ParseResult<SpannedToken> {
        match self.next_spanned() {
            Some(token) if token.token() == expected => Ok(token),
            Some(token) => Err(ParseError::UnexpectedToken {
                expected: format!("{expected}"),
                found: format!("{}", token.token()),
                span: *token.span(),
            }),
            None => Err(ParseError::UnexpectedEndOfInput {
                expected: format!("{expected} ({context})"),
                span: self.current_span(),
            }),
        }
    }

    fn expect_expression(&mut self) -> ParseResult<Expr> {
        match self.parse_expression()? {
            Some(expr) => Ok(expr),
            None => Err(ParseError::ExpectedExpression {
                span: self.current_span(),
            }),
        }
    }

    fn expect_expression_factor(&mut self) -> ParseResult<Expr> {
        match self.parse_expression_factor()? {
            Some(expr) => Ok(expr),
            None => Err(ParseError::ExpectedExpression {
                span: self.current_span(),
            }),
        }
    }

    fn parse_exponent_expr(&mut self) -> ParseResult<Option<Expr>> {
        if let Some(mut left) = self.parse_expression_factor()? {
            while self.peek_token() == &Token::Symbol(Symbol::Exp) {
                self.next_spanned();
                let right = self.expect_expression_factor()?;
                left = Expr::new(ExprInner::Binary(
                    Box::new(left),
                    BinaryOp::Exp,
                    Box::new(right),
                ));
            }

            Ok(Some(left))
        } else {
            Ok(None)
        }
    }

    fn parse_unary_op_expr(&mut self) -> ParseResult<Option<Expr>> {
        let maybe_op = self.next_if_token(|token| {
            matches!(
                token,
                Token::Symbol(Symbol::Add) | Token::Symbol(Symbol::Sub)
            )
        });
        match maybe_op.as_ref().map(|s| s.token()) {
            Some(Token::Symbol(Symbol::Add)) => {
                let right = self.expect_unary_op_expr()?;
                Ok(Some(Expr::new(ExprInner::Unary(
                    UnaryOp::Plus,
                    Box::new(right),
                ))))
            }
            Some(Token::Symbol(Symbol::Sub)) => {
                let right = self.expect_unary_op_expr()?;
                Ok(Some(Expr::new(ExprInner::Unary(
                    UnaryOp::Minus,
                    Box::new(right),
                ))))
            }
            _ => Ok(self.parse_exponent_expr()?),
        }
    }

    fn expect_unary_op_expr(&mut self) -> ParseResult<Expr> {
        match self.parse_unary_op_expr()? {
            Some(expr) => Ok(expr),
            None => Err(ParseError::ExpectedExpression {
                span: self.current_span(),
            }),
        }
    }

    fn parse_mul_div_expr(&mut self) -> ParseResult<Option<Expr>> {
        if let Some(mut left) = self.parse_unary_op_expr()? {
            while let Some(op) = self.next_if_token(|token| {
                matches!(
                    token,
                    Token::Symbol(Symbol::Mul) | Token::Symbol(Symbol::Div)
                )
            }) {
                let right = self.expect_unary_op_expr()?;
                let binary_op = match op.token() {
                    Token::Symbol(Symbol::Mul) => BinaryOp::Mul,
                    Token::Symbol(Symbol::Div) => BinaryOp::Div,
                    _ => unreachable!(),
                };
                left = Expr::new(ExprInner::Binary(
                    Box::new(left),
                    binary_op,
                    Box::new(right),
                ));
            }

            Ok(Some(left))
        } else {
            Ok(None)
        }
    }

    fn expect_mul_div_expr(&mut self) -> ParseResult<Expr> {
        match self.parse_mul_div_expr()? {
            Some(expr) => Ok(expr),
            None => Err(ParseError::ExpectedExpression {
                span: self.current_span(),
            }),
        }
    }

    fn parse_add_sub_expr(&mut self) -> ParseResult<Option<Expr>> {
        if let Some(mut left) = self.parse_mul_div_expr()? {
            while let Some(op) = self.next_if_token(|token| {
                matches!(
                    token,
                    Token::Symbol(Symbol::Add) | Token::Symbol(Symbol::Sub)
                )
            }) {
                let right = self.expect_mul_div_expr()?;
                let binary_op = match op.token() {
                    Token::Symbol(Symbol::Add) => BinaryOp::Add,
                    Token::Symbol(Symbol::Sub) => BinaryOp::Sub,
                    _ => unreachable!(),
                };
                left = Expr::new(ExprInner::Binary(
                    Box::new(left),
                    binary_op,
                    Box::new(right),
                ));
            }

            Ok(Some(left))
        } else {
            Ok(None)
        }
    }

    fn expect_add_sub_expr(&mut self) -> ParseResult<Expr> {
        match self.parse_add_sub_expr()? {
            Some(expr) => Ok(expr),
            None => Err(ParseError::ExpectedExpression {
                span: self.current_span(),
            }),
        }
    }

    fn parse_comparison_expr(&mut self) -> ParseResult<Option<Expr>> {
        if let Some(mut left) = self.parse_add_sub_expr()? {
            while let Some(op) = self.next_if_token(|token| {
                matches!(
                    token,
                    Token::Symbol(Symbol::Eq)
                        | Token::Symbol(Symbol::Neq)
                        | Token::Symbol(Symbol::Lt)
                        | Token::Symbol(Symbol::Leq)
                        | Token::Symbol(Symbol::Gt)
                        | Token::Symbol(Symbol::Geq)
                )
            }) {
                let right = self.expect_add_sub_expr()?;
                let binary_op = match op.token() {
                    Token::Symbol(Symbol::Eq) => BinaryOp::Eq,
                    Token::Symbol(Symbol::Neq) => BinaryOp::Neq,
                    Token::Symbol(Symbol::Lt) => BinaryOp::Lt,
                    Token::Symbol(Symbol::Leq) => BinaryOp::Leq,
                    Token::Symbol(Symbol::Gt) => BinaryOp::Gt,
                    Token::Symbol(Symbol::Geq) => BinaryOp::Geq,
                    _ => unreachable!(),
                };
                left = Expr::new(ExprInner::Binary(
                    Box::new(left),
                    binary_op,
                    Box::new(right),
                ));
            }

            Ok(Some(left))
        } else {
            Ok(None)
        }
    }

    fn parse_unary_logical_expr(&mut self) -> ParseResult<Option<Expr>> {
        let maybe_op = self.next_if_token_eq(&Token::Keyword(Keyword::Not));
        match maybe_op {
            Some(_) => {
                let right = self.expect_unary_logical_expr()?;
                Ok(Some(Expr::new(ExprInner::Unary(
                    UnaryOp::Not,
                    Box::new(right),
                ))))
            }
            None => self.parse_comparison_expr(),
        }
    }

    fn expect_unary_logical_expr(&mut self) -> ParseResult<Expr> {
        match self.parse_unary_logical_expr()? {
            Some(expr) => Ok(expr),
            None => Err(ParseError::ExpectedExpression {
                span: self.current_span(),
            }),
        }
    }

    fn parse_and_or_expr(&mut self) -> ParseResult<Option<Expr>> {
        if let Some(mut left) = self.parse_unary_logical_expr()? {
            while let Some(op) = self.next_if_token(|token| {
                matches!(
                    token,
                    Token::Keyword(Keyword::Or) | Token::Keyword(Keyword::And)
                )
            }) {
                let right = self.expect_unary_logical_expr()?;
                left = Expr::new(ExprInner::Binary(
                    Box::new(left),
                    match op.token() {
                        Token::Keyword(Keyword::Or) => BinaryOp::Or,
                        Token::Keyword(Keyword::And) => BinaryOp::And,
                        _ => unreachable!(),
                    },
                    Box::new(right),
                ));
            }

            Ok(Some(left))
        } else {
            Ok(None)
        }
    }

    fn parse_expression(&mut self) -> ParseResult<Option<expression::Expr>> {
        self.parse_and_or_expr()
    }

    fn parse_binary_number(&mut self) -> Option<BinaryNumber> {
        match self.peek_mut_token()? {
            Token::BinaryNumber(h) => {
                let h = *h;
                self.tokens.next()?;
                Some(h)
            }
            _ => None,
        }
    }

    fn parse_decimal_number(&mut self) -> Option<DecimalNumber> {
        match self.peek_mut_token()? {
            Token::DecimalNumber(n) => {
                let n = *n;
                self.tokens.next()?;
                Some(n)
            }
            _ => None,
        }
    }

    fn parse_string_literal(&mut self) -> Option<String> {
        match self.peek_mut_token()? {
            Token::StringLiteral(s) => {
                let string = mem::take(s);
                self.tokens.next()?;
                Some(string)
            }
            _ => None,
        }
    }

    fn parse_expression_factor(&mut self) -> ParseResult<Option<Expr>> {
        if let Some(Token::Symbol(Symbol::LParen)) = self.peek_mut_token() {
            self.tokens.next(); // Consume '('
            let expr = self.expect_expression()?;
            self.expect_token(
                &Token::Symbol(Symbol::RParen),
                "Expected closing parenthesis",
            )?;
            return Ok(Some(expr));
        }

        if let Some(binary_number) = self.parse_binary_number() {
            return Ok(Some(Expr::new(ExprInner::BinaryNumber(binary_number))));
        }

        if let Some(decimal_number) = self.parse_decimal_number() {
            return Ok(Some(Expr::new(ExprInner::DecimalNumber(decimal_number))));
        }

        if let Some(string_literal) = self.parse_string_literal() {
            return Ok(Some(Expr::new(ExprInner::StringLiteral(string_literal))));
        }

        if let Some(lvalue) = self.parse_lvalue()? {
            return Ok(Some(Expr::new(ExprInner::LValue(lvalue))));
        }

        if let Some(function) = self.parse_function()? {
            return Ok(Some(Expr::new(ExprInner::FunctionCall(function))));
        }

        Ok(None)
    }

    fn parse_function(&mut self) -> ParseResult<Option<Function>> {
        match self.peek_token() {
            Token::Keyword(Keyword::Int) => {
                self.tokens.next();
                let expr = self.expect_expression_factor()?;
                Ok(Some(Function::Int {
                    expr: Box::new(expr),
                }))
            }
            Token::Keyword(Keyword::Sgn) => {
                self.tokens.next();
                let expr = self.expect_expression_factor()?;
                Ok(Some(Function::Sgn {
                    expr: Box::new(expr),
                }))
            }
            Token::Keyword(Keyword::Status) => {
                self.tokens.next();
                let arg = self.expect_expression_factor()?;
                Ok(Some(Function::Status { arg: Box::new(arg) }))
            }
            Token::Keyword(Keyword::Val) => {
                self.tokens.next();
                let expr = self.expect_expression_factor()?;
                Ok(Some(Function::Val {
                    expr: Box::new(expr),
                }))
            }
            Token::Keyword(Keyword::StrDollar) => {
                self.tokens.next();
                let expr = self.expect_expression_factor()?;
                Ok(Some(Function::Str {
                    expr: Box::new(expr),
                }))
            }
            Token::Keyword(Keyword::ChrDollar) => {
                self.tokens.next();
                let expr = self.expect_expression_factor()?;
                Ok(Some(Function::Chr {
                    expr: Box::new(expr),
                }))
            }
            Token::Keyword(Keyword::Abs) => {
                self.tokens.next();
                let expr = self.expect_expression_factor()?;
                Ok(Some(Function::Abs {
                    expr: Box::new(expr),
                }))
            }
            Token::Keyword(Keyword::Len) => {
                self.tokens.next();
                let expr = self.expect_expression_factor()?;
                Ok(Some(Function::Len {
                    expr: Box::new(expr),
                }))
            }
            Token::Keyword(Keyword::PeekMem0) => {
                self.tokens.next();
                let address = self.expect_expression_factor()?;
                Ok(Some(Function::Peek {
                    memory_area: MemoryArea::Me0,
                    address: Box::new(address),
                }))
            }
            Token::Keyword(Keyword::PeekMem1) => {
                self.tokens.next();
                let address = self.expect_expression_factor()?;
                Ok(Some(Function::Peek {
                    memory_area: MemoryArea::Me1,
                    address: Box::new(address),
                }))
            }
            Token::Keyword(Keyword::Ln) => {
                self.tokens.next();
                let expr = self.expect_expression_factor()?;
                Ok(Some(Function::Ln {
                    expr: Box::new(expr),
                }))
            }
            Token::Keyword(Keyword::Log) => {
                self.tokens.next();
                let expr = self.expect_expression_factor()?;
                Ok(Some(Function::Log {
                    expr: Box::new(expr),
                }))
            }
            Token::Keyword(Keyword::Dms) => {
                self.tokens.next();
                let expr = self.expect_expression_factor()?;
                Ok(Some(Function::Dms {
                    expr: Box::new(expr),
                }))
            }
            Token::Keyword(Keyword::Deg) => {
                self.tokens.next();
                let expr = self.expect_expression_factor()?;
                Ok(Some(Function::Deg {
                    expr: Box::new(expr),
                }))
            }
            Token::Keyword(Keyword::Tan) => {
                self.tokens.next();
                let expr = self.expect_expression_factor()?;
                Ok(Some(Function::Tan {
                    expr: Box::new(expr),
                }))
            }
            Token::Keyword(Keyword::Cos) => {
                self.tokens.next();
                let expr = self.expect_expression_factor()?;
                Ok(Some(Function::Cos {
                    expr: Box::new(expr),
                }))
            }
            Token::Keyword(Keyword::Sin) => {
                self.tokens.next();
                let expr = self.expect_expression_factor()?;
                Ok(Some(Function::Sin {
                    expr: Box::new(expr),
                }))
            }
            Token::Keyword(Keyword::Sqr) => {
                self.tokens.next();
                let expr = self.expect_expression_factor()?;
                Ok(Some(Function::Sqr {
                    expr: Box::new(expr),
                }))
            }
            Token::Keyword(Keyword::MidDollar) => {
                self.tokens.next();
                // Parse '('
                self.expect_token(&Token::Symbol(Symbol::LParen), "Expected '(' after MID$")?;
                let string = self.expect_expression()?;

                self.expect_token(
                    &Token::Symbol(Symbol::Comma),
                    "Expected ',' after MID$ string",
                )?;
                let start = self.expect_expression()?;

                self.expect_token(
                    &Token::Symbol(Symbol::Comma),
                    "Expected ',' after MID$ start",
                )?;
                let length = self.expect_expression()?;

                self.expect_token(
                    &Token::Symbol(Symbol::RParen),
                    "Expected ')' after MID$ length",
                )?;
                Ok(Some(Function::Mid {
                    string: Box::new(string),
                    start: Box::new(start),
                    length: Box::new(length),
                }))
            }
            Token::Keyword(Keyword::LeftDollar) => {
                self.tokens.next();
                // Parse '('
                self.expect_token(&Token::Symbol(Symbol::LParen), "Expected '(' after LEFT$")?;
                let string = self.expect_expression()?;
                // Parse ','
                self.expect_token(
                    &Token::Symbol(Symbol::Comma),
                    "Expected ',' after LEFT$ string",
                )?;
                let length = self.expect_expression()?;
                self.expect_token(
                    &Token::Symbol(Symbol::RParen),
                    "Expected ')' after LEFT$ length",
                )?;
                Ok(Some(Function::Left {
                    string: Box::new(string),
                    length: Box::new(length),
                }))
            }
            Token::Keyword(Keyword::RightDollar) => {
                self.tokens.next();
                // Parse '('
                self.expect_token(&Token::Symbol(Symbol::LParen), "Expected '(' after RIGHT$")?;
                let string = self.expect_expression()?;
                self.expect_token(
                    &Token::Symbol(Symbol::Comma),
                    "Expected ',' after RIGHT$ string",
                )?;
                let length = self.expect_expression()?;
                self.expect_token(
                    &Token::Symbol(Symbol::RParen),
                    "Expected ')' after RIGHT$ length",
                )?;
                Ok(Some(Function::Right {
                    string: Box::new(string),
                    length: Box::new(length),
                }))
            }
            Token::Keyword(Keyword::Asc) => {
                self.tokens.next();
                let expr = self.expect_expression_factor()?;
                Ok(Some(Function::Asc {
                    expr: Box::new(expr),
                }))
            }
            Token::Keyword(Keyword::Point) => {
                self.tokens.next();
                let position = self.expect_expression_factor()?;
                Ok(Some(Function::Point {
                    position: Box::new(position),
                }))
            }
            Token::Keyword(Keyword::Rnd) => {
                self.tokens.next();
                let range_end = self.expect_expression_factor()?;
                Ok(Some(Function::Rnd {
                    range_end: Box::new(range_end),
                }))
            }
            _ => Ok(None),
        }
    }

    fn parse_lvalue(&mut self) -> ParseResult<Option<LValue>> {
        match self.peek_token() {
            Token::Identifier(identifier) => {
                let identifier = *identifier;
                self.tokens.next();

                // Check for array access
                if self.peek_token() == &Token::Symbol(Symbol::LParen) {
                    self.tokens.next(); // Consume '('
                    let index = self.expect_expression()?;

                    if self.peek_token() == &Token::Symbol(Symbol::Comma) {
                        self.tokens.next(); // Consume ','
                        let col_index = self.expect_expression()?;
                        self.expect_token(
                            &Token::Symbol(Symbol::RParen),
                            "Expected ')' after 2D array indices",
                        )?;
                        return Ok(Some(LValue::Array2DAccess {
                            identifier,
                            row_index: Box::new(index),
                            col_index: Box::new(col_index),
                        }));
                    }

                    self.expect_token(
                        &Token::Symbol(Symbol::RParen),
                        "Expected ')' after 1D array index",
                    )?;
                    return Ok(Some(LValue::Array1DAccess {
                        identifier,
                        index: Box::new(index),
                    }));
                }

                Ok(Some(LValue::Identifier(identifier)))
            }
            Token::Symbol(Symbol::At) => {
                self.tokens.next();

                let has_dollar = self
                    .next_if_token_eq(&Token::Symbol(Symbol::Dollar))
                    .is_some();

                self.expect_token(&Token::Symbol(Symbol::LParen), "Expected '(' after @")?;
                let index = self.expect_expression()?;
                self.expect_token(
                    &Token::Symbol(Symbol::RParen),
                    "Expected ')' after memory area index",
                )?;

                Ok(Some(LValue::FixedMemoryAreaAccess {
                    index: Box::new(index),
                    has_dollar,
                }))
            }
            Token::Keyword(Keyword::Time) => {
                self.tokens.next();
                Ok(Some(LValue::BuiltInIdentifier(Keyword::Time)))
            }
            Token::Keyword(Keyword::InkeyDollar) => {
                self.tokens.next();
                Ok(Some(LValue::BuiltInIdentifier(Keyword::InkeyDollar)))
            }
            _ => Ok(None),
        }
    }

    fn expect_lvalue(&mut self) -> ParseResult<LValue> {
        match self.parse_lvalue()? {
            Some(lvalue) => Ok(lvalue),
            None => Err(ParseError::ExpectedLValue {
                span: self.current_span(),
            }),
        }
    }

    fn parse_assignment(&mut self) -> ParseResult<Option<Assignment>> {
        if let Some(lhs) = self.parse_lvalue()? {
            self.expect_token(&Token::Symbol(Symbol::Eq), "Expected '=' in assignment")?;
            let rhs = self.expect_expression()?;
            Ok(Some(Assignment {
                lvalue: lhs,
                expr: Box::new(rhs),
            }))
        } else {
            Ok(None)
        }
    }

    fn expect_assignment(&mut self) -> ParseResult<Assignment> {
        match self.parse_assignment()? {
            Some(assignment) => Ok(assignment),
            None => Err(ParseError::ExpectedAssignment {
                span: self.current_span(),
            }),
        }
    }

    fn parse_let_inner(&mut self, is_let_mandatory: bool) -> ParseResult<Option<LetInner>> {
        let let_kw = self.next_if_token_eq(&Token::Keyword(Keyword::Let));

        if is_let_mandatory {
            if let_kw.is_none() {
                return Ok(None);
            }

            let mut assignments = vec![];

            loop {
                let assignment = self.expect_assignment()?;
                assignments.push(assignment);
                if self.peek_token() == &Token::Symbol(Symbol::Comma) {
                    self.tokens.next(); // Consume ','
                } else {
                    break;
                }
            }

            Ok(Some(LetInner { assignments }))
        } else if let Some(first_assignment) = self.parse_assignment()? {
            let mut assignments = vec![first_assignment];

            if self.peek_token() == &Token::Symbol(Symbol::Comma) {
                self.tokens.next(); // Consume ',' {
                loop {
                    let assignment = self.expect_assignment()?;
                    assignments.push(assignment);
                    if self.peek_token() == &Token::Symbol(Symbol::Comma) {
                        self.tokens.next(); // Consume ','
                    } else {
                        break;
                    }
                }
            }

            Ok(Some(LetInner { assignments }))
        } else {
            Ok(None)
        }
    }

    fn parse_using_clause(&mut self) -> Option<UsingClause> {
        self.next_if_token_eq(&Token::Keyword(Keyword::Using))?;
        let format = self.parse_string_literal();
        Some(UsingClause { format })
    }

    fn parse_print_separator(&mut self) -> PrintSeparator {
        match self.peek_token() {
            Token::Symbol(Symbol::Comma) => {
                self.tokens.next();
                PrintSeparator::Comma
            }
            Token::Symbol(Symbol::Semicolon) => {
                self.tokens.next();
                PrintSeparator::Semicolon
            }
            _ => PrintSeparator::Empty,
        }
    }

    fn parse_print_inner(&mut self) -> ParseResult<PrintInner> {
        let mut exprs = Vec::new();

        loop {
            if let Some(expr) = self.parse_expression()? {
                let print_separator = self.parse_print_separator();
                exprs.push((Printable::Expr(expr), print_separator));
            } else if let Some(using_clause) = self.parse_using_clause() {
                let print_separator = self.parse_print_separator();
                exprs.push((Printable::UsingClause(using_clause), print_separator));
            } else {
                break;
            }
        }

        Ok(PrintInner { exprs })
    }

    fn parse_print_pause_stmt(&mut self) -> ParseResult<Option<Statement>> {
        match self.peek_token() {
            Token::Keyword(Keyword::Print) => {
                self.tokens.next();
                let print_inner = self.parse_print_inner()?;
                Ok(Some(Statement::Print { inner: print_inner }))
            }
            Token::Keyword(Keyword::Pause) => {
                self.tokens.next();
                let pause_inner = self.parse_print_inner()?;
                Ok(Some(Statement::Pause { inner: pause_inner }))
            }
            _ => Ok(None),
        }
    }

    fn parse_end_statement(&mut self) -> Option<Statement> {
        self.next_if_token_eq(&Token::Keyword(Keyword::End))?;
        Some(Statement::End)
    }

    fn parse_dim_inner(&mut self) -> ParseResult<Option<DimInner>> {
        match self.peek_token() {
            Token::Identifier(identifier) => {
                let identifier = *identifier;
                self.tokens.next();
                self.expect_token(
                    &Token::Symbol(Symbol::LParen),
                    "Expected '(' after identifier in DIM statement",
                )?;
                let rows = self.expect_expression()?;
                let maybe_cols = if self
                    .next_if_token_eq(&Token::Symbol(Symbol::Comma))
                    .is_some()
                {
                    Some(self.expect_expression()?)
                } else {
                    None
                };

                self.expect_token(
                    &Token::Symbol(Symbol::RParen),
                    "Expected ')' after dimensions in DIM statement",
                )?;

                let string_length = if self.next_if_token_eq(&Token::Symbol(Symbol::Mul)).is_some()
                {
                    Some(self.expect_expression()?)
                } else {
                    None
                };

                match maybe_cols {
                    Some(cols) => Ok(Some(DimInner::DimInner2D {
                        identifier,
                        rows,
                        cols,
                        string_length,
                    })),
                    None => Ok(Some(DimInner::DimInner1D {
                        identifier,
                        size: rows,
                        string_length,
                    })),
                }
            }
            _ => Ok(None),
        }
    }

    fn expect_dim_inner(&mut self) -> ParseResult<DimInner> {
        match self.parse_dim_inner()? {
            Some(dim_inner) => Ok(dim_inner),
            None => Err(ParseError::ExpectedDimInner {
                span: self.current_span(),
            }),
        }
    }

    fn parse_dim_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Dim))
            .is_some()
        {
            let mut decls = vec![];

            loop {
                let decl = self.expect_dim_inner()?;
                decls.push(decl);

                if self
                    .next_if_token_eq(&Token::Symbol(Symbol::Comma))
                    .is_none()
                {
                    break;
                }
            }

            Ok(Some(Statement::Dim { decls }))
        } else {
            Ok(None)
        }
    }

    fn parse_statement(&mut self, is_let_mandatory: bool) -> ParseResult<Option<Statement>> {
        if let Some(stmt) = self.parse_print_pause_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_using_clause() {
            return Ok(Some(Statement::Using { using_clause: stmt }));
        }

        if let Some(stmt) = self.parse_end_statement() {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_if_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_input_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_remark_stmt() {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_for_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_next_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_clear_stmt() {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_goto_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_gosub_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_on_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_wait_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_cls_stmt() {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_random_stmt() {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_gprint_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_gcursor_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_cursor_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_beep_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_return_stmt() {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_poke_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_dim_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_read_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_data_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_restore_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_arun_stmt() {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_lock_stmt() {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_unlock_stmt() {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_call_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_text_stmt() {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_graph_stmt() {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_line_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_rline_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_color_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_csize_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_lf_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_radian_stmt() {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_lcursor_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_lprint_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_glcursor_stmt()? {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_sorgn_stmt() {
            return Ok(Some(stmt));
        }

        if let Some(stmt) = self.parse_let_inner(is_let_mandatory)? {
            return Ok(Some(Statement::Let { inner: stmt }));
        }

        Ok(None)
    }

    fn parse_sorgn_stmt(&mut self) -> Option<Statement> {
        self.next_if_token_eq(&Token::Keyword(Keyword::Sorgn))?;
        Some(Statement::Sorgn)
    }

    fn parse_if_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::If))
            .is_some()
        {
            let condition = self.expect_expression()?;
            self.next_if_token_eq(&Token::Keyword(Keyword::Then)); // optional
            if let Some(then_stmt) = self.parse_statement(true)? {
                Ok(Some(Statement::If {
                    condition,
                    then_stmt: Box::new(then_stmt),
                }))
            } else {
                let goto_expr = self.expect_expression()?;
                Ok(Some(Statement::If {
                    condition,
                    then_stmt: Box::new(Statement::Goto { target: goto_expr }),
                }))
            }
        } else {
            Ok(None)
        }
    }

    fn parse_input_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Input))
            .is_some()
        {
            let mut input_exprs = vec![];

            loop {
                // Check for prompt string
                let prompt = if let Some(s) = self.parse_string_literal() {
                    self.expect_token(
                        &Token::Symbol(Symbol::Semicolon),
                        "Expected semicolon after input prompt",
                    )?;
                    Some(s)
                } else {
                    None
                };

                let lvalue = self.expect_lvalue()?;
                input_exprs.push((prompt, lvalue));

                if self
                    .next_if_token_eq(&Token::Symbol(Symbol::Comma))
                    .is_none()
                {
                    break;
                }
            }

            Ok(Some(Statement::Input { input_exprs }))
        } else {
            Ok(None)
        }
    }

    fn parse_remark_stmt(&mut self) -> Option<Statement> {
        match self.peek_mut_token()? {
            Token::Remark(s) => {
                let s = mem::take(s);
                self.tokens.next();
                Some(Statement::Remark {
                    text: s.to_string(),
                })
            }
            _ => None,
        }
    }

    fn parse_for_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::For))
            .is_some()
        {
            let assignment = self.expect_assignment()?;
            self.expect_token(
                &Token::Keyword(Keyword::To),
                "Expected 'TO' after FOR assignment",
            )?;
            let to_expr = self.expect_expression()?;
            let step_expr = if self
                .next_if_token_eq(&Token::Keyword(Keyword::Step))
                .is_some()
            {
                Some(self.expect_expression()?)
            } else {
                None
            };

            Ok(Some(Statement::For {
                assignment,
                to_expr,
                step_expr,
            }))
        } else {
            Ok(None)
        }
    }

    fn parse_next_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Next))
            .is_some()
        {
            let lvalue = self.expect_lvalue()?;
            Ok(Some(Statement::Next { lvalue }))
        } else {
            Ok(None)
        }
    }

    fn parse_clear_stmt(&mut self) -> Option<Statement> {
        self.next_if_token_eq(&Token::Keyword(Keyword::Clear))?;
        Some(Statement::Clear)
    }

    fn parse_goto_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Goto))
            .is_some()
        {
            let target = self.expect_expression()?;
            Ok(Some(Statement::Goto { target }))
        } else {
            Ok(None)
        }
    }

    fn parse_gosub_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Gosub))
            .is_some()
        {
            let target = self.expect_expression()?;
            Ok(Some(Statement::Gosub { target }))
        } else {
            Ok(None)
        }
    }

    fn parse_on_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::On))
            .is_some()
        {
            if self
                .next_if_token_eq(&Token::Keyword(Keyword::Error))
                .is_some()
            {
                self.expect_token(
                    &Token::Keyword(Keyword::Goto),
                    "Expected 'GOTO' after 'ON ERROR'",
                )?;

                let expr = self.expect_expression()?;
                return Ok(Some(Statement::OnErrorGoto { target: expr }));
            }

            let expr = self.expect_expression()?;

            if self
                .next_if_token_eq(&Token::Keyword(Keyword::Goto))
                .is_some()
            {
                let mut targets = vec![self.expect_expression()?];
                while self
                    .next_if_token_eq(&Token::Symbol(Symbol::Comma))
                    .is_some()
                {
                    targets.push(self.expect_expression()?);
                }
                Ok(Some(Statement::OnGoto { expr, targets }))
            } else if self
                .next_if_token_eq(&Token::Keyword(Keyword::Gosub))
                .is_some()
            {
                let mut targets = vec![self.expect_expression()?];
                while self
                    .next_if_token_eq(&Token::Symbol(Symbol::Comma))
                    .is_some()
                {
                    targets.push(self.expect_expression()?);
                }
                Ok(Some(Statement::OnGosub { expr, targets }))
            } else {
                Err(ParseError::ExpectedGotoOrGosub {
                    span: self.current_span(),
                })
            }
        } else {
            Ok(None)
        }
    }

    fn parse_wait_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Wait))
            .is_some()
        {
            let expr = self.parse_expression()?;
            return Ok(Some(Statement::Wait { expr }));
        }
        Ok(None)
    }

    fn parse_cls_stmt(&mut self) -> Option<Statement> {
        self.next_if_token_eq(&Token::Keyword(Keyword::Cls))?;
        Some(Statement::Cls)
    }

    fn parse_random_stmt(&mut self) -> Option<Statement> {
        self.next_if_token_eq(&Token::Keyword(Keyword::Random))?;
        Some(Statement::Random)
    }

    fn parse_gprint_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Gprint))
            .is_some()
        {
            let mut exprs = vec![];

            while let Some(expr) = self.parse_expression()? {
                let print_separator = self.parse_print_separator();
                exprs.push((expr, print_separator));
            }

            Ok(Some(Statement::Gprint { exprs }))
        } else {
            Ok(None)
        }
    }

    fn parse_gcursor_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Gcursor))
            .is_some()
        {
            let expr = self.expect_expression()?;
            return Ok(Some(Statement::GCursor { expr }));
        }
        Ok(None)
    }

    fn parse_cursor_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Cursor))
            .is_some()
        {
            let expr = self.expect_expression()?;
            return Ok(Some(Statement::Cursor { expr }));
        }
        Ok(None)
    }

    fn parse_beep_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Beep))
            .is_some()
        {
            // Check for BEEP ON/OFF
            if self.peek_token() == &Token::Keyword(Keyword::On) {
                self.tokens.next();
                return Ok(Some(Statement::BeepOnOff {
                    switch_beep_on: true,
                }));
            }
            if self.peek_token() == &Token::Keyword(Keyword::Off) {
                self.tokens.next();
                return Ok(Some(Statement::BeepOnOff {
                    switch_beep_on: false,
                }));
            }

            // Parse BEEP repetitions[,frequency[,duration]]
            let repetitions_expr = self.expect_expression()?;

            let optional_params = if self
                .next_if_token_eq(&Token::Symbol(Symbol::Comma))
                .is_some()
            {
                let frequency = self.expect_expression()?;
                let duration = if self
                    .next_if_token_eq(&Token::Symbol(Symbol::Comma))
                    .is_some()
                {
                    Some(self.expect_expression()?)
                } else {
                    None
                };
                Some(BeepOptionalParams {
                    frequency,
                    duration,
                })
            } else {
                None
            };

            Ok(Some(Statement::Beep {
                repetitions_expr,
                optional_params,
            }))
        } else {
            Ok(None)
        }
    }

    fn parse_return_stmt(&mut self) -> Option<Statement> {
        self.next_if_token_eq(&Token::Keyword(Keyword::Return))?;
        Some(Statement::Return)
    }

    fn parse_poke_stmt(&mut self) -> ParseResult<Option<Statement>> {
        let memory_area = match self.peek_token() {
            Token::Keyword(Keyword::PokeMem0) => {
                self.tokens.next();
                MemoryArea::Me0
            }
            Token::Keyword(Keyword::PokeMem1) => {
                self.tokens.next();
                MemoryArea::Me1
            }
            _ => return Ok(None),
        };

        let mut exprs = vec![self.expect_expression()?];
        while self
            .next_if_token_eq(&Token::Symbol(Symbol::Comma))
            .is_some()
        {
            exprs.push(self.expect_expression()?);
        }

        Ok(Some(Statement::Poke { memory_area, exprs }))
    }

    fn parse_read_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Read))
            .is_some()
        {
            let mut destinations = vec![self.expect_lvalue()?];
            while self
                .next_if_token_eq(&Token::Symbol(Symbol::Comma))
                .is_some()
            {
                destinations.push(self.expect_lvalue()?);
            }
            return Ok(Some(Statement::Read { destinations }));
        }
        Ok(None)
    }

    fn parse_data_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Data))
            .is_some()
        {
            let mut exprs = vec![self.expect_expression()?];
            while self
                .next_if_token_eq(&Token::Symbol(Symbol::Comma))
                .is_some()
            {
                exprs.push(self.expect_expression()?);
            }
            return Ok(Some(Statement::Data(exprs)));
        }
        Ok(None)
    }

    fn parse_restore_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Restore))
            .is_some()
        {
            let expr = self.parse_expression()?;
            return Ok(Some(Statement::Restore { expr }));
        }
        Ok(None)
    }

    fn parse_arun_stmt(&mut self) -> Option<Statement> {
        self.next_if_token_eq(&Token::Keyword(Keyword::Arun))?;
        Some(Statement::Arun)
    }

    fn parse_lock_stmt(&mut self) -> Option<Statement> {
        self.next_if_token_eq(&Token::Keyword(Keyword::Lock))?;
        Some(Statement::Lock)
    }

    fn parse_unlock_stmt(&mut self) -> Option<Statement> {
        self.next_if_token_eq(&Token::Keyword(Keyword::Unlock))?;
        Some(Statement::Unlock)
    }

    fn parse_call_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Call))
            .is_some()
        {
            let expr = self.expect_expression()?;
            let variable = if self
                .next_if_token_eq(&Token::Symbol(Symbol::Comma))
                .is_some()
            {
                Some(self.expect_lvalue()?)
            } else {
                None
            };
            return Ok(Some(Statement::Call { expr, variable }));
        }
        Ok(None)
    }

    fn parse_text_stmt(&mut self) -> Option<Statement> {
        self.next_if_token_eq(&Token::Keyword(Keyword::Text))?;
        Some(Statement::Text)
    }

    fn parse_graph_stmt(&mut self) -> Option<Statement> {
        self.next_if_token_eq(&Token::Keyword(Keyword::Graph))?;
        Some(Statement::Graph)
    }

    fn parse_line_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Line))
            .is_none()
        {
            return Ok(None);
        }

        let mut start_point = None;
        let mut end_points = Vec::new();

        // Check if it starts with '-' (omit first point, use current position)
        if self.peek_token() == &Token::Symbol(Symbol::Sub) {
            self.next_spanned(); // consume the '-'

            self.expect_token(
                &Token::Symbol(Symbol::LParen),
                "expected '(' for coordinates",
            )?;
            let x = self.expect_expression()?;
            self.expect_token(&Token::Symbol(Symbol::Comma), "expected ',' in coordinates")?;
            let y = self.expect_expression()?;
            self.expect_token(
                &Token::Symbol(Symbol::RParen),
                "expected ')' after coordinates",
            )?;
            end_points.push((x, y));
        } else if self.peek_token() == &Token::Symbol(Symbol::LParen) {
            // Parse the first point explicitly
            self.expect_token(
                &Token::Symbol(Symbol::LParen),
                "expected '(' for coordinates",
            )?;
            let x = self.expect_expression()?;
            self.expect_token(&Token::Symbol(Symbol::Comma), "expected ',' in coordinates")?;
            let y = self.expect_expression()?;
            self.expect_token(
                &Token::Symbol(Symbol::RParen),
                "expected ')' after coordinates",
            )?;
            start_point = Some((x, y));
        }

        // Parse subsequent points preceded by '-'
        while self.peek_token() == &Token::Symbol(Symbol::Sub) {
            self.next_spanned(); // consume the '-'

            self.expect_token(
                &Token::Symbol(Symbol::LParen),
                "expected '(' for coordinates",
            )?;
            let x = self.expect_expression()?;
            self.expect_token(&Token::Symbol(Symbol::Comma), "expected ',' in coordinates")?;
            let y = self.expect_expression()?;
            self.expect_token(
                &Token::Symbol(Symbol::RParen),
                "expected ')' after coordinates",
            )?;
            end_points.push((x, y));
        }

        // Parse optional line-type parameter
        let line_type = if self.peek_token() == &Token::Symbol(Symbol::Comma) {
            self.next_spanned(); // consume comma
            if self.peek_token() == &Token::Symbol(Symbol::Comma) {
                // Empty line-type parameter
                None
            } else {
                Some(self.expect_expression()?)
            }
        } else {
            None
        };

        // Parse optional color parameter
        let color = if self.peek_token() == &Token::Symbol(Symbol::Comma) {
            self.next_spanned(); // consume comma
            if self.peek_token() == &Token::Symbol(Symbol::Comma)
                || matches!(self.peek_token(), Token::Identifier(_))
                || matches!(
                    self.peek_token(),
                    Token::Symbol(Symbol::Newline) | Token::Symbol(Symbol::Eof)
                )
            {
                // Empty color parameter or end of statement
                None
            } else {
                Some(self.expect_expression()?)
            }
        } else {
            None
        };

        // Parse optional box parameter (B)
        let is_box = if self.peek_token() == &Token::Symbol(Symbol::Comma) {
            self.next_spanned(); // consume comma
            if let Some(Token::Identifier(id)) = self.peek_mut_token() {
                if id.to_string().to_uppercase() == "B" {
                    self.next_spanned(); // consume B
                    true
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        };

        let inner = LineInner {
            start_point,
            end_points,
            line_type,
            color,
            is_box,
        };

        Ok(Some(Statement::Line { inner }))
    }

    fn parse_rline_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Rline))
            .is_none()
        {
            return Ok(None);
        }

        let mut start_point = None;
        let mut end_points = Vec::new();

        // Check if it starts with '-' (omit first point, use current position)
        if self.peek_token() == &Token::Symbol(Symbol::Sub) {
            self.next_spanned(); // consume the '-'

            self.expect_token(
                &Token::Symbol(Symbol::LParen),
                "expected '(' for coordinates",
            )?;
            let x = self.expect_expression()?;
            self.expect_token(&Token::Symbol(Symbol::Comma), "expected ',' in coordinates")?;
            let y = self.expect_expression()?;
            self.expect_token(
                &Token::Symbol(Symbol::RParen),
                "expected ')' after coordinates",
            )?;
            end_points.push((x, y));
        } else if self.peek_token() == &Token::Symbol(Symbol::LParen) {
            // Parse the first point explicitly
            self.expect_token(
                &Token::Symbol(Symbol::LParen),
                "expected '(' for coordinates",
            )?;
            let x = self.expect_expression()?;
            self.expect_token(&Token::Symbol(Symbol::Comma), "expected ',' in coordinates")?;
            let y = self.expect_expression()?;
            self.expect_token(
                &Token::Symbol(Symbol::RParen),
                "expected ')' after coordinates",
            )?;
            start_point = Some((x, y));
        }

        // Parse subsequent points preceded by '-'
        while self.peek_token() == &Token::Symbol(Symbol::Sub) {
            self.next_spanned(); // consume the '-'

            self.expect_token(
                &Token::Symbol(Symbol::LParen),
                "expected '(' for coordinates",
            )?;
            let x = self.expect_expression()?;
            self.expect_token(&Token::Symbol(Symbol::Comma), "expected ',' in coordinates")?;
            let y = self.expect_expression()?;
            self.expect_token(
                &Token::Symbol(Symbol::RParen),
                "expected ')' after coordinates",
            )?;
            end_points.push((x, y));
        }

        // Parse optional line-type parameter
        let line_type = if self.peek_token() == &Token::Symbol(Symbol::Comma) {
            self.next_spanned(); // consume comma
            if self.peek_token() == &Token::Symbol(Symbol::Comma) {
                // Empty line-type parameter
                None
            } else {
                Some(self.expect_expression()?)
            }
        } else {
            None
        };

        // Parse optional color parameter
        let color = if self.peek_token() == &Token::Symbol(Symbol::Comma) {
            self.next_spanned(); // consume comma
            if self.peek_token() == &Token::Symbol(Symbol::Comma)
                || matches!(self.peek_token(), Token::Identifier(_))
                || matches!(
                    self.peek_token(),
                    Token::Symbol(Symbol::Newline) | Token::Symbol(Symbol::Eof)
                )
            {
                // Empty color parameter or end of statement
                None
            } else {
                Some(self.expect_expression()?)
            }
        } else {
            None
        };

        // Parse optional box parameter (B)
        let is_box = if self.peek_token() == &Token::Symbol(Symbol::Comma) {
            self.next_spanned(); // consume comma
            if let Some(Token::Identifier(id)) = self.peek_mut_token() {
                if id.to_string().to_uppercase() == "B" {
                    self.next_spanned(); // consume B
                    true
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        };

        let inner = LineInner {
            start_point,
            end_points,
            line_type,
            color,
            is_box,
        };

        Ok(Some(Statement::RLine { inner }))
    }

    fn parse_color_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Color))
            .is_some()
        {
            let color = self.expect_expression()?;
            return Ok(Some(Statement::Color { expr: color }));
        }
        Ok(None)
    }

    fn parse_csize_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Csize))
            .is_some()
        {
            let expr = self.expect_expression()?;
            return Ok(Some(Statement::CSize { expr }));
        }
        Ok(None)
    }

    fn parse_lf_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Lf))
            .is_some()
        {
            let expr = self.expect_expression()?;
            return Ok(Some(Statement::Lf { expr }));
        }
        Ok(None)
    }

    fn parse_radian_stmt(&mut self) -> Option<Statement> {
        self.next_if_token_eq(&Token::Keyword(Keyword::Radian))?;
        Some(Statement::Radian)
    }

    fn parse_lcursor_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Lcursor))
            .is_some()
        {
            let expr = self.expect_expression()?;
            return Ok(Some(Statement::LCursor(LCursorClause { expr })));
        }
        Ok(None)
    }

    fn parse_lprint_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Lprint))
            .is_some()
        {
            let inner = self.parse_lprint_inner()?;
            return Ok(Some(Statement::LPrint { inner }));
        }
        Ok(None)
    }

    fn parse_lprint_inner(&mut self) -> ParseResult<LPrintInner> {
        let mut exprs = vec![];

        loop {
            let printable = if self.peek_token() == (&Token::Keyword(Keyword::Tab)) {
                self.tokens.next();
                let expr = self.expect_expression()?;
                LPrintable::LCursorClause(LCursorClause { expr })
            } else if let Some(expr) = self.parse_expression()? {
                LPrintable::Expr(expr)
            } else {
                break;
            };

            let print_separator = self.parse_print_separator();
            let is_empty = matches!(print_separator, PrintSeparator::Empty);
            exprs.push((printable, print_separator));

            if is_empty {
                break;
            }
        }

        Ok(LPrintInner { exprs })
    }

    fn parse_glcursor_stmt(&mut self) -> ParseResult<Option<Statement>> {
        if self
            .next_if_token_eq(&Token::Keyword(Keyword::Glcursor))
            .is_some()
        {
            self.expect_token(
                &Token::Symbol(Symbol::LParen),
                "Expected '(' after GLCURSOR",
            )?;
            let x_expr = self.expect_expression()?;
            self.expect_token(
                &Token::Symbol(Symbol::Comma),
                "Expected ',' between GLCURSOR coordinates",
            )?;
            let y_expr = self.expect_expression()?;
            self.expect_token(
                &Token::Symbol(Symbol::RParen),
                "Expected ')' after GLCURSOR coordinates",
            )?;
            return Ok(Some(Statement::GlCursor { x_expr, y_expr }));
        }
        Ok(None)
    }

    /// Parse with error collection - collects all parsing errors instead of stopping at first
    pub fn parse_with_error_recovery(&mut self) -> (program::Program, Vec<ParseError>) {
        let mut lines = std::collections::BTreeMap::new();
        let mut errors = Vec::new();

        while self.tokens.peek().is_some() {
            match self.parse_line_with_recovery() {
                Ok(Some(line)) => {
                    lines.insert(line.number, line);
                }
                Ok(None) => {
                    // Skip to next line on parse failure
                    self.skip_to_next_line();
                }
                Err(error) => {
                    errors.push(error);
                    // Skip to next line and continue parsing
                    self.skip_to_next_line();
                }
            }
        }

        (program::Program { lines }, errors)
    }

    /// Skip tokens until we reach a newline or EOF
    fn skip_to_next_line(&mut self) {
        loop {
            match self.peek_token() {
                Token::Symbol(Symbol::Newline) | Token::Symbol(Symbol::Eof) => {
                    self.next_spanned(); // consume the newline/eof
                    break;
                }
                _ => {
                    self.next_spanned(); // skip this token
                }
            }
        }
    }

    /// Parse a line with proper error reporting
    fn parse_line_with_recovery(&mut self) -> ParseResult<Option<Line>> {
        let start_span = self.current_span();

        match self.peek_token() {
            Token::DecimalNumber(line_number) => {
                let line_number = line_number
                    .as_integer()
                    .and_then(|line_number| u16::try_from(line_number).ok())
                    .ok_or_else(|| ParseError::InvalidExpression {
                        message: format!(
                            "Invalid line number: expected a valid integer between 0 and 65535, got {}",
                            line_number
                        ),
                        span: start_span,
                    })?;

                self.tokens.next(); // Consume line number

                // Try to parse a line label: a string literal optionally followed by ':'
                let label = if let Some(Token::StringLiteral(label)) = self.peek_mut_token() {
                    let label = mem::take(label);
                    self.tokens.next(); // Consume label
                    self.next_if_token_eq(&Token::Symbol(Symbol::Colon)); // Consume ':'
                    Some(label)
                } else {
                    None
                };

                let mut statements = vec![];

                // Parse statements with error recovery
                loop {
                    match self.parse_statement(false) {
                        Ok(Some(statement)) => {
                            statements.push(statement);
                            // Statements are separated by ':'
                            if self
                                .next_if_token_eq(&Token::Symbol(Symbol::Colon))
                                .is_none()
                            {
                                break;
                            }
                        }
                        Ok(None) => {
                            // No more statements to parse
                            break;
                        }
                        Err(parse_error) => {
                            // Return the error for higher-level error recovery
                            return Err(parse_error);
                        }
                    }
                }

                // Parse line end or EOF
                let newline = self.next_if_token_eq(&Token::Symbol(Symbol::Newline));
                let eof = self.next_if_token_eq(&Token::Symbol(Symbol::Eof));

                if newline.is_none() && eof.is_none() {
                    return Err(ParseError::MissingToken {
                        token: "newline or end of file".to_string(),
                        span: self.current_span(),
                    });
                }

                Ok(Some(Line {
                    label,
                    number: line_number,
                    statements,
                }))
            }
            _ => {
                // Check if we're at EOF - this is not an error, just no more lines
                if matches!(self.peek_token(), Token::Symbol(Symbol::Eof)) {
                    return Ok(None);
                }

                // Unexpected token at start of line
                let token = self.next_spanned().unwrap();
                Err(ParseError::UnexpectedToken {
                    expected: "line number".to_string(),
                    found: format!("{}", token.token()),
                    span: *token.span(),
                })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex::Lexer;

    fn parse_expression_from_str(input: &str) -> Option<Expr> {
        let lexer = Lexer::new(input);
        let tokens: Vec<SpannedToken> = lexer.filter_map(|result| result.ok()).collect();
        let mut parser = Parser::new(tokens.into_iter());
        parser.parse_expression().ok().flatten()
    }

    #[test]
    fn test_basic_arithmetic_precedence() {
        // Test that multiplication has higher precedence than addition
        let expr = parse_expression_from_str("2 + 3 * 4").unwrap();

        // Should parse as 2 + (3 * 4), not (2 + 3) * 4
        match &expr.inner {
            ExprInner::Binary(left, BinaryOp::Add, right) => {
                match (&left.inner, &right.inner) {
                    (
                        ExprInner::DecimalNumber(_),
                        ExprInner::Binary(mul_left, BinaryOp::Mul, mul_right),
                    ) => {
                        // This is the expected structure
                        assert!(matches!(mul_left.inner, ExprInner::DecimalNumber(_)));
                        assert!(matches!(mul_right.inner, ExprInner::DecimalNumber(_)));
                    }
                    _ => panic!("Expected 2 + (3 * 4) structure, got {expr:?}"),
                }
            }
            _ => panic!("Expected addition at top level, got {expr:?}"),
        }
    }

    #[test]
    fn test_left_associativity_addition() {
        // Test that addition is left associative: 1 + 2 + 3 should parse as (1 + 2) + 3
        let expr = parse_expression_from_str("1 + 2 + 3").unwrap();

        match &expr.inner {
            ExprInner::Binary(left, BinaryOp::Add, right) => {
                // Right operand should be 3
                assert!(matches!(right.inner, ExprInner::DecimalNumber(_)));

                // Left operand should be (1 + 2)
                match &left.inner {
                    ExprInner::Binary(inner_left, BinaryOp::Add, inner_right) => {
                        assert!(matches!(inner_left.inner, ExprInner::DecimalNumber(_)));
                        assert!(matches!(inner_right.inner, ExprInner::DecimalNumber(_)));
                    }
                    _ => panic!("Expected binary operation on left side, got {left:?}"),
                }
            }
            _ => panic!("Expected addition at top level, got {expr:?}"),
        }
    }

    #[test]
    fn test_left_associativity_subtraction() {
        // Test that subtraction is left associative: 10 - 5 - 2 should parse as (10 - 5) - 2
        let expr = parse_expression_from_str("10 - 5 - 2").unwrap();

        match &expr.inner {
            ExprInner::Binary(left, BinaryOp::Sub, right) => {
                // Right operand should be 2
                assert!(matches!(right.inner, ExprInner::DecimalNumber(_)));

                // Left operand should be (10 - 5)
                match &left.inner {
                    ExprInner::Binary(inner_left, BinaryOp::Sub, inner_right) => {
                        assert!(matches!(inner_left.inner, ExprInner::DecimalNumber(_)));
                        assert!(matches!(inner_right.inner, ExprInner::DecimalNumber(_)));
                    }
                    _ => panic!("Expected binary operation on left side, got {left:?}"),
                }
            }
            _ => panic!("Expected subtraction at top level, got {expr:?}"),
        }
    }

    #[test]
    fn test_left_associativity_multiplication() {
        // Test that multiplication is left associative: 2 * 3 * 4 should parse as (2 * 3) * 4
        let expr = parse_expression_from_str("2 * 3 * 4").unwrap();

        match &expr.inner {
            ExprInner::Binary(left, BinaryOp::Mul, right) => {
                // Right operand should be 4
                assert!(matches!(right.inner, ExprInner::DecimalNumber(_)));

                // Left operand should be (2 * 3)
                match &left.inner {
                    ExprInner::Binary(inner_left, BinaryOp::Mul, inner_right) => {
                        assert!(matches!(inner_left.inner, ExprInner::DecimalNumber(_)));
                        assert!(matches!(inner_right.inner, ExprInner::DecimalNumber(_)));
                    }
                    _ => panic!("Expected binary operation on left side, got {left:?}"),
                }
            }
            _ => panic!("Expected multiplication at top level, got {expr:?}"),
        }
    }

    #[test]
    fn test_left_associativity_division() {
        // Test that division is left associative: 12 / 3 / 2 should parse as (12 / 3) / 2
        let expr = parse_expression_from_str("12 / 3 / 2").unwrap();

        match &expr.inner {
            ExprInner::Binary(left, BinaryOp::Div, right) => {
                // Right operand should be 2
                assert!(matches!(right.inner, ExprInner::DecimalNumber(_)));

                // Left operand should be (12 / 3)
                match &left.inner {
                    ExprInner::Binary(inner_left, BinaryOp::Div, inner_right) => {
                        assert!(matches!(inner_left.inner, ExprInner::DecimalNumber(_)));
                        assert!(matches!(inner_right.inner, ExprInner::DecimalNumber(_)));
                    }
                    _ => panic!("Expected binary operation on left side, got {left:?}"),
                }
            }
            _ => panic!("Expected division at top level, got {expr:?}"),
        }
    }

    #[test]
    fn test_left_associativity_comparison() {
        // Test that comparison operators are left associative: 1 < 2 < 3 should parse as (1 < 2) < 3
        let expr = parse_expression_from_str("1 < 2 < 3").unwrap();

        match &expr.inner {
            ExprInner::Binary(left, BinaryOp::Lt, right) => {
                // Right operand should be 3
                assert!(matches!(right.inner, ExprInner::DecimalNumber(_)));

                // Left operand should be (1 < 2)
                match &left.inner {
                    ExprInner::Binary(inner_left, BinaryOp::Lt, inner_right) => {
                        assert!(matches!(inner_left.inner, ExprInner::DecimalNumber(_)));
                        assert!(matches!(inner_right.inner, ExprInner::DecimalNumber(_)));
                    }
                    _ => panic!("Expected binary operation on left side, got {left:?}"),
                }
            }
            _ => panic!("Expected less-than at top level, got {expr:?}"),
        }
    }

    #[test]
    fn test_left_associativity_exponentiation() {
        // Test that exponentiation is left associative: 2 ^ 3 ^ 4 should parse as (2 ^ 3) ^ 4
        let expr = parse_expression_from_str("2 ^ 3 ^ 4").unwrap();

        match &expr.inner {
            ExprInner::Binary(left, BinaryOp::Exp, right) => {
                // Left operand should be (2 ^ 3)
                match &left.inner {
                    ExprInner::Binary(inner_left, BinaryOp::Exp, inner_right) => {
                        assert!(matches!(inner_left.inner, ExprInner::DecimalNumber(_)));
                        assert!(matches!(inner_right.inner, ExprInner::DecimalNumber(_)));
                    }
                    _ => panic!("Expected binary operation on left side, got {left:?}"),
                }

                // Right operand should be 4
                assert!(matches!(right.inner, ExprInner::DecimalNumber(_)));
            }
            _ => panic!("Expected exponentiation at top level, got {expr:?}"),
        }
    }

    #[test]
    fn test_left_associativity_and() {
        // Test that AND is left associative: A AND B AND C should parse as (A AND B) AND C
        let expr = parse_expression_from_str("A AND B AND C").unwrap();

        match &expr.inner {
            ExprInner::Binary(left, BinaryOp::And, right) => {
                // Left operand should be (A AND B)
                match &left.inner {
                    ExprInner::Binary(inner_left, BinaryOp::And, inner_right) => {
                        assert!(matches!(inner_left.inner, ExprInner::LValue(_)));
                        assert!(matches!(inner_right.inner, ExprInner::LValue(_)));
                    }
                    _ => panic!("Expected binary operation on left side, got {left:?}"),
                }

                // Right operand should be C
                assert!(matches!(right.inner, ExprInner::LValue(_)));
            }
            _ => panic!("Expected AND at top level, got {expr:?}"),
        }
    }

    #[test]
    fn test_left_associativity_or() {
        // Test that OR is left associative: A OR B OR C should parse as (A OR B) OR C
        let expr = parse_expression_from_str("A OR B OR C").unwrap();

        match &expr.inner {
            ExprInner::Binary(left, BinaryOp::Or, right) => {
                // Left operand should be (A OR B)
                assert!(matches!(right.inner, ExprInner::LValue(_)));

                // Right operand should be (B OR C)
                match &left.inner {
                    ExprInner::Binary(inner_left, BinaryOp::Or, inner_right) => {
                        assert!(matches!(inner_left.inner, ExprInner::LValue(_)));
                        assert!(matches!(inner_right.inner, ExprInner::LValue(_)));
                    }
                    _ => panic!("Expected binary operation on right side, got {right:?}"),
                }
            }
            _ => panic!("Expected OR at top level, got {expr:?}"),
        }
    }

    #[test]
    fn test_operator_precedence_chain() {
        // Test complex precedence: 1 + 2 * 3 ^ 4 should parse as 1 + (2 * (3 ^ 4))
        let expr = parse_expression_from_str("1 + 2 * 3 ^ 4").unwrap();

        match &expr.inner {
            ExprInner::Binary(left, BinaryOp::Add, right) => {
                // Left should be 1
                assert!(matches!(left.inner, ExprInner::DecimalNumber(_)));

                // Right should be (2 * (3 ^ 4))
                match &right.inner {
                    ExprInner::Binary(mul_left, BinaryOp::Mul, mul_right) => {
                        // mul_left should be 2
                        assert!(matches!(mul_left.inner, ExprInner::DecimalNumber(_)));

                        // mul_right should be (3 ^ 4)
                        match &mul_right.inner {
                            ExprInner::Binary(exp_left, BinaryOp::Exp, exp_right) => {
                                assert!(matches!(exp_left.inner, ExprInner::DecimalNumber(_)));
                                assert!(matches!(exp_right.inner, ExprInner::DecimalNumber(_)));
                            }
                            _ => panic!(
                                "Expected exponentiation on right of multiplication, got {mul_right:?}"
                            ),
                        }
                    }
                    _ => panic!("Expected multiplication on right of addition, got {right:?}"),
                }
            }
            _ => panic!("Expected addition at top level, got {expr:?}"),
        }
    }

    #[test]
    fn test_parentheses_override_precedence() {
        // Test that parentheses override precedence: (1 + 2) * 3 should parse as (1 + 2) * 3
        let expr = parse_expression_from_str("(1 + 2) * 3").unwrap();

        match &expr.inner {
            ExprInner::Binary(left, BinaryOp::Mul, right) => {
                // Right should be 3
                assert!(matches!(right.inner, ExprInner::DecimalNumber(_)));

                // Left should be (1 + 2)
                match &left.inner {
                    ExprInner::Binary(add_left, BinaryOp::Add, add_right) => {
                        assert!(matches!(add_left.inner, ExprInner::DecimalNumber(_)));
                        assert!(matches!(add_right.inner, ExprInner::DecimalNumber(_)));
                    }
                    _ => panic!("Expected addition in parentheses, got {left:?}"),
                }
            }
            _ => panic!("Expected multiplication at top level, got {expr:?}"),
        }
    }

    #[test]
    fn test_unary_minus_precedence() {
        // Test that unary minus has low precedence: -2 ^ 3 should parse as -(2 ^ 3)
        let expr = parse_expression_from_str("-2 ^ 3").unwrap();

        match &expr.inner {
            ExprInner::Unary(UnaryOp::Minus, unary_operand) => {
                let inner_expr = &unary_operand.inner;
                assert!(
                    matches!(inner_expr, ExprInner::Binary(_, BinaryOp::Exp, _)),
                    "Expected exponentiation on right of unary minus, got {inner_expr:?}"
                );
            }
            expr => panic!("Expected unary minus, got {expr:?}"),
        }
    }

    #[test]
    fn test_unary_plus_precedence() {
        // Test that unary plus has high precedence: +2 * 3 should parse as (+2) * 3
        let expr = parse_expression_from_str("+2 * 3").unwrap();

        match &expr.inner {
            ExprInner::Binary(left, BinaryOp::Mul, right) => {
                // Left should be (+2)
                match &left.inner {
                    ExprInner::Unary(UnaryOp::Plus, unary_operand) => {
                        assert!(matches!(unary_operand.inner, ExprInner::DecimalNumber(_)));
                    }
                    _ => panic!("Expected unary plus, got {left:?}"),
                }

                // Right should be 3
                assert!(matches!(right.inner, ExprInner::DecimalNumber(_)));
            }
            _ => panic!("Expected multiplication at top level, got {expr:?}"),
        }
    }

    #[test]
    fn test_not_precedence() {
        // Test that NOT has high precedence: NOT A AND B should parse as (NOT A) AND B
        let expr = parse_expression_from_str("NOT A AND B").unwrap();

        match &expr.inner {
            ExprInner::Binary(left, BinaryOp::And, right) => {
                // Left should be (NOT A)
                match &left.inner {
                    ExprInner::Unary(UnaryOp::Not, unary_operand) => {
                        assert!(matches!(unary_operand.inner, ExprInner::LValue(_)));
                    }
                    _ => panic!("Expected NOT operation, got {left:?}"),
                }

                // Right should be B
                assert!(matches!(right.inner, ExprInner::LValue(_)));
            }
            _ => panic!("Expected AND at top level, got {expr:?}"),
        }
    }
}
