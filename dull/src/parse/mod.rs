use std::{iter::Peekable, vec};

use crate::{
    lex::{Token, keyword::Keyword, symbol::Symbol},
    parse::{
        expression::{
            Expr, binary_op::BinaryOp, expr_inner::ExprInner, function::Function,
            memory_area::MemoryArea, unary_op::UnaryOp,
        },
        line::Line,
        statement::{
            Assignment, LetInner, PrintInner, PrintSeparator, Printable, Statement, UsingClause,
        },
    },
};

mod expression;
mod line;
mod program;
mod statement;

pub struct Parser<I>
where
    I: Iterator<Item = Token>,
{
    tokens: Peekable<I>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    fn parse_exponent_expr(&mut self) -> Option<Expr> {
        let mut left = self.parse_expression_factor()?;

        while let Some(Token::Symbol(Symbol::Exp)) = self.tokens.peek() {
            self.tokens.next();
            let right = self.parse_expression_factor()?;
            left = Expr::new(ExprInner::Binary(
                Box::new(left),
                BinaryOp::Exp,
                Box::new(right),
            ));
        }

        Some(left)
    }

    fn parse_unary_op_expr(&mut self) -> Option<Expr> {
        let maybe_op = self.tokens.next_if(|token| {
            matches!(
                token,
                Token::Symbol(Symbol::Add) | Token::Symbol(Symbol::Sub)
            )
        });
        match maybe_op {
            Some(Token::Symbol(Symbol::Add)) => {
                let right = self.parse_exponent_expr()?;
                Some(Expr::new(ExprInner::Unary(UnaryOp::Plus, Box::new(right))))
            }
            Some(Token::Symbol(Symbol::Sub)) => {
                let right = self.parse_exponent_expr()?;
                Some(Expr::new(ExprInner::Unary(UnaryOp::Minus, Box::new(right))))
            }
            _ => self.parse_exponent_expr(),
        }
    }

    fn parse_mul_div_expr(&mut self) -> Option<Expr> {
        let mut left = self.parse_unary_op_expr()?;

        while let Some(op) = self.tokens.next_if(|token| {
            matches!(
                token,
                Token::Symbol(Symbol::Mul) | Token::Symbol(Symbol::Div)
            )
        }) {
            let right = self.parse_unary_op_expr()?;
            let binary_op = match op {
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

        Some(left)
    }

    fn parse_add_sub_expr(&mut self) -> Option<Expr> {
        let mut left = self.parse_mul_div_expr()?;

        while let Some(op) = self.tokens.next_if(|token| {
            matches!(
                token,
                Token::Symbol(Symbol::Add) | Token::Symbol(Symbol::Sub)
            )
        }) {
            let right = self.parse_mul_div_expr()?;
            let binary_op = match op {
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

        Some(left)
    }

    fn parse_comparison_expr(&mut self) -> Option<Expr> {
        let mut left = self.parse_add_sub_expr()?;

        while let Some(op) = self.tokens.next_if(|token| {
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
            let right = self.parse_add_sub_expr()?;
            let binary_op = match op {
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

        Some(left)
    }

    fn parse_unary_logical_expr(&mut self) -> Option<Expr> {
        let maybe_op = self.tokens.next_if_eq(&Token::Keyword(Keyword::Not));
        match maybe_op {
            Some(_) => {
                let right = self.parse_unary_logical_expr()?;
                Some(Expr::new(ExprInner::Unary(UnaryOp::Not, Box::new(right))))
            }
            None => self.parse_comparison_expr(),
        }
    }

    fn parse_and_expr(&mut self) -> Option<Expr> {
        let mut left = self.parse_unary_logical_expr()?;

        while self
            .tokens
            .next_if_eq(&Token::Keyword(Keyword::And))
            .is_some()
        {
            let right = self.parse_unary_logical_expr()?; // Right-associative recursion
            left = Expr::new(ExprInner::Binary(
                Box::new(left),
                BinaryOp::And,
                Box::new(right),
            ));
        }

        Some(left)
    }

    fn parse_or_expr(&mut self) -> Option<Expr> {
        let mut left = self.parse_and_expr()?;

        while self
            .tokens
            .next_if_eq(&Token::Keyword(Keyword::Or))
            .is_some()
        {
            let right = self.parse_and_expr()?; // Right-associative recursion
            left = Expr::new(ExprInner::Binary(
                Box::new(left),
                BinaryOp::Or,
                Box::new(right),
            ));
        }

        Some(left)
    }

    fn parse_expression(&mut self) -> Option<expression::Expr> {
        self.parse_or_expr()
    }

    fn parse_expression_factor(&mut self) -> Option<Expr> {
        match self.tokens.peek()? {
            Token::BinaryNumber(h) => {
                let hex_number = *h;
                self.tokens.next()?;
                Some(Expr::new(ExprInner::BinaryNumber(hex_number)))
            }
            Token::DecimalNumber(n) => {
                // FIXME: The clone should be optimized away with a move, check
                let number = n.clone();
                self.tokens.next()?;
                Some(Expr::new(ExprInner::DecimalNumber(number)))
            }
            Token::StringLiteral(s) => {
                let string = s.clone();
                self.tokens.next()?;
                Some(Expr::new(ExprInner::StringLiteral(string)))
            }
            Token::Identifier(_) => {
                let identifier = self.parse_lvalue()?;
                Some(Expr::new(ExprInner::LValue(identifier)))
            }
            Token::Symbol(Symbol::LParen) => {
                self.tokens.next(); // Consume '('
                let expr = self.parse_expression()?;
                self.tokens
                    .next_if_eq(&Token::Symbol(Symbol::RParen))
                    .expect("Expected closing parenthesis");
                Some(expr)
            }
            _ => self
                .parse_function()
                .map(ExprInner::FunctionCall)
                .map(Expr::new),
        }
    }

    fn parse_function(&mut self) -> Option<Function> {
        match self.tokens.peek()? {
            Token::Keyword(Keyword::Int) => {
                self.tokens.next();
                let expr = self.parse_expression()?;
                Some(Function::Int {
                    expr: Box::new(expr),
                })
            }
            Token::Keyword(Keyword::Sgn) => {
                self.tokens.next();
                let expr = self.parse_expression()?;
                Some(Function::Sgn {
                    expr: Box::new(expr),
                })
            }
            Token::Keyword(Keyword::Status) => {
                self.tokens.next();
                let arg = self.parse_expression()?;
                Some(Function::Status { arg: Box::new(arg) })
            }
            Token::Keyword(Keyword::Val) => {
                self.tokens.next();
                let expr = self.parse_expression()?;
                Some(Function::Val {
                    expr: Box::new(expr),
                })
            }
            Token::Keyword(Keyword::StrDollar) => {
                self.tokens.next();
                let expr = self.parse_expression()?;
                Some(Function::Str {
                    expr: Box::new(expr),
                })
            }
            Token::Keyword(Keyword::ChrDollar) => {
                self.tokens.next();
                let expr = self.parse_expression()?;
                Some(Function::Chr {
                    expr: Box::new(expr),
                })
            }
            Token::Keyword(Keyword::Abs) => {
                self.tokens.next();
                let expr = self.parse_expression()?;
                Some(Function::Abs {
                    expr: Box::new(expr),
                })
            }
            Token::Keyword(Keyword::Len) => {
                self.tokens.next();
                let expr = self.parse_expression()?;
                Some(Function::Len {
                    expr: Box::new(expr),
                })
            }
            Token::Keyword(Keyword::PeekMem0) => {
                self.tokens.next();
                let address = self.parse_expression()?;
                Some(Function::Peek {
                    memory_area: MemoryArea::Me0,
                    address: Box::new(address),
                })
            }
            Token::Keyword(Keyword::PeekMem1) => {
                self.tokens.next();
                let address = self.parse_expression()?;
                Some(Function::Peek {
                    memory_area: MemoryArea::Me1,
                    address: Box::new(address),
                })
            }
            Token::Keyword(Keyword::Ln) => {
                self.tokens.next();
                let expr = self.parse_expression()?;
                Some(Function::Ln {
                    expr: Box::new(expr),
                })
            }
            Token::Keyword(Keyword::Log) => {
                self.tokens.next();
                let expr = self.parse_expression()?;
                Some(Function::Log {
                    expr: Box::new(expr),
                })
            }
            Token::Keyword(Keyword::Dms) => {
                self.tokens.next();
                let expr = self.parse_expression()?;
                Some(Function::Dms {
                    expr: Box::new(expr),
                })
            }
            Token::Keyword(Keyword::Deg) => {
                self.tokens.next();
                let expr = self.parse_expression()?;
                Some(Function::Deg {
                    expr: Box::new(expr),
                })
            }
            Token::Keyword(Keyword::Tan) => {
                self.tokens.next();
                let expr = self.parse_expression()?;
                Some(Function::Tan {
                    expr: Box::new(expr),
                })
            }
            Token::Keyword(Keyword::Cos) => {
                self.tokens.next();
                let expr = self.parse_expression()?;
                Some(Function::Cos {
                    expr: Box::new(expr),
                })
            }
            Token::Keyword(Keyword::Sin) => {
                self.tokens.next();
                let expr = self.parse_expression()?;
                Some(Function::Sin {
                    expr: Box::new(expr),
                })
            }
            Token::Keyword(Keyword::Sqr) => {
                self.tokens.next();
                let expr = self.parse_expression()?;
                Some(Function::Sqr {
                    expr: Box::new(expr),
                })
            }
            Token::Keyword(Keyword::MidDollar) => {
                self.tokens.next();
                let string = self.parse_expression()?;
                let start = self.parse_expression()?;
                let length = self.parse_expression()?;
                Some(Function::Mid {
                    string: Box::new(string),
                    start: Box::new(start),
                    length: Box::new(length),
                })
            }
            Token::Keyword(Keyword::LeftDollar) => {
                self.tokens.next();
                let string = self.parse_expression()?;
                let length = self.parse_expression()?;
                Some(Function::Left {
                    string: Box::new(string),
                    length: Box::new(length),
                })
            }
            Token::Keyword(Keyword::RightDollar) => {
                self.tokens.next();
                let string = self.parse_expression()?;
                let length = self.parse_expression()?;
                Some(Function::Right {
                    string: Box::new(string),
                    length: Box::new(length),
                })
            }
            Token::Keyword(Keyword::Asc) => {
                self.tokens.next();
                let expr = self.parse_expression()?;
                Some(Function::Asc {
                    expr: Box::new(expr),
                })
            }
            Token::Keyword(Keyword::Point) => {
                self.tokens.next();
                let position = self.parse_expression()?;
                Some(Function::Point {
                    position: Box::new(position),
                })
            }
            Token::Keyword(Keyword::Rnd) => {
                self.tokens.next();
                let range_end = self.parse_expression()?;
                Some(Function::Rnd {
                    range_end: Box::new(range_end),
                })
            }
            _ => None,
        }
    }

    fn parse_lvalue(&mut self) -> Option<expression::lvalue::LValue> {
        match self.tokens.peek()? {
            Token::Identifier(identifier) => {
                let identifier = *identifier;
                self.tokens.next();

                // Check for array access
                if self.tokens.peek() == Some(&Token::Symbol(Symbol::LParen)) {
                    self.tokens.next(); // Consume '('
                    let index = self.parse_expression()?;

                    if self.tokens.peek() == Some(&Token::Symbol(Symbol::Comma)) {
                        self.tokens.next(); // Consume ','
                        let col_index = self.parse_expression()?;
                        self.tokens
                            .next_if_eq(&Token::Symbol(Symbol::RParen))
                            .unwrap_or_else(|| {
                                panic!("Expected closing parenthesis for 2D array access");
                            });
                        return Some(expression::lvalue::LValue::Array2DAccess {
                            identifier,
                            row_index: Box::new(index),
                            col_index: Box::new(col_index),
                        });
                    }

                    self.tokens
                        .next_if_eq(&Token::Symbol(Symbol::RParen))
                        .unwrap_or_else(|| {
                            panic!("Expected closing parenthesis for 1D array access");
                        });
                    return Some(expression::lvalue::LValue::Array1DAccess {
                        identifier,
                        index: Box::new(index),
                    });
                }

                Some(expression::lvalue::LValue::Identifier(identifier))
            }
            Token::BuiltInIdentifier(built_in) => {
                let built_in = *built_in;
                self.tokens.next();
                Some(expression::lvalue::LValue::BuiltInIdentifier(built_in))
            }
            Token::Symbol(Symbol::At) => {
                self.tokens.next();
                let index = self.parse_expression()?;
                let has_dollar = self
                    .tokens
                    .next_if_eq(&Token::Symbol(Symbol::Dollar))
                    .is_some();

                Some(expression::lvalue::LValue::FixedMemoryAreaAccess {
                    index: Box::new(index),
                    has_dollar,
                })
            }
            _ => None,
        }
    }

    fn parse_assignment(&mut self) -> Option<Assignment> {
        let lhs = self.parse_lvalue()?;
        self.tokens.next_if_eq(&Token::Symbol(Symbol::Eq))?;
        let rhs = self.parse_expression()?;
        Some(Assignment {
            lvalue: lhs,
            expr: Box::new(rhs),
        })
    }

    fn parse_let_inner(&mut self, is_let_mandatory: bool) -> Option<LetInner> {
        let let_kw = self.tokens.next_if_eq(&Token::Keyword(Keyword::Let));

        if is_let_mandatory && let_kw.is_none() {
            return None;
        }

        let mut assignments = vec![];

        while let Some(assignment) = self.parse_assignment() {
            assignments.push(assignment);
            if self.tokens.peek() == Some(&Token::Symbol(Symbol::Comma)) {
                self.tokens.next(); // Consume ','
            } else {
                break;
            }
        }

        Some(LetInner { assignments })
    }

    fn parse_using_clause(&mut self) -> Option<UsingClause> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Using))?;

        match self.tokens.peek()? {
            // USING "##.##"
            Token::StringLiteral(format) => {
                let format = format.clone();
                self.tokens.next();
                Some(UsingClause {
                    format: Some(format),
                })
            }
            // USING
            _ => Some(UsingClause { format: None }),
        }
    }

    fn parse_print_separator(&mut self) -> PrintSeparator {
        match self
            .tokens
            .peek()
            .unwrap_or_else(|| panic!("Expected a token for print separator"))
        {
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

    fn parse_print_inner(&mut self) -> Option<PrintInner> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Print))?;
        let mut exprs = Vec::new();

        loop {
            if let Some(expr) = self.parse_expression() {
                let print_separator = self.parse_print_separator();
                exprs.push((Printable::Expr(expr), print_separator));
            } else if let Some(using_clause) = self.parse_using_clause() {
                let print_separator = self.parse_print_separator();
                exprs.push((Printable::UsingClause(using_clause), print_separator));
            } else {
                break;
            }
        }

        Some(PrintInner { exprs })
    }

    fn parse_print_pause_stmt(&mut self) -> Option<Statement> {
        match self.tokens.peek()? {
            Token::Keyword(Keyword::Print) => {
                self.tokens.next();
                let print_inner = self.parse_print_inner()?;
                Some(Statement::Print { inner: print_inner })
            }
            Token::Keyword(Keyword::Pause) => {
                self.tokens.next();
                let pause_inner = self.parse_print_inner()?;
                Some(Statement::Pause { inner: pause_inner })
            }
            _ => None,
        }
    }

    fn parse_end_statement(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::End))?;
        Some(Statement::End)
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        if let Some(stmt) = self.parse_print_pause_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_using_clause() {
            return Some(Statement::Using { using_clause: stmt });
        }

        if let Some(stmt) = self.parse_end_statement() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_let_inner(false) {
            return Some(Statement::Let { inner: stmt });
        }

        None
    }

    fn parse_line(&mut self) -> Option<Line> {
        match self.tokens.peek()? {
            Token::DecimalNumber(line_number) => {
                let line_number = line_number
                    .into_integer()
                    .and_then(|line_number| u16::try_from(line_number).ok())
                    .unwrap_or_else(|| {
                        panic!("Expected a valid line number, got: {line_number:?}")
                    });
                self.tokens.next(); // Consume line number

                // Try to parse a line label: a string literal optionally followed by ':'
                let label = if let Some(token) = self.tokens.peek()
                    && let Token::StringLiteral(label) = token
                {
                    let label = label.clone();
                    self.tokens.next(); // Consume label
                    self.tokens.next_if_eq(&Token::Symbol(Symbol::Colon)); // Consume ':'
                    Some(label)
                } else {
                    None
                };

                let mut statements = vec![];

                while let Some(statement) = self.parse_statement() {
                    statements.push(statement);
                    // Statements are separated by ':'
                    if self
                        .tokens
                        .next_if_eq(&Token::Symbol(Symbol::Colon))
                        .is_none()
                    {
                        break;
                    }
                }

                Some(Line {
                    label,
                    number: line_number,
                    statements,
                })
            }
            _ => None,
        }
    }

    fn parse_program(&mut self) -> program::Program {
        let mut lines = std::collections::BTreeMap::new();

        while let Some(line) = self.parse_line() {
            lines.insert(line.number, line);
        }

        program::Program { lines }
    }

    pub fn parse(&mut self) -> program::Program {
        self.parse_program()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex::{Lexer, decimal_number::DecimalNumber};

    fn parse_expression_from_str(input: &str) -> Option<Expr> {
        let lexer = Lexer::new(input);
        let tokens: Vec<Token> = lexer.filter_map(|result| result.ok()).collect();
        let mut parser = Parser::new(tokens.into_iter());
        parser.parse_expression()
    }

    // Helper to create a decimal number from an i64
    fn decimal_from_int(value: i64) -> DecimalNumber {
        DecimalNumber::new(0, value).expect("Valid decimal number")
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
