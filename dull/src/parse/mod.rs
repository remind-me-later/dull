use std::{iter::Peekable, mem, vec};

use crate::{
    lex::{Token, keyword::Keyword, symbol::Symbol},
    parse::{
        expression::{
            Expr, binary_op::BinaryOp, expr_inner::ExprInner, function::Function, lvalue::LValue,
            memory_area::MemoryArea, unary_op::UnaryOp,
        },
        line::Line,
        statement::{
            Assignment, BeepOptionalParams, DimInner, LCursorClause, LPrintInner, LPrintable,
            LetInner, PrintInner, PrintSeparator, Printable, Statement, UsingClause,
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

    fn parse_and_or_expr(&mut self) -> Option<Expr> {
        let mut left = self.parse_unary_logical_expr()?;

        while let Some(op) = self.tokens.next_if(|token| {
            matches!(
                token,
                Token::Keyword(Keyword::Or) | Token::Keyword(Keyword::And)
            )
        }) {
            let right = self.parse_unary_logical_expr()?;
            left = Expr::new(ExprInner::Binary(
                Box::new(left),
                match op {
                    Token::Keyword(Keyword::Or) => BinaryOp::Or,
                    Token::Keyword(Keyword::And) => BinaryOp::And,
                    _ => unreachable!(),
                },
                Box::new(right),
            ));
        }

        Some(left)
    }

    fn parse_expression(&mut self) -> Option<expression::Expr> {
        self.parse_and_or_expr()
    }

    fn parse_expression_factor(&mut self) -> Option<Expr> {
        match self.tokens.peek_mut()? {
            Token::BinaryNumber(h) => {
                let hex_number = *h;
                self.tokens.next()?;
                return Some(Expr::new(ExprInner::BinaryNumber(hex_number)));
            }
            Token::DecimalNumber(n) => {
                let number = *n;
                self.tokens.next()?;
                return Some(Expr::new(ExprInner::DecimalNumber(number)));
            }
            Token::StringLiteral(s) => {
                let string = mem::take(s);
                self.tokens.next()?;
                return Some(Expr::new(ExprInner::StringLiteral(string)));
            }
            Token::Symbol(Symbol::LParen) => {
                self.tokens.next(); // Consume '('
                let expr = self.parse_expression()?;
                self.tokens
                    .next_if_eq(&Token::Symbol(Symbol::RParen))
                    .expect("Expected closing parenthesis");
                return Some(expr);
            }
            _ => (),
        }

        if let Some(lvalue) = self.parse_lvalue() {
            return Some(Expr::new(ExprInner::LValue(lvalue)));
        }

        if let Some(function) = self.parse_function() {
            return Some(Expr::new(ExprInner::FunctionCall(function)));
        }

        None
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
                // Parse '('
                self.tokens
                    .next_if_eq(&Token::Symbol(Symbol::LParen))
                    .unwrap_or_else(|| panic!("Expected '(' after MID$"));
                let string = self.parse_expression()?;
                self.tokens
                    .next_if_eq(&Token::Symbol(Symbol::Comma))
                    .unwrap_or_else(|| panic!("Expected ',' after MID$"));
                let start = self.parse_expression()?;
                self.tokens
                    .next_if_eq(&Token::Symbol(Symbol::Comma))
                    .unwrap_or_else(|| panic!("Expected ',' after MID$"));
                let length = self.parse_expression()?;
                self.tokens
                    .next_if_eq(&Token::Symbol(Symbol::RParen))
                    .unwrap_or_else(|| panic!("Expected ')' after MID$"));
                Some(Function::Mid {
                    string: Box::new(string),
                    start: Box::new(start),
                    length: Box::new(length),
                })
            }
            Token::Keyword(Keyword::LeftDollar) => {
                self.tokens.next();
                // Parse '('
                self.tokens
                    .next_if_eq(&Token::Symbol(Symbol::LParen))
                    .unwrap_or_else(|| panic!("Expected '(' after LEFT$"));
                let string = self.parse_expression()?;
                // Parse ','
                self.tokens
                    .next_if_eq(&Token::Symbol(Symbol::Comma))
                    .unwrap_or_else(|| panic!("Expected ',' after LEFT$"));
                let length = self.parse_expression()?;
                self.tokens
                    .next_if_eq(&Token::Symbol(Symbol::RParen))
                    .unwrap_or_else(|| panic!("Expected ')' after LEFT$"));
                Some(Function::Left {
                    string: Box::new(string),
                    length: Box::new(length),
                })
            }
            Token::Keyword(Keyword::RightDollar) => {
                self.tokens.next();
                // Parse '('
                self.tokens
                    .next_if_eq(&Token::Symbol(Symbol::LParen))
                    .unwrap_or_else(|| panic!("Expected '(' after RIGHT$"));
                let string = self.parse_expression()?;
                self.tokens
                    .next_if_eq(&Token::Symbol(Symbol::Comma))
                    .unwrap_or_else(|| panic!("Expected ',' after RIGHT$"));
                let length = self.parse_expression()?;
                self.tokens
                    .next_if_eq(&Token::Symbol(Symbol::RParen))
                    .unwrap_or_else(|| panic!("Expected ')' after RIGHT$"));
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

    fn parse_lvalue(&mut self) -> Option<LValue> {
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
                        return Some(LValue::Array2DAccess {
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
                    return Some(LValue::Array1DAccess {
                        identifier,
                        index: Box::new(index),
                    });
                }

                Some(LValue::Identifier(identifier))
            }
            Token::Symbol(Symbol::At) => {
                self.tokens.next();

                let has_dollar = self
                    .tokens
                    .next_if_eq(&Token::Symbol(Symbol::Dollar))
                    .is_some();

                // Parse '('
                self.tokens
                    .next_if_eq(&Token::Symbol(Symbol::LParen))
                    .unwrap_or_else(|| panic!("Expected '(' after fixed memory area access '@'"));

                let index = self.parse_expression()?;

                self.tokens
                    .next_if_eq(&Token::Symbol(Symbol::RParen))
                    .unwrap_or_else(|| panic!("Expected ')' after fixed memory area access '@'"));

                Some(LValue::FixedMemoryAreaAccess {
                    index: Box::new(index),
                    has_dollar,
                })
            }
            Token::Keyword(Keyword::Time) => {
                self.tokens.next();
                Some(LValue::BuiltInIdentifier(Keyword::Time))
            }
            Token::Keyword(Keyword::InkeyDollar) => {
                self.tokens.next();
                Some(LValue::BuiltInIdentifier(Keyword::InkeyDollar))
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

        match self.tokens.peek_mut()? {
            // USING "##.##"
            Token::StringLiteral(format) => {
                let format = mem::take(format);
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

    fn parse_dim_inner(&mut self) -> Option<DimInner> {
        match self.tokens.peek()? {
            Token::Identifier(identifier) => {
                let identifier = *identifier;
                self.tokens.next();
                self.tokens.next_if_eq(&Token::Symbol(Symbol::LParen))?;
                let rows = self.parse_expression()?;
                let maybe_cols = self
                    .tokens
                    .next_if_eq(&Token::Symbol(Symbol::Comma))
                    .and_then(|_| self.parse_expression());
                self.tokens.next_if_eq(&Token::Symbol(Symbol::RParen))?;
                let string_length = self
                    .tokens
                    .next_if_eq(&Token::Symbol(Symbol::Mul))
                    .and_then(|_| self.parse_expression());

                match maybe_cols {
                    Some(cols) => Some(DimInner::DimInner2D {
                        identifier,
                        rows,
                        cols,
                        string_length,
                    }),
                    None => Some(DimInner::DimInner1D {
                        identifier,
                        size: rows,
                        string_length,
                    }),
                }
            }
            _ => None,
        }
    }

    fn parse_dim_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Dim))?;
        let mut decls = vec![];

        while let Some(decl) = self.parse_dim_inner() {
            decls.push(decl);

            if self
                .tokens
                .next_if_eq(&Token::Symbol(Symbol::Comma))
                .is_none()
            {
                break;
            }
        }

        Some(Statement::Dim { decls })
    }

    fn parse_statement(&mut self, is_let_mandatory: bool) -> Option<Statement> {
        if let Some(stmt) = self.parse_print_pause_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_using_clause() {
            return Some(Statement::Using { using_clause: stmt });
        }

        if let Some(stmt) = self.parse_end_statement() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_if_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_input_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_remark_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_for_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_next_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_clear_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_goto_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_gosub_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_on_goto_gosub_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_on_error_goto_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_wait_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_cls_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_random_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_gprint_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_gcursor_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_cursor_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_beep_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_return_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_poke_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_dim_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_read_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_data_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_restore_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_arun_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_lock_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_unlock_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_call_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_text_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_graph_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_color_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_csize_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_lf_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_radian_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_lcursor_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_lprint_stmt() {
            return Some(stmt);
        }

        if let Some(stmt) = self.parse_let_inner(is_let_mandatory) {
            return Some(Statement::Let { inner: stmt });
        }

        None
    }

    fn parse_if_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::If))?;
        let condition = self.parse_expression()?;
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Then)); // optional
        let then_stmt = self.parse_statement(true).or_else(|| {
            // If no statement after THEN, check for a line number
            if let Some(Token::DecimalNumber(line_number)) = self.tokens.peek() {
                let line_number = *line_number;
                self.tokens.next();
                Some(Statement::Goto {
                    target: Expr::new(ExprInner::DecimalNumber(line_number)),
                })
            } else {
                None
            }
        })?;

        Some(Statement::If {
            condition,
            then_stmt: Box::new(then_stmt),
        })
    }

    fn parse_input_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Input))?;
        let mut input_exprs = vec![];

        loop {
            // Check for prompt string
            let prompt = if let Some(Token::StringLiteral(_)) = self.tokens.peek() {
                if let Some(Token::StringLiteral(s)) = self.tokens.next() {
                    self.tokens.next_if_eq(&Token::Symbol(Symbol::Semicolon));
                    Some(s)
                } else {
                    None
                }
            } else {
                None
            };

            let lvalue = self.parse_lvalue()?;
            input_exprs.push((prompt, lvalue));

            if self
                .tokens
                .next_if_eq(&Token::Symbol(Symbol::Comma))
                .is_none()
            {
                break;
            }
        }

        Some(Statement::Input { input_exprs })
    }

    fn parse_remark_stmt(&mut self) -> Option<Statement> {
        match self.tokens.peek_mut() {
            Some(Token::Remark(s)) => {
                let s = mem::take(s);
                self.tokens.next();
                Some(Statement::Remark {
                    text: s.to_string(),
                })
            }
            _ => None,
        }
    }

    fn parse_for_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::For))?;
        let assignment = self.parse_assignment()?;
        self.tokens.next_if_eq(&Token::Keyword(Keyword::To))?;
        let to_expr = self.parse_expression()?;
        let step_expr = if self
            .tokens
            .next_if_eq(&Token::Keyword(Keyword::Step))
            .is_some()
        {
            Some(self.parse_expression()?)
        } else {
            None
        };
        Some(Statement::For {
            assignment,
            to_expr,
            step_expr,
        })
    }

    fn parse_next_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Next))?;
        let ident = match self.tokens.next()? {
            Token::Identifier(id) => id,
            _ => return None,
        };
        Some(Statement::Next { ident })
    }

    fn parse_clear_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Clear))?;
        Some(Statement::Clear)
    }

    fn parse_goto_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Goto))?;
        let target = self.parse_expression()?;
        Some(Statement::Goto { target })
    }

    fn parse_gosub_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Gosub))?;
        let target = self.parse_expression()?;
        Some(Statement::Gosub { target })
    }

    fn parse_on_goto_gosub_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::On))?;
        let expr = self.parse_expression()?;

        if self
            .tokens
            .next_if_eq(&Token::Keyword(Keyword::Goto))
            .is_some()
        {
            let mut targets = vec![self.parse_expression()?];
            while self
                .tokens
                .next_if_eq(&Token::Symbol(Symbol::Comma))
                .is_some()
            {
                targets.push(self.parse_expression()?);
            }
            Some(Statement::OnGoto { expr, targets })
        } else if self
            .tokens
            .next_if_eq(&Token::Keyword(Keyword::Gosub))
            .is_some()
        {
            let mut targets = vec![self.parse_expression()?];
            while self
                .tokens
                .next_if_eq(&Token::Symbol(Symbol::Comma))
                .is_some()
            {
                targets.push(self.parse_expression()?);
            }
            Some(Statement::OnGosub { expr, targets })
        } else {
            None
        }
    }

    fn parse_on_error_goto_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::On))?;
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Error))?;
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Goto))?;
        let target = self.parse_expression()?;
        Some(Statement::OnErrorGoto { target })
    }

    fn parse_wait_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Wait))?;
        let expr = if self.tokens.peek().is_some() {
            // Check if the next token can start an expression
            match self.tokens.peek()? {
                Token::DecimalNumber(_)
                | Token::BinaryNumber(_)
                | Token::Identifier(_)
                | Token::Symbol(Symbol::LParen) => Some(self.parse_expression()?),
                _ => None,
            }
        } else {
            None
        };
        Some(Statement::Wait { expr })
    }

    fn parse_cls_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Cls))?;
        Some(Statement::Cls)
    }

    fn parse_random_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Random))?;
        Some(Statement::Random)
    }

    fn parse_gprint_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Gprint))?;
        let mut exprs = vec![];

        while let Some(expr) = self.parse_expression() {
            let separator = self.parse_print_separator();
            let is_empty = matches!(separator, PrintSeparator::Empty);
            exprs.push((expr, separator));

            if is_empty {
                break;
            }
        }

        Some(Statement::Gprint { exprs })
    }

    fn parse_gcursor_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Gcursor))?;
        let expr = self.parse_expression()?;
        Some(Statement::GCursor { expr })
    }

    fn parse_cursor_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Cursor))?;
        let expr = self.parse_expression()?;
        Some(Statement::Cursor { expr })
    }

    fn parse_beep_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Beep))?;

        // Check for BEEP ON/OFF
        if let Some(Token::Keyword(Keyword::On)) = self.tokens.peek() {
            self.tokens.next();
            return Some(Statement::BeepOnOff {
                switch_beep_on: true,
            });
        }
        if let Some(Token::Keyword(Keyword::Off)) = self.tokens.peek() {
            self.tokens.next();
            return Some(Statement::BeepOnOff {
                switch_beep_on: false,
            });
        }

        // Parse BEEP repetitions[,frequency[,duration]]
        let repetitions_expr = self.parse_expression()?;
        let optional_params = if self
            .tokens
            .next_if_eq(&Token::Symbol(Symbol::Comma))
            .is_some()
        {
            let frequency = self.parse_expression()?;
            let duration = if self
                .tokens
                .next_if_eq(&Token::Symbol(Symbol::Comma))
                .is_some()
            {
                Some(self.parse_expression()?)
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

        Some(Statement::Beep {
            repetitions_expr,
            optional_params,
        })
    }

    fn parse_return_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Return))?;
        Some(Statement::Return)
    }

    fn parse_poke_stmt(&mut self) -> Option<Statement> {
        let memory_area = match self.tokens.peek()? {
            Token::Keyword(Keyword::PokeMem0) => {
                self.tokens.next();
                MemoryArea::Me0
            }
            Token::Keyword(Keyword::PokeMem1) => {
                self.tokens.next();
                MemoryArea::Me1
            }
            _ => return None,
        };

        let mut exprs = vec![self.parse_expression()?];
        while self
            .tokens
            .next_if_eq(&Token::Symbol(Symbol::Comma))
            .is_some()
        {
            exprs.push(self.parse_expression()?);
        }

        Some(Statement::Poke { memory_area, exprs })
    }

    fn parse_read_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Read))?;
        let mut destinations = vec![self.parse_lvalue()?];
        while self
            .tokens
            .next_if_eq(&Token::Symbol(Symbol::Comma))
            .is_some()
        {
            destinations.push(self.parse_lvalue()?);
        }
        Some(Statement::Read { destinations })
    }

    fn parse_data_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Data))?;
        let mut exprs = vec![self.parse_expression()?];
        while self
            .tokens
            .next_if_eq(&Token::Symbol(Symbol::Comma))
            .is_some()
        {
            exprs.push(self.parse_expression()?);
        }
        Some(Statement::Data(exprs))
    }

    fn parse_restore_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Restore))?;
        let expr = if self.tokens.peek().is_some() {
            // Check if the next token can start an expression
            match self.tokens.peek()? {
                Token::DecimalNumber(_)
                | Token::BinaryNumber(_)
                | Token::Identifier(_)
                | Token::Symbol(Symbol::LParen) => Some(self.parse_expression()?),
                _ => None,
            }
        } else {
            None
        };
        Some(Statement::Restore { expr })
    }

    fn parse_arun_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Arun))?;
        Some(Statement::Arun)
    }

    fn parse_lock_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Lock))?;
        Some(Statement::Lock)
    }

    fn parse_unlock_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Unlock))?;
        Some(Statement::Unlock)
    }

    fn parse_call_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Call))?;
        let expr = self.parse_expression()?;
        let variable = if self
            .tokens
            .next_if_eq(&Token::Symbol(Symbol::Comma))
            .is_some()
        {
            Some(self.parse_lvalue()?)
        } else {
            None
        };
        Some(Statement::Call { expr, variable })
    }

    fn parse_text_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Text))?;
        Some(Statement::Text)
    }

    fn parse_graph_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Graph))?;
        Some(Statement::Graph)
    }

    fn parse_color_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Color))?;
        let color = self.parse_expression()?;
        Some(Statement::Color { expr: color })
    }

    fn parse_csize_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Csize))?;
        let expr = self.parse_expression()?;
        Some(Statement::CSize { expr })
    }

    fn parse_lf_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Lf))?;
        let expr = self.parse_expression()?;
        Some(Statement::Lf { expr })
    }

    fn parse_radian_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Radian))?;
        Some(Statement::Radian)
    }

    fn parse_lcursor_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Lcursor))?;
        let expr = self.parse_expression()?;
        Some(Statement::LCursor(LCursorClause { expr }))
    }

    fn parse_lprint_stmt(&mut self) -> Option<Statement> {
        self.tokens.next_if_eq(&Token::Keyword(Keyword::Lprint))?;
        let inner = self.parse_lprint_inner()?;
        Some(Statement::LPrint { inner })
    }

    fn parse_lprint_inner(&mut self) -> Option<LPrintInner> {
        let mut exprs = vec![];

        loop {
            let printable = if self.tokens.peek() == Some(&Token::Keyword(Keyword::Lcursor)) {
                self.tokens.next();
                let expr = self.parse_expression()?;
                LPrintable::LCursorClause(LCursorClause { expr })
            } else if let Some(expr) = self.parse_expression() {
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

        Some(LPrintInner { exprs })
    }

    fn parse_line(&mut self) -> Option<Line> {
        match self.tokens.peek()? {
            Token::DecimalNumber(line_number) => {
                let line_number = line_number
                    .as_integer()
                    .and_then(|line_number| u16::try_from(line_number).ok())
                    .unwrap_or_else(|| {
                        panic!("Expected a valid line number, got: {line_number:?}")
                    });
                self.tokens.next(); // Consume line number

                // Try to parse a line label: a string literal optionally followed by ':'
                let label = if let Some(token) = self.tokens.peek_mut()
                    && let Token::StringLiteral(label) = token
                {
                    let label = mem::take(label);
                    self.tokens.next(); // Consume label
                    self.tokens.next_if_eq(&Token::Symbol(Symbol::Colon)); // Consume ':'
                    Some(label)
                } else {
                    None
                };

                let mut statements = vec![];

                while let Some(statement) = self.parse_statement(false) {
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

                // Parse line end or EOF
                let newline = self.tokens.next_if_eq(&Token::Symbol(Symbol::Newline));
                let eof = self.tokens.next_if_eq(&Token::Symbol(Symbol::Eof));

                if newline.is_none() && eof.is_none() {
                    panic!("Expected newline or EOF at the end of line {line_number}");
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
    use crate::lex::Lexer;

    fn parse_expression_from_str(input: &str) -> Option<Expr> {
        let lexer = Lexer::new(input);
        let tokens: Vec<Token> = lexer.filter_map(|result| result.ok()).collect();
        let mut parser = Parser::new(tokens.into_iter());
        parser.parse_expression()
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
