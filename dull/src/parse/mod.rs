use core::num;
use std::{iter::Peekable, path::Iter};

use crate::{
    lex::{Token, keyword::Keyword, symbol::Symbol},
    parse::expression::{
        Expr, binary_op::BinaryOp, expr_inner::ExprInner, function::Function,
        memory_area::MemoryArea, unary_op::UnaryOp,
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

    // expression :: Parser RawExpr
    // expression = orExpr
    //   where
    //     -- Right-associative: a ^ b ^ c = a ^ (b ^ c)
    //     exponentExpr = do
    //       left <- expressionFactor
    //       maybeOp <- optional (try (binOperator CaretOp))
    //       case maybeOp of
    //         Just op -> do
    //           right <- exponentExpr -- Right-associative: recurse on same level
    //           return Expr {exprInner = BinExpr left op right, exprType = ()}
    //         Nothing -> return left
    fn parse_exponent_expr(&mut self) -> Option<Expr> {
        let left = self.parse_expression_factor()?;
        let maybe_op = self.tokens.next_if_eq(&Token::Symbol(Symbol::Exp));
        match maybe_op {
            Some(_) => {
                let right = self.parse_exponent_expr()?;
                Some(Expr::new(ExprInner::Binary(
                    Box::new(left),
                    BinaryOp::Exp,
                    Box::new(right),
                )))
            }
            None => Some(left),
        }
    }
    //     unaryOpExpr = do
    //       maybeOp <-
    //         optional
    //           ( try (unaryOperator UnaryPlusOp)
    //               <|> try (unaryOperator UnaryMinusOp)
    //           )
    //       case maybeOp of
    //         Just op -> do
    //           right <- unaryOpExpr
    //           return Expr {exprInner = UnaryExpr op right, exprType = ()}
    //         Nothing -> exponentExpr

    fn parse_unary_op_expr(&mut self) -> Option<Expr> {
        let maybe_op = self.tokens.next_if(|token| {
            matches!(
                token,
                Token::Symbol(Symbol::Add) | Token::Symbol(Symbol::Sub)
            )
        });
        match maybe_op {
            Some(Token::Symbol(Symbol::Add)) => {
                let right = self.parse_unary_op_expr()?;
                Some(Expr::new(ExprInner::Unary(UnaryOp::Plus, Box::new(right))))
            }
            Some(Token::Symbol(Symbol::Sub)) => {
                let right = self.parse_unary_op_expr()?;
                Some(Expr::new(ExprInner::Unary(UnaryOp::Minus, Box::new(right))))
            }
            _ => self.parse_exponent_expr(),
        }
    }

    //     -- Right-associative
    //     mulDivExpr = do
    //       left <- unaryOpExpr
    //       rest <-
    //         many
    //           ( do
    //               op <- try (binOperator MultiplyOp) <|> try (binOperator DivideOp)
    //               right <- mulDivExpr
    //               return (op, right)
    //           )
    //       return $ foldr (\(op, right) acc -> Expr {exprInner = BinExpr acc op right, exprType = ()}) left rest
    fn parse_mul_div_expr(&mut self) -> Option<Expr> {
        let left = self.parse_unary_op_expr()?;
        let mut rest = Vec::new();

        while let Some(op) = self.tokens.next_if(|token| {
            matches!(
                token,
                Token::Symbol(Symbol::Mul) | Token::Symbol(Symbol::Div)
            )
        }) {
            let right = self.parse_mul_div_expr()?;
            let binary_op = match op {
                Token::Symbol(Symbol::Mul) => BinaryOp::Mul,
                Token::Symbol(Symbol::Div) => BinaryOp::Div,
                _ => unreachable!(),
            };
            rest.push((binary_op, right));
        }

        if rest.is_empty() {
            return Some(left);
        }

        let mut result = left;
        for (op, right) in rest.into_iter().rev() {
            result = Expr::new(ExprInner::Binary(Box::new(result), op, Box::new(right)));
        }
        Some(result)
    }

    //     -- Right-associative
    //     addSubExpr = do
    //       left <- mulDivExpr
    //       rest <-
    //         many
    //           ( do
    //               op <- try (binOperator AddOp) <|> try (binOperator SubtractOp)
    //               right <- addSubExpr
    //               return (op, right)
    //           )
    //       return $ foldl (\acc (op, right) -> Expr {exprInner = BinExpr acc op right, exprType = ()}) left rest

    //     -- Right-associative: A = B = C < D = E = F
    //     comparisonExpr = do
    //       left <- addSubExpr
    //       maybeOp <-
    //         optional
    //           ( -- Order matters, parse longest operators first
    //             try (binOperator EqualOp) -- =
    //               <|> try (binOperator NotEqualOp) -- <>
    //               <|> try (binOperator LessThanOrEqualOp) -- <=
    //               <|> try (binOperator LessThanOp) -- <
    //               <|> try (binOperator GreaterThanOrEqualOp) -- >=
    //               <|> try (binOperator GreaterThanOp) -- >
    //           )
    //       case maybeOp of
    //         Just op -> do
    //           right <- comparisonExpr
    //           return Expr {exprInner = BinExpr left op right, exprType = ()}
    //         Nothing -> return left

    //     unaryLogicalExpr = do
    //       maybeOp <- optional (try (unaryOperator UnaryNotOp))
    //       case maybeOp of
    //         Just op -> do
    //           right <- unaryLogicalExpr
    //           return Expr {exprInner = UnaryExpr op right, exprType = ()}
    //         Nothing -> comparisonExpr

    //     andExpr = do
    //       left <- unaryLogicalExpr
    //       rest <-
    //         many
    //           ( do
    //               op <- try (binOperator AndOp)
    //               right <- andExpr
    //               return (op, right)
    //           )
    //       return $ foldl (\acc (op, right) -> Expr {exprInner = BinExpr acc op right, exprType = ()}) left rest

    //     orExpr = do
    //       left <- andExpr
    //       rest <-
    //         many
    //           ( do
    //               op <- try (binOperator OrOp)
    //               right <- orExpr
    //               return (op, right)
    //           )
    //       return $ foldl (\acc (op, right) -> Expr {exprInner = BinExpr acc op right, exprType = ()}) left rest

    fn parse_expression(&mut self) -> Option<expression::Expr> {
        // This is a placeholder for the actual expression parsing logic
        // It should return an Expr based on the tokens available
        None
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

    pub fn parse(&mut self) -> program::Program {
        unimplemented!();
    }
}
