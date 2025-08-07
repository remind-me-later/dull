use crate::{
    lex::{binary_number::BinaryNumber, decimal_number::DecimalNumber},
    parse::expression::{
        Expr, binary_op::BinaryOp, function::Function, lvalue::LValue, unary_op::UnaryOp,
    },
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprInner {
    UnaryExpr(UnaryOp, Box<Expr>),
    BinExpr(Box<Expr>, BinaryOp, Box<Expr>),
    DecNumLitExpr(DecimalNumber),
    HexNumLitExpr(BinaryNumber),
    LValueExpr(LValue),
    StrLitExpr(String),
    FunCallExpr(Function),
}

impl ExprInner {
    pub fn show_with_context(&self, parent_prec: u8, is_right_side: bool) -> String {
        match self {
            ExprInner::UnaryExpr(op, expr) => {
                format!("{}{}", op, expr.show_with_context(7, false)) // Unary has highest precedence
            }
            ExprInner::BinExpr(left, op, right) => {
                let my_prec = op.precedence();
                // Everything is left associative except for OR and AND
                // If the operator is not associative, we don't need parentheses
                let is_associative = op.is_associative();
                let needs_parens = is_associative
                    && (my_prec < parent_prec || (my_prec == parent_prec && is_right_side));
                let left_str = left.show_with_context(my_prec, false);
                let right_str = right.show_with_context(my_prec, true);
                let result = match op {
                    BinaryOp::Or => format!("{left_str} OR {right_str}"),
                    BinaryOp::And => format!("{left_str} AND {right_str}"),
                    _ => format!("{left_str}{op}{right_str}"), // For other operators, use the default spacing
                };
                if needs_parens {
                    format!("({result})")
                } else {
                    result
                }
            }
            ExprInner::DecNumLitExpr(n) => n.to_string(),
            ExprInner::HexNumLitExpr(h) => h.to_string(),
            ExprInner::LValueExpr(lval) => lval.to_string(),
            ExprInner::StrLitExpr(s) => format!("\"{s}\""),
            ExprInner::FunCallExpr(f) => f.to_string(),
        }
    }
}

impl std::fmt::Display for ExprInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprInner::UnaryExpr(op, expr) => write!(f, "{op}{}", expr.show_with_context(7, false)),
            e @ ExprInner::BinExpr(..) => {
                write!(f, "{}", e.show_with_context(0, false))
            }
            ExprInner::DecNumLitExpr(n) => write!(f, "{n}"),
            ExprInner::HexNumLitExpr(h) => write!(f, "{h}"),
            ExprInner::LValueExpr(lval) => write!(f, "{lval}"),
            ExprInner::StrLitExpr(s) => write!(f, "\"{s}\""),
            ExprInner::FunCallExpr(function) => write!(f, "{function}"),
        }
    }
}
