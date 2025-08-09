use crate::{
    lex::{binary_number::BinaryNumber, decimal_number::DecimalNumber},
    parse::expression::{
        Expr, binary_op::BinaryOp, function::Function, lvalue::LValue, unary_op::UnaryOp,
    },
};

#[derive(Debug, Clone, PartialEq)]
pub enum ExprInner {
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    DecimalNumber(DecimalNumber),
    BinaryNumber(BinaryNumber),
    LValue(LValue),
    StringLiteral(String),
    FunctionCall(Function),
}

impl ExprInner {
    pub fn show_with_context(&self, parent_prec: u8, is_right_side: bool) -> String {
        match self {
            ExprInner::Unary(op, expr) => {
                format!("{}{}", op, expr.show_with_context(7, false)) // Unary has highest precedence
            }
            ExprInner::Binary(left, op, right) => {
                let my_prec = op.precedence();
                // Everything is left associative
                let needs_parens =
                    my_prec < parent_prec || (my_prec == parent_prec && is_right_side);
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
            ExprInner::DecimalNumber(n) => n.to_string(),
            ExprInner::BinaryNumber(h) => h.to_string(),
            ExprInner::LValue(lval) => lval.to_string(),
            ExprInner::StringLiteral(s) => format!("\"{s}\""),
            ExprInner::FunctionCall(f) => f.to_string(),
        }
    }

    pub fn write_bytes_with_context(
        &self,
        bytes: &mut Vec<u8>,
        parent_prec: u8,
        is_right_side: bool,
    ) {
        match self {
            ExprInner::Unary(op, expr) => {
                op.write_bytes(bytes);
                expr.write_bytes_with_context(bytes, 7, false);
            }
            ExprInner::Binary(left, op, right) => {
                let my_prec = op.precedence();
                let needs_parens =
                    my_prec < parent_prec || (my_prec == parent_prec && is_right_side);
                if needs_parens {
                    bytes.push(b'(');
                }
                left.write_bytes_with_context(bytes, my_prec, false);
                op.write_bytes(bytes);
                right.write_bytes_with_context(bytes, my_prec, true);
                if needs_parens {
                    bytes.push(b')');
                }
            }
            ExprInner::DecimalNumber(n) => n.write_bytes(bytes),
            ExprInner::BinaryNumber(h) => h.write_bytes(bytes),
            ExprInner::LValue(lval) => lval.write_bytes(bytes),
            ExprInner::StringLiteral(s) => {
                bytes.extend_from_slice(format!("\"{s}\"").as_bytes());
            }
            ExprInner::FunctionCall(f) => f.write_bytes(bytes),
        }
    }

    pub fn write_bytes(&self, bytes: &mut Vec<u8>) {
        self.write_bytes_with_context(bytes, 0, false);
    }
}

impl std::fmt::Display for ExprInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprInner::Unary(op, expr) => write!(f, "{op}{}", expr.show_with_context(7, false)),
            e @ ExprInner::Binary(..) => {
                write!(f, "{}", e.show_with_context(0, false))
            }
            ExprInner::DecimalNumber(n) => write!(f, "{n}"),
            ExprInner::BinaryNumber(h) => write!(f, "{h}"),
            ExprInner::LValue(lval) => write!(f, "{lval}"),
            ExprInner::StringLiteral(s) => write!(f, "\"{s}\""),
            ExprInner::FunctionCall(function) => write!(f, "{function}"),
        }
    }
}
