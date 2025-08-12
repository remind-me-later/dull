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
    StringLiteral {
        value: String,
        is_quote_closed_in_source: bool,
    },
    FunctionCall(Function),
    Parentheses(Box<Expr>),
}

impl ExprInner {
    pub fn show_with_context_and_parens(
        &self,
        parent_prec: u8,
        is_right_side: bool,
        preserve_source_wording: bool,
    ) -> String {
        match self {
            ExprInner::Unary(op, expr) => {
                format!(
                    "{}{}",
                    op,
                    expr.show_with_context_and_parens(7, false, preserve_source_wording)
                ) // Unary has highest precedence
            }
            ExprInner::Binary(left, op, right) => {
                let my_prec = op.precedence();

                let left_str =
                    left.show_with_context_and_parens(my_prec, false, preserve_source_wording);
                let right_str =
                    right.show_with_context_and_parens(my_prec, true, preserve_source_wording);

                let result = match op {
                    BinaryOp::Or => format!("{left_str} OR {right_str}"),
                    BinaryOp::And => format!("{left_str} AND {right_str}"),
                    _ => format!("{left_str}{op}{right_str}"), // For other operators, use the default spacing
                };

                // Everything is left associative
                let needs_parens = !preserve_source_wording
                    && (my_prec < parent_prec || (my_prec == parent_prec && is_right_side));

                if needs_parens {
                    format!("({result})")
                } else {
                    result
                }
            }
            ExprInner::DecimalNumber(n) => n.to_string(),
            ExprInner::BinaryNumber(h) => h.to_string(),
            ExprInner::LValue(lval) => lval.to_string(),
            ExprInner::StringLiteral { value, .. } => format!("\"{value}\""),
            ExprInner::FunctionCall(f) => f.to_string(),
            ExprInner::Parentheses(expr) => {
                if preserve_source_wording {
                    format!(
                        "({})",
                        expr.show_with_context_and_parens(0, false, preserve_source_wording)
                    )
                } else {
                    expr.show_with_context_and_parens(8, false, preserve_source_wording)
                }
            }
        }
    }

    pub fn write_bytes_with_context_and_parens(
        &self,
        bytes: &mut Vec<u8>,
        parent_prec: u8,
        is_right_side: bool,
        preserve_source_wording: bool,
    ) {
        match self {
            ExprInner::Unary(op, expr) => {
                op.write_bytes(bytes);
                expr.write_bytes_with_context_and_parens(bytes, 7, false, preserve_source_wording);
            }
            ExprInner::Binary(left, op, right) => {
                let my_prec = op.precedence();
                let needs_parens = !preserve_source_wording
                    && (my_prec < parent_prec || (my_prec == parent_prec && is_right_side));
                if needs_parens {
                    bytes.push(b'(');
                }
                left.write_bytes_with_context_and_parens(
                    bytes,
                    my_prec,
                    false,
                    preserve_source_wording,
                );
                op.write_bytes(bytes);
                right.write_bytes_with_context_and_parens(
                    bytes,
                    my_prec,
                    true,
                    preserve_source_wording,
                );
                if needs_parens {
                    bytes.push(b')');
                }
            }
            ExprInner::DecimalNumber(n) => n.write_bytes(bytes),
            ExprInner::BinaryNumber(h) => h.write_bytes(bytes),
            ExprInner::LValue(lval) => lval.write_bytes(bytes, preserve_source_wording),
            ExprInner::StringLiteral {
                value,
                is_quote_closed_in_source,
            } => {
                bytes.extend_from_slice(format!("\"{value}").as_bytes());
                if *is_quote_closed_in_source {
                    bytes.push(b'"');
                }
            }
            ExprInner::FunctionCall(f) => {
                f.write_bytes_preserving_source_parens(bytes, preserve_source_wording)
            }
            ExprInner::Parentheses(expr) => {
                expr.write_bytes_with_context_and_parens(bytes, 8, false, preserve_source_wording);
            }
        }
    }
}

impl std::fmt::Display for ExprInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprInner::Unary(op, expr) => write!(
                f,
                "{op}{}",
                expr.show_with_context_and_parens(7, false, false)
            ),
            e @ ExprInner::Binary(..) => {
                write!(f, "{}", e.show_with_context_and_parens(0, false, false))
            }
            ExprInner::DecimalNumber(n) => write!(f, "{n}"),
            ExprInner::BinaryNumber(h) => write!(f, "{h}"),
            ExprInner::LValue(lval) => write!(f, "{lval}"),
            ExprInner::StringLiteral { value, .. } => write!(f, "\"{value}\""),
            ExprInner::FunctionCall(function) => write!(f, "{function}"),
            ExprInner::Parentheses(expr) => {
                write!(f, "{}", expr.show_with_context_and_parens(0, false, false))
            }
        }
    }
}
