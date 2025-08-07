pub mod binary_op;
pub mod expr_inner;
pub mod function;
pub mod lvalue;
pub mod memory_area;
pub mod unary_op;

use crate::parse::expression::expr_inner::ExprInner;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    inner: ExprInner,
}

impl Expr {
    pub fn show_with_context(&self, parent_prec: u8, is_right_side: bool) -> String {
        self.inner.show_with_context(parent_prec, is_right_side)
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}
