pub mod binary_op;
pub mod expr_inner;
pub mod function;
pub mod lvalue;
pub mod memory_area;
pub mod unary_op;

use crate::parse::expression::expr_inner::ExprInner;

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub inner: ExprInner,
}

impl Expr {
    pub fn new(inner: ExprInner) -> Self {
        Self { inner }
    }

    pub fn show_with_context(&self, parent_prec: u8, is_right_side: bool) -> String {
        self.inner.show_with_context(parent_prec, is_right_side)
    }

    pub fn write_bytes_with_context(
        &self,
        bytes: &mut Vec<u8>,
        parent_prec: u8,
        is_right_side: bool,
    ) {
        self.inner
            .write_bytes_with_context(bytes, parent_prec, is_right_side);
    }

    pub fn write_bytes(&self, bytes: &mut Vec<u8>) {
        self.inner.write_bytes(bytes);
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}
