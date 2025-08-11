pub mod binary_op;
pub mod expr_inner;
pub mod function;
pub mod lvalue;
pub mod memory_area;
pub mod unary_op;

use crate::{error::Span, parse::expression::expr_inner::ExprInner};

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    inner: ExprInner,
    span: Span,
}

impl Expr {
    pub fn new(inner: ExprInner, span: Span) -> Self {
        Self { inner, span }
    }

    pub fn show_with_context_and_parens(
        &self,
        parent_prec: u8,
        is_right_side: bool,
        preserve_source_wording: bool,
    ) -> String {
        self.inner
            .show_with_context_and_parens(parent_prec, is_right_side, preserve_source_wording)
    }

    pub fn write_bytes_with_context_and_parens(
        &self,
        bytes: &mut Vec<u8>,
        parent_prec: u8,
        is_right_side: bool,
        preserve_source_wording: bool,
    ) {
        self.inner.write_bytes_with_context_and_parens(
            bytes,
            parent_prec,
            is_right_side,
            preserve_source_wording,
        );
    }

    pub fn write_bytes(&self, bytes: &mut Vec<u8>, preserve_source_wording: bool) {
        self.inner
            .write_bytes_with_context_and_parens(bytes, 0, false, preserve_source_wording);
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn inner(&self) -> &ExprInner {
        &self.inner
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}
