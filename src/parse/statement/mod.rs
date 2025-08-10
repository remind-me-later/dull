pub mod assignment;
pub mod beep_params;
pub mod dim_inner;
pub mod lcursor_clause;
pub mod let_inner;
pub mod line_inner;
pub mod lprint_inner;
pub mod lprint_printable;
pub mod print_inner;
pub mod print_separator;
pub mod printable;
pub mod statement_inner;
pub mod using_clause;

use crate::{error::Span, parse::statement::statement_inner::StatementInner};

#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub inner: StatementInner,
    pub span: Span,
}

impl Statement {
    pub fn new(inner: StatementInner, span: Span) -> Self {
        Self { inner, span }
    }
}

impl Statement {
    pub fn write_bytes(&self, bytes: &mut Vec<u8>, preserve_source_wording: bool) {
        self.inner.write_bytes(bytes, preserve_source_wording);
    }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}
