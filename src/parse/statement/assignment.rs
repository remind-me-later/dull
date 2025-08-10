use crate::{
    error::Span,
    parse::expression::{Expr, lvalue::LValue},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    lvalue: LValue,
    expr: Box<Expr>,
    span: Span,
}

impl Assignment {
    pub fn new(lvalue: LValue, expr: Box<Expr>, span: Span) -> Self {
        Self { lvalue, expr, span }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn lvalue(&self) -> &LValue {
        &self.lvalue
    }

    pub fn expr(&self) -> &Expr {
        &self.expr
    }

    pub fn write_bytes(&self, bytes: &mut Vec<u8>, preserve_source_wording: bool) {
        self.lvalue.write_bytes(bytes, preserve_source_wording);
        bytes.push(b'=');
        self.expr.write_bytes(bytes, preserve_source_wording);
    }
}

impl std::fmt::Display for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}={}", self.lvalue, self.expr)
    }
}
