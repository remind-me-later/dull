use crate::parse::{expression::Expr, statement::using_clause::UsingClause};

#[derive(Debug, Clone, PartialEq)]
pub enum Printable {
    Expr(Expr),
    UsingClause(UsingClause),
}

impl Printable {
    pub fn write_bytes(&self, bytes: &mut Vec<u8>, preserve_source_wording: bool) {
        match self {
            Printable::Expr(expr) => expr.write_bytes(bytes, preserve_source_wording),
            Printable::UsingClause(using) => using.write_bytes(bytes, preserve_source_wording),
        }
    }
}
