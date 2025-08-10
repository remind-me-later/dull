use crate::parse::{expression::Expr, statement::lcursor_clause::LCursorClause};

#[derive(Debug, Clone, PartialEq)]
pub enum LPrintPrintable {
    Expr(Expr),
    LCursorClause(LCursorClause),
}
