use crate::{error::Span, lex::identifier::Identifier, parse::expression::Expr};

#[derive(Debug, Clone, PartialEq)]
pub enum DimInner {
    DimInner1D {
        identifier: Identifier,
        size: Expr,
        string_length: Option<Expr>,
        span: Span,
    },
    DimInner2D {
        identifier: Identifier,
        rows: Expr,
        cols: Expr,
        string_length: Option<Expr>,
        span: Span,
    },
}

impl std::fmt::Display for DimInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DimInner::DimInner1D {
                identifier,
                size,
                string_length,
                span: _,
            } => {
                write!(f, "{identifier}({size})")?;
                if let Some(len) = string_length {
                    write!(f, "*{len}")?;
                }
            }
            DimInner::DimInner2D {
                identifier,
                rows,
                cols,
                string_length,
                span: _,
            } => {
                write!(f, "{identifier}({rows},{cols})")?;
                if let Some(len) = string_length {
                    write!(f, "*{len}")?;
                }
            }
        }
        Ok(())
    }
}
