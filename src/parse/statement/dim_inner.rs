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

impl DimInner {
    pub fn show(&self, preserve_source_wording: bool) -> String {
        match self {
            DimInner::DimInner1D {
                identifier,
                size,
                string_length,
                span: _,
            } => {
                let mut result = format!("{identifier}({})", size.show(preserve_source_wording));
                if let Some(len) = string_length {
                    result.push_str(&format!("*{}", len.show(preserve_source_wording)));
                }
                result
            }
            DimInner::DimInner2D {
                identifier,
                rows,
                cols,
                string_length,
                span: _,
            } => {
                let mut result = format!(
                    "{identifier}({},{})",
                    rows.show(preserve_source_wording),
                    cols.show(preserve_source_wording)
                );
                if let Some(len) = string_length {
                    result.push_str(&format!("*{}", len.show(preserve_source_wording)));
                }
                result
            }
        }
    }
}
