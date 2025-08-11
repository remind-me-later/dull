use crate::{error::Span, lex::keyword::Keyword, parse::expression::Expr};

#[derive(Debug, Clone, PartialEq)]
pub struct UsingClause {
    format: Option<Expr>,
    span: Span,
}

impl UsingClause {
    pub fn new(format: Option<Expr>, span: Span) -> Self {
        Self { format, span }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn format(&self) -> &Option<Expr> {
        &self.format
    }

    pub fn write_bytes(&self, bytes: &mut Vec<u8>, preserve_source_wording: bool) {
        bytes.extend_from_slice(Keyword::Using.internal_code().to_be_bytes().as_slice());
        if let Some(format) = &self.format {
            format.write_bytes(bytes, preserve_source_wording);
        }
    }
}

impl std::fmt::Display for UsingClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ref format) = self.format {
            write!(f, "USING {format}")
        } else {
            write!(f, "USING")
        }
    }
}
