use crate::{error::Span, lex::keyword::Keyword};

#[derive(Debug, Clone, PartialEq)]
pub struct UsingClause {
    // -- FIXME: maybe this should allow also variables as format strings?
    format: Option<String>,
    span: Span,
}

impl UsingClause {
    pub fn new(format: Option<String>, span: Span) -> Self {
        Self { format, span }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn write_bytes(&self, bytes: &mut Vec<u8>) {
        bytes.extend_from_slice(Keyword::Using.internal_code().to_le_bytes().as_slice());
        if let Some(format) = &self.format {
            bytes.push(b'"');
            bytes.extend_from_slice(format.as_bytes());
            bytes.push(b'"');
        }
    }
}

impl std::fmt::Display for UsingClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ref format) = self.format {
            write!(f, "USING \"{format}\"")
        } else {
            write!(f, "USING")
        }
    }
}
