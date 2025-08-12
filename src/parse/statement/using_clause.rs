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

    pub fn show(&self, preserve_source_wording: bool) -> String {
        let mut result = String::new();
        result.push_str("USING");
        if let Some(ref format) = self.format {
            result.push(' ');
            result.push_str(&format.show(preserve_source_wording));
        }
        result
    }
}
