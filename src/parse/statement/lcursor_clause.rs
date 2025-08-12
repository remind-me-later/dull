use crate::{error::Span, lex::keyword::Keyword, parse::expression::Expr};

#[derive(Debug, Clone, PartialEq)]
pub struct LCursorClause {
    pub expr: Expr,
    pub span: Span,
}

impl LCursorClause {
    pub fn new(expr: Expr, span: Span) -> Self {
        Self { expr, span }
    }

    pub fn to_string_with_context(
        &self,
        is_inside_lprint: bool,
        preserve_source_wording: bool,
    ) -> String {
        if is_inside_lprint {
            format!("LCURSOR {}", self.expr.show(preserve_source_wording))
        } else {
            format!("TAB {}", self.expr.show(preserve_source_wording))
        }
    }

    pub fn write_bytes_with_context(
        &self,
        is_inside_lprint: bool,
        bytes: &mut Vec<u8>,
        preserve_source_wording: bool,
    ) {
        if is_inside_lprint {
            bytes.extend_from_slice(Keyword::Lcursor.internal_code().to_be_bytes().as_slice());
        } else {
            bytes.extend_from_slice(Keyword::Tab.internal_code().to_be_bytes().as_slice());
        }
        self.expr.write_bytes(bytes, preserve_source_wording);
    }
}
