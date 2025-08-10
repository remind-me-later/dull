use crate::{
    error::Span,
    parse::statement::{lprint_printable::LPrintPrintable, print_separator::PrintSeparator},
};

#[derive(Debug, Clone, PartialEq)]
pub struct LPrintInner {
    pub exprs: Vec<(LPrintPrintable, PrintSeparator)>,
    pub span: Span,
}

impl LPrintInner {
    pub fn new(exprs: Vec<(LPrintPrintable, PrintSeparator)>, span: Span) -> Self {
        Self { exprs, span }
    }

    pub fn to_string_with_context(&self, is_inside_lprint: bool) -> String {
        let mut result = String::new();
        for (printable, sep) in self.exprs.iter() {
            match printable {
                LPrintPrintable::Expr(expr) => {
                    result.push_str(&expr.to_string());
                }
                LPrintPrintable::LCursorClause(cursor) => {
                    result.push_str(&cursor.to_string_with_context(is_inside_lprint));
                }
            }
            result.push_str(&sep.to_string());
        }
        result
    }

    pub fn write_bytes_with_context(
        &self,
        is_inside_lprint: bool,
        bytes: &mut Vec<u8>,
        preserve_source_wording: bool,
    ) {
        for (printable, sep) in self.exprs.iter() {
            match printable {
                LPrintPrintable::Expr(expr) => expr.write_bytes(bytes, preserve_source_wording),
                LPrintPrintable::LCursorClause(cursor) => {
                    cursor.write_bytes_with_context(is_inside_lprint, bytes, preserve_source_wording)
                }
            }
            sep.write_bytes(bytes);
        }
    }
}
