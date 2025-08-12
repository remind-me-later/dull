use crate::{
    error::Span,
    lex::{identifier::Identifier, keyword::Keyword},
    parse::expression::Expr,
};

#[derive(Debug, Clone, PartialEq)]
pub struct LValue {
    pub inner: LValueInner,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LValueInner {
    // A, BB$, etc.
    Identifier(Identifier),
    // TIME, INKEY$, etc.
    BuiltInIdentifier(Keyword),
    // A(2), B$(E), etc.
    Array1DAccess {
        identifier: Identifier,
        index: Box<Expr>,
    },
    // A(2,3), B$(E, 2), etc.
    Array2DAccess {
        identifier: Identifier,
        row_index: Box<Expr>,
        col_index: Box<Expr>,
    },
    // @(2), @(E), @$(A), etc.
    FixedMemoryAreaAccess {
        index: Box<Expr>,
        has_dollar: bool,
    },
}

impl LValue {
    pub fn new(inner: LValueInner, span: Span) -> Self {
        Self { inner, span }
    }

    pub fn write_bytes(&self, bytes: &mut Vec<u8>, preserve_source_wording: bool) {
        match &self.inner {
            LValueInner::Identifier(id) => id.write_bytes(bytes),
            LValueInner::BuiltInIdentifier(bi) => {
                bytes.extend_from_slice(bi.internal_code().to_be_bytes().as_slice());
            }
            LValueInner::Array1DAccess { identifier, index } => {
                identifier.write_bytes(bytes);
                bytes.push(b'(');
                index.write_bytes(bytes, preserve_source_wording);
                bytes.push(b')');
            }
            LValueInner::Array2DAccess {
                identifier,
                row_index,
                col_index,
            } => {
                identifier.write_bytes(bytes);
                bytes.push(b'(');
                row_index.write_bytes(bytes, preserve_source_wording);
                bytes.push(b',');
                col_index.write_bytes(bytes, preserve_source_wording);
                bytes.push(b')');
            }
            LValueInner::FixedMemoryAreaAccess { index, has_dollar } => {
                bytes.push(b'@');
                if *has_dollar {
                    bytes.push(b'$');
                }
                bytes.push(b'(');
                index.write_bytes(bytes, preserve_source_wording);
                bytes.push(b')');
            }
        }
    }

    pub fn show(&self, preserve_source_wording: bool) -> String {
        match &self.inner {
            LValueInner::Identifier(id) => format!("{}", id),
            LValueInner::BuiltInIdentifier(bi) => format!("{}", bi),
            LValueInner::Array1DAccess { identifier, index } => {
                format!(
                    "{}({})",
                    format!("{}", identifier),
                    index.show(preserve_source_wording)
                )
            }
            LValueInner::Array2DAccess {
                identifier,
                row_index,
                col_index,
            } => {
                format!(
                    "{}({},{})",
                    format!("{}", identifier),
                    row_index.show(preserve_source_wording),
                    col_index.show(preserve_source_wording)
                )
            }
            LValueInner::FixedMemoryAreaAccess { index, has_dollar } => {
                if *has_dollar {
                    format!("@$({})", index.show(preserve_source_wording))
                } else {
                    format!("@({})", index.show(preserve_source_wording))
                }
            }
        }
    }
}
