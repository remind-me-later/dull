use crate::{
    lex::{identifier::Identifier, keyword::Keyword},
    parse::expression::Expr,
    error::Span,
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

    pub fn write_bytes(&self, bytes: &mut Vec<u8>) {
        match &self.inner {
            LValueInner::Identifier(id) => id.write_bytes(bytes),
            LValueInner::BuiltInIdentifier(bi) => {
                bytes.extend_from_slice(bi.internal_code().to_le_bytes().as_slice());
            }
            LValueInner::Array1DAccess { identifier, index } => {
                identifier.write_bytes(bytes);
                bytes.push(b'(');
                index.write_bytes(bytes);
                bytes.push(b')');
            }
            LValueInner::Array2DAccess {
                identifier,
                row_index,
                col_index,
            } => {
                identifier.write_bytes(bytes);
                bytes.push(b'(');
                row_index.write_bytes(bytes);
                bytes.push(b',');
                col_index.write_bytes(bytes);
                bytes.push(b')');
            }
            LValueInner::FixedMemoryAreaAccess { index, has_dollar } => {
                bytes.push(b'@');
                if *has_dollar {
                    bytes.push(b'$');
                }
                bytes.push(b'(');
                index.write_bytes(bytes);
                bytes.push(b')');
            }
        }
    }
}

impl std::fmt::Display for LValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.inner {
            LValueInner::Identifier(id) => write!(f, "{id}"),
            LValueInner::BuiltInIdentifier(bi) => write!(f, "{bi}"),
            LValueInner::Array1DAccess { identifier, index } => write!(f, "{identifier}({index})"),
            LValueInner::Array2DAccess {
                identifier,
                row_index,
                col_index,
            } => write!(f, "{identifier}({row_index},{col_index})"),
            LValueInner::FixedMemoryAreaAccess { index, has_dollar } => {
                if *has_dollar {
                    write!(f, "@$({index})")
                } else {
                    write!(f, "@({index})")
                }
            }
        }
    }
}
