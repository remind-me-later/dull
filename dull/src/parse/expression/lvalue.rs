use crate::{
    lex::identifier::{BuiltInIdentifier, Identifier},
    parse::expression::Expr,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LValue {
    // A, BB$, etc.
    Identifier(Identifier),
    // TIME, INKEY$, etc.
    BuiltInIdentifier(BuiltInIdentifier),
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

impl std::fmt::Display for LValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LValue::Identifier(id) => write!(f, "{id}"),
            LValue::BuiltInIdentifier(bi) => write!(f, "{bi}"),
            LValue::Array1DAccess { identifier, index } => write!(f, "{identifier}({index})"),
            LValue::Array2DAccess {
                identifier,
                row_index,
                col_index,
            } => write!(f, "{identifier}({row_index},{col_index})"),
            LValue::FixedMemoryAreaAccess { index, has_dollar } => {
                if *has_dollar {
                    write!(f, "@$({index})")
                } else {
                    write!(f, "@({index})")
                }
            }
        }
    }
}
