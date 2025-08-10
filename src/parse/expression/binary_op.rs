use crate::lex::keyword::Keyword;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic operations
    Add,
    Sub,
    Mul,
    Div,
    Exp,
    // Comparison operations
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    // Logical operations
    And,
    Or,
}

impl BinaryOp {
    // Manual page 151
    pub fn precedence(&self) -> u8 {
        match self {
            Self::Or | Self::And => 1,
            Self::Eq | Self::Neq | Self::Lt | Self::Gt | Self::Leq | Self::Geq => 2,
            Self::Add | Self::Sub => 3,
            Self::Mul | Self::Div => 4,
            Self::Exp => 5,
        }
    }

    pub fn write_bytes(&self, bytes: &mut Vec<u8>) {
        match self {
            BinaryOp::Add => bytes.push(b'+'),
            BinaryOp::Sub => bytes.push(b'-'),
            BinaryOp::Mul => bytes.push(b'*'),
            BinaryOp::Div => bytes.push(b'/'),
            BinaryOp::Exp => bytes.push(b'^'),
            BinaryOp::Eq => bytes.push(b'='),
            BinaryOp::Neq => bytes.extend_from_slice(b"<>"),
            BinaryOp::Lt => bytes.extend_from_slice(b"<"),
            BinaryOp::Gt => bytes.extend_from_slice(b">"),
            BinaryOp::Leq => bytes.extend_from_slice(b"<="),
            BinaryOp::Geq => bytes.extend_from_slice(b">="),
            BinaryOp::And => {
                bytes.extend_from_slice(Keyword::And.internal_code().to_be_bytes().as_slice())
            }
            BinaryOp::Or => {
                bytes.extend_from_slice(Keyword::Or.internal_code().to_be_bytes().as_slice())
            }
        }
    }
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Exp => write!(f, "^"),
            BinaryOp::Eq => write!(f, "="),
            BinaryOp::Neq => write!(f, "<>"),
            BinaryOp::Lt => write!(f, "<"),
            BinaryOp::Gt => write!(f, ">"),
            BinaryOp::Leq => write!(f, "<="),
            BinaryOp::Geq => write!(f, ">="),
            BinaryOp::And => write!(f, "AND"),
            BinaryOp::Or => write!(f, "OR"),
        }
    }
}
