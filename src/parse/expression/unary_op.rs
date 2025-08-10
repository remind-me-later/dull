use crate::lex::keyword::Keyword;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Minus,
    Plus,
    Not,
}

impl UnaryOp {
    pub fn write_bytes(&self, bytes: &mut Vec<u8>) {
        match self {
            UnaryOp::Minus => bytes.push(b'-'),
            UnaryOp::Plus => bytes.push(b'+'),
            UnaryOp::Not => {
                bytes.extend_from_slice(Keyword::Not.internal_code().to_be_bytes().as_slice())
            }
        }
    }
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Minus => write!(f, "-"),
            UnaryOp::Plus => write!(f, ""),
            UnaryOp::Not => write!(f, "NOT"),
        }
    }
}
