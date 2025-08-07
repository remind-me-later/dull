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
    pub fn precedence(&self) -> u8 {
        match self {
            Self::Or => 1,
            Self::And => 2,
            Self::Eq | Self::Neq | Self::Lt | Self::Gt | Self::Leq | Self::Geq => 3,
            Self::Add | Self::Sub => 4,
            Self::Mul | Self::Div => 5,
            Self::Exp => 6,
        }
    }

    pub fn is_associative(&self) -> bool {
        !matches!(self, Self::And | Self::Or)
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
