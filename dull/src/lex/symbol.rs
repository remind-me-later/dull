#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Symbol {
    Add,
    Sub,
    Mul,
    Div,
    Exp,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    Comma,
    Semicolon,
    Colon,
    At,
    Dollar,
    LParen,
    RParen,
    Newline, // Newlines have meaning in BASIC
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Add => write!(f, "+"),
            Symbol::Sub => write!(f, "-"),
            Symbol::Mul => write!(f, "*"),
            Symbol::Div => write!(f, "/"),
            Symbol::Exp => write!(f, "^"),
            Symbol::Eq => write!(f, "="),
            Symbol::Neq => write!(f, "<>"),
            Symbol::Lt => write!(f, "<"),
            Symbol::Gt => write!(f, ">"),
            Symbol::Lte => write!(f, "<="),
            Symbol::Gte => write!(f, ">="),
            Symbol::Comma => write!(f, ","),
            Symbol::Semicolon => write!(f, ";"),
            Symbol::Colon => write!(f, ":"),
            Symbol::LParen => write!(f, "("),
            Symbol::RParen => write!(f, ")"),
            Symbol::Newline => write!(f, "\\n"),
            Symbol::At => write!(f, "@"),
            Symbol::Dollar => write!(f, "$"),
        }
    }
}
