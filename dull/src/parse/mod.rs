use crate::lex::identifier::Identifier;

pub struct Expr {
    inner: ExprInner
}

pub enum ExprInner {}

pub enum LValue {
    Identifier(Identifier),
    Array1DAccess { identifier: Identifier, index: Expr },
    Array2DAccess { identifier: Identifier, index1: Expr, index2: Expr },
    
}