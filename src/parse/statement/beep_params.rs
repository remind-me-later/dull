use crate::{error::Span, parse::expression::Expr};

#[derive(Debug, Clone, PartialEq)]
pub struct BeepParams {
    pub frequency: Expr,
    pub duration: Option<Expr>,
    pub span: Span,
}

impl BeepParams {
    pub fn new(frequency: Expr, duration: Option<Expr>, span: Span) -> Self {
        Self {
            frequency,
            duration,
            span,
        }
    }
}

impl std::fmt::Display for BeepParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ",{}", self.frequency)?;
        if let Some(duration) = &self.duration {
            write!(f, ",{duration}")?;
        }
        Ok(())
    }
}
