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

    pub fn show(&self, preserve_source_wording: bool) -> String {
        if let Some(duration) = &self.duration {
            format!(
                ",{},{}",
                self.frequency.show(preserve_source_wording),
                duration.show(preserve_source_wording)
            )
        } else {
            format!(",{}", self.frequency.show(preserve_source_wording))
        }
    }
}
