use crate::{error::Span, parse::statement::assignment::Assignment};

#[derive(Debug, Clone, PartialEq)]
pub struct LetInner {
    assignments: Vec<Assignment>,
    span: Span,
}

impl LetInner {
    pub fn new(assignments: Vec<Assignment>, span: Span) -> Self {
        Self { assignments, span }
    }

    pub fn assignments(&self) -> &Vec<Assignment> {
        &self.assignments
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn show_with_context(&self, is_mandatory_let: bool) -> String {
        if is_mandatory_let {
            format!(
                "LET {}",
                self.assignments
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(",")
            )
        } else {
            self.assignments
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(",")
        }
    }
}
