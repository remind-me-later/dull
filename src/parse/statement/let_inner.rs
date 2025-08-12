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

    pub fn show_with_context(
        &self,
        is_mandatory_let: bool,
        preserve_source_wording: bool,
    ) -> String {
        if is_mandatory_let {
            format!(
                "LET {}",
                self.assignments
                    .iter()
                    .map(|a| a.show(preserve_source_wording))
                    .collect::<Vec<_>>()
                    .join(",")
            )
        } else {
            self.assignments
                .iter()
                .map(|a| a.show(preserve_source_wording))
                .collect::<Vec<_>>()
                .join(",")
        }
    }

    pub fn write_bytes(&self, bytes: &mut Vec<u8>, preserve_source_wording: bool) {
        for (i, assignment) in self.assignments.iter().enumerate() {
            assignment.write_bytes(bytes, preserve_source_wording);
            if i < self.assignments.len() - 1 {
                bytes.push(b',');
            }
        }
    }
}
