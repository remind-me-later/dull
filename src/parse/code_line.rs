use crate::error::Span;
use crate::parse::statement::Statement;

pub struct CodeLine {
    number: u16,
    label: Option<(String, bool)>, // The label and whether its quotes are closed
    statements: Vec<Statement>,
    span: Span,
}

impl CodeLine {
    pub fn new(
        number: u16,
        label: Option<(String, bool)>,
        statements: Vec<Statement>,
        span: Span,
    ) -> Self {
        Self {
            number,
            label,
            statements,
            span,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn statements(&self) -> &Vec<Statement> {
        &self.statements
    }

    pub fn number(&self) -> u16 {
        self.number
    }

    pub fn label(&self) -> Option<&String> {
        self.label.as_ref().map(|(label, _)| label)
    }

    pub fn write_bytes(&self, bytes: &mut Vec<u8>, preserve_source_wording: bool) {
        bytes.extend_from_slice(self.number.to_be_bytes().as_slice());
        // Add placeholder bytes for line size
        let line_size_pos = bytes.len();
        bytes.push(0);

        if let Some((label, is_label_closed_in_source)) = &self.label {
            bytes.push(b'"');
            bytes.extend_from_slice(label.as_bytes());
            if *is_label_closed_in_source {
                bytes.push(b'"');
            }
        }
        for (i, statement) in self.statements.iter().enumerate() {
            statement.write_bytes(bytes, preserve_source_wording);
            if i < self.statements.len() - 1 {
                bytes.push(b':');
            }
        }
        // Update the line size placeholder
        let line_size = (bytes.len() - line_size_pos) as u8;
        bytes[line_size_pos] = line_size;
    }
}

impl std::fmt::Display for CodeLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some((label, _)) = &self.label {
            write!(f, "{} \"{}\"", self.number, label)?;
        } else {
            write!(f, "{}", self.number)?;
        }
        if !self.statements.is_empty() {
            write!(
                f,
                " {}",
                self.statements
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(":")
            )?;
        }
        Ok(())
    }
}
