use crate::error::Span;
use crate::parse::statement::Statement;

pub struct CodeLineLabel {
    name: String,
    are_quotes_closed_in_source: bool,
    has_colon_in_source: bool,
}

impl CodeLineLabel {
    pub fn new(
        name: String,
        are_quotes_closed_in_source: bool,
        span: Span,
        has_colon_in_source: bool,
    ) -> Self {
        Self {
            name,
            are_quotes_closed_in_source,
            has_colon_in_source,
        }
    }
}

pub struct CodeLine {
    number: u16,
    label: Option<CodeLineLabel>,
    statements: Vec<Statement>,
    span: Span,
}

impl CodeLine {
    pub fn new(
        number: u16,
        label: Option<CodeLineLabel>,
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

    pub fn label(&self) -> Option<&str> {
        self.label.as_ref().map(|label| label.name.as_str())
    }

    pub fn write_bytes(&self, bytes: &mut Vec<u8>, preserve_source_wording: bool) {
        bytes.extend_from_slice(self.number.to_be_bytes().as_slice());
        // Add placeholder bytes for line size
        let line_size_pos = bytes.len();
        bytes.push(0);

        if let Some(label) = &self.label {
            bytes.push(b'"');
            bytes.extend_from_slice(label.name.as_bytes());
            if label.are_quotes_closed_in_source {
                bytes.push(b'"');
            }
            if label.has_colon_in_source {
                bytes.push(b':');
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
        if let Some(label) = &self.label {
            write!(f, "{} \"{}\"", self.number, label.name)?;
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
