use crate::error::Span;
use crate::parse::statement::Statement;

pub struct CodeLine {
    pub number: u16,
    pub label: Option<String>,
    pub statements: Vec<Statement>,
    pub span: Span,
}

impl CodeLine {
    pub fn new(number: u16, label: Option<String>, statements: Vec<Statement>, span: Span) -> Self {
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

    pub fn write_bytes(&self, bytes: &mut Vec<u8>) {
        bytes.extend_from_slice(self.number.to_le_bytes().as_slice());
        if let Some(label) = &self.label {
            bytes.push(b'"');
            bytes.extend_from_slice(label.as_bytes());
            bytes.push(b'"');
        }
        for (i, statement) in self.statements.iter().enumerate() {
            statement.write_bytes(bytes);
            if i < self.statements.len() - 1 {
                bytes.push(b':');
            }
        }
    }
}

impl std::fmt::Display for CodeLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(label) = &self.label {
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
