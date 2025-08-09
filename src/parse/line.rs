use crate::parse::statement::Statement;

pub struct Line {
    pub number: u16,
    pub label: Option<String>,
    pub statements: Vec<Statement>,
}

impl Line {
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

impl std::fmt::Display for Line {
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
