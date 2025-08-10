#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrintSeparator {
    Comma,
    Semicolon,
    None, // used for the last expression in a PRINT statement
}

impl PrintSeparator {
    pub fn write_bytes(&self, bytes: &mut Vec<u8>) {
        match self {
            PrintSeparator::Comma => bytes.push(b','),
            PrintSeparator::Semicolon => bytes.push(b';'),
            PrintSeparator::None => {}
        }
    }
}

impl std::fmt::Display for PrintSeparator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrintSeparator::Comma => write!(f, ","),
            PrintSeparator::Semicolon => write!(f, ";"),
            PrintSeparator::None => Ok(()), // No output for empty separator
        }
    }
}
