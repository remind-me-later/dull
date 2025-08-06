use std::num::NonZero;

// Identifiers can have a maximum length of 2 character (ASCII).
// The first character must be an ASCII letter (a-z, A-Z).
// The second character, if present, can be an ASCII letter or digit (0-9).
// If the identifier ends with a '$', it is a string variable.
// Else it's a numeric variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Identifier {
    char1: NonZero<u8>,         // ASCII character
    char2: Option<NonZero<u8>>, // ASCII character or None
    has_dollar: bool,
}

impl Identifier {
    pub fn new(char1: u8, char2: Option<u8>, has_dollar: bool) -> Option<Self> {
        let char1 = NonZero::new(char1)?;
        let char2 = char2.and_then(NonZero::new);

        Some(Identifier {
            char1,
            char2,
            has_dollar,
        })
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let printable_char1 = char::from_u32(u32::from(self.char1.get())).ok_or(std::fmt::Error)?;
        write!(f, "{printable_char1}")?;
        if let Some(char2) = self.char2 {
            let printable_char2 = char::from_u32(u32::from(char2.get())).ok_or(std::fmt::Error)?;
            write!(f, "{printable_char2}")?;
        }
        if self.has_dollar {
            write!(f, "$")?;
        }
        Ok(())
    }
}
