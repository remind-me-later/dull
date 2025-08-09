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

    pub fn has_dollar(&self) -> bool {
        self.has_dollar
    }

    pub fn write_bytes(&self, bytes: &mut Vec<u8>) {
        bytes.push(self.char1.get());
        if let Some(char2) = self.char2 {
            bytes.push(char2.get());
        }
        if self.has_dollar {
            bytes.push(b'$');
        }
    }
}

impl std::str::FromStr for Identifier {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let bytes = s.as_bytes();

        if bytes.is_empty() {
            return Err("Invalid empty identifier");
        }

        // Check if first character is alphabetic
        if !bytes[0].is_ascii_alphabetic() {
            return Err("Invalid first character for identifier");
        }

        match bytes.len() {
            1 => {
                // Single character identifier
                Identifier::new(bytes[0], None, false).ok_or("Invalid single character identifier")
            }
            2 => {
                if bytes[1] == b'$' {
                    Identifier::new(bytes[0], None, true).ok_or("Invalid identifier ending with $")
                } else if bytes[1].is_ascii_alphanumeric() {
                    Identifier::new(bytes[0], Some(bytes[1]), false)
                        .ok_or("Invalid second character for identifier")
                } else {
                    Err("Invalid second character for identifier")
                }
            }
            3 => {
                if bytes[2] == b'$' && bytes[1].is_ascii_alphanumeric() {
                    Identifier::new(bytes[0], Some(bytes[1]), true)
                        .ok_or("Invalid identifier ending with $")
                } else {
                    Err("Invalid 3-character pattern")
                }
            }
            _ => Err("Invalid identifier length"),
        }
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
