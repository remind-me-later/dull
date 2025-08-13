use std::num::NonZero;

// Identifiers can have a maximum length of 2 character (ASCII).
// The first character must be an ASCII letter (a-z, A-Z).
// The second character, if present, can be an ASCII letter or digit (0-9).
// If the identifier ends with a '$', it is a string variable.
// Else it's a numeric variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier {
    char1: NonZero<u8>,         // ASCII character
    char2: Option<NonZero<u8>>, // ASCII character or None
    has_dollar: bool,
}

impl Identifier {
    pub fn new(char1: char, char2: Option<char>, has_dollar: bool) -> Result<Self, &'static str> {
        let char1 = if char1.is_ascii_alphabetic() {
            NonZero::new(char1 as u8).ok_or("Invalid first character")?
        } else {
            return Err("Invalid first character");
        };

        let char2 = if let Some(c) = char2 {
            if c.is_ascii_alphanumeric() {
                Some(NonZero::new(c as u8).ok_or("Invalid second character")?)
            } else {
                return Err("Invalid second character");
            }
        } else {
            None
        };

        Ok(Identifier {
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
        match s.len() {
            0 => Err("Invalid empty identifier"),
            1 => {
                let char = s.chars().next().unwrap();
                Identifier::new(char, None, false)
            }
            2 => {
                let char1 = s.chars().next().unwrap();
                let char2 = s.chars().nth(1).unwrap();

                if char2 == '$' {
                    Identifier::new(char1, None, true)
                } else {
                    Identifier::new(char1, Some(char2), false)
                }
            }
            3 => {
                let char1 = s.chars().next().unwrap();
                let char2 = s.chars().nth(1).unwrap();
                let char3 = s.chars().nth(2).unwrap();

                if char3 == '$' {
                    Identifier::new(char1, Some(char2), true)
                } else {
                    Err("Invalid 3-character pattern")
                }
            }
            _ => Err("Identifier too long, maximum length is 2 characters and an optional $"),
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
