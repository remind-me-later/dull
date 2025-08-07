pub mod binary_number;
pub mod decimal_number;
pub mod identifier;
pub mod keyword;
pub mod symbol;

use std::iter::Peekable;

use crate::lex::identifier::BuiltInIdentifier;

use self::{
    binary_number::BinaryNumber, decimal_number::DecimalNumber, identifier::Identifier,
    keyword::Keyword, symbol::Symbol,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Keyword(Keyword),
    Symbol(Symbol),
    Identifier(Identifier),
    BuiltInIdentifier(BuiltInIdentifier),
    DecimalNumber(DecimalNumber),
    BinaryNumber(BinaryNumber),
    StringLiteral(String),
    Remark(String), // For BASIC comments
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Keyword(keyword) => write!(f, "{keyword}"),
            Token::Symbol(symbol) => write!(f, "{symbol}"),
            Token::Identifier(identifier) => write!(f, "{identifier}"),
            Token::DecimalNumber(decimal_number) => write!(f, "{decimal_number}"),
            Token::BinaryNumber(binary_number) => write!(f, "{binary_number}"),
            Token::StringLiteral(string_literal) => write!(f, "\"{string_literal}\""),
            Token::Remark(remark) => write!(f, "REM {remark}"),
            Token::BuiltInIdentifier(built_in) => write!(f, "{built_in}"),
        }
    }
}

pub struct Lexer<'a> {
    input: Peekable<std::str::Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
        }
    }

    fn parse_identifier(&self, word: &str) -> Result<Token, &'static str> {
        // try to parse a built-in identifier first
        if let Ok(built_in) = word.parse::<BuiltInIdentifier>() {
            return Ok(Token::BuiltInIdentifier(built_in));
        }

        // If not a built-in identifier, try to parse a regular identifier
        word.parse::<Identifier>().map(Token::Identifier)
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        // Skip whitespace (except newlines which are significant in BASIC)
        while let Some(&ch) = self.input.peek() {
            if ch.is_whitespace() && ch != '\n' {
                self.input.next();
            } else {
                break;
            }
        }

        let ch = self.input.next()?;

        match ch {
            // Newlines are significant in BASIC
            '\n' => Some(Ok(Token::Symbol(Symbol::Newline))),

            // Single character symbols
            '+' => Some(Ok(Token::Symbol(Symbol::Add))),
            '-' => Some(Ok(Token::Symbol(Symbol::Sub))),
            '*' => Some(Ok(Token::Symbol(Symbol::Mul))),
            '/' => Some(Ok(Token::Symbol(Symbol::Div))),
            '^' => Some(Ok(Token::Symbol(Symbol::Exp))),
            ',' => Some(Ok(Token::Symbol(Symbol::Comma))),
            ';' => Some(Ok(Token::Symbol(Symbol::Semicolon))),
            ':' => Some(Ok(Token::Symbol(Symbol::Colon))),
            '(' => Some(Ok(Token::Symbol(Symbol::LParen))),
            ')' => Some(Ok(Token::Symbol(Symbol::RParen))),

            // Comparison operators
            '=' => Some(Ok(Token::Symbol(Symbol::Eq))),
            '<' => {
                if self.input.peek() == Some(&'=') {
                    self.input.next();
                    Some(Ok(Token::Symbol(Symbol::Leq)))
                } else if self.input.peek() == Some(&'>') {
                    self.input.next();
                    Some(Ok(Token::Symbol(Symbol::Neq)))
                } else {
                    Some(Ok(Token::Symbol(Symbol::Lt)))
                }
            }
            '>' => {
                if self.input.peek() == Some(&'=') {
                    self.input.next();
                    Some(Ok(Token::Symbol(Symbol::Geq)))
                } else {
                    Some(Ok(Token::Symbol(Symbol::Gt)))
                }
            }
            '@' => Some(Ok(Token::Symbol(Symbol::At))),
            '$' => Some(Ok(Token::Symbol(Symbol::Dollar))),

            // String literals
            '"' => {
                let mut string_content = String::new();
                for ch in self.input.by_ref() {
                    if ch == '"' {
                        break;
                    }
                    string_content.push(ch);
                }
                Some(Ok(Token::StringLiteral(string_content)))
            }

            // Binary numbers (hexadecimal with & prefix)
            '&' => {
                let mut hex_digits = String::new();
                while let Some(&ch) = self.input.peek() {
                    if ch.is_ascii_hexdigit() {
                        hex_digits.push(ch.to_ascii_uppercase());
                        self.input.next();
                    } else {
                        break;
                    }
                }

                hex_digits
                    .parse::<BinaryNumber>()
                    .map(Token::BinaryNumber)
                    .ok()
                    .map(Ok)
            }

            // Numbers (decimal)
            ch if ch.is_ascii_digit() || ch == '.' => {
                let mut number_str = String::new();
                number_str.push(ch);

                let mut has_dot = ch == '.';
                let mut has_e = false;

                while let Some(&ch) = self.input.peek() {
                    if ch.is_ascii_digit() {
                        number_str.push(ch);
                        self.input.next();
                    } else if ch == '.' && !has_dot && !has_e {
                        has_dot = true;
                        number_str.push(ch);
                        self.input.next();
                    } else if (ch == 'E') && !has_e {
                        has_e = true;
                        number_str.push(ch);
                        self.input.next();

                        // Handle optional + or - after E
                        if let Some(&next_ch) = self.input.peek() {
                            if next_ch == '+' || next_ch == '-' {
                                number_str.push(next_ch);
                                self.input.next();
                            }
                        }
                    } else {
                        break;
                    }
                }

                number_str
                    .parse::<DecimalNumber>()
                    .map(Token::DecimalNumber)
                    .ok()
                    .map(Ok)
            }

            // Identifiers and keywords
            ch if ch.is_ascii_alphabetic() => {
                let mut word = String::new();
                word.push(ch.to_ascii_uppercase());

                // We need to do some lookahead, as identifiers don't need to be separated by whitespace
                // for example FOR I = BBTO 10 is a valid line that means:
                // FOR I = BB TO 10
                // So we need to read until we hit a non-alphanumeric character or a dollar sign
                {
                    let cloned_iter = self.input.clone();
                    for next_ch in cloned_iter {
                        if next_ch.is_ascii_alphanumeric() || next_ch == '$' || next_ch == '#' {
                            word.push(next_ch.to_ascii_uppercase());
                        } else {
                            break;
                        }
                    }
                }

                // Check if the word has any keyword as suffix
                for keyword in Keyword::LONGEST_SUFFIX_FIRST.iter() {
                    if word.ends_with(keyword) {
                        let keyword_len = keyword.len();
                        let ident_len = word.len() - keyword_len;
                        if ident_len == 0 {
                            // The whole word is a keyword
                            for _ in 1..keyword_len {
                                self.input.next();
                            }

                            let kw = Keyword::try_from(word.as_str()).unwrap();

                            // We need to handle comments in a special way, cosume until end of line
                            if kw == Keyword::Rem {
                                // The space after REM is mandatory
                                if self.input.next() != Some(' ') {
                                    return Some(Err(())); // Invalid REM syntax
                                }

                                let mut comment = String::new();
                                while let Some(&next_ch) = self.input.peek() {
                                    if next_ch == '\n' {
                                        break; // End of comment
                                    }
                                    comment.push(next_ch);
                                    self.input.next();
                                }
                                return Some(Ok(Token::Remark(comment)));
                            } else {
                                return Some(Ok(Token::Keyword(kw)));
                            }
                        } else {
                            // Remove the identifier from the original iterator
                            for _ in 1..ident_len {
                                self.input.next();
                            }

                            return Some(Ok(self.parse_identifier(&word[..ident_len]).unwrap()));
                        }
                    }
                }

                // If no keyword suffix found, treat as identifier
                for _ in 1..word.len() {
                    self.input.next(); // Consume the rest of the identifier
                }

                Some(Ok(self.parse_identifier(&word).unwrap()))
            }

            _ => None, // Unknown character, skip or handle as error
        }
    }
}
