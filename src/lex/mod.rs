pub mod binary_number;
pub mod decimal_number;
pub mod identifier;
pub mod keyword;
pub mod symbol;

use std::iter::Peekable;

use self::{
    binary_number::BinaryNumber, decimal_number::DecimalNumber, identifier::Identifier,
    keyword::Keyword, symbol::Symbol,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Keyword(Keyword),
    Symbol(Symbol),
    Identifier(Identifier),
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
        }
    }
}

pub struct Lexer<'a> {
    input: Peekable<std::str::Chars<'a>>,
    is_done: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
            is_done: false,
        }
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

        let ch = self.input.next();

        if ch.is_none() {
            if self.is_done {
                return None; // Already done
            } else {
                self.is_done = true;
                return Some(Ok(Token::Symbol(Symbol::Eof))); // End of input
            }
        }

        let ch = ch?;

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
            '√' => Some(Ok(Token::Keyword(Keyword::Sqr))),

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
                let mut hex_digits = String::from("&");
                while let Some(&ch) = self.input.peek() {
                    if ch.is_ascii_hexdigit() {
                        hex_digits.push(ch.to_ascii_uppercase());
                        self.input.next();
                    } else {
                        break;
                    }
                }

                Some(Ok(Token::BinaryNumber(
                    hex_digits
                        .parse::<BinaryNumber>()
                        .unwrap_or_else(|_| panic!("Invalid binary number format: {hex_digits}")),
                )))
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
                    // This logic is complex so we can handle identifiers next to keywords next to numbers,
                    // for example IF B=AGOSUB3 => IF B=A GOSUB 3
                    let cloned_iter = self.input.clone();
                    for (i, next_ch) in cloned_iter.enumerate() {
                        if (i < 1 && (next_ch.is_ascii_alphanumeric()))
                            || next_ch.is_ascii_alphabetic()
                        {
                            word.push(next_ch.to_ascii_uppercase());
                        } else if next_ch == '$' || next_ch == '#' {
                            word.push(next_ch.to_ascii_uppercase());
                            break;
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

                            return Some(Ok(word[..ident_len]
                                .parse::<Identifier>()
                                .map(Token::Identifier)
                                .unwrap()));
                        }
                    }
                }

                // If no keyword suffix found, treat as identifier
                for _ in 1..word.len() {
                    self.input.next(); // Consume the rest of the identifier
                }

                Some(Ok(word
                    .parse::<Identifier>()
                    .map(Token::Identifier)
                    .unwrap()))
            }

            _ => panic!("Unexpected character: {ch}"),
        }
    }
}
