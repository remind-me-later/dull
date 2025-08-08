use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

/// Represents a span in the source code
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn single(position: usize) -> Self {
        Self {
            start: position,
            end: position + 1,
        }
    }

    pub fn extend(&self, other: Span) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

/// Error types that can occur during lexical analysis
#[derive(Debug, Clone)]
pub enum LexError {
    UnexpectedCharacter { ch: char, span: Span },
    UnterminatedStringLiteral { span: Span },
    InvalidBinaryNumber { text: String, span: Span },
    InvalidRemark { span: Span },
}

/// Error types that can occur during parsing
#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedToken {
        expected: String,
        found: String,
        span: Span,
    },
    UnexpectedEndOfInput {
        expected: String,
        span: Span,
    },
    InvalidExpression {
        message: String,
        span: Span,
    },
    MissingToken {
        token: String,
        span: Span,
    },
}

/// Error types that can occur during semantic analysis
#[derive(Debug, Clone)]
pub enum SemanticError {
    TypeMismatch {
        expected: crate::semantic_analysis::BasicType,
        found: crate::semantic_analysis::BasicType,
        span: Span,
    },
    InvalidArrayIndex {
        message: String,
        span: Span,
    },
    InvalidFunctionArgument {
        function_name: String,
        message: String,
        span: Span,
    },
    InvalidExpression {
        message: String,
        span: Span,
    },
}

/// Combined error type for all compilation phases
#[derive(Debug, Clone)]
pub enum CompileError {
    Lex(LexError),
    Parse(ParseError),
    Semantic(SemanticError),
}

impl From<LexError> for CompileError {
    fn from(err: LexError) -> Self {
        CompileError::Lex(err)
    }
}

impl From<ParseError> for CompileError {
    fn from(err: ParseError) -> Self {
        CompileError::Parse(err)
    }
}

impl From<SemanticError> for CompileError {
    fn from(err: SemanticError) -> Self {
        CompileError::Semantic(err)
    }
}

/// Pretty-print errors using codespan
pub fn print_error(error: &CompileError, filename: &str, source: &str) {
    let mut files = SimpleFiles::new();
    let file_id = files.add(filename, source);

    let diagnostic = match error {
        CompileError::Lex(lex_err) => match lex_err {
            LexError::UnexpectedCharacter { ch, span } => Diagnostic::error()
                .with_message(format!("Unexpected character '{ch}'"))
                .with_labels(vec![
                    Label::primary(file_id, span.start..span.end)
                        .with_message(format!("unexpected character '{ch}'")),
                ]),
            LexError::UnterminatedStringLiteral { span } => Diagnostic::error()
                .with_message("Unterminated string literal")
                .with_labels(vec![
                    Label::primary(file_id, span.start..span.end)
                        .with_message("string literal is not closed"),
                ]),
            LexError::InvalidBinaryNumber { text, span } => Diagnostic::error()
                .with_message("Invalid binary number format")
                .with_labels(vec![
                    Label::primary(file_id, span.start..span.end)
                        .with_message(format!("invalid binary number '{text}'")),
                ]),
            LexError::InvalidRemark { span } => Diagnostic::error()
                .with_message("Invalid REM statement")
                .with_labels(vec![
                    Label::primary(file_id, span.start..span.end)
                        .with_message("invalid REM syntax"),
                ]),
        },
        CompileError::Parse(parse_err) => match parse_err {
            ParseError::UnexpectedToken {
                expected,
                found,
                span,
            } => Diagnostic::error()
                .with_message(format!("Expected {expected}, found {found}"))
                .with_labels(vec![
                    Label::primary(file_id, span.start..span.end)
                        .with_message(format!("expected {expected}")),
                ]),
            ParseError::UnexpectedEndOfInput { expected, span } => Diagnostic::error()
                .with_message(format!("Unexpected end of input, expected {expected}"))
                .with_labels(vec![
                    Label::primary(file_id, span.start..span.end)
                        .with_message(format!("expected {expected}")),
                ]),
            ParseError::InvalidExpression { message, span } => Diagnostic::error()
                .with_message("Invalid expression")
                .with_labels(vec![
                    Label::primary(file_id, span.start..span.end).with_message(message),
                ]),
            ParseError::MissingToken { token, span } => Diagnostic::error()
                .with_message(format!("Missing {token}"))
                .with_labels(vec![
                    Label::primary(file_id, span.start..span.end)
                        .with_message(format!("expected {token}")),
                ]),
        },
        CompileError::Semantic(semantic_err) => match semantic_err {
            SemanticError::TypeMismatch {
                expected,
                found,
                span,
            } => Diagnostic::error()
                .with_message("Type mismatch")
                .with_labels(vec![
                    Label::primary(file_id, span.start..span.end)
                        .with_message(format!("expected {expected:?}, found {found:?}")),
                ]),
            SemanticError::InvalidArrayIndex { message, span } => Diagnostic::error()
                .with_message("Invalid array index")
                .with_labels(vec![
                    Label::primary(file_id, span.start..span.end).with_message(message),
                ]),
            SemanticError::InvalidFunctionArgument {
                function_name,
                message,
                span,
            } => Diagnostic::error()
                .with_message(format!("Invalid argument to function '{function_name}'"))
                .with_labels(vec![
                    Label::primary(file_id, span.start..span.end).with_message(message),
                ]),
            SemanticError::InvalidExpression { message, span } => Diagnostic::error()
                .with_message("Invalid expression")
                .with_labels(vec![
                    Label::primary(file_id, span.start..span.end).with_message(message),
                ]),
        },
    };

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    if let Err(e) = term::emit(&mut writer.lock(), &config, &files, &diagnostic) {
        eprintln!("Error printing diagnostic: {e}");
    }
}

pub type LexResult<T> = Result<T, LexError>;
pub type ParseResult<T> = Result<T, ParseError>;
pub type SemanticResult<T> = Result<T, SemanticError>;
pub type CompileResult<T> = Result<T, CompileError>;
