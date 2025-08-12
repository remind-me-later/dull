use crate::{
    error::Span,
    lex::keyword::Keyword,
    parse::expression::{Expr, memory_area::MemoryArea},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub inner: FunctionInner,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionInner {
    Mid {
        string: Box<Expr>,
        start: Box<Expr>,
        length: Box<Expr>,
    },
    Left {
        string: Box<Expr>,
        length: Box<Expr>,
    },
    Right {
        string: Box<Expr>,
        length: Box<Expr>,
    },
    Asc {
        expr: Box<Expr>,
    },
    Point {
        position: Box<Expr>, // Returns the color of the pixel at the position
    },
    Rnd {
        range_end: Box<Expr>,
    },
    Int {
        expr: Box<Expr>,
    },
    Sgn {
        expr: Box<Expr>, // Returns 1, 0 or -1
    },
    Status {
        arg: Box<Expr>,
    },
    Val {
        expr: Box<Expr>, // The string to convert
    },
    Str {
        expr: Box<Expr>, // The expression to convert to string
    },
    Chr {
        expr: Box<Expr>, // The numeric expression to convert to a character
    },
    Abs {
        expr: Box<Expr>, // The numeric expression to get the absolute value of
    },
    Len {
        expr: Box<Expr>, // The string expression to get the length of
    },
    Peek {
        memory_area: MemoryArea, // Me0 or Me1
        address: Box<Expr>,      // The address to peek from, must be a numeric expression
    },
    Ln {
        expr: Box<Expr>, // The numeric expression to get the natural logarithm of
    },
    Log {
        expr: Box<Expr>, // The numeric expression to get the base-10 logarithm of
    },
    Dms {
        expr: Box<Expr>,
    },
    Deg {
        expr: Box<Expr>,
    },
    Tan {
        expr: Box<Expr>,
    },
    Cos {
        expr: Box<Expr>,
    },
    Sin {
        expr: Box<Expr>,
    },
    Sqr {
        expr: Box<Expr>, // The numeric expression to get the square root of
    },
}

impl Function {
    pub fn new(inner: FunctionInner, span: Span) -> Self {
        Self { inner, span }
    }

    pub fn write_bytes_preserving_source_parens(
        &self,
        bytes: &mut Vec<u8>,
        preserve_source_wording: bool,
    ) {
        match &self.inner {
            FunctionInner::Mid {
                string,
                start,
                length,
            } => {
                bytes
                    .extend_from_slice(Keyword::MidDollar.internal_code().to_be_bytes().as_slice());
                bytes.push(b'(');
                string.write_bytes(bytes, preserve_source_wording);
                bytes.push(b',');
                start.write_bytes(bytes, preserve_source_wording);
                bytes.push(b',');
                length.write_bytes(bytes, preserve_source_wording);
                bytes.push(b')');
            }
            FunctionInner::Left { string, length } => {
                bytes.extend_from_slice(
                    Keyword::LeftDollar.internal_code().to_be_bytes().as_slice(),
                );
                bytes.push(b'(');
                string.write_bytes(bytes, preserve_source_wording);
                bytes.push(b',');
                length.write_bytes(bytes, preserve_source_wording);
                bytes.push(b')');
            }
            FunctionInner::Right { string, length } => {
                bytes.extend_from_slice(
                    Keyword::RightDollar
                        .internal_code()
                        .to_be_bytes()
                        .as_slice(),
                );
                bytes.push(b'(');
                string.write_bytes(bytes, preserve_source_wording);
                bytes.push(b',');
                length.write_bytes(bytes, preserve_source_wording);
                bytes.push(b')');
            }
            FunctionInner::Asc { expr } => {
                bytes.extend_from_slice(Keyword::Asc.internal_code().to_be_bytes().as_slice());
                expr.write_bytes_with_context_and_parens(bytes, 20, false, preserve_source_wording);
            }
            FunctionInner::Point { position } => {
                bytes.extend_from_slice(Keyword::Point.internal_code().to_be_bytes().as_slice());
                position.write_bytes_with_context_and_parens(
                    bytes,
                    8,
                    false,
                    preserve_source_wording,
                );
            }
            FunctionInner::Rnd { range_end } => {
                bytes.extend_from_slice(Keyword::Rnd.internal_code().to_be_bytes().as_slice());
                range_end.write_bytes_with_context_and_parens(
                    bytes,
                    8,
                    false,
                    preserve_source_wording,
                );
            }
            FunctionInner::Int { expr } => {
                bytes.extend_from_slice(Keyword::Int.internal_code().to_be_bytes().as_slice());
                expr.write_bytes_with_context_and_parens(bytes, 20, false, preserve_source_wording);
            }
            FunctionInner::Sgn { expr } => {
                bytes.extend_from_slice(Keyword::Sgn.internal_code().to_be_bytes().as_slice());
                expr.write_bytes_with_context_and_parens(bytes, 20, false, preserve_source_wording);
            }
            FunctionInner::Status { arg } => {
                bytes.extend_from_slice(Keyword::Status.internal_code().to_be_bytes().as_slice());
                arg.write_bytes_with_context_and_parens(bytes, 20, false, preserve_source_wording);
            }
            FunctionInner::Val { expr } => {
                bytes.extend_from_slice(Keyword::Val.internal_code().to_be_bytes().as_slice());
                expr.write_bytes_with_context_and_parens(bytes, 20, false, preserve_source_wording);
            }
            FunctionInner::Str { expr } => {
                bytes
                    .extend_from_slice(Keyword::StrDollar.internal_code().to_be_bytes().as_slice());
                expr.write_bytes_with_context_and_parens(bytes, 20, false, preserve_source_wording);
            }
            FunctionInner::Chr { expr } => {
                bytes
                    .extend_from_slice(Keyword::ChrDollar.internal_code().to_be_bytes().as_slice());
                expr.write_bytes_with_context_and_parens(bytes, 20, false, preserve_source_wording);
            }
            FunctionInner::Abs { expr } => {
                bytes.extend_from_slice(Keyword::Abs.internal_code().to_be_bytes().as_slice());
                expr.write_bytes_with_context_and_parens(bytes, 20, false, preserve_source_wording);
            }
            FunctionInner::Len { expr } => {
                bytes.extend_from_slice(Keyword::Len.internal_code().to_be_bytes().as_slice());
                expr.write_bytes_with_context_and_parens(bytes, 20, false, preserve_source_wording);
            }
            FunctionInner::Peek {
                memory_area,
                address,
            } => {
                match memory_area {
                    MemoryArea::Me0 => bytes.extend_from_slice(
                        Keyword::PeekMem0.internal_code().to_be_bytes().as_slice(),
                    ),
                    MemoryArea::Me1 => bytes.extend_from_slice(
                        Keyword::PeekMem1.internal_code().to_be_bytes().as_slice(),
                    ),
                }
                address.write_bytes_with_context_and_parens(
                    bytes,
                    8,
                    false,
                    preserve_source_wording,
                );
            }
            FunctionInner::Ln { expr } => {
                bytes.extend_from_slice(Keyword::Ln.internal_code().to_be_bytes().as_slice());
                expr.write_bytes_with_context_and_parens(bytes, 20, false, preserve_source_wording);
            }
            FunctionInner::Log { expr } => {
                bytes.extend_from_slice(Keyword::Log.internal_code().to_be_bytes().as_slice());
                expr.write_bytes_with_context_and_parens(bytes, 20, false, preserve_source_wording);
            }
            FunctionInner::Dms { expr } => {
                bytes.extend_from_slice(Keyword::Dms.internal_code().to_be_bytes().as_slice());
                expr.write_bytes_with_context_and_parens(bytes, 20, false, preserve_source_wording);
            }
            FunctionInner::Deg { expr } => {
                bytes.extend_from_slice(Keyword::Deg.internal_code().to_be_bytes().as_slice());
                expr.write_bytes_with_context_and_parens(bytes, 20, false, preserve_source_wording);
            }
            FunctionInner::Tan { expr } => {
                bytes.extend_from_slice(Keyword::Tan.internal_code().to_be_bytes().as_slice());
                expr.write_bytes(bytes, preserve_source_wording);
            }
            FunctionInner::Cos { expr } => {
                bytes.extend_from_slice(Keyword::Cos.internal_code().to_be_bytes().as_slice());
                expr.write_bytes_with_context_and_parens(bytes, 20, false, preserve_source_wording);
            }
            FunctionInner::Sin { expr } => {
                bytes.extend_from_slice(Keyword::Sin.internal_code().to_be_bytes().as_slice());
                expr.write_bytes_with_context_and_parens(bytes, 20, false, preserve_source_wording);
            }
            FunctionInner::Sqr { expr } => {
                bytes.extend_from_slice(Keyword::Sqr.internal_code().to_be_bytes().as_slice());
                expr.write_bytes_with_context_and_parens(bytes, 20, false, preserve_source_wording);
            }
        }
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.inner {
            FunctionInner::Mid {
                string,
                start,
                length,
            } => {
                write!(f, "MID({string}, {start}, {length})")
            }
            FunctionInner::Left { string, length } => write!(f, "LEFT({string}, {length})"),
            FunctionInner::Right { string, length } => write!(f, "RIGHT({string}, {length})"),
            FunctionInner::Asc { expr } => write!(f, "ASC({expr})"),
            FunctionInner::Point { position } => write!(
                f,
                "POINT {}",
                position.show_with_context_and_parens(20, false, false)
            ),
            FunctionInner::Rnd { range_end } => write!(
                f,
                "RND {}",
                range_end.show_with_context_and_parens(20, false, false)
            ),
            FunctionInner::Int { expr } => write!(
                f,
                "INT {}",
                expr.show_with_context_and_parens(20, false, false)
            ),
            FunctionInner::Sgn { expr } => write!(
                f,
                "SGN {}",
                expr.show_with_context_and_parens(20, false, false)
            ),
            FunctionInner::Status { arg } => write!(
                f,
                "STATUS {}",
                arg.show_with_context_and_parens(20, false, false)
            ),
            FunctionInner::Val { expr } => write!(
                f,
                "VAL {}",
                expr.show_with_context_and_parens(20, false, false)
            ),
            FunctionInner::Str { expr } => write!(
                f,
                "STR$ {}",
                expr.show_with_context_and_parens(20, false, false)
            ),
            FunctionInner::Chr { expr } => write!(
                f,
                "CHR$ {}",
                expr.show_with_context_and_parens(20, false, false)
            ),
            FunctionInner::Abs { expr } => write!(
                f,
                "ABS {}",
                expr.show_with_context_and_parens(20, false, false)
            ),
            FunctionInner::Len { expr } => write!(
                f,
                "LEN {}",
                expr.show_with_context_and_parens(20, false, false)
            ),
            FunctionInner::Peek {
                memory_area,
                address,
            } => {
                let area_str = match memory_area {
                    MemoryArea::Me0 => "",
                    MemoryArea::Me1 => "#",
                };
                write!(
                    f,
                    "PEEK{area_str} {}",
                    address.show_with_context_and_parens(20, false, false)
                )
            }
            FunctionInner::Ln { expr } => write!(
                f,
                "LN {}",
                expr.show_with_context_and_parens(20, false, false)
            ),
            FunctionInner::Log { expr } => write!(
                f,
                "LOG {}",
                expr.show_with_context_and_parens(20, false, false)
            ),
            FunctionInner::Dms { expr } => write!(
                f,
                "DMS {}",
                expr.show_with_context_and_parens(20, false, false)
            ),
            FunctionInner::Deg { expr } => write!(
                f,
                "DEG {}",
                expr.show_with_context_and_parens(20, false, false)
            ),
            FunctionInner::Tan { expr } => write!(
                f,
                "TAN {}",
                expr.show_with_context_and_parens(20, false, false)
            ),
            FunctionInner::Cos { expr } => write!(
                f,
                "COS {}",
                expr.show_with_context_and_parens(20, false, false)
            ),
            FunctionInner::Sin { expr } => write!(
                f,
                "SIN {}",
                expr.show_with_context_and_parens(20, false, false)
            ),
            FunctionInner::Sqr { expr } => write!(
                f,
                "√{}",
                expr.show_with_context_and_parens(20, false, false)
            ),
        }
    }
}
