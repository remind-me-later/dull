use crate::{
    lex::keyword::Keyword,
    parse::expression::{Expr, memory_area::MemoryArea},
};

#[derive(Debug, Clone, PartialEq)]

pub enum Function {
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
    pub fn write_bytes(&self, bytes: &mut Vec<u8>) {
        match self {
            Function::Mid {
                string,
                start,
                length,
            } => {
                bytes
                    .extend_from_slice(Keyword::MidDollar.internal_code().to_le_bytes().as_slice());
                bytes.push(b'(');
                string.write_bytes(bytes);
                bytes.push(b',');
                start.write_bytes(bytes);
                bytes.push(b',');
                length.write_bytes(bytes);
                bytes.push(b')');
            }
            Function::Left { string, length } => {
                bytes.extend_from_slice(
                    Keyword::LeftDollar.internal_code().to_le_bytes().as_slice(),
                );
                bytes.push(b'(');
                string.write_bytes(bytes);
                bytes.push(b',');
                length.write_bytes(bytes);
                bytes.push(b')');
            }
            Function::Right { string, length } => {
                bytes.extend_from_slice(
                    Keyword::RightDollar
                        .internal_code()
                        .to_le_bytes()
                        .as_slice(),
                );
                bytes.push(b'(');
                string.write_bytes(bytes);
                bytes.push(b',');
                length.write_bytes(bytes);
                bytes.push(b')');
            }
            Function::Asc { expr } => {
                bytes.extend_from_slice(Keyword::Asc.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
            Function::Point { position } => {
                bytes.extend_from_slice(Keyword::Point.internal_code().to_le_bytes().as_slice());
                position.write_bytes(bytes);
            }
            Function::Rnd { range_end } => {
                bytes.extend_from_slice(Keyword::Rnd.internal_code().to_le_bytes().as_slice());
                range_end.write_bytes(bytes);
            }
            Function::Int { expr } => {
                bytes.extend_from_slice(Keyword::Int.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
            Function::Sgn { expr } => {
                bytes.extend_from_slice(Keyword::Sgn.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
            Function::Status { arg } => {
                bytes.extend_from_slice(Keyword::Status.internal_code().to_le_bytes().as_slice());
                arg.write_bytes(bytes);
            }
            Function::Val { expr } => {
                bytes.extend_from_slice(Keyword::Val.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
            Function::Str { expr } => {
                bytes
                    .extend_from_slice(Keyword::StrDollar.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
            Function::Chr { expr } => {
                bytes
                    .extend_from_slice(Keyword::ChrDollar.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
            Function::Abs { expr } => {
                bytes.extend_from_slice(Keyword::Abs.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
            Function::Len { expr } => {
                bytes.extend_from_slice(Keyword::Len.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
            Function::Peek {
                memory_area,
                address,
            } => {
                match memory_area {
                    MemoryArea::Me0 => bytes.extend_from_slice(
                        Keyword::PeekMem0.internal_code().to_le_bytes().as_slice(),
                    ),
                    MemoryArea::Me1 => bytes.extend_from_slice(
                        Keyword::PeekMem1.internal_code().to_le_bytes().as_slice(),
                    ),
                }
                address.write_bytes(bytes);
            }
            Function::Ln { expr } => {
                bytes.extend_from_slice(Keyword::Ln.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
            Function::Log { expr } => {
                bytes.extend_from_slice(Keyword::Log.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
            Function::Dms { expr } => {
                bytes.extend_from_slice(Keyword::Dms.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
            Function::Deg { expr } => {
                bytes.extend_from_slice(Keyword::Deg.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
            Function::Tan { expr } => {
                bytes.extend_from_slice(Keyword::Tan.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
            Function::Cos { expr } => {
                bytes.extend_from_slice(Keyword::Cos.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
            Function::Sin { expr } => {
                bytes.extend_from_slice(Keyword::Sin.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
            Function::Sqr { expr } => {
                bytes.extend_from_slice(Keyword::Sqr.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
        }
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Mid {
                string,
                start,
                length,
            } => {
                write!(f, "MID({string}, {start}, {length})")
            }
            Function::Left { string, length } => write!(f, "LEFT({string}, {length})"),
            Function::Right { string, length } => write!(f, "RIGHT({string}, {length})"),
            Function::Asc { expr } => write!(f, "ASC({expr})"),
            Function::Point { position } => write!(f, "POINT {position}"),
            Function::Rnd { range_end } => write!(f, "RND {range_end}"),
            Function::Int { expr } => write!(f, "INT {expr}"),
            Function::Sgn { expr } => write!(f, "SGN {expr}"),
            Function::Status { arg } => write!(f, "STATUS {arg}"),
            Function::Val { expr } => write!(f, "VAL {expr}"),
            Function::Str { expr } => write!(f, "STR$ {expr}"),
            Function::Chr { expr } => write!(f, "CHR$ {expr}"),
            Function::Abs { expr } => write!(f, "ABS {expr}"),
            Function::Len { expr } => write!(f, "LEN {expr}"),
            Function::Peek {
                memory_area,
                address,
            } => {
                let area_str = match memory_area {
                    MemoryArea::Me0 => "",
                    MemoryArea::Me1 => "#",
                };
                write!(f, "PEEK{area_str} {address}")
            }
            Function::Ln { expr } => write!(f, "LN {expr}"),
            Function::Log { expr } => write!(f, "LOG {expr}"),
            Function::Dms { expr } => write!(f, "DMS {expr}"),
            Function::Deg { expr } => write!(f, "DEG {expr}"),
            Function::Tan { expr } => write!(f, "TAN {expr}"),
            Function::Cos { expr } => write!(f, "COS {expr}"),
            Function::Sin { expr } => write!(f, "SIN {expr}"),
            Function::Sqr { expr } => write!(f, "√{expr}"),
        }
    }
}
