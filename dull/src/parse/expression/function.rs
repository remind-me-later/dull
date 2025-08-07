use crate::parse::expression::{Expr, memory_area::MemoryArea};

#[derive(Debug, Clone, PartialEq, Eq)]

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
    Ascii {
        argument: Box<Expr>,
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
            Function::Ascii { argument } => write!(f, "ASC({argument})"),
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
