use crate::{
    lex::identifier::Identifier,
    parse::expression::{Expr, lvalue::LValue, memory_area::MemoryArea},
};

pub struct UsingClause {
    // -- FIXME: maybe this should allow also variables as format strings?
    pub format: Option<String>,
}

impl std::fmt::Display for UsingClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ref format) = self.format {
            write!(f, "USING \"{format}\"")
        } else {
            write!(f, "USING")
        }
    }
}

pub struct Assignment {
    pub lvalue: LValue,
    pub expr: Box<Expr>,
}

impl std::fmt::Display for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}={}", self.lvalue, self.expr)
    }
}

pub enum DimInner {
    DimInner1D {
        identifier: Identifier,
        size: Expr,
        string_length: Option<Expr>,
    },
    DimInner2D {
        identifier: Identifier,
        rows: Expr,
        cols: Expr,
        string_length: Option<Expr>,
    },
}

impl std::fmt::Display for DimInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DimInner::DimInner1D {
                identifier,
                size,
                string_length,
            } => {
                write!(f, "{identifier}({size})")?;
                if let Some(len) = string_length {
                    write!(f, "*{len}")?;
                }
            }
            DimInner::DimInner2D {
                identifier,
                rows,
                cols,
                string_length,
            } => {
                write!(f, "{identifier}({rows},{cols})")?;
                if let Some(len) = string_length {
                    write!(f, "*{len}")?;
                }
            }
        }
        Ok(())
    }
}

pub struct BeepOptionalParams {
    pub frequency: Expr,
    pub duration: Option<Expr>,
}

impl std::fmt::Display for BeepOptionalParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ",{}", self.frequency)?;
        if let Some(duration) = &self.duration {
            write!(f, ",{duration}")?;
        }
        Ok(())
    }
}

pub enum PrintSeparator {
    Comma,
    Semicolon,
    Empty, // used for the last expression in a PRINT statement
}

impl std::fmt::Display for PrintSeparator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrintSeparator::Comma => write!(f, ","),
            PrintSeparator::Semicolon => write!(f, ";"),
            PrintSeparator::Empty => Ok(()), // No output for empty separator
        }
    }
}

pub enum Printable {
    Expr(Expr),
    UsingClause(UsingClause),
}

pub struct PrintInner {
    pub exprs: Vec<(Printable, PrintSeparator)>,
}

impl std::fmt::Display for PrintInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (printable, sep) in self.exprs.iter() {
            match printable {
                Printable::Expr(expr) => write!(f, "{expr}{sep}")?,
                Printable::UsingClause(using) => write!(f, "{using}{sep}")?,
            }
        }
        Ok(())
    }
}

pub struct LCursorClause {
    pub expr: Expr,
}

impl std::fmt::Display for LCursorClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "LCURSOR {}", self.expr)
    }
}

pub enum LPrintable {
    Expr(Expr),
    LCursorClause(LCursorClause),
}

pub struct LPrintInner {
    pub exprs: Vec<(LPrintable, PrintSeparator)>,
}
impl std::fmt::Display for LPrintInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (printable, sep) in self.exprs.iter() {
            match printable {
                LPrintable::Expr(expr) => write!(f, "{expr}{sep}")?,
                LPrintable::LCursorClause(cursor) => write!(f, "{cursor}{sep}")?,
            }
        }
        Ok(())
    }
}

pub struct LetStatement {
    pub assignments: Vec<Assignment>,
}

impl LetStatement {
    pub fn show_with_context(&self, is_mandatory_let: bool) -> String {
        if is_mandatory_let {
            format!(
                "LET {}",
                self.assignments
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(",")
            )
        } else {
            self.assignments
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(",")
        }
    }
}
pub enum Statement {
    Let(LetStatement),
    If {
        condition: Expr,
        then_stmt: Box<Statement>,
    },
    Print {
        print_inner: PrintInner,
    },
    Pause {
        pause_inner: PrintInner,
    },
    LPrint {
        lprint_inner: LPrintInner,
    },
    Using {
        using_clause: UsingClause,
    },
    Input {
        input_exprs: Vec<(Option<String>, LValue)>,
    },
    End,
    Remark {
        text: String,
    },
    For {
        assignment: Assignment,
        to_expr: Expr,
        step_expr: Option<Expr>,
    },
    Next {
        ident: Identifier,
    },
    Clear,
    Goto {
        target: Expr,
    },
    Gosub {
        target: Expr,
    },
    OnGoto {
        expr: Expr,
        targets: Vec<Expr>,
    },
    OnGosub {
        expr: Expr,
        targets: Vec<Expr>,
    },
    OnErrorGoto {
        target: Expr,
    },
    Wait {
        expr: Option<Expr>,
    },
    Cls,
    Random,
    Gprint {
        exprs: Vec<(Expr, PrintSeparator)>,
    },
    GCursor {
        expr: Expr,
    },
    Cursor {
        expr: Expr,
    },
    Beep {
        repetitions_expr: Expr,
        optional_params: Option<BeepOptionalParams>,
    },
    BeepOnOff {
        switch_beep_on: bool, // true for BEEP ON, false for BEEP OFF
    },
    Return,
    Poke {
        memory_area: MemoryArea,
        exprs: Vec<Expr>,
    },
    Dim {
        decls: Vec<DimInner>,
    },
    Read {
        destinations: Vec<LValue>,
    },
    Data(Vec<Expr>),
    Restore {
        expr: Option<Expr>,
    },
    Arun,
    Lock,
    Unlock,
    Call {
        expr: Expr,
        variable: Option<LValue>,
    },
    Radian,
    Text,
    Graph,
    Color {
        expr: Expr,
    },
    CSize {
        expr: Expr,
    },
    Lf {
        expr: Expr,
    },
    LCursor(LCursorClause),
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let(stmt) => write!(f, "{}", stmt.show_with_context(false)),
            Statement::If {
                condition,
                then_stmt,
            } => match then_stmt.as_ref() {
                Statement::Let(l) => write!(f, "IF {condition} THEN {}", l.show_with_context(true)),
                _ => write!(f, "IF {condition} THEN {then_stmt}"),
            },
            Statement::Print { print_inner } => write!(f, "PRINT {print_inner}"),
            Statement::Pause { pause_inner } => write!(f, "PAUSE {pause_inner}"),
            Statement::LPrint { lprint_inner } => write!(f, "LPRINT {lprint_inner}"),
            Statement::Using { using_clause } => write!(f, "{using_clause}"),
            Statement::Input { input_exprs } => {
                let inputs = input_exprs
                    .iter()
                    .map(|(prompt, lval)| {
                        format!(
                            "{};{}",
                            prompt
                                .as_ref()
                                .map_or(String::new(), |p| format!("\"{p}\" ")),
                            lval
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, "INPUT {inputs}")
            }
            Statement::End => write!(f, "END"),
            Statement::Remark { text } => write!(f, "REM {text}"),
            Statement::For {
                assignment,
                to_expr,
                step_expr,
            } => {
                let step_str = if let Some(step) = step_expr {
                    format!(" STEP {step}")
                } else {
                    String::new()
                };
                write!(f, "FOR {assignment} TO {to_expr}{step_str}")
            }
            Statement::Next { ident } => write!(f, "NEXT {ident}"),
            Statement::Clear => write!(f, "CLEAR"),
            Statement::Goto { target } => write!(f, "GOTO {target}"),
            Statement::Gosub { target } => write!(f, "GOSUB {target}"),
            Statement::OnGoto { expr, targets } => {
                let targets_str = targets
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, "ON {expr} GOTO {targets_str}")
            }
            Statement::OnGosub { expr, targets } => {
                let targets_str = targets
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, "ON {expr} GOSUB {targets_str}")
            }
            Statement::OnErrorGoto { target } => write!(f, "ON ERROR GOTO {target}"),
            Statement::Wait { expr } => write!(
                f,
                "WAIT {}",
                expr.as_ref().map_or(String::new(), ToString::to_string)
            ),
            Statement::Cls => write!(f, "CLS"),
            Statement::Random => write!(f, "RANDOM"),
            Statement::Gprint { exprs } => {
                let exprs_str = exprs
                    .iter()
                    .map(|(expr, sep)| format!("{expr}{sep}"))
                    .collect::<Vec<_>>()
                    .join(" ");
                write!(f, "GPRINT {exprs_str}")
            }
            Statement::GCursor { expr } => write!(f, "GCURSOR {expr}"),
            Statement::Cursor { expr } => write!(f, "CURSOR {expr}"),
            Statement::Beep {
                repetitions_expr,
                optional_params,
            } => {
                let params_str = optional_params
                    .as_ref()
                    .map_or(String::new(), |params| params.to_string());
                write!(f, "BEEP {repetitions_expr}{params_str}")
            }
            Statement::BeepOnOff { switch_beep_on } => {
                write!(f, "BEEP {}", if *switch_beep_on { "ON" } else { "OFF" })
            }
            Statement::Return => write!(f, "RETURN"),
            Statement::Poke { memory_area, exprs } => {
                let exprs_str = exprs
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, "POKE")?;
                match memory_area {
                    MemoryArea::Me0 => write!(f, " {exprs_str}"),
                    MemoryArea::Me1 => write!(f, "# {exprs_str}"),
                }
            }
            Statement::Dim { decls } => {
                let decls_str = decls
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, "DIM {decls_str}")
            }
            Statement::Read { destinations } => {
                let destinations_str = destinations
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, "READ {destinations_str}")
            }
            Statement::Data(exprs) => {
                let exprs_str = exprs
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, "DATA {exprs_str}")
            }
            Statement::Restore { expr } => write!(
                f,
                "RESTORE {}",
                expr.as_ref().map_or(String::new(), ToString::to_string)
            ),
            Statement::Arun => write!(f, "ARUN"),
            Statement::Lock => write!(f, "LOCK"),
            Statement::Unlock => write!(f, "UNLOCK"),
            Statement::Call { expr, variable } => {
                let var_str = variable
                    .as_ref()
                    .map_or(String::new(), |v| format!(", {v}"));
                write!(f, "CALL {expr}{var_str}")
            }
            Statement::Radian => write!(f, "RADIAN"),
            Statement::Text => write!(f, "TEXT"),
            Statement::Graph => write!(f, "GRAPH"),
            Statement::Color { expr } => write!(f, "COLOR {expr}"),
            Statement::CSize { expr } => write!(f, "CSIZE {expr}"),
            Statement::Lf { expr } => write!(f, "LF {expr}"),
            Statement::LCursor(l_cursor_clause) => write!(f, "{l_cursor_clause}"),
        }
    }
}
