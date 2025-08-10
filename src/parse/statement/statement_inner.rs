use crate::parse::{
    expression::{Expr, lvalue::LValue, memory_area::MemoryArea},
    statement::{
        Statement, assignment::Assignment, beep_params::BeepParams, dim_inner::DimInner,
        lcursor_clause::LCursorClause, let_inner::LetInner, line_inner::LineInner,
        lprint_inner::LPrintInner, print_inner::PrintInner, print_separator::PrintSeparator,
        using_clause::UsingClause,
    },
};

#[derive(Debug, Clone, PartialEq)]
pub enum StatementInner {
    Let {
        inner: LetInner,
        is_let_kw_present_in_source: bool,
    },
    If {
        condition: Expr,
        then_stmt: Box<Statement>,
        is_then_kw_present_in_source: bool,
        is_goto_kw_present_in_source: bool,
    },
    Print {
        inner: PrintInner,
    },
    Pause {
        inner: PrintInner,
    },
    LPrint {
        inner: LPrintInner,
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
        lvalue: LValue,
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
        optional_params: Option<BeepParams>,
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
    GlCursor {
        x_expr: Expr,
        y_expr: Expr,
    },
    Line {
        inner: LineInner,
    },
    RLine {
        inner: LineInner,
    },
    Sorgn,
    Rotate {
        expr: Expr,
    },
}

impl StatementInner {
    pub fn write_bytes(&self, bytes: &mut Vec<u8>, preserve_source_parens: bool) {
        use crate::lex::keyword::Keyword;

        match self {
            StatementInner::Let {
                inner,
                is_let_kw_present_in_source,
            } => {
                if preserve_source_parens && *is_let_kw_present_in_source {
                    bytes.extend_from_slice(Keyword::Let.internal_code().to_be_bytes().as_slice());
                }

                inner.write_bytes(bytes, preserve_source_parens);
            }
            StatementInner::If {
                condition,
                then_stmt,
                is_then_kw_present_in_source,
                is_goto_kw_present_in_source,
            } => {
                bytes.extend_from_slice(Keyword::If.internal_code().to_be_bytes().as_slice());
                condition.write_bytes(bytes, preserve_source_parens);
                if preserve_source_parens && *is_then_kw_present_in_source {
                    bytes.extend_from_slice(Keyword::Then.internal_code().to_be_bytes().as_slice());
                }
                match &then_stmt.inner {
                    StatementInner::Let { inner, .. } => {
                        // LET keyword is mandatory here
                        bytes.extend_from_slice(
                            Keyword::Let.internal_code().to_be_bytes().as_slice(),
                        );
                        inner.write_bytes(bytes, preserve_source_parens);
                    }
                    StatementInner::Goto { target } => {
                        if preserve_source_parens && *is_goto_kw_present_in_source {
                            bytes.extend_from_slice(
                                Keyword::Goto.internal_code().to_be_bytes().as_slice(),
                            );
                        }
                        target.write_bytes(bytes, preserve_source_parens);
                    }
                    _ => {
                        then_stmt.write_bytes(bytes, preserve_source_parens);
                    }
                }
            }
            StatementInner::Print { inner } => {
                bytes.extend_from_slice(Keyword::Print.internal_code().to_be_bytes().as_slice());
                for (printable, sep) in &inner.exprs {
                    printable.write_bytes(bytes, preserve_source_parens);
                    sep.write_bytes(bytes);
                }
            }
            StatementInner::Pause { inner } => {
                bytes.extend_from_slice(Keyword::Pause.internal_code().to_be_bytes().as_slice());
                for (printable, sep) in &inner.exprs {
                    printable.write_bytes(bytes, preserve_source_parens);
                    sep.write_bytes(bytes);
                }
            }
            StatementInner::LPrint { inner } => {
                bytes.extend_from_slice(Keyword::Lprint.internal_code().to_be_bytes().as_slice());
                inner.write_bytes_with_context(true, bytes, preserve_source_parens);
            }
            StatementInner::Using { using_clause } => {
                using_clause.write_bytes(bytes);
            }
            StatementInner::Input { input_exprs } => {
                bytes.extend_from_slice(Keyword::Input.internal_code().to_be_bytes().as_slice());
                for (i, (prompt, lvalue)) in input_exprs.iter().enumerate() {
                    if let Some(prompt) = prompt {
                        bytes.push(b'"');
                        bytes.extend_from_slice(prompt.as_bytes());
                        bytes.push(b'"');
                        bytes.push(b';');
                    }
                    lvalue.write_bytes(bytes, preserve_source_parens);
                    if i < input_exprs.len() - 1 {
                        bytes.push(b',');
                    }
                }
            }
            StatementInner::End => {
                bytes.extend_from_slice(Keyword::End.internal_code().to_be_bytes().as_slice());
            }
            StatementInner::Remark { text } => {
                // FIXME: can we safely omit the text?
                bytes.extend_from_slice(Keyword::Rem.internal_code().to_be_bytes().as_slice());
                bytes.extend_from_slice(text.as_bytes());
            }
            StatementInner::For {
                assignment,
                to_expr,
                step_expr,
            } => {
                bytes.extend_from_slice(Keyword::For.internal_code().to_be_bytes().as_slice());
                assignment.write_bytes(bytes, preserve_source_parens);
                bytes.extend_from_slice(Keyword::To.internal_code().to_be_bytes().as_slice());
                to_expr.write_bytes(bytes, preserve_source_parens);
                if let Some(step) = step_expr {
                    bytes.extend_from_slice(Keyword::Step.internal_code().to_be_bytes().as_slice());
                    step.write_bytes(bytes, preserve_source_parens);
                }
            }
            StatementInner::Next { lvalue } => {
                bytes.extend_from_slice(Keyword::Next.internal_code().to_be_bytes().as_slice());
                lvalue.write_bytes(bytes, preserve_source_parens);
            }
            StatementInner::Clear => {
                bytes.extend_from_slice(Keyword::Clear.internal_code().to_be_bytes().as_slice());
            }
            StatementInner::Goto { target } => {
                bytes.extend_from_slice(Keyword::Goto.internal_code().to_be_bytes().as_slice());
                target.write_bytes(bytes, preserve_source_parens);
            }
            StatementInner::Gosub { target } => {
                bytes.extend_from_slice(Keyword::Gosub.internal_code().to_be_bytes().as_slice());
                target.write_bytes(bytes, preserve_source_parens);
            }
            StatementInner::OnGoto { expr, targets } => {
                bytes.extend_from_slice(Keyword::On.internal_code().to_be_bytes().as_slice());
                expr.write_bytes(bytes, preserve_source_parens);
                bytes.extend_from_slice(Keyword::Goto.internal_code().to_be_bytes().as_slice());
                for (i, target) in targets.iter().enumerate() {
                    target.write_bytes(bytes, preserve_source_parens);
                    if i < targets.len() - 1 {
                        bytes.push(b',');
                    }
                }
            }
            StatementInner::OnGosub { expr, targets } => {
                bytes.extend_from_slice(Keyword::On.internal_code().to_be_bytes().as_slice());
                expr.write_bytes(bytes, preserve_source_parens);
                bytes.extend_from_slice(Keyword::Gosub.internal_code().to_be_bytes().as_slice());
                for (i, target) in targets.iter().enumerate() {
                    target.write_bytes(bytes, preserve_source_parens);
                    if i < targets.len() - 1 {
                        bytes.push(b',');
                    }
                }
            }
            StatementInner::OnErrorGoto { target } => {
                bytes.extend_from_slice(Keyword::On.internal_code().to_be_bytes().as_slice());
                bytes.extend_from_slice(Keyword::Error.internal_code().to_be_bytes().as_slice());
                bytes.extend_from_slice(Keyword::Goto.internal_code().to_be_bytes().as_slice());
                target.write_bytes(bytes, preserve_source_parens);
            }
            StatementInner::Wait { expr } => {
                bytes.extend_from_slice(Keyword::Wait.internal_code().to_be_bytes().as_slice());
                if let Some(expr) = expr {
                    expr.write_bytes(bytes, preserve_source_parens);
                }
            }
            StatementInner::Cls => {
                bytes.extend_from_slice(Keyword::Cls.internal_code().to_be_bytes().as_slice());
            }
            StatementInner::Random => {
                bytes.extend_from_slice(Keyword::Random.internal_code().to_be_bytes().as_slice());
            }
            StatementInner::Gprint { exprs } => {
                bytes.extend_from_slice(Keyword::Gprint.internal_code().to_be_bytes().as_slice());
                for (expr, sep) in exprs {
                    expr.write_bytes(bytes, preserve_source_parens);
                    sep.write_bytes(bytes);
                }
            }
            StatementInner::GCursor { expr } => {
                bytes.extend_from_slice(Keyword::Gcursor.internal_code().to_be_bytes().as_slice());
                expr.write_bytes(bytes, preserve_source_parens);
            }
            StatementInner::Cursor { expr } => {
                bytes.extend_from_slice(Keyword::Cursor.internal_code().to_be_bytes().as_slice());
                expr.write_bytes(bytes, preserve_source_parens);
            }
            StatementInner::Beep {
                repetitions_expr,
                optional_params,
            } => {
                bytes.extend_from_slice(Keyword::Beep.internal_code().to_be_bytes().as_slice());
                repetitions_expr.write_bytes(bytes, preserve_source_parens);
                if let Some(params) = optional_params {
                    bytes.push(b',');
                    params.frequency.write_bytes(bytes, preserve_source_parens);
                    if let Some(duration) = &params.duration {
                        bytes.push(b',');
                        duration.write_bytes(bytes, preserve_source_parens);
                    }
                }
            }
            StatementInner::BeepOnOff { switch_beep_on } => {
                bytes.extend_from_slice(Keyword::Beep.internal_code().to_be_bytes().as_slice());
                if *switch_beep_on {
                    bytes.extend_from_slice(Keyword::On.internal_code().to_be_bytes().as_slice());
                } else {
                    bytes.extend_from_slice(Keyword::Off.internal_code().to_be_bytes().as_slice());
                }
            }
            StatementInner::Return => {
                bytes.extend_from_slice(Keyword::Return.internal_code().to_be_bytes().as_slice());
            }
            StatementInner::Poke { memory_area, exprs } => {
                match memory_area {
                    MemoryArea::Me0 => bytes.extend_from_slice(
                        Keyword::PokeMem0.internal_code().to_be_bytes().as_slice(),
                    ),
                    MemoryArea::Me1 => bytes.extend_from_slice(
                        Keyword::PokeMem1.internal_code().to_be_bytes().as_slice(),
                    ),
                }
                for (i, expr) in exprs.iter().enumerate() {
                    expr.write_bytes(bytes, preserve_source_parens);
                    if i < exprs.len() - 1 {
                        bytes.push(b',');
                    }
                }
            }
            StatementInner::Dim { decls } => {
                bytes.extend_from_slice(Keyword::Dim.internal_code().to_be_bytes().as_slice());
                for (i, decl) in decls.iter().enumerate() {
                    match decl {
                        DimInner::DimInner1D {
                            identifier,
                            size,
                            string_length,
                            span: _,
                        } => {
                            identifier.write_bytes(bytes);
                            bytes.push(b'(');
                            size.write_bytes(bytes, preserve_source_parens);
                            bytes.push(b')');
                            if let Some(len) = string_length {
                                bytes.push(b'*');
                                len.write_bytes(bytes, preserve_source_parens);
                            }
                        }
                        DimInner::DimInner2D {
                            identifier,
                            rows,
                            cols,
                            string_length,
                            span: _,
                        } => {
                            identifier.write_bytes(bytes);
                            bytes.push(b'(');
                            rows.write_bytes(bytes, preserve_source_parens);
                            bytes.push(b',');
                            cols.write_bytes(bytes, preserve_source_parens);
                            bytes.push(b')');
                            if let Some(len) = string_length {
                                bytes.push(b'*');
                                len.write_bytes(bytes, preserve_source_parens);
                            }
                        }
                    }

                    if i < decls.len() - 1 {
                        bytes.push(b',');
                    }
                }
            }
            StatementInner::Read { destinations } => {
                bytes.extend_from_slice(Keyword::Read.internal_code().to_be_bytes().as_slice());
                for (i, dest) in destinations.iter().enumerate() {
                    dest.write_bytes(bytes, preserve_source_parens);
                    if i < destinations.len() - 1 {
                        bytes.push(b',');
                    }
                }
            }
            StatementInner::Data(exprs) => {
                bytes.extend_from_slice(Keyword::Data.internal_code().to_be_bytes().as_slice());
                for (i, expr) in exprs.iter().enumerate() {
                    expr.write_bytes(bytes, preserve_source_parens);
                    if i < exprs.len() - 1 {
                        bytes.push(b',');
                    }
                }
            }
            StatementInner::Restore { expr } => {
                bytes.extend_from_slice(Keyword::Restore.internal_code().to_be_bytes().as_slice());
                if let Some(expr) = expr {
                    expr.write_bytes(bytes, preserve_source_parens);
                }
            }
            StatementInner::Arun => {
                bytes.extend_from_slice(Keyword::Arun.internal_code().to_be_bytes().as_slice());
            }
            StatementInner::Lock => {
                bytes.extend_from_slice(Keyword::Lock.internal_code().to_be_bytes().as_slice());
            }
            StatementInner::Unlock => {
                bytes.extend_from_slice(Keyword::Unlock.internal_code().to_be_bytes().as_slice());
            }
            StatementInner::Call { expr, variable } => {
                bytes.extend_from_slice(Keyword::Call.internal_code().to_be_bytes().as_slice());
                expr.write_bytes(bytes, preserve_source_parens);
                if let Some(var) = variable {
                    bytes.push(b',');
                    var.write_bytes(bytes, preserve_source_parens);
                }
            }
            StatementInner::Radian => {
                bytes.extend_from_slice(Keyword::Radian.internal_code().to_be_bytes().as_slice());
            }
            StatementInner::Text => {
                bytes.extend_from_slice(Keyword::Text.internal_code().to_be_bytes().as_slice());
            }
            StatementInner::Graph => {
                bytes.extend_from_slice(Keyword::Graph.internal_code().to_be_bytes().as_slice());
            }
            StatementInner::Color { expr } => {
                bytes.extend_from_slice(Keyword::Color.internal_code().to_be_bytes().as_slice());
                expr.write_bytes(bytes, preserve_source_parens);
            }
            StatementInner::CSize { expr } => {
                bytes.extend_from_slice(Keyword::Csize.internal_code().to_be_bytes().as_slice());
                expr.write_bytes(bytes, preserve_source_parens);
            }
            StatementInner::Lf { expr } => {
                bytes.extend_from_slice(Keyword::Lf.internal_code().to_be_bytes().as_slice());
                expr.write_bytes(bytes, preserve_source_parens);
            }
            StatementInner::LCursor(l_cursor_clause) => {
                l_cursor_clause.write_bytes_with_context(false, bytes, preserve_source_parens);
            }
            StatementInner::GlCursor { x_expr, y_expr } => {
                bytes.extend_from_slice(Keyword::Glcursor.internal_code().to_be_bytes().as_slice());
                bytes.push(b'(');
                x_expr.write_bytes(bytes, preserve_source_parens);
                bytes.push(b',');
                y_expr.write_bytes(bytes, preserve_source_parens);
                bytes.push(b')');
            }
            StatementInner::Line { inner } => {
                bytes.extend_from_slice(Keyword::Line.internal_code().to_be_bytes().as_slice());
                inner.write_bytes(bytes, preserve_source_parens);
            }
            StatementInner::RLine { inner } => {
                bytes.extend_from_slice(Keyword::Rline.internal_code().to_be_bytes().as_slice());
                inner.write_bytes(bytes, preserve_source_parens);
            }
            StatementInner::Sorgn => {
                bytes.extend_from_slice(Keyword::Sorgn.internal_code().to_be_bytes().as_slice());
            }
            StatementInner::Rotate { expr } => {
                bytes.extend_from_slice(Keyword::Rotate.internal_code().to_be_bytes().as_slice());
                expr.write_bytes(bytes, preserve_source_parens);
            }
        }
    }
}

impl std::fmt::Display for StatementInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StatementInner::Let { inner, .. } => write!(f, "{}", inner.show_with_context(false)),
            StatementInner::If {
                condition,
                then_stmt,
                ..
            } => match &then_stmt.as_ref().inner {
                StatementInner::Let { inner, .. } => {
                    write!(f, "IF {condition} THEN {}", inner.show_with_context(true))
                }
                _ => write!(f, "IF {condition} THEN {then_stmt}"),
            },
            StatementInner::Print { inner } => write!(f, "PRINT {inner}"),
            StatementInner::Pause { inner } => write!(f, "PAUSE {inner}"),
            StatementInner::LPrint { inner } => {
                write!(f, "LPRINT {}", inner.to_string_with_context(true))
            }
            StatementInner::Using { using_clause } => write!(f, "{using_clause}"),
            StatementInner::Input { input_exprs } => {
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
            StatementInner::End => write!(f, "END"),
            StatementInner::Remark { text } => write!(f, "REM {text}"),
            StatementInner::For {
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
            StatementInner::Next { lvalue: ident } => write!(f, "NEXT {ident}"),
            StatementInner::Clear => write!(f, "CLEAR"),
            StatementInner::Goto { target } => write!(f, "GOTO {target}"),
            StatementInner::Gosub { target } => write!(f, "GOSUB {target}"),
            StatementInner::OnGoto { expr, targets } => {
                let targets_str = targets
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, "ON {expr} GOTO {targets_str}")
            }
            StatementInner::OnGosub { expr, targets } => {
                let targets_str = targets
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, "ON {expr} GOSUB {targets_str}")
            }
            StatementInner::OnErrorGoto { target } => write!(f, "ON ERROR GOTO {target}"),
            StatementInner::Wait { expr } => write!(
                f,
                "WAIT {}",
                expr.as_ref().map_or(String::new(), ToString::to_string)
            ),
            StatementInner::Cls => write!(f, "CLS"),
            StatementInner::Random => write!(f, "RANDOM"),
            StatementInner::Gprint { exprs } => {
                let exprs_str = exprs
                    .iter()
                    .map(|(expr, sep)| format!("{expr}{sep}"))
                    .collect::<Vec<_>>()
                    .join(" ");
                write!(f, "GPRINT {exprs_str}")
            }
            StatementInner::GCursor { expr } => write!(f, "GCURSOR {expr}"),
            StatementInner::Cursor { expr } => write!(f, "CURSOR {expr}"),
            StatementInner::Beep {
                repetitions_expr,
                optional_params,
            } => {
                let params_str = optional_params
                    .as_ref()
                    .map_or(String::new(), |params| params.to_string());
                write!(f, "BEEP {repetitions_expr}{params_str}")
            }
            StatementInner::BeepOnOff { switch_beep_on } => {
                write!(f, "BEEP {}", if *switch_beep_on { "ON" } else { "OFF" })
            }
            StatementInner::Return => write!(f, "RETURN"),
            StatementInner::Poke { memory_area, exprs } => {
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
            StatementInner::Dim { decls } => {
                let decls_str = decls
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, "DIM {decls_str}")
            }
            StatementInner::Read { destinations } => {
                let destinations_str = destinations
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, "READ {destinations_str}")
            }
            StatementInner::Data(exprs) => {
                let exprs_str = exprs
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, "DATA {exprs_str}")
            }
            StatementInner::Restore { expr } => write!(
                f,
                "RESTORE {}",
                expr.as_ref().map_or(String::new(), ToString::to_string)
            ),
            StatementInner::Arun => write!(f, "ARUN"),
            StatementInner::Lock => write!(f, "LOCK"),
            StatementInner::Unlock => write!(f, "UNLOCK"),
            StatementInner::Call { expr, variable } => {
                let var_str = variable
                    .as_ref()
                    .map_or(String::new(), |v| format!(", {v}"));
                write!(f, "CALL {expr}{var_str}")
            }
            StatementInner::Radian => write!(f, "RADIAN"),
            StatementInner::Text => write!(f, "TEXT"),
            StatementInner::Graph => write!(f, "GRAPH"),
            StatementInner::Color { expr } => write!(f, "COLOR {expr}"),
            StatementInner::CSize { expr } => write!(f, "CSIZE {expr}"),
            StatementInner::Lf { expr } => write!(f, "LF {expr}"),
            StatementInner::LCursor(l_cursor_clause) => write!(
                f,
                "LCURSOR {}",
                l_cursor_clause.to_string_with_context(false)
            ),
            StatementInner::GlCursor { x_expr, y_expr } => {
                write!(f, "GLCURSOR ({x_expr}, {y_expr})")
            }
            StatementInner::Line { inner } => {
                write!(f, "{}", inner.to_string_with_prefix("LINE"))
            }
            StatementInner::RLine { inner } => {
                write!(f, "{}", inner.to_string_with_prefix("RLINE"))
            }
            StatementInner::Sorgn => write!(f, "SORGN"),
            StatementInner::Rotate { expr } => write!(f, "ROTATE {expr}"),
        }
    }
}
