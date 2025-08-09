use crate::{
    lex::{identifier::Identifier, keyword::Keyword},
    parse::expression::{Expr, lvalue::LValue, memory_area::MemoryArea},
};

pub struct UsingClause {
    // -- FIXME: maybe this should allow also variables as format strings?
    pub format: Option<String>,
}

impl UsingClause {
    fn write_bytes(&self, bytes: &mut Vec<u8>) {
        bytes.extend_from_slice(Keyword::Using.internal_code().to_le_bytes().as_slice());
        if let Some(format) = &self.format {
            bytes.push(b'"');
            bytes.extend_from_slice(format.as_bytes());
            bytes.push(b'"');
        }
    }
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

impl Assignment {
    pub fn write_bytes(&self, bytes: &mut Vec<u8>) {
        self.lvalue.write_bytes(bytes);
        bytes.push(b'=');
        self.expr.write_bytes(bytes);
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrintSeparator {
    Comma,
    Semicolon,
    Empty, // used for the last expression in a PRINT statement
}

impl PrintSeparator {
    fn write_bytes(&self, bytes: &mut Vec<u8>) {
        match self {
            PrintSeparator::Comma => bytes.push(b','),
            PrintSeparator::Semicolon => bytes.push(b';'),
            PrintSeparator::Empty => {}
        }
    }
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

impl Printable {
    fn write_bytes(&self, bytes: &mut Vec<u8>) {
        match self {
            Printable::Expr(expr) => expr.write_bytes(bytes),
            Printable::UsingClause(using) => using.write_bytes(bytes),
        }
    }
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

impl LCursorClause {
    pub fn to_string_with_context(&self, is_inside_lprint: bool) -> String {
        if is_inside_lprint {
            format!("LCURSOR {}", self.expr)
        } else {
            format!("TAB {}", self.expr)
        }
    }

    pub fn write_bytes_with_context(&self, is_inside_lprint: bool, bytes: &mut Vec<u8>) {
        if is_inside_lprint {
            bytes.extend_from_slice(Keyword::Lcursor.internal_code().to_le_bytes().as_slice());
        } else {
            bytes.extend_from_slice(Keyword::Tab.internal_code().to_le_bytes().as_slice());
        }
        self.expr.write_bytes(bytes);
    }
}

pub enum LPrintable {
    Expr(Expr),
    LCursorClause(LCursorClause),
}

pub struct LPrintInner {
    pub exprs: Vec<(LPrintable, PrintSeparator)>,
}

impl LPrintInner {
    pub fn to_string_with_context(&self, is_inside_lprint: bool) -> String {
        let mut result = String::new();
        for (printable, sep) in self.exprs.iter() {
            match printable {
                LPrintable::Expr(expr) => {
                    result.push_str(&expr.to_string());
                }
                LPrintable::LCursorClause(cursor) => {
                    result.push_str(&cursor.to_string_with_context(is_inside_lprint));
                }
            }
            result.push_str(&sep.to_string());
        }
        result
    }

    pub fn write_bytes_with_context(&self, is_inside_lprint: bool, bytes: &mut Vec<u8>) {
        for (printable, sep) in self.exprs.iter() {
            match printable {
                LPrintable::Expr(expr) => expr.write_bytes(bytes),
                LPrintable::LCursorClause(cursor) => {
                    cursor.write_bytes_with_context(is_inside_lprint, bytes)
                }
            }
            sep.write_bytes(bytes);
        }
    }
}

pub struct LetInner {
    pub assignments: Vec<Assignment>,
}

impl LetInner {
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

pub struct LineInner {
    pub start_point: Option<(Expr, Expr)>, // None means use current pen position
    pub end_points: Vec<(Expr, Expr)>,
    pub line_type: Option<Expr>,
    pub color: Option<Expr>,
    pub is_box: bool,
}

impl LineInner {
    pub fn write_bytes(&self, bytes: &mut Vec<u8>) {
        // Handle start point - if None, start with '-'
        if let Some((x, y)) = &self.start_point {
            bytes.push(b'(');
            x.write_bytes(bytes);
            bytes.push(b',');
            y.write_bytes(bytes);
            bytes.push(b')');
        }

        for (x, y) in &self.end_points {
            bytes.push(b'-');
            bytes.push(b'(');
            x.write_bytes(bytes);
            bytes.push(b',');
            y.write_bytes(bytes);
            bytes.push(b')');
        }

        if let Some(line_type) = &self.line_type {
            bytes.push(b',');
            line_type.write_bytes(bytes);
        }

        if let Some(color) = &self.color {
            bytes.push(b',');
            color.write_bytes(bytes);
        }

        if self.is_box {
            bytes.push(b',');
            bytes.push(b'B');
        }
    }

    pub fn to_string_with_prefix(&self, prefix: &str) -> String {
        let mut result = format!("{prefix} ");

        // Handle start point - if None, start with '-'
        if let Some((x, y)) = &self.start_point {
            result.push_str(&format!("({x},{y})"));
        }

        for (x, y) in &self.end_points {
            result.push_str(&format!("-({x},{y})"));
        }

        if let Some(line_type) = &self.line_type {
            result.push_str(&format!(",{line_type}"));
        }

        if let Some(color) = &self.color {
            result.push_str(&format!(",{color}"));
        }

        if self.is_box {
            result.push_str(",B");
        }

        result
    }
}
pub enum Statement {
    Let {
        inner: LetInner,
    },
    If {
        condition: Expr,
        then_stmt: Box<Statement>,
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
}

impl Statement {
    pub fn write_bytes(&self, bytes: &mut Vec<u8>) {
        use crate::lex::keyword::Keyword;

        match self {
            Statement::Let { inner } => {
                for (i, assignment) in inner.assignments.iter().enumerate() {
                    if i > 0 {
                        bytes.push(b',');
                    }
                    assignment.lvalue.write_bytes(bytes);
                    bytes.push(b'=');
                    assignment.expr.write_bytes(bytes);
                }
            }
            Statement::If {
                condition,
                then_stmt,
            } => {
                bytes.extend_from_slice(Keyword::If.internal_code().to_le_bytes().as_slice());
                condition.write_bytes(bytes);
                match &**then_stmt {
                    Statement::Let { inner } => {
                        bytes.extend_from_slice(
                            Keyword::Let.internal_code().to_le_bytes().as_slice(),
                        );
                        bytes.extend_from_slice(inner.show_with_context(true).as_bytes());
                    }
                    Statement::Goto { target } => {
                        target.write_bytes(bytes);
                    }
                    _ => {
                        then_stmt.write_bytes(bytes);
                    }
                }
            }
            Statement::Print { inner } => {
                bytes.extend_from_slice(Keyword::Print.internal_code().to_le_bytes().as_slice());
                for (printable, sep) in &inner.exprs {
                    printable.write_bytes(bytes);
                    sep.write_bytes(bytes);
                }
            }
            Statement::Pause { inner } => {
                bytes.extend_from_slice(Keyword::Pause.internal_code().to_le_bytes().as_slice());
                for (printable, sep) in &inner.exprs {
                    printable.write_bytes(bytes);
                    sep.write_bytes(bytes);
                }
            }
            Statement::LPrint { inner } => {
                bytes.extend_from_slice(Keyword::Lprint.internal_code().to_le_bytes().as_slice());
                inner.write_bytes_with_context(true, bytes);
            }
            Statement::Using { using_clause } => {
                using_clause.write_bytes(bytes);
            }
            Statement::Input { input_exprs } => {
                bytes.extend_from_slice(Keyword::Input.internal_code().to_le_bytes().as_slice());
                for (i, (prompt, lvalue)) in input_exprs.iter().enumerate() {
                    if let Some(prompt) = prompt {
                        bytes.push(b'"');
                        bytes.extend_from_slice(prompt.as_bytes());
                        bytes.push(b'"');
                        bytes.push(b';');
                    }
                    lvalue.write_bytes(bytes);
                    if i < input_exprs.len() - 1 {
                        bytes.push(b',');
                    }
                }
            }
            Statement::End => {
                bytes.extend_from_slice(Keyword::End.internal_code().to_le_bytes().as_slice());
            }
            Statement::Remark { text: _ } => {
                // FIXME: can we safely omit the text?
                bytes.extend_from_slice(Keyword::Rem.internal_code().to_le_bytes().as_slice());
            }
            Statement::For {
                assignment,
                to_expr,
                step_expr,
            } => {
                bytes.extend_from_slice(Keyword::For.internal_code().to_le_bytes().as_slice());
                assignment.write_bytes(bytes);
                bytes.extend_from_slice(Keyword::To.internal_code().to_le_bytes().as_slice());
                to_expr.write_bytes(bytes);
                if let Some(step) = step_expr {
                    bytes.extend_from_slice(Keyword::Step.internal_code().to_le_bytes().as_slice());
                    step.write_bytes(bytes);
                }
            }
            Statement::Next { lvalue } => {
                bytes.extend_from_slice(Keyword::Next.internal_code().to_le_bytes().as_slice());
                lvalue.write_bytes(bytes);
            }
            Statement::Clear => {
                bytes.extend_from_slice(Keyword::Clear.internal_code().to_le_bytes().as_slice());
            }
            Statement::Goto { target } => {
                bytes.extend_from_slice(Keyword::Goto.internal_code().to_le_bytes().as_slice());
                target.write_bytes(bytes);
            }
            Statement::Gosub { target } => {
                bytes.extend_from_slice(Keyword::Gosub.internal_code().to_le_bytes().as_slice());
                target.write_bytes(bytes);
            }
            Statement::OnGoto { expr, targets } => {
                bytes.extend_from_slice(Keyword::On.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
                bytes.extend_from_slice(Keyword::Goto.internal_code().to_le_bytes().as_slice());
                for (i, target) in targets.iter().enumerate() {
                    target.write_bytes(bytes);
                    if i < targets.len() - 1 {
                        bytes.push(b',');
                    }
                }
            }
            Statement::OnGosub { expr, targets } => {
                bytes.extend_from_slice(Keyword::On.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
                bytes.extend_from_slice(Keyword::Gosub.internal_code().to_le_bytes().as_slice());
                for (i, target) in targets.iter().enumerate() {
                    target.write_bytes(bytes);
                    if i < targets.len() - 1 {
                        bytes.push(b',');
                    }
                }
            }
            Statement::OnErrorGoto { target } => {
                bytes.extend_from_slice(Keyword::On.internal_code().to_le_bytes().as_slice());
                bytes.extend_from_slice(Keyword::Error.internal_code().to_le_bytes().as_slice());
                bytes.extend_from_slice(Keyword::Goto.internal_code().to_le_bytes().as_slice());
                target.write_bytes(bytes);
            }
            Statement::Wait { expr } => {
                bytes.extend_from_slice(Keyword::Wait.internal_code().to_le_bytes().as_slice());
                if let Some(expr) = expr {
                    expr.write_bytes(bytes);
                }
            }
            Statement::Cls => {
                bytes.extend_from_slice(Keyword::Cls.internal_code().to_le_bytes().as_slice());
            }
            Statement::Random => {
                bytes.extend_from_slice(Keyword::Random.internal_code().to_le_bytes().as_slice());
            }
            Statement::Gprint { exprs } => {
                bytes.extend_from_slice(Keyword::Gprint.internal_code().to_le_bytes().as_slice());
                for (expr, sep) in exprs {
                    expr.write_bytes(bytes);
                    sep.write_bytes(bytes);
                }
            }
            Statement::GCursor { expr } => {
                bytes.extend_from_slice(Keyword::Gcursor.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
            Statement::Cursor { expr } => {
                bytes.extend_from_slice(Keyword::Cursor.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
            Statement::Beep {
                repetitions_expr,
                optional_params,
            } => {
                bytes.extend_from_slice(Keyword::Beep.internal_code().to_le_bytes().as_slice());
                repetitions_expr.write_bytes(bytes);
                if let Some(params) = optional_params {
                    bytes.push(b',');
                    params.frequency.write_bytes(bytes);
                    if let Some(duration) = &params.duration {
                        bytes.push(b',');
                        duration.write_bytes(bytes);
                    }
                }
            }
            Statement::BeepOnOff { switch_beep_on } => {
                bytes.extend_from_slice(Keyword::Beep.internal_code().to_le_bytes().as_slice());
                if *switch_beep_on {
                    bytes.extend_from_slice(Keyword::On.internal_code().to_le_bytes().as_slice());
                } else {
                    bytes.extend_from_slice(Keyword::Off.internal_code().to_le_bytes().as_slice());
                }
            }
            Statement::Return => {
                bytes.extend_from_slice(Keyword::Return.internal_code().to_le_bytes().as_slice());
            }
            Statement::Poke { memory_area, exprs } => {
                match memory_area {
                    MemoryArea::Me0 => bytes.extend_from_slice(
                        Keyword::PokeMem0.internal_code().to_le_bytes().as_slice(),
                    ),
                    MemoryArea::Me1 => bytes.extend_from_slice(
                        Keyword::PokeMem1.internal_code().to_le_bytes().as_slice(),
                    ),
                }
                for (i, expr) in exprs.iter().enumerate() {
                    expr.write_bytes(bytes);
                    if i < exprs.len() - 1 {
                        bytes.push(b',');
                    }
                }
            }
            Statement::Dim { decls } => {
                bytes.extend_from_slice(Keyword::Dim.internal_code().to_le_bytes().as_slice());
                for (i, decl) in decls.iter().enumerate() {
                    match decl {
                        DimInner::DimInner1D {
                            identifier,
                            size,
                            string_length,
                        } => {
                            identifier.write_bytes(bytes);
                            bytes.push(b'(');
                            size.write_bytes(bytes);
                            bytes.push(b')');
                            if let Some(len) = string_length {
                                bytes.push(b'*');
                                len.write_bytes(bytes);
                            }
                        }
                        DimInner::DimInner2D {
                            identifier,
                            rows,
                            cols,
                            string_length,
                        } => {
                            identifier.write_bytes(bytes);
                            bytes.push(b'(');
                            rows.write_bytes(bytes);
                            bytes.push(b',');
                            cols.write_bytes(bytes);
                            bytes.push(b')');
                            if let Some(len) = string_length {
                                bytes.push(b'*');
                                len.write_bytes(bytes);
                            }
                        }
                    }

                    if i < decls.len() - 1 {
                        bytes.push(b',');
                    }
                }
            }
            Statement::Read { destinations } => {
                bytes.extend_from_slice(Keyword::Read.internal_code().to_le_bytes().as_slice());
                for (i, dest) in destinations.iter().enumerate() {
                    dest.write_bytes(bytes);
                    if i < destinations.len() - 1 {
                        bytes.push(b',');
                    }
                }
            }
            Statement::Data(exprs) => {
                bytes.extend_from_slice(Keyword::Data.internal_code().to_le_bytes().as_slice());
                for (i, expr) in exprs.iter().enumerate() {
                    expr.write_bytes(bytes);
                    if i < exprs.len() - 1 {
                        bytes.push(b',');
                    }
                }
            }
            Statement::Restore { expr } => {
                bytes.extend_from_slice(Keyword::Restore.internal_code().to_le_bytes().as_slice());
                if let Some(expr) = expr {
                    expr.write_bytes(bytes);
                }
            }
            Statement::Arun => {
                bytes.extend_from_slice(Keyword::Arun.internal_code().to_le_bytes().as_slice());
            }
            Statement::Lock => {
                bytes.extend_from_slice(Keyword::Lock.internal_code().to_le_bytes().as_slice());
            }
            Statement::Unlock => {
                bytes.extend_from_slice(Keyword::Unlock.internal_code().to_le_bytes().as_slice());
            }
            Statement::Call { expr, variable } => {
                bytes.extend_from_slice(Keyword::Call.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
                if let Some(var) = variable {
                    bytes.push(b',');
                    var.write_bytes(bytes);
                }
            }
            Statement::Radian => {
                bytes.extend_from_slice(Keyword::Radian.internal_code().to_le_bytes().as_slice());
            }
            Statement::Text => {
                bytes.extend_from_slice(Keyword::Text.internal_code().to_le_bytes().as_slice());
            }
            Statement::Graph => {
                bytes.extend_from_slice(Keyword::Graph.internal_code().to_le_bytes().as_slice());
            }
            Statement::Color { expr } => {
                bytes.extend_from_slice(Keyword::Color.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
            Statement::CSize { expr } => {
                bytes.extend_from_slice(Keyword::Csize.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
            Statement::Lf { expr } => {
                bytes.extend_from_slice(Keyword::Lf.internal_code().to_le_bytes().as_slice());
                expr.write_bytes(bytes);
            }
            Statement::LCursor(l_cursor_clause) => {
                l_cursor_clause.write_bytes_with_context(false, bytes);
            }
            Statement::GlCursor { x_expr, y_expr } => {
                bytes.extend_from_slice(Keyword::Glcursor.internal_code().to_le_bytes().as_slice());
                bytes.push(b'(');
                x_expr.write_bytes(bytes);
                bytes.push(b',');
                y_expr.write_bytes(bytes);
                bytes.push(b')');
            }
            Statement::Line { inner } => {
                bytes.extend_from_slice(Keyword::Line.internal_code().to_le_bytes().as_slice());
                inner.write_bytes(bytes);
            }
            Statement::RLine { inner } => {
                bytes.extend_from_slice(Keyword::Rline.internal_code().to_le_bytes().as_slice());
                inner.write_bytes(bytes);
            }
            Statement::Sorgn => {
                bytes.extend_from_slice(Keyword::Sorgn.internal_code().to_le_bytes().as_slice());
            }
        }
    }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let { inner } => write!(f, "{}", inner.show_with_context(false)),
            Statement::If {
                condition,
                then_stmt,
            } => match then_stmt.as_ref() {
                Statement::Let { inner } => {
                    write!(f, "IF {condition} THEN {}", inner.show_with_context(true))
                }
                _ => write!(f, "IF {condition} THEN {then_stmt}"),
            },
            Statement::Print { inner } => write!(f, "PRINT {inner}"),
            Statement::Pause { inner } => write!(f, "PAUSE {inner}"),
            Statement::LPrint { inner } => {
                write!(f, "LPRINT {}", inner.to_string_with_context(true))
            }
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
            Statement::Next { lvalue: ident } => write!(f, "NEXT {ident}"),
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
            Statement::LCursor(l_cursor_clause) => write!(
                f,
                "LCURSOR {}",
                l_cursor_clause.to_string_with_context(false)
            ),
            Statement::GlCursor { x_expr, y_expr } => write!(f, "GLCURSOR ({x_expr}, {y_expr})"),
            Statement::Line { inner } => {
                write!(f, "{}", inner.to_string_with_prefix("LINE"))
            }
            Statement::RLine { inner } => {
                write!(f, "{}", inner.to_string_with_prefix("RLINE"))
            }
            Statement::Sorgn => write!(f, "SORGN"),
        }
    }
}
