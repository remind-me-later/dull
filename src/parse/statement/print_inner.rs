use crate::{
    error::Span,
    parse::statement::{print_separator::PrintSeparator, printable::Printable},
};

#[derive(Debug, Clone, PartialEq)]
pub struct PrintInner {
    pub exprs: Vec<(Printable, PrintSeparator)>,
    pub span: Span,
}

impl PrintInner {
    pub fn new(exprs: Vec<(Printable, PrintSeparator)>, span: Span) -> Self {
        Self { exprs, span }
    }
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
