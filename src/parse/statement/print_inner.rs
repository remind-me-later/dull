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

    pub fn show(&self, preserve_source_wording: bool) -> String {
        let mut result = String::new();
        for (printable, sep) in self.exprs.iter() {
            result.push_str(&printable.show(preserve_source_wording));
            result.push_str(&sep.to_string());
        }
        result
    }
}
