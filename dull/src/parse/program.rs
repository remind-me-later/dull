pub struct Program {
    pub lines: std::collections::BTreeMap<u16, crate::parse::line::Line>,
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in self.lines.values() {
            writeln!(f, "{line}")?;
        }
        Ok(())
    }
}
