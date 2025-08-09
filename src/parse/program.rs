pub struct Program {
    pub lines: std::collections::BTreeMap<u16, crate::parse::code_line::CodeLine>,
}

impl Program {
    pub fn write_bytes(&self, bytes: &mut Vec<u8>) {
        for line in self.lines.values() {
            line.write_bytes(bytes);
            // Carriage return at the end of every line
            bytes.push(b'\r');
        }

        // 0xFF at the end of the program
        bytes.push(0xFF);
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in self.lines.values() {
            writeln!(f, "{line}")?;
        }
        Ok(())
    }
}
