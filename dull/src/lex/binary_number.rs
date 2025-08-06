#[derive(Debug)]
pub struct BinaryNumber {
    value: u16,
}
impl BinaryNumber {
    pub fn new(value: u16) -> Self {
        BinaryNumber { value }
    }

    pub fn value(&self) -> u16 {
        self.value
    }
}

impl std::fmt::Display for BinaryNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Show value in all caps hexadecimal format
        write!(f, "&{:X}", self.value)
    }
}
