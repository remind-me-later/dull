// A fixed point decimal number, in BASIC it has a mantissa of 10 decimal places,
// In BCD, it has a 4-bit packed representation for each decimal digit,
// an i8 exponent, and a sign byte.

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DecimalNumber {
    double: f64,
}

impl DecimalNumber {
    pub fn new(double: f64) -> Self {
        Self { double }
    }

    // If the decimal number is a whole number, convert it to an integer
    pub fn as_integer(&self) -> Option<i64> {
        if self.double.fract() == 0.0 {
            Some(self.double as i64)
        } else {
            None
        }
    }

    pub fn write_bytes(&self, bytes: &mut Vec<u8>) {
        if self.double == 0.0 {
            bytes.push(b'0');
            return;
        }

        let double_str = self.double.to_string();

        if self.double.abs() < 1.0 {
            // dont add 0 before .
            let trimmed = double_str.trim_start_matches('0');
            bytes.extend_from_slice(trimmed.as_bytes());
            return;
        }

        bytes.extend_from_slice(double_str.as_bytes());
    }
}

impl std::str::FromStr for DecimalNumber {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let double = s.parse::<f64>().map_err(|_| "Invalid decimal number")?;
        Ok(Self::new(double))
    }
}

impl std::fmt::Display for DecimalNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.double)
    }
}
