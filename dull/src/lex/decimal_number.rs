#[derive(Debug)]
pub struct DecimalNumber {
    value: f64,
}

impl DecimalNumber {
    pub fn new(value: f64) -> Result<Self, &'static str> {
        // For now, accept any valid f64 value
        // In a real BASIC implementation, you might want to check ranges
        if value.is_finite() {
            Ok(DecimalNumber { value })
        } else {
            Err("DecimalNumber: invalid value (infinite or NaN)")
        }
    }

    pub fn value(&self) -> f64 {
        self.value
    }
}

impl std::fmt::Display for DecimalNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let abs_n = self.value.abs();

        fn format_mantissa(m: f64) -> String {
            let rounded = (m * 1e10).round() / 1e10; // Round to 10 decimal places
            let mut str = rounded.to_string();
            if str.contains('.') {
                // Remove trailing zeros after the decimal point
                str = str.trim_end_matches('0').to_string();
                if str.ends_with('.') {
                    str.pop(); // Remove trailing dot if it exists
                }
            }
            str
        }

        fn format_scientific(n: f64) -> String {
            let sign = if n < 0.0 { "-" } else { "" };
            let abs_n = n.abs();
            let exp = abs_n.log10().floor() as i32;
            let mantissa = abs_n / 10f64.powi(exp);
            let mantissa = format_mantissa(mantissa);
            format!("{sign}{mantissa}E{exp}")
        }

        fn format_regular(n: f64) -> String {
            let abs_n_string = n.abs().to_string();

            let (whole_part, fractional_part) = abs_n_string
                .split_once('.')
                .unwrap_or((abs_n_string.as_str(), ""));
            let whole_str = whole_part.to_string();
            let fractional_str = fractional_part.to_string();
            if fractional_str.is_empty() {
                whole_str
            } else {
                format!("{}.{}", whole_str, fractional_str.trim_end_matches('0'))
            }
        }

        if abs_n >= 1e6 || (abs_n > 0.0 && abs_n < 1e-4) {
            write!(f, "{}", format_scientific(self.value))
        } else {
            write!(f, "{}", format_regular(self.value))
        }
    }
}
