// A fixed point decimal number, in BASIC it has a mantissa of 10 decimal places,
// In BCD, it has a 4-bit packed representation for each decimal digit,
// an i8 exponent, and a sign byte.

#[derive(Debug)]
pub struct DecimalNumber {
    exp: i8,
    sign: bool,        // true for positive, false for negative
    mantissa: [u8; 5], // 10 decimal digits, each 4 bits
}

impl DecimalNumber {
    pub fn new(exp: i8, mantissa_value: i64) -> Result<Self, &'static str> {
        let sign = mantissa_value >= 0;
        let abs_mantissa = mantissa_value.abs();

        // The mantissa represents the significant digits of the number
        // For 1500, we need mantissa to be 1500000000 (padded to 10 digits)
        let temp = abs_mantissa;
        let mut mantissa = [0u8; 5];

        // Convert to string to get digits in correct order
        let mantissa_str = format!("{temp:010}"); // Pad to 10 digits with leading zeros
        if mantissa_str.len() > 10 {
            return Err("Mantissa overflow: number has more than 10 significant digits");
        }

        // Pack digits into BCD format (2 digits per byte)
        // For 1500000000, we want bytes: 15H 00H 00H 00H 00H
        for (i, mantissa_byte) in mantissa.iter_mut().enumerate() {
            let start_idx = i * 2;
            let digit1_char = mantissa_str.chars().nth(start_idx).unwrap_or('0');
            let digit2_char = mantissa_str.chars().nth(start_idx + 1).unwrap_or('0');

            let digit1 = digit1_char.to_digit(10).unwrap_or(0) as u8;
            let digit2 = digit2_char.to_digit(10).unwrap_or(0) as u8;

            *mantissa_byte = (digit1 << 4) | digit2;
        }

        Ok(DecimalNumber {
            exp,
            sign,
            mantissa,
        })
    }

    // Convert BCD mantissa to i64 for display purposes
    fn mantissa_to_i64(&self) -> i64 {
        let mut result = 0i64;

        // Unpack BCD bytes to get the 10 digits
        // The mantissa is stored most significant byte first
        for &byte in &self.mantissa {
            let upper_digit = ((byte & 0xF0) >> 4) as i64;
            let lower_digit = (byte & 0x0F) as i64;

            result = result * 100 + upper_digit * 10 + lower_digit;
        }

        result
    }

    // Check if mantissa is zero
    fn is_zero(&self) -> bool {
        self.mantissa.iter().all(|&b| b == 0)
    }

    // Create from raw 8-byte representation (ignoring last byte as per documentation)
    #[allow(dead_code)]
    pub fn from_bytes(bytes: &[u8; 8]) -> Result<Self, &'static str> {
        if bytes.len() != 8 {
            return Err("Expected exactly 8 bytes");
        }

        let exp = bytes[0] as i8;
        let sign_byte = bytes[1];
        let sign = sign_byte == 0x00; // 0x00 = positive, 0x80 = negative

        let mut mantissa = [0u8; 5];
        mantissa.copy_from_slice(&bytes[2..7]);

        Ok(DecimalNumber {
            exp,
            sign,
            mantissa,
        })
    }

    // Convert to raw 8-byte representation
    #[allow(dead_code)]
    pub fn to_bytes(&self) -> [u8; 8] {
        let mut bytes = [0u8; 8];
        bytes[0] = self.exp as u8;
        bytes[1] = if self.sign { 0x00 } else { 0x80 };
        bytes[2..7].copy_from_slice(&self.mantissa);
        bytes[7] = 0x00; // Last byte always 0
        bytes
    }

    // If the decimal number is a whole number, convert it to an integer
    pub fn into_integer(self) -> Option<i64> {
        // Handle zero case
        if self.is_zero() {
            return Some(0);
        }

        let mantissa_i64 = self.mantissa_to_i64();
        let exp = self.exp;

        // Calculate the position of the decimal point
        let decimal_pos = exp + 1;

        // For a number to be a whole integer:
        // 1. The decimal point must be at or after all significant digits
        // 2. Or all digits after the decimal point must be zeros

        let mantissa_str = mantissa_i64.to_string();
        let mantissa_digits = mantissa_str.len() as i8;

        if decimal_pos <= 0 {
            // Number is less than 1 (like 0.5, 0.001), so not a whole number
            None
        } else if decimal_pos >= mantissa_digits {
            // Number is an integer or needs trailing zeros (like 123, 1500)
            let trailing_zeros = (decimal_pos - mantissa_digits) as u32;
            let result = mantissa_i64 * 10_i64.pow(trailing_zeros);

            // Apply sign
            if self.sign {
                Some(result)
            } else {
                Some(-result)
            }
        } else {
            // Decimal point is within the mantissa, check if fractional part is all zeros
            let decimal_pos = decimal_pos as usize;
            let (whole_part_str, frac_part_str) = mantissa_str.split_at(decimal_pos);

            // Check if fractional part is all zeros
            if frac_part_str.chars().all(|c| c == '0') {
                // It's a whole number
                let result = whole_part_str.parse::<i64>().unwrap_or(0);

                // Apply sign
                if self.sign {
                    Some(result)
                } else {
                    Some(-result)
                }
            } else {
                // Has fractional part, not a whole number
                None
            }
        }
    }
}

fn count_decimal_digits(n: i64) -> u32 {
    if n == 0 {
        return 1; // "0" has 1 digit
    }
    n.abs().ilog10() + 1
}

impl std::str::FromStr for DecimalNumber {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Valid decimal numbers can be integers: "123", "0", "-456"
        // or floating point numbers: "123.456", "-0.001", "123.0000000001"
        // or scientific notation: "1.23E4", "5.67E-3", "-8.90E+2"
        let number_str = s.trim();

        // Handle sign
        let (is_negative, number_str) = if let Some(stripped) = number_str.strip_prefix('-') {
            (true, stripped)
        } else {
            (false, number_str)
        };

        // Whole part can be empty: ".5" is valid
        let (whole_part, fractional_part, exponent_part) =
            if let Some((whole, frac_and_exp)) = number_str.split_once('.') {
                if let Some((frac, exp)) = frac_and_exp.split_once('E') {
                    (whole, Some(frac), Some(exp))
                } else {
                    (whole, Some(frac_and_exp), None)
                }
            } else if let Some((whole, exp)) = number_str.split_once('E') {
                (whole, None, Some(exp))
            } else {
                (number_str, None, None)
            };

        // Parse whole part (now always positive since we handled sign separately)
        let whole_part = if whole_part.is_empty() {
            0
        } else {
            whole_part
                .parse::<i64>()
                .map_err(|_| "Invalid whole part")?
        };

        // Parse fractional part
        let (fractional_part, fractional_str_len) = if let Some(frac) = fractional_part {
            if frac.is_empty() {
                (0, 0)
            } else {
                let parsed = frac.parse::<i64>().map_err(|_| "Invalid fractional part")?;
                (parsed, frac.len())
            }
        } else {
            (0, 0)
        };

        let explicit_exponent_part = if let Some(exp) = exponent_part {
            if exp.is_empty() {
                0
            } else {
                exp.parse::<i8>().map_err(|_| "Invalid exponent part")?
            }
        } else {
            0
        };

        // Calculate the mantissa and exponent
        // Use the original string length instead of counting digits after parsing
        let mantissa_digits_count = fractional_str_len;

        // Create the normalized mantissa (always 10 digits)
        let mantissa = if whole_part == 0 && fractional_part != 0 {
            // For numbers like "0.001", the mantissa should be the fractional part
            // normalized to 10 digits: 1000000000
            fractional_part * 10_i64.pow(10 - count_decimal_digits(fractional_part))
        } else {
            // For regular numbers, combine whole and fractional parts
            whole_part * 10_i64.pow(mantissa_digits_count as u32) + fractional_part
        };

        // Apply sign to mantissa
        let signed_mantissa = if is_negative { -mantissa } else { mantissa };

        // Calculate the correct exponent
        // The exponent represents the position of the decimal point
        // For "1": mantissa=1000000000, exp should be 0 (1.000000000 × 10^0 = 1)
        // For "123": mantissa=1230000000, exp should be 2 (1.230000000 × 10^2 = 123)
        // For "0.001": mantissa=1000000000, exp should be -3 (1.000000000 × 10^-3 = 0.001)
        let whole_digits_count = if whole_part == 0 {
            0
        } else {
            count_decimal_digits(whole_part)
        } as i8;
        let exp = if whole_part == 0 && fractional_part != 0 {
            // For numbers like "0.001", we need to find position of first non-zero digit
            // Use the original string length, not the parsed number's digit count
            explicit_exponent_part - mantissa_digits_count as i8
        } else {
            // For numbers like "1", "123"
            explicit_exponent_part + whole_digits_count - 1
        };

        let decimal_number = DecimalNumber::new(exp, signed_mantissa)?;
        Ok(decimal_number)
    }
}

impl std::fmt::Display for DecimalNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let exp = self.exp;

        // Handle zero case
        if self.is_zero() {
            return write!(f, "0");
        }

        // Handle sign
        let sign = if self.sign { "" } else { "-" };

        // Convert BCD mantissa to decimal string
        let mantissa_i64 = self.mantissa_to_i64();
        let mantissa_str = mantissa_i64.to_string();
        let mantissa_digits = mantissa_str.len() as i8;

        // Calculate the position of the decimal point
        let decimal_pos = exp + 1;

        if decimal_pos <= 0 {
            // Number is less than 1, need leading zeros after decimal point
            let leading_zeros = (-decimal_pos) as usize;
            write!(
                f,
                "{sign}0.{}{}",
                "0".repeat(leading_zeros),
                mantissa_str.trim_end_matches('0')
            )
        } else if decimal_pos >= mantissa_digits {
            // Number is an integer or needs trailing zeros
            let trailing_zeros = (decimal_pos - mantissa_digits) as usize;
            write!(f, "{sign}{mantissa_str}{}", "0".repeat(trailing_zeros))
        } else {
            // Decimal point is within the mantissa
            let decimal_pos = decimal_pos as usize;
            let (whole_part, frac_part) = mantissa_str.split_at(decimal_pos);

            // Remove trailing zeros from fractional part
            let frac_part = frac_part.trim_end_matches('0');

            if frac_part.is_empty() {
                write!(f, "{sign}{whole_part}")
            } else {
                write!(f, "{sign}{whole_part}.{frac_part}")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_1500_example() {
        // From documentation: 1500 should be 03H 00H 15H 00H 00H 00H 00H 00H
        let expected_bytes = [0x03, 0x00, 0x15, 0x00, 0x00, 0x00, 0x00, 0x00];

        // Create DecimalNumber from the documented bytes
        let decimal = DecimalNumber::from_bytes(&expected_bytes).unwrap();
        println!(
            "1500 example - mantissa_to_i64: {}",
            decimal.mantissa_to_i64()
        );
        println!("1500 example - display: {decimal}");

        // Test our implementation creates the same bytes
        // For 1500, if exp=3, then mantissa should be 1500000000 (10 digits)
        let decimal_from_constructor = DecimalNumber::new(3, 1500000000).unwrap();
        let generated_bytes = decimal_from_constructor.to_bytes();

        println!("Generated bytes: {generated_bytes:02X?}");
        println!("Expected bytes:  {expected_bytes:02X?}");

        assert_eq!(generated_bytes, expected_bytes);
    }

    #[test]
    fn test_123456_example() {
        // From documentation: 1.23456 should be 00H 00H 12H 34H 56H 00H 00H 00H
        let expected_bytes = [0x00, 0x00, 0x12, 0x34, 0x56, 0x00, 0x00, 0x00];

        let decimal = DecimalNumber::from_bytes(&expected_bytes).unwrap();
        println!(
            "1.23456 example - mantissa_to_i64: {}",
            decimal.mantissa_to_i64()
        );
        println!("1.23456 example - display: {decimal}");
    }

    #[test]
    fn test_simple_integers() {
        // Test parsing simple integers like line numbers
        let test_cases = vec![
            ("1", "1"),
            ("2", "2"),
            ("8", "8"),
            ("10", "10"),
            ("20", "20"),
            ("123", "123"),
        ];

        for (input, expected) in test_cases {
            let decimal: DecimalNumber = input.parse().unwrap();
            let display_result = decimal.to_string();
            println!("Input: '{input}' -> Display: '{display_result}' (expected: '{expected}')");
            assert_eq!(display_result, expected, "Failed for input: {input}");
        }
    }

    #[test]
    fn test_decimal_numbers() {
        // Test parsing decimal numbers
        let test_cases = vec![
            ("0.5", "0.5"),
            ("3.14", "3.14"),
            ("0.001", "0.001"),
            ("123.456", "123.456"),
        ];

        for (input, expected) in test_cases {
            let decimal: DecimalNumber = input.parse().unwrap();
            let display_result = decimal.to_string();
            println!(
                "Decimal Input: '{input}' -> Display: '{display_result}' (expected: '{expected}')"
            );
            assert_eq!(
                display_result, expected,
                "Failed for decimal input: {input}"
            );
        }
    }

    #[test]
    fn test_into_integer() {
        // Test converting decimal numbers to integers
        let test_cases = vec![
            // (input_string, expected_integer_result)
            ("0", Some(0)),
            ("1", Some(1)),
            ("123", Some(123)),
            ("-456", Some(-456)),
            ("1500", Some(1500)),
            ("1.0", Some(1)),
            ("123.0", Some(123)),
            ("1500.000", Some(1500)),
            ("-42.0", Some(-42)),
            // Non-integers should return None
            ("0.5", None),
            ("3.14", None),
            ("0.001", None),
            ("123.456", None),
            ("-1.5", None),
        ];

        for (input, expected) in test_cases {
            let decimal: DecimalNumber = input.parse().unwrap();
            let result = decimal.into_integer();
            println!("into_integer('{input}') -> {result:?} (expected: {expected:?})");
            assert_eq!(result, expected, "Failed for input: {input}");
        }
    }
}
