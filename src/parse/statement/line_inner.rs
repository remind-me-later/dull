use crate::{error::Span, parse::expression::Expr};

#[derive(Debug, Clone, PartialEq)]
pub struct LineInner {
    pub start_point: Option<(Expr, Expr)>, // None means use current pen position
    pub end_points: Vec<(Expr, Expr)>,
    pub line_type: Option<Expr>,
    pub color: Option<Expr>,
    pub is_box: bool,
    pub span: Span,
}

impl LineInner {
    pub fn new(
        start_point: Option<(Expr, Expr)>,
        end_points: Vec<(Expr, Expr)>,
        line_type: Option<Expr>,
        color: Option<Expr>,
        is_box: bool,
        span: Span,
    ) -> Self {
        Self {
            start_point,
            end_points,
            line_type,
            color,
            is_box,
            span,
        }
    }

    pub fn write_bytes(&self, bytes: &mut Vec<u8>, preserve_source_wording: bool) {
        // Handle start point - if None, start with '-'
        if let Some((x, y)) = &self.start_point {
            bytes.push(b'(');
            x.write_bytes(bytes, preserve_source_wording);
            bytes.push(b',');
            y.write_bytes(bytes, preserve_source_wording);
            bytes.push(b')');
        }

        for (x, y) in &self.end_points {
            bytes.push(b'-');
            bytes.push(b'(');
            x.write_bytes(bytes, preserve_source_wording);
            bytes.push(b',');
            y.write_bytes(bytes, preserve_source_wording);
            bytes.push(b')');
        }

        if let Some(line_type) = &self.line_type {
            bytes.push(b',');
            line_type.write_bytes(bytes, preserve_source_wording);
        }

        if let Some(color) = &self.color {
            bytes.push(b',');
            color.write_bytes(bytes, preserve_source_wording);
        }

        if self.is_box {
            bytes.push(b',');
            bytes.push(b'B');
        }
    }

    pub fn to_string_with_prefix(&self, prefix: &str, preserve_source_wording: bool) -> String {
        let mut result = format!("{prefix} ");

        // Handle start point - if None, start with '-'
        if let Some((x, y)) = &self.start_point {
            result.push_str(&format!(
                "({},{})",
                x.show(preserve_source_wording),
                y.show(preserve_source_wording)
            ));
        }

        for (x, y) in &self.end_points {
            result.push_str(&format!(
                "-({},{})",
                x.show(preserve_source_wording),
                y.show(preserve_source_wording)
            ));
        }

        if let Some(line_type) = &self.line_type {
            result.push_str(&format!(",{}", line_type.show(preserve_source_wording)));
        }

        if let Some(color) = &self.color {
            result.push_str(&format!(",{}", color.show(preserve_source_wording)));
        }

        if self.is_box {
            result.push_str(",B");
        }

        result
    }
}
