use crate::error::{SemanticError, SemanticResult, Span};
use crate::lex::{identifier::Identifier, keyword::Keyword};
use crate::parse::{
    expression::{
        Expr, binary_op::BinaryOp, expr_inner::ExprInner, function::Function, lvalue::LValue,
        unary_op::UnaryOp,
    },
    line::Line,
    program::Program,
    statement::{Assignment, BeepOptionalParams, DimInner, LCursorClause, Statement},
};
use std::collections::HashMap;

// Helper function to create a placeholder span
fn placeholder_span() -> Span {
    Span::new(0, 1)
}

/// Basic types in the BASIC language type system
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BasicType {
    Numeric,
    String,
    NumericArray,
    StringArray,
    Numeric2DArray,
    String2DArray,
}

/// Symbol table for tracking variable and label declarations
#[derive(Debug, Clone)]
pub struct SymbolTable {
    variables: HashMap<String, BasicType>,
    labels: HashMap<String, u16>, // label name -> line number
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            labels: HashMap::new(),
        }
    }

    pub fn insert_variable(&mut self, name: String, type_: BasicType) {
        self.variables.insert(name, type_);
    }

    pub fn insert_label(&mut self, name: String, line_number: u16) {
        self.labels.insert(name, line_number);
    }
}

#[derive(Debug)]
pub struct SemanticAnalysisState {
    symbol_table: SymbolTable,
}

impl SemanticAnalysisState {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn insert_variable(&mut self, identifier: &Identifier, type_: BasicType) {
        let name = identifier.to_string();
        self.symbol_table.insert_variable(name, type_);
    }

    pub fn insert_label(&mut self, name: String, line_number: u16) {
        self.symbol_table.insert_label(name, line_number);
    }
}

pub struct SemanticAnalyzer {
    state: SemanticAnalysisState,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            state: SemanticAnalysisState::new(),
        }
    }

    /// Analyze a complete program
    pub fn analyze_program(&mut self, program: &Program) -> SemanticResult<()> {
        // First pass: collect all labels
        for line in program.lines.values() {
            if let Some(ref label) = line.label {
                self.state.insert_label(label.clone(), line.number);
            }
        }

        // Second pass: analyze statements
        for line in program.lines.values() {
            self.analyze_line(line)?;
        }

        Ok(())
    }

    /// Analyze a single line
    fn analyze_line(&mut self, line: &Line) -> SemanticResult<()> {
        for statement in &line.statements {
            self.analyze_statement(statement)?;
        }
        Ok(())
    }

    /// Analyze a statement
    fn analyze_statement(&mut self, statement: &Statement) -> SemanticResult<()> {
        match statement {
            Statement::Let { inner } => {
                for assignment in &inner.assignments {
                    self.analyze_assignment(assignment)?;
                }
            }
            Statement::If {
                condition,
                then_stmt,
            } => {
                let condition_type = self.analyze_expression(condition)?;
                if condition_type != BasicType::Numeric {
                    return Err(SemanticError::TypeMismatch {
                        expected: BasicType::Numeric,
                        found: condition_type,
                        span: placeholder_span(),
                    });
                }
                self.analyze_statement(then_stmt)?;
            }
            Statement::For {
                assignment,
                to_expr,
                step_expr,
            } => {
                self.analyze_assignment(assignment)?;
                let to_type = self.analyze_expression(to_expr)?;
                if to_type != BasicType::Numeric {
                    return Err(SemanticError::TypeMismatch {
                        expected: BasicType::Numeric,
                        found: to_type,
                        span: placeholder_span(),
                    });
                }
                if let Some(step) = step_expr {
                    let step_type = self.analyze_expression(step)?;
                    if step_type != BasicType::Numeric {
                        return Err(SemanticError::TypeMismatch {
                            expected: BasicType::Numeric,
                            found: step_type,
                            span: placeholder_span(),
                        });
                    }
                }
            }
            Statement::Next { ident } => {
                // Ensure the identifier is numeric (FOR loop variable)
                self.state.insert_variable(ident, BasicType::Numeric);
            }
            Statement::Goto { target } => {
                self.analyze_expression(target)?;
            }
            Statement::Gosub { target } => {
                self.analyze_expression(target)?;
            }
            Statement::Dim { decls } => {
                for dim in decls {
                    self.analyze_dim_declaration(dim)?;
                }
            }
            Statement::Read { destinations } => {
                for lvalue in destinations {
                    self.analyze_lvalue(lvalue)?;
                }
            }
            Statement::Data(expressions) => {
                for expr in expressions {
                    self.analyze_expression(expr)?;
                }
            }
            Statement::Restore { expr } => {
                if let Some(expr) = expr {
                    self.analyze_expression(expr)?;
                }
            }
            Statement::OnGoto { expr, targets } => {
                let expr_type = self.analyze_expression(expr)?;
                if expr_type != BasicType::Numeric {
                    return Err(SemanticError::TypeMismatch {
                        expected: BasicType::Numeric,
                        found: expr_type,
                        span: placeholder_span(),
                    });
                }
                for target in targets {
                    self.analyze_expression(target)?;
                }
            }
            Statement::OnGosub { expr, targets } => {
                let expr_type = self.analyze_expression(expr)?;
                if expr_type != BasicType::Numeric {
                    return Err(SemanticError::TypeMismatch {
                        expected: BasicType::Numeric,
                        found: expr_type,
                        span: placeholder_span(),
                    });
                }
                for target in targets {
                    self.analyze_expression(target)?;
                }
            }
            Statement::Wait { expr } => {
                if let Some(expr) = expr {
                    let expr_type = self.analyze_expression(expr)?;
                    if expr_type != BasicType::Numeric {
                        return Err(SemanticError::TypeMismatch {
                            expected: BasicType::Numeric,
                            found: expr_type,
                            span: placeholder_span(),
                        });
                    }
                }
            }
            Statement::Beep {
                repetitions_expr,
                optional_params,
            } => {
                let rep_type = self.analyze_expression(repetitions_expr)?;
                if rep_type != BasicType::Numeric {
                    return Err(SemanticError::TypeMismatch {
                        expected: BasicType::Numeric,
                        found: rep_type,
                        span: placeholder_span(),
                    });
                }
                if let Some(params) = optional_params {
                    self.analyze_beep_params(params)?;
                }
            }
            Statement::Cursor { expr } => {
                let expr_type = self.analyze_expression(expr)?;
                if expr_type != BasicType::Numeric {
                    return Err(SemanticError::TypeMismatch {
                        expected: BasicType::Numeric,
                        found: expr_type,
                        span: placeholder_span(),
                    });
                }
            }
            Statement::GCursor { expr } => {
                let expr_type = self.analyze_expression(expr)?;
                if expr_type != BasicType::Numeric {
                    return Err(SemanticError::TypeMismatch {
                        expected: BasicType::Numeric,
                        found: expr_type,
                        span: placeholder_span(),
                    });
                }
            }
            Statement::LCursor(clause) => {
                self.analyze_lcursor_clause(clause)?;
            }
            Statement::Color { expr } => {
                let expr_type = self.analyze_expression(expr)?;
                if expr_type != BasicType::Numeric {
                    return Err(SemanticError::TypeMismatch {
                        expected: BasicType::Numeric,
                        found: expr_type,
                        span: placeholder_span(),
                    });
                }
            }
            Statement::CSize { expr } => {
                let expr_type = self.analyze_expression(expr)?;
                if expr_type != BasicType::Numeric {
                    return Err(SemanticError::TypeMismatch {
                        expected: BasicType::Numeric,
                        found: expr_type,
                        span: placeholder_span(),
                    });
                }
            }
            Statement::Lf { expr } => {
                let expr_type = self.analyze_expression(expr)?;
                if expr_type != BasicType::Numeric {
                    return Err(SemanticError::TypeMismatch {
                        expected: BasicType::Numeric,
                        found: expr_type,
                        span: placeholder_span(),
                    });
                }
            }
            // Statements that don't require type checking
            Statement::Print { .. } => {}  // Print can handle any type
            Statement::LPrint { .. } => {} // LPrint can handle any type
            Statement::Gprint { .. } => {} // GPrint can handle any type
            Statement::Pause { .. } => {}  // Pause can handle any type
            Statement::Input { .. } => {}  // Input determines types at runtime
            Statement::Using { .. } => {}  // Using is a formatting directive
            Statement::End => {}
            Statement::Clear => {}
            Statement::Cls => {}
            Statement::Random => {}
            Statement::Return => {}
            Statement::Remark { .. } => {}
            Statement::Poke { .. } => {} // Poke requires numeric but we'll handle separately
            Statement::Arun => {}
            Statement::Lock => {}
            Statement::Unlock => {}
            Statement::OnErrorGoto { .. } => {}
            Statement::Call { .. } => {}
            Statement::BeepOnOff { .. } => {}
            Statement::Text => {}
            Statement::Graph => {}
            Statement::Radian => {}
        }
        Ok(())
    }

    /// Analyze an assignment statement
    fn analyze_assignment(&mut self, assignment: &Assignment) -> SemanticResult<()> {
        let lvalue_type = self.analyze_lvalue(&assignment.lvalue)?;
        let expr_type = self.analyze_expression(&assignment.expr)?;

        if lvalue_type != expr_type {
            return Err(SemanticError::TypeMismatch {
                expected: lvalue_type,
                found: expr_type,
                span: placeholder_span(),
            });
        }

        Ok(())
    }

    /// Analyze an lvalue and return its type, also inserting it into the symbol table if it's a new variable
    fn analyze_lvalue(&mut self, lvalue: &LValue) -> SemanticResult<BasicType> {
        match lvalue {
            LValue::Identifier(identifier) => {
                let var_type = if identifier.has_dollar() {
                    BasicType::String
                } else {
                    BasicType::Numeric
                };
                self.state.insert_variable(identifier, var_type);
                Ok(var_type)
            }
            LValue::BuiltInIdentifier(keyword) => {
                let var_type = match keyword {
                    Keyword::Time => BasicType::Numeric,
                    Keyword::InkeyDollar => BasicType::String,
                    _ => {
                        return Err(SemanticError::InvalidExpression {
                            span: placeholder_span(),
                            message: format!("Invalid built-in identifier: {keyword:?}"),
                        });
                    }
                };
                Ok(var_type)
            }
            LValue::Array1DAccess { identifier, index } => {
                let index_type = self.analyze_expression(index)?;
                if index_type != BasicType::Numeric {
                    return Err(SemanticError::InvalidArrayIndex {
                        span: placeholder_span(),
                        message: "Array index must be numeric".to_string(),
                    });
                }
                let array_type = if identifier.has_dollar() {
                    BasicType::StringArray
                } else {
                    BasicType::NumericArray
                };
                self.state.insert_variable(identifier, array_type);

                let element_type = if identifier.has_dollar() {
                    BasicType::String
                } else {
                    BasicType::Numeric
                };
                Ok(element_type)
            }
            LValue::Array2DAccess {
                identifier,
                row_index,
                col_index,
            } => {
                let row_type = self.analyze_expression(row_index)?;
                let col_type = self.analyze_expression(col_index)?;
                if row_type != BasicType::Numeric || col_type != BasicType::Numeric {
                    return Err(SemanticError::InvalidArrayIndex {
                        span: placeholder_span(),
                        message: "2D array indices must be numeric".to_string(),
                    });
                }
                let array_type = if identifier.has_dollar() {
                    BasicType::String2DArray
                } else {
                    BasicType::Numeric2DArray
                };
                self.state.insert_variable(identifier, array_type);

                let element_type = if identifier.has_dollar() {
                    BasicType::String
                } else {
                    BasicType::Numeric
                };
                Ok(element_type)
            }
            LValue::FixedMemoryAreaAccess { index, has_dollar } => {
                let index_type = self.analyze_expression(index)?;
                if index_type != BasicType::Numeric {
                    return Err(SemanticError::InvalidArrayIndex {
                        span: placeholder_span(),
                        message: "Memory area index must be numeric".to_string(),
                    });
                }
                let var_type = if *has_dollar {
                    BasicType::String
                } else {
                    BasicType::Numeric
                };
                Ok(var_type)
            }
        }
    }

    /// Analyze a dimension declaration
    fn analyze_dim_declaration(&mut self, dim: &DimInner) -> SemanticResult<()> {
        match dim {
            DimInner::DimInner1D {
                identifier,
                size,
                string_length,
            } => {
                let size_type = self.analyze_expression(size)?;
                if size_type != BasicType::Numeric {
                    return Err(SemanticError::TypeMismatch {
                        expected: BasicType::Numeric,
                        found: size_type,
                        span: placeholder_span(),
                    });
                }

                if let Some(str_len) = string_length {
                    let str_len_type = self.analyze_expression(str_len)?;
                    if str_len_type != BasicType::Numeric {
                        return Err(SemanticError::TypeMismatch {
                            expected: BasicType::Numeric,
                            found: str_len_type,
                            span: placeholder_span(),
                        });
                    }
                }

                let array_type = if identifier.has_dollar() {
                    BasicType::StringArray
                } else {
                    BasicType::NumericArray
                };
                self.state.insert_variable(identifier, array_type);
            }
            DimInner::DimInner2D {
                identifier,
                rows,
                cols,
                string_length,
            } => {
                let rows_type = self.analyze_expression(rows)?;
                let cols_type = self.analyze_expression(cols)?;
                if rows_type != BasicType::Numeric || cols_type != BasicType::Numeric {
                    return Err(SemanticError::TypeMismatch {
                        expected: BasicType::Numeric,
                        found: if rows_type != BasicType::Numeric {
                            rows_type
                        } else {
                            cols_type
                        },
                        span: placeholder_span(),
                    });
                }

                if let Some(str_len) = string_length {
                    let str_len_type = self.analyze_expression(str_len)?;
                    if str_len_type != BasicType::Numeric {
                        return Err(SemanticError::TypeMismatch {
                            expected: BasicType::Numeric,
                            found: str_len_type,
                            span: placeholder_span(),
                        });
                    }
                }

                let array_type = if identifier.has_dollar() {
                    BasicType::String2DArray
                } else {
                    BasicType::Numeric2DArray
                };
                self.state.insert_variable(identifier, array_type);
            }
        }
        Ok(())
    }

    /// Analyze BEEP optional parameters
    fn analyze_beep_params(&mut self, params: &BeepOptionalParams) -> SemanticResult<()> {
        let freq_type = self.analyze_expression(&params.frequency)?;
        if freq_type != BasicType::Numeric {
            return Err(SemanticError::TypeMismatch {
                expected: BasicType::Numeric,
                found: freq_type,
                span: placeholder_span(),
            });
        }

        if let Some(ref duration) = params.duration {
            let dur_type = self.analyze_expression(duration)?;
            if dur_type != BasicType::Numeric {
                return Err(SemanticError::TypeMismatch {
                    expected: BasicType::Numeric,
                    found: dur_type,
                    span: placeholder_span(),
                });
            }
        }

        Ok(())
    }

    /// Analyze LCURSOR clause
    fn analyze_lcursor_clause(&mut self, clause: &LCursorClause) -> SemanticResult<()> {
        let expr_type = self.analyze_expression(&clause.expr)?;
        if expr_type != BasicType::Numeric {
            return Err(SemanticError::TypeMismatch {
                expected: BasicType::Numeric,
                found: expr_type,
                span: placeholder_span(),
            });
        }
        Ok(())
    }

    /// Analyze an expression and return its type
    fn analyze_expression(&mut self, expr: &Expr) -> SemanticResult<BasicType> {
        self.analyze_expr_inner(&expr.inner)
    }

    /// Analyze expression inner and return its type
    fn analyze_expr_inner(&mut self, expr_inner: &ExprInner) -> SemanticResult<BasicType> {
        match expr_inner {
            ExprInner::DecimalNumber(_) => Ok(BasicType::Numeric),
            ExprInner::BinaryNumber(_) => Ok(BasicType::Numeric),
            ExprInner::StringLiteral(_) => Ok(BasicType::String),
            ExprInner::LValue(lvalue) => {
                let lvalue_type = self.analyze_lvalue(lvalue)?;
                Ok(lvalue_type)
            }
            ExprInner::Unary(op, expr) => {
                let expr_type = self.analyze_expression(expr)?;
                match op {
                    UnaryOp::Plus | UnaryOp::Minus => {
                        if expr_type != BasicType::Numeric {
                            return Err(SemanticError::TypeMismatch {
                                expected: BasicType::Numeric,
                                found: expr_type,
                                span: placeholder_span(),
                            });
                        }
                        Ok(BasicType::Numeric)
                    }
                    UnaryOp::Not => {
                        if expr_type != BasicType::Numeric {
                            return Err(SemanticError::TypeMismatch {
                                expected: BasicType::Numeric,
                                found: expr_type,
                                span: placeholder_span(),
                            });
                        }
                        Ok(BasicType::Numeric)
                    }
                }
            }
            ExprInner::Binary(left, op, right) => {
                let left_type = self.analyze_expression(left)?;
                let right_type = self.analyze_expression(right)?;

                match op {
                    // Arithmetic operations require numeric operands and return numeric
                    BinaryOp::Add => {
                        if left_type != right_type {
                            return Err(SemanticError::TypeMismatch {
                                expected: left_type,
                                found: right_type,
                                span: placeholder_span(),
                            });
                        }
                        Ok(left_type)
                    }
                    BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Exp => {
                        if left_type != BasicType::Numeric || right_type != BasicType::Numeric {
                            return Err(SemanticError::TypeMismatch {
                                expected: BasicType::Numeric,
                                found: if left_type != BasicType::Numeric {
                                    left_type
                                } else {
                                    right_type
                                },
                                span: placeholder_span(),
                            });
                        }
                        Ok(BasicType::Numeric)
                    }
                    // Comparison operations can work on matching types and return numeric (boolean)
                    BinaryOp::Eq
                    | BinaryOp::Neq
                    | BinaryOp::Lt
                    | BinaryOp::Gt
                    | BinaryOp::Leq
                    | BinaryOp::Geq => {
                        if left_type != right_type {
                            return Err(SemanticError::TypeMismatch {
                                expected: left_type,
                                found: right_type,
                                span: placeholder_span(),
                            });
                        }
                        Ok(BasicType::Numeric) // Comparisons return boolean (represented as numeric)
                    }
                    // Logical operations require numeric operands and return numeric
                    BinaryOp::And | BinaryOp::Or => {
                        if left_type != BasicType::Numeric || right_type != BasicType::Numeric {
                            return Err(SemanticError::TypeMismatch {
                                expected: BasicType::Numeric,
                                found: if left_type != BasicType::Numeric {
                                    left_type
                                } else {
                                    right_type
                                },
                                span: placeholder_span(),
                            });
                        }
                        Ok(BasicType::Numeric)
                    }
                }
            }
            ExprInner::FunctionCall(function) => self.analyze_function_call(function),
        }
    }

    /// Analyze a function call and return its return type
    fn analyze_function_call(&mut self, function: &Function) -> SemanticResult<BasicType> {
        match function {
            Function::Mid {
                string,
                start,
                length,
            } => {
                let string_type = self.analyze_expression(string)?;
                let start_type = self.analyze_expression(start)?;
                let length_type = self.analyze_expression(length)?;

                if string_type != BasicType::String {
                    return Err(SemanticError::InvalidFunctionArgument {
                        span: placeholder_span(),
                        function_name: "MID$".to_string(),
                        message: "first argument must be string".to_string(),
                    });
                }
                if start_type != BasicType::Numeric {
                    return Err(SemanticError::InvalidFunctionArgument {
                        span: placeholder_span(),
                        function_name: "MID$".to_string(),
                        message: "start position must be numeric".to_string(),
                    });
                }
                if length_type != BasicType::Numeric {
                    return Err(SemanticError::InvalidFunctionArgument {
                        span: placeholder_span(),
                        function_name: "MID$".to_string(),
                        message: "length must be numeric".to_string(),
                    });
                }
                Ok(BasicType::String)
            }
            Function::Left { string, length } => {
                let string_type = self.analyze_expression(string)?;
                let length_type = self.analyze_expression(length)?;

                if string_type != BasicType::String {
                    return Err(SemanticError::InvalidFunctionArgument {
                        span: placeholder_span(),
                        function_name: "LEFT$".to_string(),
                        message: "first argument must be string".to_string(),
                    });
                }
                if length_type != BasicType::Numeric {
                    return Err(SemanticError::InvalidFunctionArgument {
                        span: placeholder_span(),
                        function_name: "LEFT$".to_string(),
                        message: "length must be numeric".to_string(),
                    });
                }
                Ok(BasicType::String)
            }
            Function::Right { string, length } => {
                let string_type = self.analyze_expression(string)?;
                let length_type = self.analyze_expression(length)?;

                if string_type != BasicType::String {
                    return Err(SemanticError::InvalidFunctionArgument {
                        span: placeholder_span(),
                        function_name: "RIGHT$".to_string(),
                        message: "first argument must be string".to_string(),
                    });
                }
                if length_type != BasicType::Numeric {
                    return Err(SemanticError::InvalidFunctionArgument {
                        span: placeholder_span(),
                        function_name: "RIGHT$".to_string(),
                        message: "length must be numeric".to_string(),
                    });
                }
                Ok(BasicType::String)
            }
            Function::Asc { expr } => {
                let expr_type = self.analyze_expression(expr)?;
                if expr_type != BasicType::String {
                    return Err(SemanticError::InvalidFunctionArgument {
                        span: placeholder_span(),
                        function_name: "ASC".to_string(),
                        message: "argument must be string".to_string(),
                    });
                }
                Ok(BasicType::Numeric)
            }
            Function::Point { position } => {
                let pos_type = self.analyze_expression(position)?;
                if pos_type != BasicType::Numeric {
                    return Err(SemanticError::InvalidFunctionArgument {
                        span: placeholder_span(),
                        function_name: "POINT".to_string(),
                        message: "position must be numeric".to_string(),
                    });
                }
                Ok(BasicType::Numeric)
            }
            Function::Rnd { range_end } => {
                let range_type = self.analyze_expression(range_end)?;
                if range_type != BasicType::Numeric {
                    return Err(SemanticError::InvalidFunctionArgument {
                        span: placeholder_span(),
                        function_name: "RND".to_string(),
                        message: "range must be numeric".to_string(),
                    });
                }
                Ok(BasicType::Numeric)
            }
            Function::Int { expr }
            | Function::Sgn { expr }
            | Function::Abs { expr }
            | Function::Ln { expr }
            | Function::Log { expr }
            | Function::Dms { expr }
            | Function::Deg { expr }
            | Function::Tan { expr }
            | Function::Cos { expr }
            | Function::Sin { expr }
            | Function::Sqr { expr } => {
                let expr_type = self.analyze_expression(expr)?;
                if expr_type != BasicType::Numeric {
                    return Err(SemanticError::InvalidFunctionArgument {
                        span: placeholder_span(),
                        function_name: format!("{function:?}"),
                        message: "argument must be numeric".to_string(),
                    });
                }
                Ok(BasicType::Numeric)
            }
            Function::Status { arg } => {
                let arg_type = self.analyze_expression(arg)?;
                if arg_type != BasicType::Numeric {
                    return Err(SemanticError::InvalidFunctionArgument {
                        span: placeholder_span(),
                        function_name: "STATUS".to_string(),
                        message: "argument must be numeric".to_string(),
                    });
                }
                Ok(BasicType::Numeric)
            }
            Function::Val { expr } => {
                let expr_type = self.analyze_expression(expr)?;
                if expr_type != BasicType::String {
                    return Err(SemanticError::InvalidFunctionArgument {
                        span: placeholder_span(),
                        function_name: "VAL".to_string(),
                        message: "argument must be string".to_string(),
                    });
                }
                Ok(BasicType::Numeric)
            }
            Function::Str { expr } => {
                let expr_type = self.analyze_expression(expr)?;
                if expr_type != BasicType::Numeric {
                    return Err(SemanticError::InvalidFunctionArgument {
                        span: placeholder_span(),
                        function_name: "STR$".to_string(),
                        message: "argument must be numeric".to_string(),
                    });
                }
                Ok(BasicType::String)
            }
            Function::Chr { expr } => {
                let expr_type = self.analyze_expression(expr)?;
                if expr_type != BasicType::Numeric {
                    return Err(SemanticError::InvalidFunctionArgument {
                        span: placeholder_span(),
                        function_name: "CHR$".to_string(),
                        message: "argument must be numeric".to_string(),
                    });
                }
                Ok(BasicType::String)
            }
            Function::Len { expr } => {
                let expr_type = self.analyze_expression(expr)?;
                if expr_type != BasicType::String {
                    return Err(SemanticError::InvalidFunctionArgument {
                        span: placeholder_span(),
                        function_name: "LEN".to_string(),
                        message: "argument must be string".to_string(),
                    });
                }
                Ok(BasicType::Numeric)
            }
            Function::Peek { address, .. } => {
                let addr_type = self.analyze_expression(address)?;
                if addr_type != BasicType::Numeric {
                    return Err(SemanticError::InvalidFunctionArgument {
                        span: placeholder_span(),
                        function_name: "PEEK".to_string(),
                        message: "address must be numeric".to_string(),
                    });
                }
                Ok(BasicType::Numeric)
            }
        }
    }
}

/// Public function to analyze a program
pub fn analyze_program(program: &Program) -> SemanticResult<SymbolTable> {
    let mut analyzer = SemanticAnalyzer::new();
    analyzer.analyze_program(program)?;
    Ok(analyzer.state.symbol_table)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex::decimal_number::DecimalNumber;
    use crate::lex::identifier::Identifier;
    use crate::parse::expression::{Expr, expr_inner::ExprInner};

    #[test]
    fn test_numeric_assignment() {
        let mut analyzer = SemanticAnalyzer::new();

        // Create identifier "X" (numeric)
        let identifier = Identifier::new(b'X', None, false).unwrap();

        let assignment = Assignment {
            lvalue: LValue::Identifier(identifier),
            expr: Box::new(Expr::new(ExprInner::DecimalNumber(DecimalNumber::new(
                42.0,
            )))),
        };

        let result = analyzer.analyze_assignment(&assignment);
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_mismatch() {
        let mut analyzer = SemanticAnalyzer::new();

        // Create identifier "X$" (string)
        let identifier = Identifier::new(b'X', None, true).unwrap();

        let assignment = Assignment {
            lvalue: LValue::Identifier(identifier),
            expr: Box::new(Expr::new(ExprInner::DecimalNumber(DecimalNumber::new(
                42.0,
            )))),
        };

        let result = analyzer.analyze_assignment(&assignment);
        assert!(result.is_err());
        if let Err(SemanticError::TypeMismatch {
            expected, found, ..
        }) = result
        {
            assert_eq!(expected, BasicType::String);
            assert_eq!(found, BasicType::Numeric);
        }
    }
}
