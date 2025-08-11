pub mod error;
mod header;
mod lex;
mod parse;
mod semantic_analysis;

use crate::error::{CompileError, print_error};
use crate::header::Header;
use crate::lex::{Lexer, RemarkLexOption, SpannedToken};
use crate::parse::Parser;
use crate::semantic_analysis::analyze_program;
use clap::Parser as ClapParser;
use std::{fs, path::PathBuf};

#[derive(clap::ValueEnum, Clone, Debug)]
enum RemarkMode {
    TrimWhitespace,
    KeepWhole,
}

impl From<RemarkMode> for RemarkLexOption {
    fn from(mode: RemarkMode) -> Self {
        match mode {
            RemarkMode::TrimWhitespace => RemarkLexOption::TrimWhitespace,
            RemarkMode::KeepWhole => RemarkLexOption::KeepWhole,
        }
    }
}

/// A BASIC lexer and parser
#[derive(ClapParser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Input BASIC file to process
    #[arg(value_name = "FILE")]
    input: Option<PathBuf>,

    /// Just tokenize the input (output tokens only)
    #[arg(short, long)]
    lex: bool,

    /// Parse the input into an AST instead of compiling
    #[arg(short, long)]
    parse: bool,

    /// Run semantic analysis on the parsed AST (implies --parse)
    #[arg(short, long)]
    analyze: bool,

    /// Compile without header (only program bytes)
    #[arg(long)]
    no_header: bool,

    /// Output file for compiled bytes (defaults to a.bin)
    #[arg(short, long, value_name = "FILE")]
    output: Option<PathBuf>,

    /// Preserve source parentheses in output
    #[arg(short = 'w', long)]
    preserve_source_wording: bool,

    /// Remark handling mode: trim-whitespace or keep-whole
    #[arg(long, value_enum, default_value_t = RemarkMode::TrimWhitespace)]
    remark_mode: RemarkMode,
}

fn main() {
    let args = Args::parse();

    // Get the input source
    let (input, filename, program_name) = match args.input {
        Some(file_path) => {
            // Read from file
            match fs::read_to_string(&file_path) {
                Ok(content) => {
                    let prog_name = file_path
                        .file_stem()
                        .and_then(|s| s.to_str())
                        .unwrap_or("PROGRAM")
                        .to_string();
                    (content, file_path.display().to_string(), prog_name)
                }
                Err(e) => {
                    eprintln!("Error reading file {}: {}", file_path.display(), e);
                    std::process::exit(1);
                }
            }
        }
        None => {
            eprintln!("Error: Must specify input file");
            std::process::exit(1);
        }
    };

    // Create lexer and tokenize
    let lexer = Lexer::new(&input, args.remark_mode.into());
    let mut tokens: Vec<SpannedToken> = Vec::new();

    for token_result in lexer {
        match token_result {
            Ok(spanned_token) => tokens.push(spanned_token),
            Err(lex_error) => {
                let compile_error = CompileError::from(lex_error);
                print_error(&compile_error, &filename, &input);
                std::process::exit(1);
            }
        }
    }

    if args.analyze {
        // Parse the tokens into an AST and run semantic analysis
        let mut parser = Parser::new(tokens.into_iter());

        let (program, parse_errors) = parser.parse_with_error_recovery();

        // Report any parse errors but continue if we got some lines
        for parse_error in &parse_errors {
            let compile_error = CompileError::from(parse_error.clone());
            print_error(&compile_error, &filename, &input);
        }

        if !parse_errors.is_empty() {
            eprintln!(
                "\nWarning: {} parse error(s) occurred. Continuing with {} successfully parsed line(s).",
                parse_errors.len(),
                program.num_lines()
            );
            std::process::exit(1);
        }

        match analyze_program(&program) {
            Ok(symbol_table) => {
                println!("Semantic analysis completed successfully!");
                println!("Symbol table: {symbol_table:#?}");
            }
            Err(semantic_error) => {
                let compile_error = CompileError::from(semantic_error);
                print_error(&compile_error, &filename, &input);
                std::process::exit(1);
            }
        }
    } else if args.parse {
        // Parse the tokens into an AST
        let mut parser = Parser::new(tokens.into_iter());

        let (program, parse_errors) = parser.parse_with_error_recovery();

        // Report any parse errors but continue if we got some lines
        for parse_error in &parse_errors {
            let compile_error = CompileError::from(parse_error.clone());
            print_error(&compile_error, &filename, &input);
        }

        if !parse_errors.is_empty() {
            eprintln!(
                "\nWarning: {} parse error(s) occurred. Continuing with {} successfully parsed line(s).",
                parse_errors.len(),
                program.num_lines()
            );
            std::process::exit(1);
        }

        print!("{program}");
    } else if args.lex {
        // Just output the tokens
        for (i, token) in tokens.iter().enumerate() {
            print!("{token} ");
            if i < tokens.len() - 1 {
                print!(" ");
            }
        }
        println!();
    } else {
        // Default: compile the tokens into an AST and compile to bytes
        let mut parser = Parser::new(tokens.into_iter());

        let (program, parse_errors) = parser.parse_with_error_recovery();

        // Report any parse errors but continue if we got some lines
        for parse_error in &parse_errors {
            let compile_error = CompileError::from(parse_error.clone());
            print_error(&compile_error, &filename, &input);
        }

        if !parse_errors.is_empty() {
            eprintln!(
                "\nWarning: {} parse error(s) occurred. Continuing with {} successfully parsed line(s).",
                parse_errors.len(),
                program.num_lines()
            );
            std::process::exit(1);
        }

        // Generate program bytes
        let mut program_bytes = Vec::new();
        program.write_bytes(&mut program_bytes, args.preserve_source_wording);

        // Create output bytes - with or without header
        let output_bytes = if args.no_header {
            // Just the program bytes without header
            program_bytes
        } else {
            // Create header with program length and prepend it
            let header = Header::new(&program_name, program_bytes.len() as u16);
            let mut output_bytes = header.to_bytes();
            output_bytes.extend(program_bytes);
            output_bytes
        };

        // Write to output file or default a.bin
        let output_path = args.output.unwrap_or_else(|| PathBuf::from("a.bin"));
        if let Err(e) = fs::write(&output_path, &output_bytes) {
            eprintln!("Error writing output file {}: {}", output_path.display(), e);
            std::process::exit(1);
        }
        let header_info = if args.no_header {
            " (no header)"
        } else {
            " (with header)"
        };
        println!(
            "Compiled program written to {}{}",
            output_path.display(),
            header_info
        );
    }
}
