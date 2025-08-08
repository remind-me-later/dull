mod error;
mod header;
mod lex;
mod parse;
mod semantic_analysis;

use crate::error::{print_error, CompileError};
use crate::lex::{Lexer, Token};
use crate::parse::Parser;
use crate::semantic_analysis::analyze_program;
use clap::Parser as ClapParser;
use std::{fs, path::PathBuf, vec};

/// A BASIC lexer and parser
#[derive(ClapParser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Input BASIC file to process
    #[arg(value_name = "FILE")]
    input: Option<PathBuf>,

    /// Parse the input into an AST instead of just lexing
    #[arg(long)]
    parse: bool,

    /// Run semantic analysis on the parsed AST (implies --parse)
    #[arg(long)]
    analyze: bool,
}

#[derive(clap::ValueEnum, Clone, Debug)]
enum OutputFormat {
    Pretty,
}

fn main() {
    let args = Args::parse();

    // Get the input source
    let (input, filename) = match args.input {
        Some(file_path) => {
            // Read from file
            match fs::read_to_string(&file_path) {
                Ok(content) => (content, file_path.display().to_string()),
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
    let lexer = Lexer::new(&input);
    let mut tokens: Vec<Token> = vec![];

    for token_result in lexer {
        match token_result {
            Ok(token) => tokens.push(token),
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
        let program = parser.parse();

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
        let program = parser.parse();
        print!("{program}");
    } else {
        // Just output the tokens
        for (i, token) in tokens.iter().enumerate() {
            print!("{token:?}");
            if i < tokens.len() - 1 {
                print!(" ");
            }
        }
        println!();
    }
}
