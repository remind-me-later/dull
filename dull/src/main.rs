mod lex;
mod parse;

use crate::lex::{Lexer, Token};
use clap::Parser;
use std::{fs, path::PathBuf, vec};

/// A BASIC lexer and parser
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Input BASIC file to lex
    #[arg(value_name = "FILE")]
    input: Option<PathBuf>,

    /// BASIC code to lex directly from command line
    #[arg(short, long)]
    code: Option<String>,

    /// Show detailed token information
    #[arg(short, long)]
    verbose: bool,

    /// Output format
    #[arg(short = 'f', long, value_enum, default_value_t = OutputFormat::Pretty)]
    output_format: OutputFormat,
}

#[derive(clap::ValueEnum, Clone, Debug)]
enum OutputFormat {
    Pretty,
}

fn main() {
    let args = Args::parse();

    // Get the input source
    let input = match (args.input, args.code) {
        (Some(file_path), None) => {
            // Read from file
            match fs::read_to_string(&file_path) {
                Ok(content) => content,
                Err(e) => {
                    eprintln!("Error reading file {}: {}", file_path.display(), e);
                    std::process::exit(1);
                }
            }
        }
        (None, Some(code)) => {
            // Use code from command line
            code
        }
        (Some(_), Some(_)) => {
            eprintln!("Error: Cannot specify both --input and --code options");
            std::process::exit(1);
        }
        (None, None) => {
            eprintln!("Error: Must specify either --input or --code option");
            std::process::exit(1);
        }
    };

    // Create lexer and tokenize
    let lexer = Lexer::new(&input);
    let mut tokens: Vec<Token> = vec![];

    for token in lexer {
        match token {
            Ok(token) => tokens.push(token),
            Err(_) => {
                eprintln!("Error tokenizing input");
                std::process::exit(1);
            }
        }
    }

    // Output based on format
    match args.output_format {
        OutputFormat::Pretty => {
            if args.verbose {
                println!("BASIC Lexer");
                println!("Input:");
                println!("{input}");
                println!("\nTokens ({}):", tokens.len());
            }

            for (i, token) in tokens.iter().enumerate() {
                if args.verbose {
                    println!("{:3}: {:?}", i + 1, token);
                } else {
                    print!("{token}");
                    if i < tokens.len() - 1 {
                        print!(" ");
                    }
                }
            }

            if !args.verbose {
                println!(); // Final newline
            }
        }
    }
}
