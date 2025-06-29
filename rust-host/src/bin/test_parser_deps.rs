// ============================================================================
// TEST PARSER DEPENDENCIES - Verify bootstrap can load parser chain
// ============================================================================

use pathfinder_rust_host::{
    sexp_parser::SExpParser,
    bootstrap_vm::BootstrapVM,
    effect_bridge::EffectBridge,
    initial_env::InitialEnvironment,
};
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Testing PathFinder Parser Dependencies");
    println!("=====================================\n");
    
    // Initialize components
    let mut vm = BootstrapVM::new();
    let mut effect_bridge = EffectBridge::new(".".to_string());
    let parser = SExpParser::new();
    
    // Files in dependency order
    let dependency_chain = vec![
        ("Foundation Layer 1", "../src/core/foundations.sexp"),
        ("Foundation Layer 2", "../src/types/types.sexp"),
        ("Value System", "../src/evaluator/values.sexp"),
        ("Core Eliminators", "../src/core/eliminators.sexp"),
        ("Core Operations", "../src/core/operations.sexp"),
        ("String Types", "../src/types/string.sexp"),
        ("Effects System", "../src/effects/effects.sexp"),
        ("Lexer", "../src/lexer/lexer.sexp"),
        ("Parser", "../src/parser/parser.sexp"),
    ];
    
    let mut loaded_count = 0;
    let mut error_count = 0;
    
    for (name, path) in dependency_chain {
        println!("Loading {}: {}", name, path);
        
        match fs::read_to_string(path) {
            Ok(content) => {
                match parser.parse(&content) {
                    Ok(sexps) => {
                        println!("  ✓ Parsed {} forms", sexps.len());
                        
                        // Try to evaluate just the imports and data declarations
                        let mut form_count = 0;
                        for sexp in &sexps {
                            if let pathfinder_rust_host::sexp_parser::SExp::List(elements) = sexp {
                                if let Some(pathfinder_rust_host::sexp_parser::SExp::Atom(head)) = elements.first() {
                                    match head.as_str() {
                                        "import" | "data" | "type" => {
                                            form_count += 1;
                                        }
                                        "define" => {
                                            // Skip function definitions for now
                                            if elements.len() >= 3 {
                                                if let pathfinder_rust_host::sexp_parser::SExp::List(_) = &elements[2] {
                                                    // This is likely a function definition
                                                    continue;
                                                }
                                            }
                                            form_count += 1;
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                        
                        println!("  ✓ Found {} declarations/imports", form_count);
                        loaded_count += 1;
                    }
                    Err(e) => {
                        println!("  ✗ Parse error: {}", e);
                        error_count += 1;
                    }
                }
            }
            Err(e) => {
                println!("  ✗ File error: {}", e);
                error_count += 1;
            }
        }
        
        println!();
    }
    
    println!("Summary:");
    println!("  Successfully loaded: {}/9 files", loaded_count);
    println!("  Errors: {}", error_count);
    
    if loaded_count == 9 {
        println!("\n✓ Bootstrap can successfully parse the entire parser dependency chain!");
    } else {
        println!("\n✗ Bootstrap cannot load all parser dependencies");
    }
    
    Ok(())
}