// ============================================================================
// TEST EVALUATOR DEPENDENCIES - Verify bootstrap can load evaluator chain
// ============================================================================

use pathfinder_rust_host::{
    sexp_parser::SExpParser,
    bootstrap_vm::BootstrapVM,
    effect_bridge::EffectBridge,
};
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Testing PathFinder Evaluator Dependencies");
    println!("========================================\n");
    
    // Initialize components
    let mut _vm = BootstrapVM::new();
    let mut _effect_bridge = EffectBridge::new(".".to_string());
    let parser = SExpParser::new();
    
    // Files in dependency order for the evaluator
    let dependency_chain = vec![
        // Foundation layers
        ("Core Foundations", "../src/core/foundations.sexp"),
        ("Type System", "../src/types/types.sexp"),
        
        // Value system
        ("Values", "../src/evaluator/values.sexp"),
        
        // Core components
        ("Eliminators", "../src/core/eliminators.sexp"),
        ("AST", "../src/core/ast.sexp"),
        ("Operations", "../src/core/operations.sexp"),
        
        // Additional dependencies
        ("String Types", "../src/types/string.sexp"),
        ("Literals", "../src/core/literals.sexp"),
        
        // The evaluator itself
        ("Evaluator", "../src/core/evaluator.sexp"),
    ];
    
    let mut loaded_count = 0;
    let mut error_count = 0;
    let mut total_forms = 0;
    
    let total_files = dependency_chain.len();
    
    for (name, path) in dependency_chain {
        println!("Loading {}: {}", name, path);
        
        match fs::read_to_string(path) {
            Ok(content) => {
                match parser.parse(&content) {
                    Ok(sexps) => {
                        println!("  ✓ Parsed {} forms", sexps.len());
                        total_forms += sexps.len();
                        
                        // Count different form types
                        let mut imports = 0;
                        let mut data_types = 0;
                        let mut type_decls = 0;
                        let mut definitions = 0;
                        
                        for sexp in &sexps {
                            if let pathfinder_rust_host::sexp_parser::SExp::List(elements) = sexp {
                                if let Some(pathfinder_rust_host::sexp_parser::SExp::Atom(head)) = elements.first() {
                                    match head.as_str() {
                                        "import" => imports += 1,
                                        "data" => data_types += 1,
                                        "type" => type_decls += 1,
                                        "define" => definitions += 1,
                                        _ => {}
                                    }
                                }
                            }
                        }
                        
                        if imports > 0 { println!("    - {} imports", imports); }
                        if data_types > 0 { println!("    - {} data types", data_types); }
                        if type_decls > 0 { println!("    - {} type declarations", type_decls); }
                        if definitions > 0 { println!("    - {} definitions", definitions); }
                        
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
    println!("  Files loaded: {}/{}", loaded_count, total_files);
    println!("  Total forms: {}", total_forms);
    println!("  Errors: {}", error_count);
    
    if loaded_count == total_files {
        println!("\n✓ Bootstrap can successfully parse the entire evaluator dependency chain!");
    } else {
        println!("\n✗ Bootstrap cannot load all evaluator dependencies");
    }
    
    Ok(())
}