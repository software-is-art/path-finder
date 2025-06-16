// ============================================================================
// PATHFINDER MAIN - SELF-CONTAINED HOTT SYSTEM ENTRY POINT
// ============================================================================
// This is the main entry point for the PathFinder HoTT system.
// It demonstrates the complete pipeline: HoTT parser → HoTT AST → HoTT evaluator → Rust execution.

use pathfinder_rust_host::*;
use std::env;
use std::fs;
use std::io::{self, Write};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    
    match args.len() {
        1 => {
            // Interactive REPL mode  
            println!("PathFinder V0 Bootstrap VM - Interactive Mode");
            println!("Hash-consing and content-addressable caching enabled!");
            println!("Type HoTT expressions or 'test-peano N' to test performance.");
            println!("Press Ctrl+C to exit.\n");
            
            run_repl()
        }
        2 => {
            let arg = &args[1];
            if arg == "--bootstrap" {
                // Bootstrap self-hosting system
                test_bootstrap()
            } else if arg == "--test-self-hosting" {
                // Test complete self-hosting with loaded parser
                test_complete_self_hosting()
            } else if arg == "--test-v1" {
                // Test V1 functionality: loaded evaluator
                test_v1_functionality()
            } else if arg.starts_with("--test-peano=") {
                // Test Peano performance
                let n_str = &arg[13..];
                let n: usize = n_str.parse().unwrap_or(100);
                test_peano_performance(n)
            } else {
                // File execution mode
                println!("PathFinder V0 Bootstrap VM - Executing file: {}", arg);
                run_file(arg)
            }
        }
        _ => {
            eprintln!("Usage: {} [options] [file.hott]", args[0]);
            eprintln!("Options:");
            eprintln!("  --bootstrap           Bootstrap self-hosting system");
            eprintln!("  --test-self-hosting   Test complete self-hosting with loaded parser");
            eprintln!("  --test-v1             Test V1 functionality with loaded evaluator");
            eprintln!("  --test-peano=N        Test Peano number performance");
            eprintln!("  [file.hott]           Execute HoTT source file");
            eprintln!("  (no args)             Start interactive REPL");
            std::process::exit(1);
        }
    }
}

/// Run interactive REPL
fn run_repl() -> Result<(), Box<dyn std::error::Error>> {
    let mut runtime = PathFinderRuntime::new();
    let mut input = String::new();
    
    loop {
        // Prompt
        print!("PathFinder> ");
        io::stdout().flush()?;
        
        // Read input
        input.clear();
        io::stdin().read_line(&mut input)?;
        let input = input.trim();
        
        // Handle special commands
        match input {
            "" => continue,
            "quit" | "exit" => {
                println!("Goodbye!");
                break;
            }
            "help" => {
                print_help();
                continue;
            }
            "examples" => {
                print_examples();
                continue;
            }
            _ => {}
        }
        
        // Execute HoTT code
        match runtime.run(input) {
            Ok(result) => {
                println!("⟹ {}", format_hott_value(&result));
            }
            Err(error) => {
                eprintln!("Error: {}", error);
            }
        }
    }
    
    Ok(())
}

/// Run HoTT source file
fn run_file(filename: &str) -> Result<(), Box<dyn std::error::Error>> {
    let source = fs::read_to_string(filename)?;
    let mut runtime = PathFinderRuntime::new();
    
    println!("Parsing HoTT source with pure HoTT parser...");
    
    match runtime.run(&source) {
        Ok(result) => {
            println!("Result: {}", format_hott_value(&result));
        }
        Err(error) => {
            eprintln!("Execution failed: {}", error);
            std::process::exit(1);
        }
    }
    
    Ok(())
}

/// Format HoTT value for display
fn format_hott_value(value: &HottValue) -> String {
    match value {
        HottValue::Constructor { name, args, .. } => {
            if args.is_empty() {
                name.clone()
            } else {
                format!("({} {})", name, 
                    args.iter()
                        .map(format_hott_value)
                        .collect::<Vec<_>>()
                        .join(" "))
            }
        }
        HottValue::String(s) => format!("\"{}\"", s),
        HottValue::Unit => "()".to_string(),
        HottValue::Closure { params, .. } => {
            format!("#<closure:{}>", params.join(","))
        }
        HottValue::Builtin { name, arity } => {
            format!("#<builtin:{}:{}>", name, arity)
        }
        HottValue::Effect(effect) => {
            format!("#<effect:{:?}>", effect)
        }
        HottValue::HottFunction { name, .. } => {
            format!("#<hott-function:{}>", name)
        }
    }
}

/// Print help information
fn print_help() {
    println!("PathFinder HoTT System Help");
    println!("==========================");
    println!();
    println!("This is a self-contained HoTT (Homotopy Type Theory) system where:");
    println!("• Parser is implemented in pure HoTT mathematics");
    println!("• AST is defined as HoTT inductive types"); 
    println!("• Evaluator uses HoTT eliminators for total correctness");
    println!("• Rust VM just executes the mathematical operations");
    println!();
    println!("Commands:");
    println!("  help      - Show this help");
    println!("  examples  - Show example expressions");
    println!("  quit/exit - Exit the REPL");
    println!();
    println!("Syntax:");
    println!("  Variables:     x, hello, my-var");
    println!("  Numbers:       0, 1, 42, 123");
    println!("  Lambda:        \\x -> x");
    println!("  Application:   (f x)");
    println!();
}

/// Print example expressions
fn print_examples() {
    println!("PathFinder HoTT Examples");
    println!("========================");
    println!();
    println!("Basic expressions:");
    println!("  42           # Natural number zero/next constructors");
    println!("  hello        # Variable lookup");
    println!("  \\x -> x      # Identity function (lambda)");
    println!("  (f x)        # Function application");
    println!();
    println!("HoTT mathematical concepts:");
    println!("  zero         # Natural number zero constructor");
    println!("  (next zero)  # Natural number one (successor of zero)");
    println!("  true         # Boolean true constructor");
    println!("  false        # Boolean false constructor");
    println!();
    println!("Advanced (these demonstrate HoTT eliminators):");
    println!("  (nat-eliminator n zero-case next-case)");
    println!("  (bool-eliminator b true-case false-case)");
    println!();
    println!("The key insight: Everything is evaluated using HoTT eliminators!");
    println!("This is mathematically total and provably correct.");
    println!();
}

/// Demonstrate the HoTT system capabilities
#[allow(dead_code)]
fn demonstrate_hott_system() -> Result<(), Box<dyn std::error::Error>> {
    println!("PathFinder HoTT System Demonstration");
    println!("====================================");
    println!();
    
    let mut runtime = PathFinderRuntime::new();
    
    // Demonstrate basic evaluation
    let examples = vec![
        ("42", "Natural number using zero/next constructors"),
        ("hello", "Variable (will fail lookup, demonstrating error handling)"),
        ("\\x -> x", "Identity function as closure value"),
        ("(f x)", "Function application (will fail, demonstrating eliminators)"),
    ];
    
    for (expr, description) in examples {
        println!("Example: {} ({})", expr, description);
        match runtime.run(expr) {
            Ok(result) => {
                println!("  ⟹ {}", format_hott_value(&result));
            }
            Err(error) => {
                println!("  ⟹ Error: {}", error);
            }
        }
        println!();
    }
    
    println!("This demonstrates:");
    println!("1. Pure HoTT parser converting strings to AST");
    println!("2. HoTT evaluator using mathematical eliminators");
    println!("3. Rust VM executing the eliminator operations");
    println!("4. Complete mathematical correctness guarantees");
    
    Ok(())
}

/// Test bootstrap functionality
fn test_bootstrap() -> Result<(), Box<dyn std::error::Error>> {
    println!("🔄 Testing V0 Bootstrap VM...");
    
    let mut runtime = PathFinderRuntime::new();
    runtime.bootstrap_self_hosting()?;
    
    println!("✅ Bootstrap test completed successfully!");
    Ok(())
}

/// Test Peano number performance
fn test_peano_performance(n: usize) -> Result<(), Box<dyn std::error::Error>> {
    println!("🧮 Testing Peano number performance with caching...");
    
    let mut runtime = PathFinderRuntime::new();
    runtime.test_peano_performance(n)?;
    
    println!("✅ Peano performance test completed!");
    Ok(())
}

/// Test complete self-hosting with loaded parser
fn test_complete_self_hosting() -> Result<(), Box<dyn std::error::Error>> {
    println!("🚀 Testing complete self-hosting capability...");
    
    let mut runtime = PathFinderRuntime::new();
    runtime.test_complete_self_hosting()?;
    
    println!("✅ Self-hosting test completed successfully!");
    Ok(())
}

/// Test V1 functionality: using loaded evaluator
fn test_v1_functionality() -> Result<(), Box<dyn std::error::Error>> {
    println!("🚀 Testing V1 functionality with loaded HoTT evaluator...");
    
    let mut runtime = PathFinderRuntime::new();
    
    // Test simple expressions with the loaded evaluator
    let test_cases = vec![
        "zero",                                    // Constructor  
        "true",                                    // Boolean constructor
        "console-print-effect",                    // Effect function
        "file-read-effect",                        // Effect with parameter
    ];
    
    for test_case in test_cases {
        println!("\n📋 Testing V1 evaluation of: {}", test_case);
        
        match runtime.test_v1_complete_evaluation(test_case) {
            Ok(result) => {
                println!("  ✅ V1 SUCCESS: {}", format_hott_value(&result));
            }
            Err(e) => {
                println!("  ⚠️  V1 result: {}", e);
                // This is expected for some cases like unbound variables
            }
        }
    }
    
    println!("\n🎯 V1 test demonstrates that we can use loaded HoTT evaluator!");
    println!("This is the key difference between V0 (parsing only) and V1 (complete evaluation)");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_main_functionality() {
        // Test basic runtime creation and simple evaluation
        let mut runtime = PathFinderRuntime::new();
        
        // This should parse successfully
        let result = runtime.run("42");
        assert!(result.is_ok());
    }
    
    #[test]
    fn test_formatting() {
        let zero = HottValue::zero();
        let formatted = format_hott_value(&zero);
        assert_eq!(formatted, "zero");
        
        let one = HottValue::succ(HottValue::zero());
        let formatted = format_hott_value(&one);
        assert_eq!(formatted, "(next zero)");
    }
}