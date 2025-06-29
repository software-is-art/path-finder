// ============================================================================
// BOOTSTRAP TEST - SIMPLIFIED VERSION FOR TESTING
// ============================================================================

use pathfinder_rust_host::{
    sexp_parser::{SExpParser, SExp},
    effect_bridge::EffectBridge,
    initial_env::InitialEnvironment,
    hott_values::{HottValue, HottType},
};
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("PathFinder Bootstrap Test v0.1.0");
    
    // Initialize components
    let effect_bridge = EffectBridge::new(".".to_string());
    let sexp_parser = SExpParser::new();
    let initial_env = InitialEnvironment::new();
    
    // Test parsing some of our converted files
    let test_files = vec![
        "../src/core/foundations.sexp",
        "../src/types/types.sexp",
        "../src/types/list.sexp",
    ];
    
    for file_path in test_files {
        println!("\n=== Testing {} ===", file_path);
        
        match fs::read_to_string(file_path) {
            Ok(content) => {
                // Parse just the first few forms
                let mut forms_to_parse = String::new();
                let mut paren_depth = 0;
                let mut form_count = 0;
                
                for line in content.lines() {
                    if paren_depth == 0 && (line.trim().is_empty() || line.trim().starts_with(";;")) {
                        continue;
                    }
                    
                    forms_to_parse.push_str(line);
                    forms_to_parse.push('\n');
                    
                    for ch in line.chars() {
                        match ch {
                            '(' => paren_depth += 1,
                            ')' => paren_depth -= 1,
                            _ => {}
                        }
                    }
                    
                    if paren_depth == 0 && !line.trim().is_empty() {
                        form_count += 1;
                        if form_count >= 3 {
                            break;
                        }
                    }
                }
                
                match sexp_parser.parse(&forms_to_parse) {
                    Ok(sexps) => {
                        println!("✓ Parsed {} forms", sexps.len());
                        
                        // Try to convert to HottValue
                        for (i, sexp) in sexps.iter().enumerate() {
                            println!("\nForm {}: {:?}", i + 1, format_sexp(sexp));
                            
                            match simple_sexp_to_hott(sexp) {
                                Ok(value) => {
                                    println!("  → HottValue created");
                                    test_value_operations(&value);
                                }
                                Err(e) => println!("  → Conversion error: {}", e),
                            }
                        }
                    }
                    Err(e) => println!("✗ Parse error: {}", e),
                }
            }
            Err(e) => println!("✗ Cannot read file: {}", e),
        }
    }
    
    // Test initial environment
    println!("\n=== Testing Initial Environment ===");
    let env = initial_env.to_pathfinder_env();
    match env {
        HottValue::Constructor { name, args, .. } => {
            println!("Environment type: {}", name);
            println!("Number of bindings: {}", count_env_bindings(&env));
        }
        _ => println!("Unexpected environment format"),
    }
    
    Ok(())
}

/// Format S-expression for display
fn format_sexp(sexp: &SExp) -> String {
    match sexp {
        SExp::Atom(s) => s.clone(),
        SExp::String(s) => format!("\"{}\"", s),
        SExp::Number(n) => n.to_string(),
        SExp::List(items) => {
            if items.is_empty() {
                "()".to_string()
            } else if items.len() <= 4 {
                format!("({})", items.iter()
                    .map(|s| format_sexp(s))
                    .collect::<Vec<_>>()
                    .join(" "))
            } else {
                format!("({} ... {} items)", 
                    format_sexp(&items[0]), 
                    items.len())
            }
        }
    }
}

/// Simple S-expression to HottValue conversion
fn simple_sexp_to_hott(sexp: &SExp) -> Result<HottValue, String> {
    match sexp {
        SExp::Atom(s) => {
            match s.as_str() {
                "zero" => Ok(HottValue::Constructor {
                    name: "zero".to_string(),
                    args: vec![],
                    value_type: HottType::Universe(0),
                }),
                "nil" => Ok(HottValue::Constructor {
                    name: "nil".to_string(),
                    args: vec![],
                    value_type: HottType::Universe(0),
                }),
                _ => Ok(HottValue::String(s.clone())),
            }
        }
        SExp::String(s) => Ok(HottValue::String(s.clone())),
        SExp::Number(n) => {
            // Convert to Peano
            let mut result = HottValue::Constructor {
                name: "zero".to_string(),
                args: vec![],
                value_type: HottType::Universe(0),
            };
            for _ in 0..*n {
                result = HottValue::Constructor {
                    name: "succ".to_string(),
                    args: vec![result],
                    value_type: HottType::Universe(0),
                };
            }
            Ok(result)
        }
        SExp::List(items) => {
            if items.is_empty() {
                return Ok(HottValue::Constructor {
                    name: "nil".to_string(),
                    args: vec![],
                    value_type: HottType::Universe(0),
                });
            }
            
            // Handle special forms
            if let SExp::Atom(head) = &items[0] {
                match head.as_str() {
                    "define" => Ok(HottValue::String(format!("define-form"))),
                    "type" => Ok(HottValue::String(format!("type-form"))),
                    "data" => Ok(HottValue::String(format!("data-form"))),
                    "import" => Ok(HottValue::String(format!("import-form"))),
                    _ => Ok(HottValue::String(format!("app-form: {}", head))),
                }
            } else {
                Ok(HottValue::String("list-form".to_string()))
            }
        }
    }
}

/// Test basic value operations
fn test_value_operations(value: &HottValue) {
    match value {
        HottValue::Constructor { name, args, .. } => {
            println!("  Constructor: {} with {} args", name, args.len());
        }
        HottValue::String(s) => {
            println!("  String: {}", s);
        }
        _ => {
            println!("  Other value type");
        }
    }
}

/// Count environment bindings
fn count_env_bindings(env: &HottValue) -> usize {
    match env {
        HottValue::Constructor { name, args, .. } => {
            match name.as_str() {
                "empty-env" => 0,
                "extend-env" => {
                    if args.len() >= 3 {
                        1 + count_env_bindings(&args[2])
                    } else {
                        0
                    }
                }
                _ => 0,
            }
        }
        _ => 0,
    }
}