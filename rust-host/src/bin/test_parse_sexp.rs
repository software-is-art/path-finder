// Test parsing our converted s-expression files
use pathfinder_rust_host::sexp_parser::SExpParser;
use std::fs;

fn main() {
    let parser = SExpParser::new();
    
    // Test files to parse
    let test_files = vec![
        // Layer 0 - No dependencies
        "../src/core/foundations.sexp",
        "../src/types/types.sexp",
        // Layer 1 - Basic dependencies
        "../src/core/eliminators.sexp",
        "../src/core/ast.sexp",
        "../src/evaluator/values.sexp",
        // Layer 2 - Core functionality
        "../src/core/operations.sexp",
        "../src/effects/effects.sexp",
        "../src/types/families.sexp",
        "../src/core/cache.sexp",
        "../src/core/evaluator.sexp",
        "../src/core/literals.sexp",
        // Layer 3 - Parsing chain
        "../src/types/string.sexp",
        "../src/lexer/lexer.sexp",
        "../src/parser/parser.sexp",
    ];
    
    for file_path in test_files {
        println!("\n=== Parsing {} ===", file_path);
        
        match fs::read_to_string(file_path) {
            Ok(content) => {
                // Parse complete top-level forms
                let mut complete_forms = Vec::new();
                let mut current_form = String::new();
                let mut paren_depth = 0;
                
                for line in content.lines() {
                    // Skip empty lines and comments between forms
                    if paren_depth == 0 && (line.trim().is_empty() || line.trim().starts_with(";;")) {
                        continue;
                    }
                    
                    current_form.push_str(line);
                    current_form.push('\n');
                    
                    // Count parentheses
                    for ch in line.chars() {
                        match ch {
                            '(' => paren_depth += 1,
                            ')' => paren_depth -= 1,
                            _ => {}
                        }
                    }
                    
                    // Complete form found
                    if paren_depth == 0 && !current_form.trim().is_empty() {
                        complete_forms.push(current_form.clone());
                        current_form.clear();
                        if complete_forms.len() >= 5 {
                            break; // Just test first 5 forms
                        }
                    }
                }
                
                let sample = complete_forms.join("\n");
                
                match parser.parse(&sample) {
                    Ok(sexps) => {
                        println!("✓ Successfully parsed {} s-expressions", sexps.len());
                        for (i, sexp) in sexps.iter().take(3).enumerate() {
                            println!("  [{}] {:?}", i, format_sexp(sexp));
                        }
                        if sexps.len() > 3 {
                            println!("  ... and {} more", sexps.len() - 3);
                        }
                    }
                    Err(e) => println!("✗ Parse error: {}", e),
                }
            }
            Err(e) => println!("✗ File error: {}", e),
        }
    }
}

// Helper to format s-expressions nicely
fn format_sexp(sexp: &pathfinder_rust_host::sexp_parser::SExp) -> String {
    use pathfinder_rust_host::sexp_parser::SExp;
    match sexp {
        SExp::Atom(s) => s.clone(),
        SExp::String(s) => format!("\"{}\"", s),
        SExp::Number(n) => n.to_string(),
        SExp::List(items) => {
            if items.is_empty() {
                "()".to_string()
            } else if items.len() <= 3 {
                format!("({})", items.iter()
                    .map(|s| format_sexp(s))
                    .collect::<Vec<_>>()
                    .join(" "))
            } else {
                format!("({} ...)", items.iter()
                    .take(2)
                    .map(|s| format_sexp(s))
                    .collect::<Vec<_>>()
                    .join(" "))
            }
        }
    }
}