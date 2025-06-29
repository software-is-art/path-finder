// ============================================================================
// SIMPLE BOOTSTRAP TEST - MINIMAL WORKING VERSION
// ============================================================================

use pathfinder_rust_host::{
    sexp_parser::{SExpParser, SExp},
    hott_values::{HottValue, HottType},
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("PathFinder Bootstrap Test v0.1.0");
    
    // Test S-expression parser
    let parser = SExpParser::new();
    let test_input = "(define x (succ zero))";
    
    match parser.parse(test_input) {
        Ok(sexps) => {
            println!("Parsed {} expressions:", sexps.len());
            for sexp in &sexps {
                println!("  {:?}", sexp);
            }
            
            // Convert to HottValue
            for sexp in &sexps {
                match sexp_to_hott_value(sexp) {
                    Ok(value) => println!("  Converted to: {:?}", value),
                    Err(e) => println!("  Conversion error: {}", e),
                }
            }
        }
        Err(e) => {
            eprintln!("Parse error: {}", e);
        }
    }
    
    Ok(())
}

/// Simple S-expression to HottValue conversion
fn sexp_to_hott_value(sexp: &SExp) -> Result<HottValue, String> {
    match sexp {
        SExp::Atom(s) => {
            // Handle known constructors
            match s.as_str() {
                "zero" => Ok(HottValue::Constructor {
                    name: "zero".to_string(),
                    args: vec![],
                    value_type: HottType::Universe(0),
                }),
                "unit" => Ok(HottValue::Unit),
                _ => Ok(HottValue::String(s.clone())),
            }
        }
        SExp::String(s) => Ok(HottValue::String(s.clone())),
        SExp::Number(n) => {
            // Convert number to Peano
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
        SExp::List(elements) => {
            if elements.is_empty() {
                return Ok(HottValue::Constructor {
                    name: "nil".to_string(),
                    args: vec![],
                    value_type: HottType::Universe(0),
                });
            }
            
            // Simple application handling
            if let SExp::Atom(head) = &elements[0] {
                match head.as_str() {
                    "succ" if elements.len() == 2 => {
                        let arg = sexp_to_hott_value(&elements[1])?;
                        Ok(HottValue::Constructor {
                            name: "succ".to_string(),
                            args: vec![arg],
                            value_type: HottType::Universe(0),
                        })
                    }
                    "define" if elements.len() == 3 => {
                        // Just return a placeholder for now
                        Ok(HottValue::String(format!("define {:?} = {:?}", elements[1], elements[2])))
                    }
                    _ => Ok(HottValue::String(format!("list: {:?}", elements))),
                }
            } else {
                Ok(HottValue::String(format!("list: {:?}", elements)))
            }
        }
    }
}