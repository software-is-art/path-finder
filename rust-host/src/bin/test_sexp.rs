// Test s-expression parser
use pathfinder_rust_host::sexp_parser::{SExpParser, SExp};

fn main() {
    let parser = SExpParser::new();
    
    let test_cases = vec![
        "42",
        "hello",
        "(+ 1 2)",
        "(fn (x) (+ x 1))",
        "(let ((x 5) (y 10)) (+ x y))",
        "(data Nat Type (case zero Nat) (case succ (-> Nat Nat)))",
    ];
    
    for input in test_cases {
        println!("\nParsing: {}", input);
        match parser.parse(input) {
            Ok(sexps) => {
                for sexp in sexps {
                    println!("  Result: {:?}", sexp);
                    match sexp.to_hott_ast() {
                        Ok(ast) => println!("  HoTT AST: {:?}", ast),
                        Err(e) => println!("  HoTT conversion error: {}", e),
                    }
                }
            }
            Err(e) => println!("  Parse error: {}", e),
        }
    }
}