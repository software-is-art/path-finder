// Test program for mathematical HoTT tokenizer

use pathfinder_rust_host::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let parser = HottParser::new();
    
    // Test mathematical symbols
    let test_cases = vec![
        "𝒰₀",
        "ℕ",
        "λ(x : ℕ), x",
        "Π(n : ℕ), ℕ → ℕ",
        "data ℕ : 𝒰₀ where",
        "zero : ℕ",
        "succ : ℕ → ℕ",
        "-- This is a comment",
        "Id_ℕ(zero, ₀)",
        "⟨two, three⟩",
    ];
    
    for test in test_cases {
        println!("Testing: {}", test);
        match parser.tokenize(test) {
            Ok(tokens) => {
                println!("  Tokens: {:?}", tokens);
            }
            Err(err) => {
                println!("  Error: {}", err);
            }
        }
        println!();
    }
    
    Ok(())
}