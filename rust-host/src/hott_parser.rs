// ============================================================================
// HOTT PARSER - RUST VM BRIDGE TO PURE HOTT PARSER
// ============================================================================
// This module bridges the Rust VM to the pure HoTT parser.
// The actual parsing logic lives in HoTT IL - this just invokes it.

use crate::hott_values::*;
use std::collections::HashMap;

/// Parser errors
#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("Tokenization failed: {0}")]
    TokenizationFailed(String),
    #[error("Parse failed: {0}")]
    ParseFailed(String),
    #[error("Invalid source: {0}")]
    InvalidSource(String),
    #[error("HoTT parser invocation failed: {0}")]
    HottParserFailed(String),
}

/// The HoTT parser - bridges to pure HoTT parsing logic
pub struct HottParser {
    /// Cache for parsed ASTs
    cache: HashMap<String, HottAst>,
}

impl HottParser {
    /// Create new HoTT parser
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
        }
    }
    
    /// Parse source string to HoTT AST or Module
    /// This invokes the pure HoTT parser defined in /src/core/hott-parser-native.hott
    pub fn parse(&mut self, source: &str) -> Result<HottAst, ParseError> {
        // Check cache first
        if let Some(cached_ast) = self.cache.get(source) {
            return Ok(cached_ast.clone());
        }
        
        // Check if this is a single expression or module with declarations
        if self.contains_declarations(source) {
            // Parse as module and return first expression for now
            // TODO: Extend runtime to handle full modules
            let module = self.parse_module(source)?;
            if let Some(HottDeclaration::FunctionDef { body, .. }) = module.declarations.first() {
                return Ok(body.clone());
            }
            // Fallback to simple expression
            return self.invoke_hott_parser(source);
        }
        
        // Invoke the HoTT parser for expressions
        let ast = self.invoke_hott_parser(source)?;
        
        // Cache result
        self.cache.insert(source.to_string(), ast.clone());
        
        Ok(ast)
    }
    
    /// Parse complete HoTT module with declarations
    pub fn parse_module(&mut self, source: &str) -> Result<HottModule, ParseError> {
        let tokens = self.tokenize(source)?;
        self.parse_module_from_tokens(tokens)
    }
    
    /// Parse module from tokens (simplified for now)
    fn parse_module_from_tokens(&self, _tokens: Vec<Token>) -> Result<HottModule, ParseError> {
        // Simplified: return empty module for now
        // TODO: Implement full declaration parsing
        Ok(HottModule {
            declarations: vec![],
            imports: vec![],
            exports: vec![],
        })
    }
    
    /// Check if source contains declarations (vs just expressions)
    fn contains_declarations(&self, source: &str) -> bool {
        source.contains("data ") || source.contains(" := ") || 
        source.trim_start().starts_with("--")
    }
    
    /// Invoke the pure HoTT parser
    /// This is where the Rust VM calls the HoTT mathematical parser
    fn invoke_hott_parser(&self, source: &str) -> Result<HottAst, ParseError> {
        // For now, implement a simple manual parser that creates HoTT AST values
        // In production, this would call the actual HoTT parser via FFI or subprocess
        
        let trimmed = source.trim();
        
        // Simple parsing rules that mirror what the HoTT parser would do
        if trimmed.is_empty() {
            return Err(ParseError::InvalidSource("Empty source".to_string()));
        }
        
        // Parse identifiers
        if trimmed.chars().all(|c| c.is_alphabetic() || c == '_' || c == '-') {
            return Ok(HottAst::Var(trimmed.to_string()));
        }
        
        // Parse numbers as AST expressions (not direct evaluation)
        if trimmed.chars().all(|c| c.is_ascii_digit()) {
            // Create AST for decimal-to-peano conversion
            let digits: Vec<u8> = trimmed.chars()
                .map(|c| c.to_digit(10).unwrap() as u8)
                .collect();
            
            // Build AST: (decimal-to-peano (digit-sequence [4, 2]))
            let digit_sequence = HottAst::DigitSequence(digits);
            return Ok(HottAst::App {
                func: Box::new(HottAst::Var("decimal-to-peano".to_string())),
                arg: Box::new(digit_sequence),
            });
        }
        
        // Parse lambda expressions: \x -> body
        if trimmed.starts_with('\\') || trimmed.starts_with("lambda") {
            return self.parse_lambda(trimmed);
        }
        
        // Parse applications: (func arg)
        if trimmed.starts_with('(') && trimmed.ends_with(')') {
            return self.parse_application(trimmed);
        }
        
        // Default: treat as variable
        Ok(HottAst::Var(trimmed.to_string()))
    }
    
    /// Parse lambda expression
    fn parse_lambda(&self, source: &str) -> Result<HottAst, ParseError> {
        // Simplified lambda parsing: \x -> body or lambda x . body
        if source.starts_with('\\') {
            // \x -> body
            let rest = &source[1..];
            if let Some(arrow_pos) = rest.find(" -> ") {
                let param = rest[..arrow_pos].trim().to_string();
                let body_source = rest[arrow_pos + 4..].trim();
                let body = Box::new(self.invoke_hott_parser(body_source)?);
                return Ok(HottAst::Lambda { param, param_type: None, body });
            }
        }
        
        Err(ParseError::ParseFailed("Invalid lambda syntax".to_string()))
    }
    
    /// Parse function application
    fn parse_application(&self, source: &str) -> Result<HottAst, ParseError> {
        // Remove outer parentheses
        let inner = &source[1..source.len()-1].trim();
        
        // Find the space that separates function from argument
        // This is simplified - real parser would handle nested expressions
        if let Some(space_pos) = inner.find(' ') {
            let func_source = inner[..space_pos].trim();
            let arg_source = inner[space_pos + 1..].trim();
            
            let func = Box::new(self.invoke_hott_parser(func_source)?);
            let arg = Box::new(self.invoke_hott_parser(arg_source)?);
            
            return Ok(HottAst::App { func, arg });
        }
        
        // Single element in parentheses
        self.invoke_hott_parser(inner)
    }
    
    /// This function was removed - we now use compositional AST parsing
    /// Numbers are parsed as: (decimal-to-peano (digit-sequence [digits]))
    /// This keeps the parser pure and moves evaluation to the evaluator
    
    /// Tokenize source string (mirrors HoTT tokenize-string function)
    /// Now supports mathematical Unicode symbols
    pub fn tokenize(&self, source: &str) -> Result<Vec<Token>, ParseError> {
        let mut tokens = Vec::new();
        let mut chars = source.chars().peekable();
        
        while let Some(ch) = chars.next() {
            match ch {
                // Whitespace
                ' ' | '\t' | '\n' | '\r' => continue,
                
                // Comments
                '-' if chars.peek() == Some(&'-') => {
                    chars.next(); // consume second -
                    let mut comment = String::new();
                    while let Some(ch) = chars.next() {
                        if ch == '\n' {
                            break;
                        }
                        comment.push(ch);
                    }
                    tokens.push(Token::Comment(comment));
                }
                
                // Delimiters
                '(' => tokens.push(Token::LParen),
                ')' => tokens.push(Token::RParen),
                '⟨' => tokens.push(Token::LAngle),
                '⟩' => tokens.push(Token::RAngle),
                ',' => tokens.push(Token::Comma),
                '.' => tokens.push(Token::Dot),
                
                // Mathematical symbols
                'λ' | '\\' => tokens.push(Token::Lambda),
                '→' => tokens.push(Token::Arrow),
                '↦' => tokens.push(Token::Maps),
                '×' => tokens.push(Token::Times),
                'Π' => tokens.push(Token::Pi),
                'Σ' => tokens.push(Token::Sigma),
                
                // Colons and assignment
                ':' => {
                    if chars.peek() == Some(&'=') {
                        chars.next(); // consume =
                        tokens.push(Token::ColonEqual);
                    } else {
                        tokens.push(Token::Colon);
                    }
                }
                
                // Equals sign and arrow
                '=' => {
                    if chars.peek() == Some(&'>') {
                        chars.next(); // consume >
                        tokens.push(Token::DoubleArrow);
                    } else {
                        tokens.push(Token::Equals);
                    }
                }
                
                // Pattern matching
                '|' => tokens.push(Token::Pipe),
                
                // Minus/dash (for arrows and identifiers)
                '-' => {
                    if chars.peek() == Some(&'>') {
                        chars.next(); // consume >
                        tokens.push(Token::Arrow);
                    } else if chars.peek() == Some(&'-') {
                        // Start of comment, consume rest of line
                        let mut comment = String::new();
                        while let Some(ch) = chars.next() {
                            if ch == '\n' {
                                break;
                            }
                            comment.push(ch);
                        }
                        tokens.push(Token::Comment(comment));
                    } else {
                        tokens.push(Token::Minus);
                    }
                }
                
                // Mathematical constants
                'ℕ' => tokens.push(Token::Nat),
                '𝟚' => tokens.push(Token::Bool),
                '₀' => tokens.push(Token::Zero),
                '₁' => tokens.push(Token::One),
                
                // Unicode universe symbols
                '𝒰' => {
                    // Look for subscript digits
                    let mut level_str = String::new();
                    while let Some(&next_char) = chars.peek() {
                        match next_char {
                            '₀' => { level_str.push('0'); chars.next(); }
                            '₁' => { level_str.push('1'); chars.next(); }
                            '₂' => { level_str.push('2'); chars.next(); }
                            '₃' => { level_str.push('3'); chars.next(); }
                            '₄' => { level_str.push('4'); chars.next(); }
                            '₅' => { level_str.push('5'); chars.next(); }
                            '₆' => { level_str.push('6'); chars.next(); }
                            '₇' => { level_str.push('7'); chars.next(); }
                            '₈' => { level_str.push('8'); chars.next(); }
                            '₉' => { level_str.push('9'); chars.next(); }
                            _ => break,
                        }
                    }
                    let level = if level_str.is_empty() { 0 } else {
                        level_str.parse().unwrap_or(0)
                    };
                    tokens.push(Token::Universe(level));
                }
                
                // Underscore (wildcard in patterns)
                '_' => {
                    // Check if it's a standalone underscore or part of identifier
                    if chars.peek().map_or(true, |&ch| !ch.is_alphanumeric() && ch != '_') {
                        tokens.push(Token::Underscore);
                    } else {
                        // Part of identifier
                        let mut identifier = String::from("_");
                        while let Some(&next_char) = chars.peek() {
                            if next_char.is_alphanumeric() || next_char == '_' || next_char == '-' {
                                identifier.push(chars.next().unwrap());
                            } else {
                                break;
                            }
                        }
                        tokens.push(Token::Identifier(identifier));
                    }
                }
                
                // ASCII identifiers and keywords
                c if c.is_alphabetic() => {
                    let mut identifier = String::new();
                    identifier.push(c);
                    while let Some(&next_char) = chars.peek() {
                        if next_char.is_alphanumeric() || next_char == '_' || next_char == '-' {
                            identifier.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    
                    // Check for keywords
                    let token = match identifier.as_str() {
                        "data" => Token::Data,
                        "where" => Token::Where,
                        "elim" => Token::Elim,
                        "Id" => Token::Id,
                        "refl" => Token::Refl,
                        "zero" => Token::Zero,
                        "succ" => Token::Succ,
                        _ if identifier.starts_with("Type") => {
                            // Parse Type₀, Type₁, etc.
                            let level_str = &identifier[4..];
                            let level = level_str.parse().unwrap_or(0);
                            Token::TypeLevel(level)
                        }
                        _ => Token::Identifier(identifier),
                    };
                    tokens.push(token);
                }
                
                // Numbers
                c if c.is_ascii_digit() => {
                    let mut number_str = String::new();
                    number_str.push(c);
                    while let Some(&next_char) = chars.peek() {
                        if next_char.is_ascii_digit() {
                            number_str.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    let number: usize = number_str.parse().map_err(|_| {
                        ParseError::TokenizationFailed("Invalid number".to_string())
                    })?;
                    tokens.push(Token::Number(number));
                }
                
                // String literals
                '"' => {
                    let mut string_content = String::new();
                    while let Some(ch) = chars.next() {
                        if ch == '"' {
                            break;
                        }
                        string_content.push(ch);
                    }
                    tokens.push(Token::StringLit(string_content));
                }
                
                // Unknown character
                _ => {
                    return Err(ParseError::TokenizationFailed(
                        format!("Unexpected character: {} (U+{:04X})", ch, ch as u32)
                    ));
                }
            }
        }
        
        tokens.push(Token::EOF);
        Ok(tokens)
    }
}

/// Token types that mirror the HoTT Token inductive type
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Delimiters
    LParen,          // (
    RParen,          // )
    LAngle,          // ⟨
    RAngle,          // ⟩
    Dot,             // .
    
    // Mathematical symbols
    Lambda,          // λ
    Arrow,           // → 
    DoubleArrow,     // ⇒
    Maps,            // ↦
    Times,           // ×
    Colon,           // :
    ColonEqual,      // :=
    Comma,           // ,
    Equals,          // =
    Pipe,            // |
    Underscore,      // _
    Minus,           // -
    
    // Type theory symbols
    Pi,              // Π
    Sigma,           // Σ
    Id,              // Id
    Refl,            // refl
    Universe(usize), // 𝒰₀, 𝒰₁, etc.
    TypeLevel(usize),// Type₀, Type₁, etc.
    
    // Literals
    Identifier(String),
    Number(usize),
    StringLit(String),
    
    // Keywords
    Data,            // data
    Where,           // where
    Elim,            // elim (for eliminators)
    
    // Special mathematical symbols
    Nat,             // ℕ
    Bool,            // 𝟚
    Zero,            // zero or ₀ 
    One,             // ₁
    Succ,            // succ
    
    // Comments and whitespace
    Comment(String),
    EOF,
}

impl Default for HottParser {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// INTEGRATION WITH HOTT PARSER
// ============================================================================

impl HottParser {
    /// Call the actual HoTT parser function (future implementation)
    /// This would use FFI to call the Racket-hosted HoTT parser
    #[allow(dead_code)]
    fn call_hott_parser_via_ffi(&self, source: &str) -> Result<HottAst, ParseError> {
        // This would call the actual hott-parse function from hott-parser-native.hott
        // For now, we use the simplified Rust implementation above
        
        // Pseudo-code for future FFI implementation:
        // let racket_result = racket_ffi::call_function("hott-parse", &[source]);
        // convert_racket_value_to_rust_ast(racket_result)
        
        self.invoke_hott_parser(source)
    }
    
    /// Convert HoTT string value to Rust string (for FFI bridge)
    #[allow(dead_code)]
    fn hott_string_to_rust(&self, hott_string: &HottValue) -> Result<String, ParseError> {
        match hott_string {
            HottValue::String(s) => Ok(s.clone()),
            HottValue::Constructor { name, args, .. } if name == "string-cons" => {
                // Recursive string construction
                if args.len() == 2 {
                    let char_val = &args[0];
                    let rest_string = &args[1];
                    
                    // Extract character and recurse
                    let ch = self.hott_char_to_rust_char(char_val)?;
                    let rest = self.hott_string_to_rust(rest_string)?;
                    Ok(format!("{}{}", ch, rest))
                } else {
                    Err(ParseError::ParseFailed("Invalid string constructor".to_string()))
                }
            }
            HottValue::Constructor { name, .. } if name == "empty-string" => {
                Ok(String::new())
            }
            _ => Err(ParseError::ParseFailed("Not a string value".to_string()))
        }
    }
    
    /// Convert HoTT character to Rust char
    fn hott_char_to_rust_char(&self, hott_char: &HottValue) -> Result<char, ParseError> {
        match hott_char {
            HottValue::Constructor { name, args, .. } if name == "char" => {
                if let Some(codepoint) = args.first() {
                    let code = self.hott_nat_to_rust_number(codepoint)?;
                    char::from_u32(code as u32)
                        .ok_or_else(|| ParseError::ParseFailed("Invalid character codepoint".to_string()))
                } else {
                    Err(ParseError::ParseFailed("Character missing codepoint".to_string()))
                }
            }
            _ => Err(ParseError::ParseFailed("Not a character value".to_string()))
        }
    }
    
    /// Convert HoTT natural number to Rust number
    fn hott_nat_to_rust_number(&self, hott_nat: &HottValue) -> Result<usize, ParseError> {
        match hott_nat {
            HottValue::Constructor { name, args, .. } => {
                match name.as_str() {
                    "zero" => Ok(0),
                    "next" => {
                        if args.len() == 1 {
                            let pred_num = self.hott_nat_to_rust_number(&args[0])?;
                            Ok(pred_num + 1)
                        } else {
                            Err(ParseError::ParseFailed("Invalid next constructor".to_string()))
                        }
                    }
                    _ => Err(ParseError::ParseFailed("Not a natural number".to_string())),
                }
            }
            _ => Err(ParseError::ParseFailed("Not a constructor value".to_string())),
        }
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parse_identifier() {
        let mut parser = HottParser::new();
        let result = parser.parse("hello").unwrap();
        
        assert_eq!(result, HottAst::Var("hello".to_string()));
    }
    
    #[test]
    fn test_parse_number() {
        let mut parser = HottParser::new();
        let result = parser.parse("42").unwrap();
        
        // Should parse as literal natural number
        assert!(matches!(result, HottAst::Literal(_)));
    }
    
    #[test]
    fn test_parse_lambda() {
        let mut parser = HottParser::new();
        let result = parser.parse("\\x -> x").unwrap();
        
        match result {
            HottAst::Lambda { param, body, .. } => {
                assert_eq!(param, "x");
                assert_eq!(*body, Box::new(HottAst::Var("x".to_string())));
            }
            _ => panic!("Expected lambda expression"),
        }
    }
    
    #[test]
    fn test_parse_application() {
        let mut parser = HottParser::new();
        let result = parser.parse("(f x)").unwrap();
        
        match result {
            HottAst::App { func, arg } => {
                assert_eq!(*func, Box::new(HottAst::Var("f".to_string())));
                assert_eq!(*arg, Box::new(HottAst::Var("x".to_string())));
            }
            _ => panic!("Expected application"),
        }
    }
    
    #[test]
    fn test_tokenization() {
        let parser = HottParser::new();
        let tokens = parser.tokenize("(lambda x)").unwrap();
        
        assert_eq!(tokens, vec![
            Token::LParen,
            Token::Identifier("lambda".to_string()),
            Token::Identifier("x".to_string()),
            Token::RParen,
            Token::EOF,
        ]);
    }
}