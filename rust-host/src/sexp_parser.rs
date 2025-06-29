// ============================================================================
// S-EXPRESSION PARSER - MINIMAL BOOTSTRAP
// ============================================================================
// A clean, modern s-expression parser without LISP legacy.
// This is the minimal parser needed to bootstrap PathFinder.

use crate::hott_values::*;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, thiserror::Error)]
pub enum SExpError {
    #[error("Unexpected end of input")]
    UnexpectedEOF,
    #[error("Unexpected character: {0}")]
    UnexpectedChar(char),
    #[error("Unmatched closing parenthesis")]
    UnmatchedClose,
    #[error("Invalid number: {0}")]
    InvalidNumber(String),
    #[error("Invalid string: {0}")]
    InvalidString(String),
}

/// A simple s-expression value
#[derive(Debug, Clone, PartialEq)]
pub enum SExp {
    /// An atom (identifier, keyword, number)
    Atom(String),
    /// A string literal
    String(String),
    /// A number literal
    Number(i64),
    /// A list of s-expressions
    List(Vec<SExp>),
}

/// Minimal s-expression parser
pub struct SExpParser;

impl SExpParser {
    pub fn new() -> Self {
        Self
    }
    
    /// Parse a string into s-expressions
    pub fn parse(&self, input: &str) -> Result<Vec<SExp>, SExpError> {
        let mut chars = input.chars().peekable();
        let mut results = Vec::new();
        
        while chars.peek().is_some() {
            self.skip_whitespace(&mut chars);
            if chars.peek().is_some() {
                results.push(self.parse_sexp(&mut chars)?);
            }
        }
        
        Ok(results)
    }
    
    /// Parse a single s-expression
    fn parse_sexp(&self, chars: &mut Peekable<Chars>) -> Result<SExp, SExpError> {
        self.skip_whitespace(chars);
        
        match chars.peek() {
            None => Err(SExpError::UnexpectedEOF),
            Some(&'(') => self.parse_list(chars),
            Some(&'"') => self.parse_string(chars),
            Some(&')') => Err(SExpError::UnmatchedClose),
            Some(&c) if c.is_digit(10) => self.parse_number(chars),
            Some(&'-') => {
                // Check if it's a negative number or part of an atom like ->
                chars.next(); // consume '-'
                if chars.peek().map_or(false, |c| c.is_digit(10)) {
                    // It's a negative number
                    let mut num_str = String::from("-");
                    while let Some(&ch) = chars.peek() {
                        if ch.is_digit(10) {
                            num_str.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    num_str.parse::<i64>()
                        .map(SExp::Number)
                        .map_err(|_| SExpError::InvalidNumber(num_str))
                } else {
                    // It's part of an atom like ->
                    let mut atom = String::from("-");
                    while let Some(&ch) = chars.peek() {
                        if ch.is_alphanumeric() || "->!?*+=-_/.:<>ω|".contains(ch) || ch.is_alphabetic() {
                            atom.push(chars.next().unwrap());
                        } else if ch.is_whitespace() || ch == '(' || ch == ')' {
                            break;
                        } else {
                            return Err(SExpError::UnexpectedChar(ch));
                        }
                    }
                    Ok(SExp::Atom(atom))
                }
            }
            Some(&'#') => {
                // Handle hash literals like #\0 for characters
                chars.next(); // consume '#'
                if chars.peek() == Some(&'\\') {
                    chars.next(); // consume '\'
                    // Parse character literal
                    match chars.next() {
                        Some(ch) => Ok(SExp::Atom(format!("#\\{}", ch))),
                        None => Err(SExpError::UnexpectedEOF),
                    }
                } else {
                    // Other hash literals, treat as atom
                    let mut atom = String::from("#");
                    while let Some(&ch) = chars.peek() {
                        if ch.is_alphanumeric() || "-_".contains(ch) {
                            atom.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    Ok(SExp::Atom(atom))
                }
            }
            _ => self.parse_atom(chars),
        }
    }
    
    /// Parse a list
    fn parse_list(&self, chars: &mut Peekable<Chars>) -> Result<SExp, SExpError> {
        chars.next(); // consume '('
        let mut elements = Vec::new();
        
        loop {
            self.skip_whitespace(chars);
            match chars.peek() {
                None => return Err(SExpError::UnexpectedEOF),
                Some(&')') => {
                    chars.next(); // consume ')'
                    return Ok(SExp::List(elements));
                }
                _ => elements.push(self.parse_sexp(chars)?),
            }
        }
    }
    
    /// Parse a string literal
    fn parse_string(&self, chars: &mut Peekable<Chars>) -> Result<SExp, SExpError> {
        chars.next(); // consume '"'
        let mut string = String::new();
        
        while let Some(ch) = chars.next() {
            match ch {
                '"' => return Ok(SExp::String(string)),
                '\\' => {
                    // Handle escape sequences
                    match chars.next() {
                        Some('n') => string.push('\n'),
                        Some('t') => string.push('\t'),
                        Some('r') => string.push('\r'),
                        Some('\\') => string.push('\\'),
                        Some('"') => string.push('"'),
                        Some(c) => string.push(c),
                        None => return Err(SExpError::UnexpectedEOF),
                    }
                }
                c => string.push(c),
            }
        }
        
        Err(SExpError::InvalidString("Unterminated string".to_string()))
    }
    
    /// Parse a number
    fn parse_number(&self, chars: &mut Peekable<Chars>) -> Result<SExp, SExpError> {
        let mut num_str = String::new();
        
        // Collect digits
        while let Some(&ch) = chars.peek() {
            if ch.is_digit(10) {
                num_str.push(chars.next().unwrap());
            } else {
                break;
            }
        }
        
        num_str.parse::<i64>()
            .map(SExp::Number)
            .map_err(|_| SExpError::InvalidNumber(num_str))
    }
    
    /// Parse an atom (identifier/keyword)
    fn parse_atom(&self, chars: &mut Peekable<Chars>) -> Result<SExp, SExpError> {
        let mut atom = String::new();
        
        while let Some(&ch) = chars.peek() {
            if ch.is_alphanumeric() || "->!?*+=-_/.:<>ω|".contains(ch) || ch.is_alphabetic() {
                atom.push(chars.next().unwrap());
            } else if ch.is_whitespace() || ch == '(' || ch == ')' {
                break;
            } else {
                return Err(SExpError::UnexpectedChar(ch));
            }
        }
        
        Ok(SExp::Atom(atom))
    }
    
    /// Skip whitespace and comments
    fn skip_whitespace(&self, chars: &mut Peekable<Chars>) {
        while let Some(&ch) = chars.peek() {
            if ch.is_whitespace() {
                chars.next();
            } else if ch == ';' {
                // Skip comment line
                chars.next(); // consume ';'
                while let Some(&ch) = chars.peek() {
                    chars.next();
                    if ch == '\n' {
                        break;
                    }
                }
            } else {
                break;
            }
        }
    }
}

/// Convert s-expression to HoTT AST
impl SExp {
    pub fn to_hott_ast(&self) -> Result<HottAst, String> {
        match self {
            SExp::Atom(s) => Ok(HottAst::Var(s.clone())),
            SExp::Number(n) => {
                // Convert number to Peano representation
                let mut result = HottAst::Var("zero".to_string());
                for _ in 0..*n {
                    result = HottAst::App {
                        func: Box::new(HottAst::Var("succ".to_string())),
                        arg: Box::new(result),
                    };
                }
                Ok(result)
            }
            SExp::String(s) => {
                // For now, represent strings as a constructor
                // In a full implementation, we'd add StringLit to HottAst
                Ok(HottAst::Constructor {
                    name: "string-literal".to_string(),
                    args: vec![HottAst::Var(s.clone())],
                })
            }
            SExp::List(items) => {
                if items.is_empty() {
                    return Err("Empty list".to_string());
                }
                
                match &items[0] {
                    SExp::Atom(op) => match op.as_str() {
                        "fn" => self.parse_fn(items),
                        "let" => self.parse_let(items),
                        "match" => self.parse_match(items),
                        "data" => self.parse_data(items),
                        "import" => self.parse_import(items),
                        "type" => self.parse_type_ann(items),
                        "define" => self.parse_define(items),
                        _ => self.parse_app(items),
                    },
                    _ => self.parse_app(items),
                }
            }
        }
    }
    
    fn parse_fn(&self, items: &[SExp]) -> Result<HottAst, String> {
        if items.len() != 3 {
            return Err("fn requires 2 arguments: params and body".to_string());
        }
        
        let params = match &items[1] {
            SExp::List(params) => {
                params.iter()
                    .map(|p| match p {
                        SExp::Atom(s) => Ok(s.clone()),
                        _ => Err("Parameter must be atom".to_string()),
                    })
                    .collect::<Result<Vec<_>, _>>()?
            }
            SExp::Atom(s) => vec![s.clone()],
            _ => return Err("fn params must be list or atom".to_string()),
        };
        
        let body = items[2].to_hott_ast()?;
        
        // Create nested lambdas for multiple parameters
        let mut result = body;
        for param in params.into_iter().rev() {
            result = HottAst::Lambda {
                param,
                param_type: None,
                body: Box::new(result),
            };
        }
        
        Ok(result)
    }
    
    fn parse_let(&self, items: &[SExp]) -> Result<HottAst, String> {
        if items.len() != 3 {
            return Err("let requires bindings and body".to_string());
        }
        
        let bindings = match &items[1] {
            SExp::List(bindings) => bindings,
            _ => return Err("let bindings must be a list".to_string()),
        };
        
        let body = items[2].to_hott_ast()?;
        
        // Convert let bindings to nested let expressions
        let mut result = body;
        for binding in bindings.iter().rev() {
            match binding {
                SExp::List(pair) if pair.len() == 2 => {
                    let var = match &pair[0] {
                        SExp::Atom(s) => s.clone(),
                        _ => return Err("Binding variable must be atom".to_string()),
                    };
                    let value = pair[1].to_hott_ast()?;
                    result = HottAst::Let {
                        var,
                        value: Box::new(value),
                        body: Box::new(result),
                    };
                }
                _ => return Err("Each binding must be a 2-element list".to_string()),
            }
        }
        
        Ok(result)
    }
    
    fn parse_app(&self, items: &[SExp]) -> Result<HottAst, String> {
        if items.is_empty() {
            return Err("Empty application".to_string());
        }
        
        let mut result = items[0].to_hott_ast()?;
        for arg in &items[1..] {
            result = HottAst::App {
                func: Box::new(result),
                arg: Box::new(arg.to_hott_ast()?),
            };
        }
        
        Ok(result)
    }
    
    // Placeholder implementations for other forms
    fn parse_match(&self, _items: &[SExp]) -> Result<HottAst, String> {
        Err("match not yet implemented".to_string())
    }
    
    fn parse_data(&self, _items: &[SExp]) -> Result<HottAst, String> {
        Err("data not yet implemented".to_string())
    }
    
    fn parse_import(&self, _items: &[SExp]) -> Result<HottAst, String> {
        Err("import not yet implemented".to_string())
    }
    
    fn parse_type_ann(&self, _items: &[SExp]) -> Result<HottAst, String> {
        Err("type annotation not yet implemented".to_string())
    }
    
    fn parse_define(&self, _items: &[SExp]) -> Result<HottAst, String> {
        Err("define not yet implemented".to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parse_atom() {
        let parser = SExpParser::new();
        let result = parser.parse("hello").unwrap();
        assert_eq!(result, vec![SExp::Atom("hello".to_string())]);
    }
    
    #[test]
    fn test_parse_list() {
        let parser = SExpParser::new();
        let result = parser.parse("(+ 1 2)").unwrap();
        assert_eq!(result, vec![SExp::List(vec![
            SExp::Atom("+".to_string()),
            SExp::Number(1),
            SExp::Number(2),
        ])]);
    }
    
    #[test]
    fn test_parse_nested() {
        let parser = SExpParser::new();
        let result = parser.parse("(fn (x) (+ x 1))").unwrap();
        assert_eq!(result.len(), 1);
    }
}