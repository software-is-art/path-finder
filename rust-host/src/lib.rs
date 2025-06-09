// ============================================================================
// PATHFINDER RUST HOST - HOTT-NATIVE AST INTEGRATION
// ============================================================================
// This module provides the Rust host that works with HoTT-native AST values.
// The AST is defined in HoTT IL and manipulated by HoTT eliminators.

pub mod hott_values;
pub mod hott_eliminators;
pub mod hott_parser;
pub mod hott_evaluator;

use std::collections::HashMap;

// Re-export main types
pub use hott_values::*;
pub use hott_eliminators::*;
pub use hott_parser::*;
pub use hott_evaluator::*;

/// The main PathFinder host runtime
pub struct PathFinderRuntime {
    /// Cache for computational values (content-addressable)
    cache: HashMap<String, HottValue>,
    /// Effect execution context
    effect_context: EffectContext,
    /// Type family registry
    type_families: HashMap<String, TypeFamily>,
}

impl PathFinderRuntime {
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
            effect_context: EffectContext::new(),
            type_families: HashMap::new(),
        }
    }

    /// Parse HoTT source code into HoTT AST values
    pub fn parse(&mut self, source: &str) -> Result<HottAst, ParseError> {
        HottParser::new().parse(source)
    }

    /// Evaluate HoTT AST using eliminators
    pub fn evaluate(&mut self, ast: HottAst) -> Result<HottValue, EvalError> {
        let context = EvaluationContext::new();
        HottEvaluator::new().evaluate(ast, context)
    }

    /// Execute a HoTT program from source
    pub fn run(&mut self, source: &str) -> Result<HottValue, RuntimeError> {
        let ast = self.parse(source)?;
        let result = self.evaluate(ast)?;
        Ok(result)
    }
}

/// Main errors
#[derive(Debug, thiserror::Error)]
pub enum RuntimeError {
    #[error("Parse error: {0}")]
    Parse(#[from] ParseError),
    #[error("Evaluation error: {0}")]
    Eval(#[from] EvalError),
}

/// Effect execution context
#[derive(Debug, Clone)]
pub struct EffectContext {
    /// Registered effect handlers
    handlers: HashMap<String, EffectHandler>,
}

impl EffectContext {
    pub fn new() -> Self {
        Self {
            handlers: HashMap::new(),
        }
    }
}

/// Type family definition
#[derive(Debug, Clone)]
pub struct TypeFamily {
    pub name: String,
    pub arity: usize,
    pub instantiation_fn: String, // Reference to HoTT function
}

/// Effect handler
#[derive(Debug, Clone)]
pub struct EffectHandler {
    pub effect_name: String,
    pub handler_type: HandlerType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HandlerType {
    CompileTime,
    Runtime,
    Capability,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_runtime_creation() {
        let runtime = PathFinderRuntime::new();
        assert_eq!(runtime.cache.len(), 0);
    }

    #[test]
    fn test_simple_parse_eval() {
        let mut runtime = PathFinderRuntime::new();
        
        // This would parse: (var "x")
        let source = "x";
        let result = runtime.run(source);
        
        // For now, just ensure we can create the runtime
        assert!(result.is_err()); // Will fail until parser is implemented
    }
}

/// Default implementation
impl Default for PathFinderRuntime {
    fn default() -> Self {
        Self::new()
    }
}