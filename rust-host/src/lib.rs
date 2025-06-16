// ============================================================================
// PATHFINDER RUST HOST - HOTT-NATIVE AST INTEGRATION
// ============================================================================
// This module provides the Rust host that works with HoTT-native AST values.
// The AST is defined in HoTT IL and manipulated by HoTT eliminators.

pub mod hott_values;
pub mod hott_eliminators;
pub mod hott_parser;
pub mod hott_evaluator;
pub mod hott_loader;
pub mod bootstrap_vm;

use std::collections::HashMap;

// Re-export main types
pub use hott_values::*;
pub use hott_eliminators::*;
pub use hott_parser::*;
pub use hott_evaluator::*;
pub use hott_loader::*;
pub use bootstrap_vm::*;

/// PathFinder Runtime using V0 Bootstrap VM
pub struct PathFinderRuntime {
    /// V0 Bootstrap VM with hash-consing and caching
    vm: BootstrapVM,
}

impl PathFinderRuntime {
    pub fn new() -> Self {
        Self {
            vm: BootstrapVM::new(),
        }
    }
    
    /// Load HoTT file using V0 bootstrap VM
    pub fn load_hott_file(&mut self, file_path: &str) -> Result<(), RuntimeError> {
        self.vm.load_hott_file(file_path)
            .map_err(|e| RuntimeError::Eval(EvalError::RuntimeError(e.to_string())))
    }

    /// Parse HoTT source code
    pub fn parse(&mut self, source: &str) -> Result<HottAst, ParseError> {
        HottParser::new().parse(source)
    }

    /// Evaluate HoTT AST using V0 bootstrap VM with caching
    pub fn evaluate(&mut self, ast: HottAst) -> Result<HottValue, EvalError> {
        let value_ptr = self.vm.eval(ast)?;
        // Convert ValuePtr back to HottValue for compatibility
        let value = self.vm.get_value(value_ptr).unwrap().clone();
        Ok(value)
    }

    /// Execute a HoTT program from source - V0 Bootstrap
    pub fn run(&mut self, source: &str) -> Result<HottValue, RuntimeError> {
        println!("🚀 V0 Bootstrap VM with Hash-Consing & Caching");
        
        let ast = self.parse(source)?;
        let result = self.evaluate(ast)?;
        Ok(result)
    }
    
    /// Test Peano number performance with caching
    pub fn test_peano_performance(&mut self, n: usize) -> Result<(), RuntimeError> {
        let value_ptr = self.vm.test_peano_caching(n)
            .map_err(RuntimeError::Eval)?;
        
        println!("🎯 Peano {} created successfully!", n);
        self.vm.print_cache_stats();
        Ok(())
    }
    
    /// Load core system to bootstrap self-hosting
    pub fn bootstrap_self_hosting(&mut self) -> Result<(), RuntimeError> {
        println!("🔄 Bootstrapping self-hosting system...");
        
        // Load minimal files needed for self-hosting
        self.load_hott_file("../src/core/foundations.hott")?;
        self.load_hott_file("../src/core/literals.hott")?;
        
        // Load the HoTT parser for true self-hosting!
        println!("🚀 Loading parser.hott for self-hosting...");
        self.load_hott_file("../src/core/parser.hott")?;
        
        // Load the HoTT evaluator too!
        println!("🚀 Loading evaluator.hott for complete HoTT system...");
        self.load_hott_file("../src/core/evaluator.hott")?;
        
        // Load the effects system
        println!("🚀 Loading effects.hott for pure mathematical effects...");
        self.load_hott_file("../src/effects/effects.hott")?;
        
        println!("✅ Bootstrap complete! Self-hosting parser, evaluator AND effects loaded");
        self.vm.print_cache_stats();
        Ok(())
    }
    
    /// Test complete self-hosting: use loaded parser to parse other .hott files
    pub fn test_complete_self_hosting(&mut self) -> Result<(), RuntimeError> {
        // First bootstrap the system
        self.bootstrap_self_hosting()?;
        
        // Check if hott-parse is loaded
        println!("🔍 Checking if hott-parse function was loaded...");
        
        // Then test using loaded parser on other files
        self.vm.test_self_hosting_parse()
            .map_err(|e| RuntimeError::Eval(EvalError::RuntimeError(e.to_string())))?;
        
        println!("🎯 Complete self-hosting test finished!");
        Ok(())
    }
    
    /// V1 FUNCTIONALITY: Evaluate HoTT AST using loaded evaluator
    pub fn evaluate_with_loaded_evaluator(&mut self, ast: HottAst) -> Result<HottValue, RuntimeError> {
        // First ensure system is bootstrapped
        self.bootstrap_self_hosting()?;
        
        // Use loaded hott-evaluate function
        let value_ptr = self.vm.evaluate_with_loaded_evaluator(ast)
            .map_err(|e| RuntimeError::Eval(EvalError::RuntimeError(e.to_string())))?;
        
        // Convert ValuePtr back to HottValue for compatibility
        let value = self.vm.get_value(value_ptr).unwrap().clone();
        Ok(value)
    }
    
    /// Test V1: Parse AND evaluate a HoTT program using loaded functions
    pub fn test_v1_complete_evaluation(&mut self, source: &str) -> Result<HottValue, RuntimeError> {
        println!("🚀 V1 COMPLETE TEST: Parse + Evaluate using loaded HoTT functions");
        
        // Parse with loaded parser (self-hosting)
        let ast = self.parse(source)?;
        println!("  ✅ Parsed with V0 parser");
        
        // Evaluate with loaded evaluator (V1 functionality!)
        let result = self.evaluate_with_loaded_evaluator(ast)?;
        println!("  ✅ Evaluated with loaded HoTT evaluator!");
        
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
        // Test that runtime creates successfully
        drop(runtime);
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