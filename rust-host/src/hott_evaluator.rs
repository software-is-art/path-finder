// ============================================================================
// HOTT EVALUATOR - RUST VM THAT INVOKES PURE HOTT FUNCTIONS
// ============================================================================
// This module implements the Rust VM that executes HoTT mathematical functions.
// It's just the "operating system" for the pure HoTT logic.

use crate::hott_values::*;
use crate::hott_eliminators::*;
use std::collections::HashMap;

/// The main HoTT evaluator - executes pure HoTT mathematical functions
pub struct HottEvaluator {
    /// Eliminator execution engine
    eliminator_engine: EliminatorEngine,
    
    /// Cache for computational values
    cache: HashMap<String, HottValue>,
    
    /// Built-in functions registry
    builtins: HashMap<String, Box<dyn Fn(&[HottValue]) -> Result<HottValue, EvalError>>>,
}

/// Evaluation errors
#[derive(Debug, thiserror::Error)]
pub enum EvalError {
    #[error("Unbound variable: {0}")]
    UnboundVariable(String),
    #[error("Type error: {0}")]
    TypeError(String),
    #[error("Cannot apply non-function: {0}")]
    NotAFunction(String),
    #[error("Wrong number of arguments: expected {expected}, got {actual}")]
    WrongArity { expected: usize, actual: usize },
    #[error("Eliminator error: {0}")]
    EliminatorError(#[from] EliminatorError),
    #[error("Effect execution error: {0}")]
    EffectError(#[from] EffectError),
    #[error("Runtime error: {0}")]
    RuntimeError(String),
}

impl HottEvaluator {
    /// Create new HoTT evaluator
    pub fn new() -> Self {
        let mut evaluator = Self {
            eliminator_engine: EliminatorEngine::new(),
            cache: HashMap::new(),
            builtins: HashMap::new(),
        };
        
        evaluator.register_builtins();
        evaluator
    }
    
    /// Main evaluation function - mirrors the HoTT hott-evaluate function
    pub fn evaluate(&mut self, ast: HottAst, context: EvaluationContext) -> Result<HottValue, EvalError> {
        // This is the Rust VM implementing the HoTT evaluator logic
        // The actual logic is defined in HoTT, we just execute it mechanically
        
        self.execute_ast_eliminator(ast, context, None)
    }
    
    /// Evaluation with HoTT loader for dogfooding bridge
    pub fn evaluate_with_loader(&mut self, ast: HottAst, context: EvaluationContext, loader: Option<&crate::HottLoader>) -> Result<HottValue, EvalError> {
        self.execute_ast_eliminator(ast, context, loader)
    }
    
    /// Execute AST eliminator - the core of evaluation with HoTT function bridge
    fn execute_ast_eliminator(&mut self, ast: HottAst, context: EvaluationContext, loader: Option<&crate::HottLoader>) -> Result<HottValue, EvalError> {
        // Simplified direct evaluation instead of complex closures
        match ast {
            HottAst::Var(name) => {
                // First check built-ins
                if let Ok(builtin_val) = self.apply_builtin(&name, vec![]) {
                    return Ok(builtin_val);
                }
                
                // Then check loaded HoTT functions - THE DOGFOODING BRIDGE!
                if let Some(hott_loader) = loader {
                    if let Some(hott_function) = hott_loader.get_function(&name) {
                        println!("🔗 Bridging to HoTT function: {}", name);
                        return Ok(hott_function.clone());
                    }
                }
                
                // Finally check local environment
                context.environment.lookup(&name)
                    .cloned()
                    .ok_or_else(|| EvalError::UnboundVariable(name))
            }
            
            HottAst::App { func, arg } => {
                let func_val = self.execute_ast_eliminator(*func, context.clone(), loader)?;
                let arg_val = self.execute_ast_eliminator(*arg, context.clone(), loader)?;
                self.apply_function(func_val, arg_val, context, loader)
            }
            
            HottAst::Lambda { param, body, .. } => {
                Ok(HottValue::closure(vec![param], *body, context.environment))
            }
            
            HottAst::PiType { var_name, domain, codomain: _ } => {
                let domain_val = self.evaluate(*domain, context)?;
                // For now, return a type representation
                Ok(HottValue::constructor(
                    "pi-type-value".to_string(),
                    vec![HottValue::String(var_name), domain_val],
                    HottType::Universe(1),
                ))
            }
            
            HottAst::Eliminator { target, cases } => {
                let target_val = self.evaluate(*target, context.clone())?;
                let case_vals: Result<Vec<_>, _> = cases
                    .into_iter()
                    .map(|case_ast| self.evaluate(case_ast, context.clone()))
                    .collect();
                let case_vals = case_vals?;
                
                self.apply_eliminator_to_value(target_val, case_vals, context)
            }
            
            HottAst::TypeApp { type_name, args } => {
                let arg_vals: Result<Vec<_>, _> = args
                    .into_iter()
                    .map(|arg_ast| self.evaluate(arg_ast, context.clone()))
                    .collect();
                let arg_vals = arg_vals?;
                
                // Create type instantiation
                Ok(HottValue::constructor(
                    type_name,
                    arg_vals,
                    HottType::Universe(0),
                ))
            }
            
            HottAst::Constructor { name, args } => {
                let arg_vals: Result<Vec<_>, _> = args
                    .into_iter()
                    .map(|arg_ast| self.evaluate(arg_ast, context.clone()))
                    .collect();
                let arg_vals = arg_vals?;
                
                Ok(HottValue::constructor(
                    name,
                    arg_vals,
                    HottType::Inductive {
                        name: "UnknownType".to_string(),
                        constructors: vec![],
                    },
                ))
            }
            
            HottAst::Literal(value) => Ok(value),
            
            HottAst::Effect(effect) => self.execute_effect(effect, context),
            
            HottAst::SigmaType { var_name, first_type, second_type } => {
                let first_val = self.evaluate(*first_type, context.clone())?;
                let second_val = self.evaluate(*second_type, context)?;
                Ok(HottValue::constructor(
                    "sigma-type".to_string(),
                    vec![HottValue::String(var_name), first_val, second_val],
                    HottType::Universe(1),
                ))
            }
            
            HottAst::IdType { type_expr, left, right } => {
                let type_val = self.evaluate(*type_expr, context.clone())?;
                let left_val = self.evaluate(*left, context.clone())?;
                let right_val = self.evaluate(*right, context)?;
                Ok(HottValue::constructor(
                    "id-type".to_string(),
                    vec![type_val, left_val, right_val],
                    HottType::Universe(0),
                ))
            }
            
            HottAst::Universe(level) => {
                Ok(HottValue::universe(level))
            }
            
            HottAst::Pair { first, second } => {
                let first_val = self.evaluate(*first, context.clone())?;
                let second_val = self.evaluate(*second, context)?;
                Ok(HottValue::constructor(
                    "pair".to_string(),
                    vec![first_val, second_val],
                    HottType::Inductive {
                        name: "Sigma".to_string(),
                        constructors: vec![],
                    },
                ))
            }
            
            HottAst::DigitSequence(digits) => {
                // Create HoTT value representing digit sequence
                // This will be processed by decimal-to-peano builtin
                Ok(HottValue::constructor(
                    "digit-sequence".to_string(),
                    vec![HottValue::String(
                        digits.iter().map(|d| char::from_digit(*d as u32, 10).unwrap()).collect()
                    )],
                    HottType::Inductive {
                        name: "DigitSequence".to_string(),
                        constructors: vec![],
                    },
                ))
            }
            
            HottAst::Let { var, value, body } => {
                // Evaluate let expression: let var := value in body
                let value_result = self.execute_ast_eliminator(*value, context.clone(), loader)?;
                
                // Extend environment with the binding
                let new_context = context.with_binding(var, value_result);
                
                // Evaluate body in the extended environment
                self.execute_ast_eliminator(*body, new_context, loader)
            }
        }
    }
    
    /// Apply function to argument - implements HoTT function application
    fn apply_function(&mut self, func: HottValue, arg: HottValue, context: EvaluationContext, loader: Option<&crate::HottLoader>) -> Result<HottValue, EvalError> {
        match func {
            HottValue::Closure { params, body, env } => {
                
                if params.is_empty() {
                    return Err(EvalError::NotAFunction("closure with no parameters".to_string()));
                }
                
                let param = &params[0];
                let remaining_params = &params[1..];
                
                // Extend environment with parameter binding
                let extended_env = env.extend(param.clone(), arg.clone());
                
                if remaining_params.is_empty() {
                    // Last parameter: evaluate body
                    let new_context = context.with_binding(param.clone(), arg);
                    self.execute_ast_eliminator(*body, new_context, loader)
                } else {
                    // More parameters: return partial application
                    Ok(HottValue::closure(remaining_params.to_vec(), *body, extended_env))
                }
            }
            
            HottValue::Builtin { name, .. } => {
                self.apply_builtin(&name, vec![arg])
            }
            
            HottValue::Constructor { name, mut args, value_type } => {
                // Partial application of constructor
                args.push(arg);
                Ok(HottValue::constructor(name, args, value_type))
            }
            
            HottValue::HottFunction { name, source_body: _ } => {
                // Execute loaded HoTT function - TRUE DOGFOODING!
                println!("🔗 Executing loaded HoTT function: {}", name);
                if let Some(hott_loader) = loader {
                    hott_loader.call_hott_function(&name, vec![arg])
                        .map_err(|e| EvalError::RuntimeError(e.to_string()))
                } else {
                    Err(EvalError::RuntimeError("No HoTT loader available".to_string()))
                }
            }
            
            _ => Err(EvalError::NotAFunction(format!("{:?}", func))),
        }
    }
    
    /// Apply eliminator to value
    fn apply_eliminator_to_value(&mut self, target: HottValue, cases: Vec<HottValue>, context: EvaluationContext) -> Result<HottValue, EvalError> {
        // This implements the HoTT eliminator application logic
        match &target {
            HottValue::Constructor { args, .. } => {
                // Find appropriate case function and apply it
                if let Some(case_func) = cases.first() {
                    // Simplified: apply first case function to constructor arguments
                    self.apply_case_function(case_func.clone(), args.clone(), context)
                } else {
                    Err(EvalError::RuntimeError("No case functions provided for eliminator".to_string()))
                }
            }
            _ => Err(EvalError::TypeError("Can only eliminate constructor values".to_string())),
        }
    }
    
    /// Apply case function to constructor arguments
    fn apply_case_function(&mut self, case_func: HottValue, args: Vec<HottValue>, context: EvaluationContext) -> Result<HottValue, EvalError> {
        // Apply case function to each argument
        args.into_iter().try_fold(case_func, |func, arg| {
            self.apply_function(func, arg, context.clone(), None)
        })
    }
    
    /// Execute effect description
    fn execute_effect(&mut self, effect: EffectDescription, context: EvaluationContext) -> Result<HottValue, EvalError> {
        match effect {
            EffectDescription::Pure(value) => Ok(*value),
            
            EffectDescription::IO { operation, args, .. } => {
                // Delegate to host I/O system
                self.execute_io_operation(operation, args)
            }
            
            EffectDescription::Sequence(first, second) => {
                let _first_result = self.execute_effect(*first, context.clone())?;
                self.execute_effect(*second, context)
            }
            
            EffectDescription::Parallel(first, second) => {
                // Simplified: execute sequentially for now
                let first_result = self.execute_effect(*first, context.clone())?;
                let second_result = self.execute_effect(*second, context)?;
                
                // Return pair result
                Ok(HottValue::constructor(
                    "pair".to_string(),
                    vec![first_result, second_result],
                    HottType::Inductive {
                        name: "Pair".to_string(),
                        constructors: vec![],
                    },
                ))
            }
            
            EffectDescription::Choice(first, _second) => {
                // Simplified: always choose first
                self.execute_effect(*first, context)
            }
        }
    }
    
    /// Execute I/O operation - this is where the Rust host does actual work
    fn execute_io_operation(&self, operation: String, args: Vec<HottValue>) -> Result<HottValue, EvalError> {
        match operation.as_str() {
            "print" => {
                if let Some(HottValue::String(msg)) = args.first() {
                    println!("{}", msg);
                    Ok(HottValue::Unit)
                } else {
                    Err(EvalError::TypeError("print requires string argument".to_string()))
                }
            }
            
            "read-file" => {
                if let Some(HottValue::String(path)) = args.first() {
                    match std::fs::read_to_string(path) {
                        Ok(content) => Ok(HottValue::String(content)),
                        Err(e) => Err(EvalError::RuntimeError(format!("File read error: {}", e))),
                    }
                } else {
                    Err(EvalError::TypeError("read-file requires string path".to_string()))
                }
            }
            
            _ => Err(EvalError::RuntimeError(format!("Unknown I/O operation: {}", operation))),
        }
    }
    
    /// Apply built-in function
    fn apply_builtin(&self, name: &str, args: Vec<HottValue>) -> Result<HottValue, EvalError> {
        match self.builtins.get(name) {
            Some(builtin_fn) => builtin_fn(&args),
            None => Err(EvalError::RuntimeError(format!("Unknown builtin: {}", name))),
        }
    }
    
    /// Register built-in functions and HoTT constructors
    fn register_builtins(&mut self) {
        // HoTT Natural Number Constructors
        self.builtins.insert(
            "zero".to_string(), 
            Box::new(|_args| Ok(HottValue::zero()))
        );
        
        self.builtins.insert(
            "succ".to_string(),
            Box::new(|args| {
                if args.len() != 1 {
                    return Err(EvalError::WrongArity { expected: 1, actual: args.len() });
                }
                Ok(HottValue::succ(args[0].clone()))
            }),
        );
        
        // HoTT Boolean Constructors  
        self.builtins.insert(
            "true".to_string(),
            Box::new(|_args| Ok(HottValue::true_value()))
        );
        
        self.builtins.insert(
            "false".to_string(), 
            Box::new(|_args| Ok(HottValue::false_value()))
        );
        
        self.builtins.insert(
            "₀".to_string(),  // Boolean zero
            Box::new(|_args| Ok(HottValue::false_value()))
        );
        
        self.builtins.insert(
            "₁".to_string(),  // Boolean one
            Box::new(|_args| Ok(HottValue::true_value()))
        );
        
        // HoTT Unit Constructor
        self.builtins.insert(
            "⋆".to_string(),  // Unit value
            Box::new(|_args| Ok(HottValue::Unit))
        );
        
        // HoTT Type Constructors
        self.builtins.insert(
            "ℕ".to_string(),
            Box::new(|_args| Ok(HottValue::nat_type()))
        );
        
        self.builtins.insert(
            "𝟚".to_string(), 
            Box::new(|_args| Ok(HottValue::bool_type()))
        );
        
        self.builtins.insert(
            "𝟙".to_string(),
            Box::new(|_args| Ok(HottValue::unit_type()))
        );
        
        self.builtins.insert(
            "𝟘".to_string(),
            Box::new(|_args| Ok(HottValue::empty_type()))
        );
        
        // HoTT Universe Levels
        self.builtins.insert(
            "𝒰₀".to_string(),
            Box::new(|_args| Ok(HottValue::universe(0)))
        );
        
        self.builtins.insert(
            "𝒰₁".to_string(),
            Box::new(|_args| Ok(HottValue::universe(1)))
        );
        
        self.builtins.insert(
            "Type₀".to_string(),
            Box::new(|_args| Ok(HottValue::universe(0)))
        );
        
        self.builtins.insert(
            "Type₁".to_string(),
            Box::new(|_args| Ok(HottValue::universe(1)))
        );
        
        // Mathematical Operations (using HoTT eliminators)
        self.builtins.insert(
            "add".to_string(),
            Box::new(|args| {
                if args.len() != 2 {
                    return Err(EvalError::WrongArity { expected: 2, actual: args.len() });
                }
                
                // TODO: Implement using ℕ-eliminator
                // For now, return symbolic result
                Ok(HottValue::constructor(
                    "add-result".to_string(),
                    vec![args[0].clone(), args[1].clone()],
                    HottType::Inductive {
                        name: "ℕ".to_string(),
                        constructors: vec![],
                    },
                ))
            }),
        );
        
        // Number parsing and conversion
        self.builtins.insert(
            "decimal-to-peano".to_string(),
            Box::new(|args| {
                if args.len() != 1 {
                    return Err(EvalError::WrongArity { expected: 1, actual: args.len() });
                }
                
                // Extract digit sequence from constructor
                match &args[0] {
                    HottValue::Constructor { name, args: digit_args, .. } if name == "digit-sequence" => {
                        if let Some(HottValue::String(digits_str)) = digit_args.first() {
                            // Convert decimal string to HoTT natural number
                            let decimal_value: u32 = digits_str.parse().unwrap_or(0);
                            let result = Self::decimal_to_hott_nat(decimal_value);
                            Ok(result)
                        } else {
                            Err(EvalError::TypeError("Invalid digit sequence format".to_string()))
                        }
                    }
                    _ => Err(EvalError::TypeError("Expected digit-sequence constructor".to_string()))
                }
            }),
        );
    }
    
    /// Convert decimal number to HoTT natural number efficiently 
    /// This is the optimized version that should replace the recursive parser version
    fn decimal_to_hott_nat(n: u32) -> HottValue {
        let mut result = HottValue::zero();
        for _ in 0..n {
            result = HottValue::succ(result);
        }
        result
    }
    
}

impl Default for HottEvaluator {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_literal_evaluation() {
        let mut evaluator = HottEvaluator::new();
        let context = EvaluationContext::new();
        
        let ast = HottAst::Literal(HottValue::String("hello".to_string()));
        let result = evaluator.evaluate(ast, context).unwrap();
        
        assert_eq!(result, HottValue::String("hello".to_string()));
    }
    
    #[test]
    fn test_constructor_evaluation() {
        let mut evaluator = HottEvaluator::new();
        let context = EvaluationContext::new();
        
        let ast = HottAst::Constructor {
            name: "zero".to_string(),
            args: vec![],
        };
        
        let result = evaluator.evaluate(ast, context).unwrap();
        
        match result {
            HottValue::Constructor { name, args, .. } => {
                assert_eq!(name, "zero");
                assert!(args.is_empty());
            }
            _ => panic!("Expected constructor value"),
        }
    }
    
    #[test]
    fn test_builtin_application() {
        let mut evaluator = HottEvaluator::new();
        
        let result = evaluator.apply_builtin("succ", vec![HottValue::zero()]).unwrap();
        
        match result {
            HottValue::Constructor { name, args, .. } => {
                assert_eq!(name, "next");
                assert_eq!(args.len(), 1);
            }
            _ => panic!("Expected successor constructor"),
        }
    }
}