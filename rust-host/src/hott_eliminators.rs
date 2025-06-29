// ============================================================================
// HOTT ELIMINATORS - RUST VM EXECUTION OF MATHEMATICAL ELIMINATORS
// ============================================================================
// This module implements the Rust VM execution of HoTT eliminators.
// The VM doesn't understand the mathematical meaning - it just follows the rules.

use crate::hott_values::*;
use std::collections::HashMap;

/// Eliminator execution errors
#[derive(Debug, thiserror::Error)]
pub enum EliminatorError {
    #[error("Invalid eliminator target: expected constructor value")]
    InvalidTarget,
    #[error("Unknown constructor: {0}")]
    UnknownConstructor(String),
    #[error("Wrong number of cases: expected {expected}, got {actual}")]
    WrongCaseCount { expected: usize, actual: usize },
    #[error("Case function application failed: {0}")]
    CaseApplicationFailed(String),
    #[error("Type error in eliminator: {0}")]
    TypeError(String),
    #[error("Evaluation error in eliminator: {0}")]
    EvalError(String),
}

// Convert from EvalError to EliminatorError
impl From<crate::hott_evaluator::EvalError> for EliminatorError {
    fn from(err: crate::hott_evaluator::EvalError) -> Self {
        EliminatorError::EvalError(err.to_string())
    }
}

/// The core eliminator execution engine
pub struct EliminatorEngine {
    /// Cache for eliminator results
    cache: HashMap<String, HottValue>,
}

impl EliminatorEngine {
    /// Create new eliminator engine
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
        }
    }
    
    /// Execute value eliminator - the fundamental eliminator for all values
    pub fn execute_value_eliminator(
        &mut self,
        target: &HottValue,
        cases: &ValueEliminatorCases,
    ) -> Result<HottValue, EliminatorError> {
        match target {
            HottValue::Constructor { name, args, value_type } => {
                (cases.constructor_case)(name.clone(), args.clone(), value_type.clone())
            }
            
            HottValue::Closure { params, body, env } => {
                (cases.closure_case)(params.clone(), (**body).clone(), env.clone())
            }
            
            HottValue::Builtin { name, arity } => {
                (cases.builtin_case)(name.clone(), *arity)
            }
            
            HottValue::Unit => (cases.unit_case)(),
            
            HottValue::String(s) => (cases.string_case)(s.clone()),
            
            HottValue::Effect(effect) => (cases.effect_case)(effect.clone()),
            
            HottValue::HottFunction { name, source_body } => {
                // HoTT functions are values that can be applied, not eliminated
                Ok(HottValue::HottFunction { 
                    name: name.clone(), 
                    source_body: source_body.clone() 
                })
            }
        }
    }
    
    /// Execute constructor eliminator - for specific inductive types
    pub fn execute_constructor_eliminator(
        &mut self,
        target: &HottValue,
        cases: &HashMap<String, Box<dyn Fn(&[HottValue]) -> Result<HottValue, EliminatorError>>>,
    ) -> Result<HottValue, EliminatorError> {
        match target {
            HottValue::Constructor { name, args, .. } => {
                match cases.get(name) {
                    Some(case_fn) => case_fn(args),
                    None => Err(EliminatorError::UnknownConstructor(name.clone())),
                }
            }
            _ => Err(EliminatorError::InvalidTarget),
        }
    }
    
    /// Execute natural number eliminator: Nat-elim : (zero : R) → (next : Nat → R) → Nat → R
    pub fn execute_nat_eliminator(
        &mut self,
        target: &HottValue,
        zero_case: impl Fn() -> Result<HottValue, EliminatorError>,
        next_case: impl Fn(&HottValue) -> Result<HottValue, EliminatorError>,
    ) -> Result<HottValue, EliminatorError> {
        match target {
            HottValue::Constructor { name, args, .. } => {
                match name.as_str() {
                    "zero" => zero_case(),
                    "next" => {
                        if args.len() == 1 {
                            next_case(&args[0])
                        } else {
                            Err(EliminatorError::TypeError(
                                "next constructor requires exactly one argument".to_string()
                            ))
                        }
                    }
                    _ => Err(EliminatorError::UnknownConstructor(name.clone())),
                }
            }
            _ => Err(EliminatorError::InvalidTarget),
        }
    }
    
    /// Execute boolean eliminator: Bool-elim : (true : R) → (false : R) → Bool → R  
    pub fn execute_bool_eliminator(
        &mut self,
        target: &HottValue,
        true_case: impl Fn() -> Result<HottValue, EliminatorError>,
        false_case: impl Fn() -> Result<HottValue, EliminatorError>,
    ) -> Result<HottValue, EliminatorError> {
        match target {
            HottValue::Constructor { name, .. } => {
                match name.as_str() {
                    "true" => true_case(),
                    "false" => false_case(),
                    _ => Err(EliminatorError::UnknownConstructor(name.clone())),
                }
            }
            _ => Err(EliminatorError::InvalidTarget),
        }
    }
    
    /// Execute list eliminator: List-elim : (nil : R) → (cons : A → List A → R) → List A → R
    pub fn execute_list_eliminator(
        &mut self,
        target: &HottValue,
        nil_case: impl Fn() -> Result<HottValue, EliminatorError>,
        cons_case: impl Fn(&HottValue, &HottValue) -> Result<HottValue, EliminatorError>,
    ) -> Result<HottValue, EliminatorError> {
        match target {
            HottValue::Constructor { name, args, .. } => {
                match name.as_str() {
                    "nil" => nil_case(),
                    "cons" => {
                        if args.len() == 2 {
                            cons_case(&args[0], &args[1])
                        } else {
                            Err(EliminatorError::TypeError(
                                "cons constructor requires exactly two arguments".to_string()
                            ))
                        }
                    }
                    _ => Err(EliminatorError::UnknownConstructor(name.clone())),
                }
            }
            _ => Err(EliminatorError::InvalidTarget),
        }
    }
    
    /// Execute effect eliminator for pure HoTT effects
    pub fn execute_effect_eliminator(
        &mut self,
        target: &EffectDescription,
        cases: &EffectEliminatorCases,
    ) -> Result<HottValue, EliminatorError> {
        match target {
            EffectDescription::Pure(value) => (cases.pure_case)((**value).clone()),
            
            EffectDescription::IO { operation, args, deterministic } => {
                (cases.io_case)(operation.clone(), args.clone(), *deterministic)
            }
            
            EffectDescription::Sequence(first, second) => {
                (cases.sequence_case)((**first).clone(), (**second).clone())
            }
            
            EffectDescription::Parallel(first, second) => {
                (cases.parallel_case)((**first).clone(), (**second).clone())
            }
            
            EffectDescription::Choice(first, second) => {
                (cases.choice_case)((**first).clone(), (**second).clone())
            }
        }
    }
    
    /// Execute AST eliminator for HoTT AST interpretation
    pub fn execute_ast_eliminator(
        &mut self,
        target: &HottAst,
        cases: &AstEliminatorCases,
    ) -> Result<HottValue, EliminatorError> {
        match target {
            HottAst::Var(name) => (cases.var_case)(name.clone()),
            
            HottAst::App { func, arg } => {
                (cases.app_case)((**func).clone(), (**arg).clone())
            }
            
            HottAst::Lambda { param, body, .. } => {
                (cases.lambda_case)(param.clone(), (**body).clone())
            }
            
            HottAst::PiType { var_name, domain, codomain } => {
                (cases.pi_type_case)(var_name.clone(), (**domain).clone(), (**codomain).clone())
            }
            
            HottAst::Eliminator { target, cases: case_asts } => {
                (cases.eliminator_case)((**target).clone(), case_asts.clone())
            }
            
            HottAst::TypeApp { type_name, args } => {
                (cases.type_app_case)(type_name.clone(), args.clone())
            }
            
            HottAst::Constructor { name, args } => {
                (cases.constructor_case)(name.clone(), args.clone())
            }
            
            HottAst::Literal(value) => (cases.literal_case)(value.clone()),
            
            HottAst::Effect(effect) => (cases.effect_case)(effect.clone()),
            
            HottAst::SigmaType { var_name, first_type, second_type } => {
                // TODO: Add sigma type case to eliminator interface
                Ok(HottValue::constructor(
                    "sigma-type-placeholder".to_string(),
                    vec![HottValue::String(var_name.clone())],
                    HottType::Universe(1),
                ))
            }
            
            HottAst::IdType { type_expr, left, right } => {
                // TODO: Add id type case to eliminator interface
                Ok(HottValue::constructor(
                    "id-type-placeholder".to_string(),
                    vec![],
                    HottType::Universe(0),
                ))
            }
            
            HottAst::Universe(level) => {
                Ok(HottValue::universe(*level))
            }
            
            HottAst::Pair { first, second } => {
                // TODO: Add pair case to eliminator interface
                Ok(HottValue::constructor(
                    "pair-placeholder".to_string(),
                    vec![],
                    HottType::Inductive {
                        name: "Sigma".to_string(),
                        constructors: vec![],
                    },
                ))
            }
            
            HottAst::DigitSequence(digits) => {
                // DigitSequence is always a literal value for parsing
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
                // TODO: Add let case to eliminator interface
                Ok(HottValue::constructor(
                    "let-placeholder".to_string(),
                    vec![HottValue::String(var.clone())],
                    HottType::Universe(0),
                ))
            }
        }
    }
}

/// Case functions for value eliminator
pub struct ValueEliminatorCases {
    pub constructor_case: Box<dyn Fn(String, Vec<HottValue>, HottType) -> Result<HottValue, EliminatorError>>,
    pub closure_case: Box<dyn Fn(Vec<String>, HottAst, Environment) -> Result<HottValue, EliminatorError>>,
    pub builtin_case: Box<dyn Fn(String, usize) -> Result<HottValue, EliminatorError>>,
    pub unit_case: Box<dyn Fn() -> Result<HottValue, EliminatorError>>,
    pub string_case: Box<dyn Fn(String) -> Result<HottValue, EliminatorError>>,
    pub effect_case: Box<dyn Fn(EffectDescription) -> Result<HottValue, EliminatorError>>,
}

/// Case functions for effect eliminator
pub struct EffectEliminatorCases {
    pub pure_case: Box<dyn Fn(HottValue) -> Result<HottValue, EliminatorError>>,
    pub io_case: Box<dyn Fn(String, Vec<HottValue>, bool) -> Result<HottValue, EliminatorError>>,
    pub sequence_case: Box<dyn Fn(EffectDescription, EffectDescription) -> Result<HottValue, EliminatorError>>,
    pub parallel_case: Box<dyn Fn(EffectDescription, EffectDescription) -> Result<HottValue, EliminatorError>>,
    pub choice_case: Box<dyn Fn(EffectDescription, EffectDescription) -> Result<HottValue, EliminatorError>>,
}

/// Case functions for AST eliminator
pub struct AstEliminatorCases {
    pub var_case: Box<dyn Fn(String) -> Result<HottValue, EliminatorError>>,
    pub app_case: Box<dyn Fn(HottAst, HottAst) -> Result<HottValue, EliminatorError>>,
    pub lambda_case: Box<dyn Fn(String, HottAst) -> Result<HottValue, EliminatorError>>,
    pub pi_type_case: Box<dyn Fn(String, HottAst, HottAst) -> Result<HottValue, EliminatorError>>,
    pub eliminator_case: Box<dyn Fn(HottAst, Vec<HottAst>) -> Result<HottValue, EliminatorError>>,
    pub type_app_case: Box<dyn Fn(String, Vec<HottAst>) -> Result<HottValue, EliminatorError>>,
    pub constructor_case: Box<dyn Fn(String, Vec<HottAst>) -> Result<HottValue, EliminatorError>>,
    pub literal_case: Box<dyn Fn(HottValue) -> Result<HottValue, EliminatorError>>,
    pub effect_case: Box<dyn Fn(EffectDescription) -> Result<HottValue, EliminatorError>>,
}

// ============================================================================
// ELIMINATOR UTILITIES
// ============================================================================

impl EliminatorEngine {
    /// Helper: Convert Rust number to HoTT natural number
    pub fn rust_number_to_hott_nat(n: usize) -> HottValue {
        if n == 0 {
            HottValue::zero()
        } else {
            HottValue::succ(Self::rust_number_to_hott_nat(n - 1))
        }
    }
    
    /// Helper: Convert HoTT natural number to Rust number
    pub fn hott_nat_to_rust_number(nat: &HottValue) -> Result<usize, EliminatorError> {
        match nat {
            HottValue::Constructor { name, args, .. } => {
                match name.as_str() {
                    "zero" => Ok(0),
                    "next" => {
                        if args.len() == 1 {
                            let pred_num = Self::hott_nat_to_rust_number(&args[0])?;
                            Ok(pred_num + 1)
                        } else {
                            Err(EliminatorError::TypeError(
                                "invalid next constructor".to_string()
                            ))
                        }
                    }
                    _ => Err(EliminatorError::UnknownConstructor(name.clone())),
                }
            }
            _ => Err(EliminatorError::InvalidTarget),
        }
    }
    
    /// Helper: Check if HoTT value is true
    pub fn is_hott_true(val: &HottValue) -> bool {
        matches!(val, HottValue::Constructor { name, .. } if name == "true")
    }
    
    /// Helper: Check if HoTT value is false
    pub fn is_hott_false(val: &HottValue) -> bool {
        matches!(val, HottValue::Constructor { name, .. } if name == "false")
    }
}

impl Default for EliminatorEngine {
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
    fn test_nat_eliminator() {
        let mut engine = EliminatorEngine::new();
        
        // Test zero case
        let zero = HottValue::zero();
        let result = engine.execute_nat_eliminator(
            &zero,
            || Ok(HottValue::String("zero!".to_string())),
            |_| Ok(HottValue::String("next!".to_string())),
        ).unwrap();
        
        assert_eq!(result, HottValue::String("zero!".to_string()));
        
        // Test successor case
        let one = HottValue::succ(HottValue::zero());
        let result = engine.execute_nat_eliminator(
            &one,
            || Ok(HottValue::String("zero!".to_string())),
            |pred| Ok(HottValue::String(format!("next of {}", pred.constructor_name().unwrap_or("unknown")))),
        ).unwrap();
        
        assert_eq!(result, HottValue::String("next of zero".to_string()));
    }
    
    #[test]
    fn test_bool_eliminator() {
        let mut engine = EliminatorEngine::new();
        
        // Test true case
        let true_val = HottValue::true_value();
        let result = engine.execute_bool_eliminator(
            &true_val,
            || Ok(HottValue::String("yes!".to_string())),
            || Ok(HottValue::String("no!".to_string())),
        ).unwrap();
        
        assert_eq!(result, HottValue::String("yes!".to_string()));
        
        // Test false case
        let false_val = HottValue::false_value();
        let result = engine.execute_bool_eliminator(
            &false_val,
            || Ok(HottValue::String("yes!".to_string())),
            || Ok(HottValue::String("no!".to_string())),
        ).unwrap();
        
        assert_eq!(result, HottValue::String("no!".to_string()));
    }
    
    #[test]
    fn test_number_conversion() {
        // Test Rust to HoTT
        let hott_three = EliminatorEngine::rust_number_to_hott_nat(3);
        
        // Test HoTT to Rust
        let rust_three = EliminatorEngine::hott_nat_to_rust_number(&hott_three).unwrap();
        assert_eq!(rust_three, 3);
    }
}