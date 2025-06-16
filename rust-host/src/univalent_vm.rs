// ============================================================================
// PURE UNIVALENT FOUNDATIONS VM
// ============================================================================
// Minimal mathematical substrate - only construction, elimination, I/O
// All intelligence lives in HoTT mathematics, not in the VM

use crate::hott_values::*;
use std::fs;
use std::io::{self, Write};

/// Pure Univalent Foundations VM - mathematical substrate only
pub struct UnivalentVM {
    /// I/O handler for interaction with host system
    io: IOHandler,
}

/// I/O operations - the only non-mathematical part of the VM
pub struct IOHandler {
    /// Standard output
    stdout: Box<dyn Write>,
}

impl UnivalentVM {
    /// Create new pure univalent VM
    pub fn new() -> Self {
        Self {
            io: IOHandler::new(),
        }
    }
    
    /// CORE OPERATION 1: Constructor
    /// Creates HoTT values using mathematical constructors
    pub fn construct(&self, name: String, args: Vec<HottValue>, value_type: HottType) -> HottValue {
        HottValue::Constructor {
            name,
            args,
            value_type,
        }
    }
    
    /// CORE OPERATION 2: Eliminator  
    /// Pattern matches/destructs HoTT values using mathematical eliminators
    pub fn eliminate(&self, target: HottValue, cases: Vec<HottValue>) -> Result<HottValue, EliminatorError> {
        match target {
            // Natural number eliminator: ℕ-elim(n, zero-case, succ-case)
            HottValue::Constructor { name, args, .. } if name == "zero" => {
                // Apply zero case
                if let Some(zero_case) = cases.get(0) {
                    Ok(zero_case.clone())
                } else {
                    Err(EliminatorError::MissingCase("zero".to_string()))
                }
            }
            
            HottValue::Constructor { name, args, .. } if name == "succ" => {
                // Apply successor case to predecessor
                if let (Some(succ_case), Some(pred)) = (cases.get(1), args.get(0)) {
                    // succ-case should be a function: pred → ℕ-elim(pred, ...) → result
                    self.apply_function(succ_case.clone(), pred.clone())
                } else {
                    Err(EliminatorError::MissingCase("succ".to_string()))
                }
            }
            
            // Boolean eliminator: 𝟚-elim(b, true-case, false-case)
            HottValue::Constructor { name, .. } if name == "true" => {
                if let Some(true_case) = cases.get(0) {
                    Ok(true_case.clone())
                } else {
                    Err(EliminatorError::MissingCase("true".to_string()))
                }
            }
            
            HottValue::Constructor { name, .. } if name == "false" => {
                if let Some(false_case) = cases.get(1) {
                    Ok(false_case.clone())
                } else {
                    Err(EliminatorError::MissingCase("false".to_string()))
                }
            }
            
            // Function application eliminator: (λx.body arg) reduces to body[x:=arg]
            HottValue::Closure { params, body, env } => {
                if let Some(arg) = cases.get(0) {
                    self.apply_lambda(params, *body, env, arg.clone())
                } else {
                    Err(EliminatorError::MissingCase("function application".to_string()))
                }
            }
            
            // Generic constructor elimination
            HottValue::Constructor { name, args, .. } => {
                // Find matching case by constructor name
                // For now, just return first case
                if let Some(case) = cases.get(0) {
                    // Apply case to constructor arguments
                    args.into_iter().try_fold(case.clone(), |acc, arg| {
                        self.apply_function(acc, arg)
                    })
                } else {
                    Err(EliminatorError::MissingCase(name))
                }
            }
            
            _ => Err(EliminatorError::InvalidTarget("Cannot eliminate this value".to_string())),
        }
    }
    
    /// Function application via elimination
    fn apply_function(&self, func: HottValue, arg: HottValue) -> Result<HottValue, EliminatorError> {
        match func {
            HottValue::Closure { params, body, env } => {
                self.apply_lambda(params, *body, env, arg)
            }
            HottValue::Builtin { name, .. } => {
                // Apply builtin function
                self.apply_builtin(&name, vec![arg])
            }
            _ => Err(EliminatorError::InvalidTarget("Not a function".to_string())),
        }
    }
    
    /// Lambda application: (λx.body arg) → body[x:=arg]
    fn apply_lambda(&self, params: Vec<String>, body: HottAst, env: Environment, arg: HottValue) -> Result<HottValue, EliminatorError> {
        if params.is_empty() {
            return Err(EliminatorError::InvalidTarget("Lambda has no parameters".to_string()));
        }
        
        let param = &params[0];
        let remaining_params = &params[1..];
        
        // Substitute argument for parameter in environment
        let extended_env = env.extend(param.clone(), arg);
        
        if remaining_params.is_empty() {
            // Last parameter: evaluate body in extended environment
            self.evaluate_in_env(body, extended_env)
        } else {
            // More parameters: return partial application
            Ok(HottValue::closure(remaining_params.to_vec(), body, extended_env))
        }
    }
    
    /// Evaluate AST in environment (minimal evaluation)
    fn evaluate_in_env(&self, ast: HottAst, env: Environment) -> Result<HottValue, EliminatorError> {
        match ast {
            HottAst::Var(name) => {
                env.lookup(&name)
                    .cloned()
                    .ok_or_else(|| EliminatorError::InvalidTarget(format!("Unbound variable: {}", name)))
            }
            
            HottAst::Lambda { param, body, .. } => {
                Ok(HottValue::closure(vec![param], *body, env))
            }
            
            HottAst::App { func, arg } => {
                let func_val = self.evaluate_in_env(*func, env.clone())?;
                let arg_val = self.evaluate_in_env(*arg, env)?;
                self.apply_function(func_val, arg_val)
            }
            
            HottAst::Constructor { name, args } => {
                let arg_vals: Result<Vec<_>, _> = args
                    .into_iter()
                    .map(|arg| self.evaluate_in_env(arg, env.clone()))
                    .collect();
                    
                Ok(self.construct(name, arg_vals?, HottType::Universe(0)))
            }
            
            HottAst::Literal(value) => Ok(value),
            
            _ => Err(EliminatorError::InvalidTarget("Unsupported AST node".to_string())),
        }
    }
    
    /// Apply builtin function (minimal set)
    fn apply_builtin(&self, name: &str, args: Vec<HottValue>) -> Result<HottValue, EliminatorError> {
        match name {
            "zero" => Ok(self.construct("zero".to_string(), vec![], HottType::Nat)),
            "succ" => {
                if args.len() == 1 {
                    Ok(self.construct("succ".to_string(), args, HottType::Nat))
                } else {
                    Err(EliminatorError::WrongArity { expected: 1, actual: args.len() })
                }
            }
            "true" => Ok(self.construct("true".to_string(), vec![], HottType::Bool)),
            "false" => Ok(self.construct("false".to_string(), vec![], HottType::Bool)),
            _ => Err(EliminatorError::InvalidTarget(format!("Unknown builtin: {}", name))),
        }
    }
    
    /// CORE OPERATION 3: I/O - Print value
    pub fn print(&mut self, value: HottValue) -> Result<(), IOError> {
        let output = self.format_value(value);
        self.io.print(&output)
    }
    
    /// CORE OPERATION 4: I/O - Read file
    pub fn read_file(&self, path: &str) -> Result<HottValue, IOError> {
        let content = fs::read_to_string(path)
            .map_err(|e| IOError::FileError(e.to_string()))?;
        Ok(HottValue::String(content))
    }
    
    /// Format HoTT value for display
    fn format_value(&self, value: HottValue) -> String {
        match value {
            HottValue::Constructor { name, args, .. } => {
                if args.is_empty() {
                    name
                } else {
                    format!("({} {})", name, 
                        args.iter()
                            .map(|arg| self.format_value(arg.clone()))
                            .collect::<Vec<_>>()
                            .join(" "))
                }
            }
            HottValue::String(s) => format!("\"{}\"", s),
            HottValue::Unit => "⋆".to_string(),
            HottValue::Closure { params, .. } => {
                format!("λ{}.⟨body⟩", params.join(" "))
            }
            HottValue::Builtin { name, .. } => {
                format!("#⟨{}⟩", name)
            }
            HottValue::Effect(effect) => {
                format!("⟨effect:{:?}⟩", effect)
            }
            HottValue::HottFunction { name, .. } => {
                format!("⟨hott-fn:{}⟩", name)
            }
        }
    }
}

impl IOHandler {
    fn new() -> Self {
        Self {
            stdout: Box::new(io::stdout()),
        }
    }
    
    fn print(&mut self, text: &str) -> Result<(), IOError> {
        writeln!(self.stdout, "{}", text)
            .map_err(|e| IOError::WriteError(e.to_string()))?;
        self.stdout.flush()
            .map_err(|e| IOError::WriteError(e.to_string()))?;
        Ok(())
    }
}

/// Eliminator errors
#[derive(Debug, thiserror::Error)]
pub enum EliminatorError {
    #[error("Missing case for: {0}")]
    MissingCase(String),
    #[error("Invalid elimination target: {0}")]
    InvalidTarget(String),
    #[error("Wrong arity: expected {expected}, got {actual}")]
    WrongArity { expected: usize, actual: usize },
}

/// I/O errors
#[derive(Debug, thiserror::Error)]
pub enum IOError {
    #[error("File error: {0}")]
    FileError(String),
    #[error("Write error: {0}")]
    WriteError(String),
}

impl Default for UnivalentVM {
    fn default() -> Self {
        Self::new()
    }
}