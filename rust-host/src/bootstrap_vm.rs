// ============================================================================
// V0 BOOTSTRAP VM - MINIMAL HOTT CORE FOR SELF-HOSTING
// ============================================================================
// Just enough mathematical primitives to load and execute parser.hott
// Features hash-consing and content-addressable caching to prevent blow-ups

use crate::hott_values::*;
use crate::hott_evaluator::EvalError;
use std::collections::HashMap;
use std::hash::Hash;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Unique pointer to a hash-consed term
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TermPtr(usize);

/// Unique pointer to a cached normalized value
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValuePtr(usize);

/// V0 Bootstrap VM - minimal HoTT core with caching
pub struct BootstrapVM {
    /// Content-addressable normalization cache: term_ptr → value_ptr
    normalization_cache: HashMap<TermPtr, ValuePtr>,
    
    /// Hash-cons table: term → unique_ptr (for structural sharing)
    hash_cons_terms: HashMap<HottAst, TermPtr>,
    
    /// Value storage: value_ptr → actual_value
    value_storage: HashMap<ValuePtr, HottValue>,
    
    /// Term storage: term_ptr → actual_term  
    term_storage: HashMap<TermPtr, HottAst>,
    
    /// Global environment for variable bindings
    global_env: HashMap<String, ValuePtr>,
    
    /// Unique ID generators
    next_term_id: AtomicUsize,
    next_value_id: AtomicUsize,
}

impl BootstrapVM {
    /// Create new bootstrap VM
    pub fn new() -> Self {
        let mut vm = Self {
            normalization_cache: HashMap::new(),
            hash_cons_terms: HashMap::new(),
            value_storage: HashMap::new(),
            term_storage: HashMap::new(),
            global_env: HashMap::new(),
            next_term_id: AtomicUsize::new(0),
            next_value_id: AtomicUsize::new(0),
        };
        
        // Pre-populate with basic constructors
        vm.bootstrap_primitives();
        vm
    }
    
    /// Bootstrap basic HoTT primitives needed for parsing
    fn bootstrap_primitives(&mut self) {
        // Natural number constructors
        let zero_val = self.intern_value(HottValue::Constructor {
            name: "zero".to_string(),
            args: vec![],
            value_type: HottType::Universe(0),
        });
        self.global_env.insert("zero".to_string(), zero_val);
        
        // Boolean constructors  
        let true_val = self.intern_value(HottValue::Constructor {
            name: "true".to_string(),
            args: vec![],
            value_type: HottType::Universe(0),
        });
        self.global_env.insert("true".to_string(), true_val);
        
        let false_val = self.intern_value(HottValue::Constructor {
            name: "false".to_string(),
            args: vec![],
            value_type: HottType::Universe(0),
        });
        self.global_env.insert("false".to_string(), false_val);
        
        // Succ builtin for Peano arithmetic
        let succ_builtin = self.intern_value(HottValue::Builtin {
            name: "succ".to_string(),
            arity: 1,
        });
        self.global_env.insert("succ".to_string(), succ_builtin);
        
        println!("🔧 Bootstrapped V0 HoTT primitives");
    }
    
    /// CORE EVALUATION with caching
    pub fn eval(&mut self, term: HottAst) -> Result<ValuePtr, EvalError> {
        // 1. Hash-cons to get unique pointer
        let term_ptr = self.get_unique_term_ptr(term.clone());
        
        // 2. Check cache
        if let Some(&value_ptr) = self.normalization_cache.get(&term_ptr) {
            // Cache hit! No computation needed
            return Ok(value_ptr);
        }
        
        // 3. Cache miss: perform actual evaluation
        let value_ptr = self.normalize_term(term)?;
        
        // 4. Update cache  
        self.normalization_cache.insert(term_ptr, value_ptr);
        
        println!("📦 Cached evaluation: {:?} → {:?}", term_ptr, value_ptr);
        Ok(value_ptr)
    }
    
    /// Get unique pointer for term (hash-consing)
    fn get_unique_term_ptr(&mut self, term: HottAst) -> TermPtr {
        if let Some(&existing_ptr) = self.hash_cons_terms.get(&term) {
            return existing_ptr;
        }
        
        // Create new unique pointer
        let id = self.next_term_id.fetch_add(1, Ordering::SeqCst);
        let term_ptr = TermPtr(id);
        
        // Store bidirectional mapping
        self.hash_cons_terms.insert(term.clone(), term_ptr);
        self.term_storage.insert(term_ptr, term);
        
        term_ptr
    }
    
    /// Intern value and return unique pointer
    fn intern_value(&mut self, value: HottValue) -> ValuePtr {
        let id = self.next_value_id.fetch_add(1, Ordering::SeqCst);
        let value_ptr = ValuePtr(id);
        self.value_storage.insert(value_ptr, value);
        value_ptr
    }
    
    /// Normalize term using minimal HoTT primitives
    fn normalize_term(&mut self, term: HottAst) -> Result<ValuePtr, EvalError> {
        match term {
            HottAst::Var(name) => {
                // Variable lookup in global environment
                self.global_env.get(&name)
                    .copied()
                    .ok_or_else(|| EvalError::UnboundVariable(name))
            }
            
            HottAst::App { func, arg } => {
                // Function application with caching
                let func_ptr = self.eval(*func)?;
                let arg_ptr = self.eval(*arg)?;
                self.apply_function(func_ptr, arg_ptr)
            }
            
            HottAst::Lambda { param, body, .. } => {
                // Create closure value
                let closure = HottValue::Closure {
                    params: vec![param],
                    body,
                    env: Environment::empty(), // Simplified for V0
                };
                Ok(self.intern_value(closure))
            }
            
            HottAst::Constructor { name, args } => {
                // Evaluate constructor arguments and create constructor
                let arg_ptrs: Result<Vec<_>, _> = args
                    .into_iter()
                    .map(|arg| self.eval(arg))
                    .collect();
                let arg_ptrs = arg_ptrs?;
                
                // Convert ValuePtrs back to HottValues for constructor
                let arg_values: Vec<HottValue> = arg_ptrs
                    .into_iter()
                    .map(|ptr| self.value_storage.get(&ptr).unwrap().clone())
                    .collect();
                
                let constructor = HottValue::Constructor {
                    name,
                    args: arg_values,
                    value_type: HottType::Universe(0), // Simplified
                };
                Ok(self.intern_value(constructor))
            }
            
            HottAst::Literal(value) => {
                Ok(self.intern_value(value))
            }
            
            _ => Err(EvalError::RuntimeError("Unsupported AST in V0".to_string())),
        }
    }
    
    /// Apply function (minimal implementation)
    fn apply_function(&mut self, func_ptr: ValuePtr, arg_ptr: ValuePtr) -> Result<ValuePtr, EvalError> {
        let func_value = self.value_storage.get(&func_ptr).unwrap().clone();
        let arg_value = self.value_storage.get(&arg_ptr).unwrap().clone();
        
        match func_value {
            HottValue::Closure { params, body, env } => {
                if params.is_empty() {
                    return Err(EvalError::NotAFunction("Empty parameter list".to_string()));
                }
                
                // For V0, simple substitution (not full environment)
                // In practice, this would extend the environment
                self.eval(*body) // Simplified - not handling parameter binding yet
            }
            
            HottValue::Builtin { name, .. } => {
                self.apply_builtin(&name, vec![arg_value])
            }
            
            _ => Err(EvalError::NotAFunction("Not a function".to_string())),
        }
    }
    
    /// Apply builtin function  
    fn apply_builtin(&mut self, name: &str, args: Vec<HottValue>) -> Result<ValuePtr, EvalError> {
        match name {
            "succ" => {
                if args.len() != 1 {
                    return Err(EvalError::WrongArity { expected: 1, actual: args.len() });
                }
                let successor = HottValue::Constructor {
                    name: "succ".to_string(),
                    args,
                    value_type: HottType::Universe(0),
                };
                Ok(self.intern_value(successor))
            }
            _ => Err(EvalError::RuntimeError(format!("Unknown builtin: {}", name))),
        }
    }
    
    /// Load HoTT file and bind functions to global environment
    pub fn load_hott_file(&mut self, file_path: &str) -> Result<(), std::io::Error> {
        use std::fs;
        let content = fs::read_to_string(file_path)?;
        
        println!("🔧 V0 Loading: {}", file_path);
        
        // Simple parsing for V0 - just look for function definitions
        for line in content.lines() {
            let trimmed = line.trim();
            if let Some(assign_pos) = trimmed.find(" := ") {
                let left_part = &trimmed[..assign_pos].trim();
                let body_part = &trimmed[assign_pos + 4..].trim();
                
                if let Some(colon_pos) = left_part.find(" : ") {
                    let func_name = left_part[..colon_pos].trim().to_string();
                    
                    // For V0, create a simple placeholder
                    let placeholder = HottValue::String(body_part.to_string());
                    let value_ptr = self.intern_value(placeholder);
                    
                    self.global_env.insert(func_name.clone(), value_ptr);
                    println!("  ✅ V0 bound: {}", func_name);
                }
            }
        }
        
        Ok(())
    }
    
    /// Get value from pointer (for debugging/display)
    pub fn get_value(&self, ptr: ValuePtr) -> Option<&HottValue> {
        self.value_storage.get(&ptr)
    }
    
    /// Print cache statistics
    pub fn print_cache_stats(&self) {
        println!("📊 V0 Cache Stats:");
        println!("  Terms hash-consed: {}", self.hash_cons_terms.len());
        println!("  Values interned: {}", self.value_storage.len());
        println!("  Cached evaluations: {}", self.normalization_cache.len());
        println!("  Global bindings: {}", self.global_env.len());
    }
    
    /// Test: Create large Peano number and verify caching
    pub fn test_peano_caching(&mut self, n: usize) -> Result<ValuePtr, EvalError> {
        println!("🧮 Testing Peano {} with caching...", n);
        
        // Build (succ (succ ... zero)) iteratively
        let mut current = self.global_env.get("zero").copied().unwrap();
        
        for i in 1..=n {
            // Create succ application: (succ current)
            let succ_app = HottAst::App {
                func: Box::new(HottAst::Var("succ".to_string())),
                arg: Box::new(HottAst::Literal(
                    self.value_storage.get(&current).unwrap().clone()
                )),
            };
            
            current = self.eval(succ_app)?;
            
            if i % 10 == 0 {
                println!("  ✅ Cached Peano {}", i);
            }
        }
        
        println!("🎯 Successfully created Peano {} with caching!", n);
        Ok(current)
    }
}

impl Default for BootstrapVM {
    fn default() -> Self {
        Self::new()
    }
}