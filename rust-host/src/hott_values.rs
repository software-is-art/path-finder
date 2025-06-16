// ============================================================================
// HOTT VALUES - RUST REPRESENTATION OF HOTT MATHEMATICAL OBJECTS
// ============================================================================
// This module defines Rust structs that mirror HoTT constructor values.
// The Rust VM manipulates these structures but doesn't understand their meaning.

use std::collections::HashMap;
use std::sync::Arc;
use serde::{Deserialize, Serialize};

/// Core HoTT Value type - mirrors the HoTT value hierarchy exactly
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum HottValue {
    /// Constructor values: (constructor-name args type)
    Constructor {
        name: String,
        args: Vec<HottValue>,
        value_type: HottType,
    },
    
    /// Closure values: function closures with captured environment
    Closure {
        params: Vec<String>,
        body: Box<HottAst>,
        env: Environment,
    },
    
    /// Builtin values: primitive operations
    Builtin {
        name: String,
        arity: usize,
    },
    
    /// Unit value: the unique inhabitant of unit type
    Unit,
    
    /// String values: for I/O and debugging
    String(String),
    
    /// Effect values: effect descriptions for the host to execute
    Effect(EffectDescription),
    
    /// HoTT Function: loaded from .hott files, executed via eliminators
    HottFunction {
        name: String,
        source_body: Box<HottAst>,
    },
}

/// HoTT Type representation
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum HottType {
    /// Universe types: Type₀, Type₁, etc.
    Universe(usize),
    
    /// Π-types: dependent function types
    Pi {
        var_name: String,
        domain: Box<HottType>,
        codomain: Box<HottType>,
    },
    
    /// Inductive types: user-defined types with constructors
    Inductive {
        name: String,
        constructors: Vec<TypeConstructor>,
    },
    
    /// Effect types: types with effect requirements
    Effect {
        base_type: Box<HottType>,
        required_effects: Vec<String>,
    },
}

/// Type constructor for inductive types
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TypeConstructor {
    pub name: String,
    pub param_types: Vec<HottType>,
    pub result_type: String,
}

/// HoTT AST representation - mirrors the HoTT AST exactly
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum HottAst {
    /// Variable reference
    Var(String),
    
    /// Function application
    App {
        func: Box<HottAst>,
        arg: Box<HottAst>,
    },
    
    /// Lambda abstraction with optional type annotation
    Lambda {
        param: String,
        param_type: Option<Box<HottAst>>,
        body: Box<HottAst>,
    },
    
    /// Pi type expression (dependent function)
    PiType {
        var_name: String,
        domain: Box<HottAst>,
        codomain: Box<HottAst>,
    },
    
    /// Sigma type expression (dependent pair)
    SigmaType {
        var_name: String,
        first_type: Box<HottAst>,
        second_type: Box<HottAst>,
    },
    
    /// Identity type
    IdType {
        type_expr: Box<HottAst>,
        left: Box<HottAst>,
        right: Box<HottAst>,
    },
    
    /// Universe level
    Universe(usize),
    
    /// Eliminator call
    Eliminator {
        target: Box<HottAst>,
        cases: Vec<HottAst>,
    },
    
    /// Type application
    TypeApp {
        type_name: String,
        args: Vec<HottAst>,
    },
    
    /// Constructor application
    Constructor {
        name: String,
        args: Vec<HottAst>,
    },
    
    /// Dependent pair constructor
    Pair {
        first: Box<HottAst>,
        second: Box<HottAst>,
    },
    
    /// Literal value
    Literal(HottValue),
    
    /// Effect expression
    Effect(EffectDescription),
    
    /// Let expression: let var := value in body
    Let {
        var: String,
        value: Box<HottAst>,
        body: Box<HottAst>,
    },
    
    /// Digit sequence for number parsing
    DigitSequence(Vec<u8>),
}

/// Top-level declarations in HoTT modules
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum HottDeclaration {
    /// Data type declaration: data T : U where constructors
    DataDecl {
        name: String,
        type_expr: HottAst,
        constructors: Vec<HottConstructor>,
    },
    
    /// Function definition with optional type signature: f : T := body
    FunctionDef {
        name: String,
        type_sig: Option<HottAst>,
        body: HottAst,
    },
    
    /// Type signature only: f : T
    TypeSig {
        name: String,
        type_expr: HottAst,
    },
}

/// Constructor for inductive types in declarations
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct HottConstructor {
    pub name: String,
    pub type_expr: HottAst,
}

/// Complete HoTT module
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct HottModule {
    pub declarations: Vec<HottDeclaration>,
    pub imports: Vec<String>,
    pub exports: Vec<String>,
}

/// Environment for variable bindings
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Environment {
    Empty,
    Extended {
        name: String,
        value: Box<HottValue>,
        parent: Box<Environment>,
    },
}

/// Effect descriptions - what the host VM needs to execute
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum EffectDescription {
    /// Pure effect: just return the value
    Pure(Box<HottValue>),
    
    /// I/O effect: operation for host to execute
    IO {
        operation: String,
        args: Vec<HottValue>,
        deterministic: bool,
    },
    
    /// Sequential composition of effects
    Sequence(Box<EffectDescription>, Box<EffectDescription>),
    
    /// Parallel composition of effects
    Parallel(Box<EffectDescription>, Box<EffectDescription>),
    
    /// Choice between effects
    Choice(Box<EffectDescription>, Box<EffectDescription>),
}

/// Evaluation context
#[derive(Debug, Clone)]
pub struct EvaluationContext {
    pub environment: Environment,
    pub effect_context: EffectContext,
    pub cache: Arc<std::sync::Mutex<HashMap<String, HottValue>>>,
}

/// Effect execution context
#[derive(Debug, Clone)]
pub struct EffectContext {
    pub handlers: HashMap<String, EffectHandler>,
}

/// Effect handler definition
#[derive(Debug, Clone)]
pub struct EffectHandler {
    pub effect_name: String,
    pub handler_type: HandlerType,
    pub implementation: fn(&[HottValue]) -> Result<HottValue, EffectError>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HandlerType {
    CompileTime,
    Runtime,
    Capability,
}

/// Errors
#[derive(Debug, thiserror::Error)]
pub enum EffectError {
    #[error("Unknown effect: {0}")]
    UnknownEffect(String),
    #[error("Effect execution failed: {0}")]
    ExecutionFailed(String),
    #[error("Invalid arguments for effect: {0}")]
    InvalidArguments(String),
}

// ============================================================================
// IMPLEMENTATIONS
// ============================================================================

impl HottValue {
    /// Check if this is a constructor value
    pub fn is_constructor(&self) -> bool {
        matches!(self, HottValue::Constructor { .. })
    }
    
    /// Get constructor name if this is a constructor
    pub fn constructor_name(&self) -> Option<&str> {
        match self {
            HottValue::Constructor { name, .. } => Some(name),
            _ => None,
        }
    }
    
    /// Get constructor arguments if this is a constructor
    pub fn constructor_args(&self) -> Option<&[HottValue]> {
        match self {
            HottValue::Constructor { args, .. } => Some(args),
            _ => None,
        }
    }
    
    /// Create a constructor value
    pub fn constructor(name: String, args: Vec<HottValue>, value_type: HottType) -> Self {
        HottValue::Constructor {
            name,
            args,
            value_type,
        }
    }
    
    /// Create a closure value
    pub fn closure(params: Vec<String>, body: HottAst, env: Environment) -> Self {
        HottValue::Closure {
            params,
            body: Box::new(body),
            env,
        }
    }
    
    // ============================================================================
    // BUILT-IN HOTT CONSTRUCTORS
    // ============================================================================
    
    /// Natural number zero constructor
    pub fn zero() -> Self {
        HottValue::Constructor {
            name: "zero".to_string(),
            args: vec![],
            value_type: HottType::Inductive {
                name: "ℕ".to_string(),
                constructors: vec![],
            },
        }
    }
    
    /// Natural number successor constructor  
    pub fn succ(pred: HottValue) -> Self {
        HottValue::Constructor {
            name: "succ".to_string(),
            args: vec![pred],
            value_type: HottType::Inductive {
                name: "ℕ".to_string(),
                constructors: vec![],
            },
        }
    }
    
    /// Successor constructor function (for built-ins)
    pub fn succ_constructor() -> Self {
        HottValue::Builtin {
            name: "succ".to_string(),
            arity: 1,
        }
    }
    
    /// Boolean true constructor
    pub fn true_value() -> Self {
        HottValue::Constructor {
            name: "₁".to_string(),
            args: vec![],
            value_type: HottType::Inductive {
                name: "𝟚".to_string(),
                constructors: vec![],
            },
        }
    }
    
    /// Boolean false constructor
    pub fn false_value() -> Self {
        HottValue::Constructor {
            name: "₀".to_string(),
            args: vec![],
            value_type: HottType::Inductive {
                name: "𝟚".to_string(),
                constructors: vec![],
            },
        }
    }
    
    /// Natural number type
    pub fn nat_type() -> Self {
        HottValue::Constructor {
            name: "ℕ".to_string(),
            args: vec![],
            value_type: HottType::Universe(0),
        }
    }
    
    /// Boolean type (2-type)
    pub fn bool_type() -> Self {
        HottValue::Constructor {
            name: "𝟚".to_string(),
            args: vec![],
            value_type: HottType::Universe(0),
        }
    }
    
    /// Unit type
    pub fn unit_type() -> Self {
        HottValue::Constructor {
            name: "𝟙".to_string(),
            args: vec![],
            value_type: HottType::Universe(0),
        }
    }
    
    /// Empty type
    pub fn empty_type() -> Self {
        HottValue::Constructor {
            name: "𝟘".to_string(),
            args: vec![],
            value_type: HottType::Universe(0),
        }
    }
    
    /// Universe level
    pub fn universe(level: usize) -> Self {
        HottValue::Constructor {
            name: format!("𝒰{}", level),
            args: vec![],
            value_type: HottType::Universe(level + 1),
        }
    }
}

impl Environment {
    /// Create empty environment
    pub fn empty() -> Self {
        Environment::Empty
    }
    
    /// Extend environment with new binding
    pub fn extend(self, name: String, value: HottValue) -> Self {
        Environment::Extended {
            name,
            value: Box::new(value),
            parent: Box::new(self),
        }
    }
    
    /// Look up variable in environment
    pub fn lookup(&self, name: &str) -> Option<&HottValue> {
        match self {
            Environment::Empty => None,
            Environment::Extended { name: bound_name, value, parent } => {
                if bound_name == name {
                    Some(value)
                } else {
                    parent.lookup(name)
                }
            }
        }
    }
}

impl EvaluationContext {
    /// Create new evaluation context
    pub fn new() -> Self {
        Self {
            environment: Environment::empty(),
            effect_context: EffectContext::new(),
            cache: Arc::new(std::sync::Mutex::new(HashMap::new())),
        }
    }
    
    /// Extend environment
    pub fn with_binding(mut self, name: String, value: HottValue) -> Self {
        self.environment = self.environment.extend(name, value);
        self
    }
}

impl EffectContext {
    /// Create new effect context
    pub fn new() -> Self {
        Self {
            handlers: HashMap::new(),
        }
    }
    
    /// Register effect handler
    pub fn register_handler(&mut self, handler: EffectHandler) {
        self.handlers.insert(handler.effect_name.clone(), handler);
    }
}

impl Default for EvaluationContext {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for EffectContext {
    fn default() -> Self {
        Self::new()
    }
}

