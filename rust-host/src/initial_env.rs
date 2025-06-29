// ============================================================================
// INITIAL ENVIRONMENT - BUILTIN TYPES AND CONSTRUCTORS
// ============================================================================
// Provides the minimal set of types and constructors needed to bootstrap

use crate::hott_values::HottValue;
use std::collections::HashMap;

/// Type representation for initial environment
#[derive(Debug, Clone)]
pub struct TypeDef {
    pub name: String,
    pub universe_level: u32,
    pub constructors: Vec<ConstructorDef>,
}

/// Constructor definition
#[derive(Debug, Clone)]
pub struct ConstructorDef {
    pub name: String,
    pub arg_types: Vec<String>, // Simplified: just type names
    pub result_type: String,
}

/// Initial environment builder
pub struct InitialEnvironment {
    types: HashMap<String, TypeDef>,
    constructors: HashMap<String, ConstructorDef>,
    values: HashMap<String, HottValue>,
}

impl InitialEnvironment {
    pub fn new() -> Self {
        let mut env = Self {
            types: HashMap::new(),
            constructors: HashMap::new(),
            values: HashMap::new(),
        };
        
        env.add_builtin_types();
        env.add_builtin_constructors();
        env.add_builtin_values();
        
        env
    }
    
    /// Add builtin types
    fn add_builtin_types(&mut self) {
        // Natural numbers
        self.add_type(TypeDef {
            name: "Nat".to_string(),
            universe_level: 0,
            constructors: vec![
                ConstructorDef {
                    name: "zero".to_string(),
                    arg_types: vec![],
                    result_type: "Nat".to_string(),
                },
                ConstructorDef {
                    name: "succ".to_string(),
                    arg_types: vec!["Nat".to_string()],
                    result_type: "Nat".to_string(),
                },
            ],
        });
        
        // Booleans
        self.add_type(TypeDef {
            name: "Bool".to_string(),
            universe_level: 0,
            constructors: vec![
                ConstructorDef {
                    name: "true".to_string(),
                    arg_types: vec![],
                    result_type: "Bool".to_string(),
                },
                ConstructorDef {
                    name: "false".to_string(),
                    arg_types: vec![],
                    result_type: "Bool".to_string(),
                },
            ],
        });
        
        // Unit type
        self.add_type(TypeDef {
            name: "Unit".to_string(),
            universe_level: 0,
            constructors: vec![
                ConstructorDef {
                    name: "unit".to_string(),
                    arg_types: vec![],
                    result_type: "Unit".to_string(),
                },
            ],
        });
        
        // Empty type
        self.add_type(TypeDef {
            name: "Empty".to_string(),
            universe_level: 0,
            constructors: vec![], // No constructors
        });
        
        // List type (parameterized, simplified for bootstrap)
        self.add_type(TypeDef {
            name: "List".to_string(),
            universe_level: 0,
            constructors: vec![
                ConstructorDef {
                    name: "nil".to_string(),
                    arg_types: vec![],
                    result_type: "List".to_string(),
                },
                ConstructorDef {
                    name: "cons".to_string(),
                    arg_types: vec!["A".to_string(), "List".to_string()],
                    result_type: "List".to_string(),
                },
            ],
        });
        
        // Maybe/Option type
        self.add_type(TypeDef {
            name: "Maybe".to_string(),
            universe_level: 0,
            constructors: vec![
                ConstructorDef {
                    name: "nothing".to_string(),
                    arg_types: vec![],
                    result_type: "Maybe".to_string(),
                },
                ConstructorDef {
                    name: "just".to_string(),
                    arg_types: vec!["A".to_string()],
                    result_type: "Maybe".to_string(),
                },
            ],
        });
        
        // Pair type
        self.add_type(TypeDef {
            name: "Pair".to_string(),
            universe_level: 0,
            constructors: vec![
                ConstructorDef {
                    name: "pair".to_string(),
                    arg_types: vec!["A".to_string(), "B".to_string()],
                    result_type: "Pair".to_string(),
                },
            ],
        });
        
        // String type (using Char and inductive definition)
        self.add_type(TypeDef {
            name: "Char".to_string(),
            universe_level: 0,
            constructors: vec![
                ConstructorDef {
                    name: "char".to_string(),
                    arg_types: vec!["Nat".to_string()], // Unicode codepoint
                    result_type: "Char".to_string(),
                },
            ],
        });
        
        self.add_type(TypeDef {
            name: "String".to_string(),
            universe_level: 0,
            constructors: vec![
                ConstructorDef {
                    name: "empty-string".to_string(),
                    arg_types: vec![],
                    result_type: "String".to_string(),
                },
                ConstructorDef {
                    name: "string-cons".to_string(),
                    arg_types: vec!["Char".to_string(), "String".to_string()],
                    result_type: "String".to_string(),
                },
            ],
        });
        
        // Type universe hierarchy
        for level in 0..10 {
            self.add_type(TypeDef {
                name: format!("Type{}", level),
                universe_level: level + 1,
                constructors: vec![], // Types don't have constructors
            });
        }
    }
    
    /// Add builtin constructors
    fn add_builtin_constructors(&mut self) {
        // Constructors are already added with their types
        // This method could add additional global constructors if needed
    }
    
    /// Add builtin values and functions
    fn add_builtin_values(&mut self) {
        // Add zero and one for convenience
        self.add_value("zero", self.make_nat(0));
        self.add_value("one", self.make_nat(1));
        
        // Add boolean values
        self.add_value("true", self.make_bool(true));
        self.add_value("false", self.make_bool(false));
        
        // Add unit value
        self.add_value("unit", self.make_unit());
        
        // Add empty string
        self.add_value("empty-string", self.make_empty_string());
    }
    
    /// Add a type to the environment
    fn add_type(&mut self, typedef: TypeDef) {
        // Add constructors
        for constructor in &typedef.constructors {
            self.constructors.insert(constructor.name.clone(), constructor.clone());
        }
        
        self.types.insert(typedef.name.clone(), typedef);
    }
    
    /// Add a value to the environment
    fn add_value(&mut self, name: &str, value: HottValue) {
        self.values.insert(name.to_string(), value);
    }
    
    /// Export to PathFinder environment format
    pub fn to_pathfinder_env(&self) -> HottValue {
        // Create list of bindings
        let mut bindings = vec![];
        
        // Add type bindings
        for (name, typedef) in &self.types {
            let type_value = self.make_type_value(typedef);
            bindings.push(self.make_binding(name, type_value));
        }
        
        // Add constructor bindings
        for (name, constructor) in &self.constructors {
            let cons_value = self.make_constructor_value(constructor);
            bindings.push(self.make_binding(name, cons_value));
        }
        
        // Add value bindings
        for (name, value) in &self.values {
            bindings.push(self.make_binding(name, value.clone()));
        }
        
        // Return as environment
        self.make_environment(bindings)
    }
    
    // ========================================================================
    // VALUE CONSTRUCTION HELPERS
    // ========================================================================
    
    fn make_nat(&self, n: u64) -> HottValue {
        let mut result = HottValue::Constructor {
            name: "zero".to_string(),
            args: vec![],
            value_type: crate::hott_values::HottType::Universe(0),
        };
        
        for _ in 0..n {
            result = HottValue::Constructor {
                name: "succ".to_string(),
                args: vec![result],
                value_type: crate::hott_values::HottType::Universe(0),
            };
        }
        
        result
    }
    
    fn make_bool(&self, b: bool) -> HottValue {
        HottValue::Constructor {
            name: if b { "true" } else { "false" }.to_string(),
            args: vec![],
            value_type: crate::hott_values::HottType::Universe(0),
        }
    }
    
    fn make_unit(&self) -> HottValue {
        HottValue::Constructor {
            name: "unit".to_string(),
            args: vec![],
            value_type: crate::hott_values::HottType::Universe(0),
        }
    }
    
    fn make_empty_string(&self) -> HottValue {
        HottValue::Constructor {
            name: "empty-string".to_string(),
            args: vec![],
            value_type: crate::hott_values::HottType::Universe(0),
        }
    }
    
    fn make_type_value(&self, typedef: &TypeDef) -> HottValue {
        // Simplified: represent type as constructor
        HottValue::Constructor {
            name: "type".to_string(),
            args: vec![HottValue::String(typedef.name.clone())],
            value_type: crate::hott_values::HottType::Universe(0),
        }
    }
    
    fn make_constructor_value(&self, constructor: &ConstructorDef) -> HottValue {
        // Represent constructor as its type signature
        HottValue::Constructor {
            name: "constructor".to_string(),
            args: vec![HottValue::String(constructor.name.clone())],
            value_type: crate::hott_values::HottType::Universe(0),
        }
    }
    
    fn make_binding(&self, name: &str, value: HottValue) -> HottValue {
        HottValue::Constructor {
            name: "binding".to_string(),
            args: vec![
                HottValue::String(name.to_string()),
                value,
            ],
            value_type: crate::hott_values::HottType::Universe(0),
        }
    }
    
    fn make_environment(&self, bindings: Vec<HottValue>) -> HottValue {
        // Build environment as nested extend-env constructors
        let mut env = HottValue::Constructor {
            name: "empty-env".to_string(),
            args: vec![],
            value_type: crate::hott_values::HottType::Universe(0),
        };
        
        for binding in bindings.into_iter().rev() {
            if let HottValue::Constructor { args, .. } = &binding {
                if args.len() >= 2 {
                    env = HottValue::Constructor {
                        name: "extend-env".to_string(),
                        args: vec![
                            args[0].clone(), // name
                            args[1].clone(), // value
                            env,
                        ],
                        value_type: crate::hott_values::HottType::Universe(0),
                    };
                }
            }
        }
        
        env
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_initial_environment() {
        let env = InitialEnvironment::new();
        assert!(env.types.contains_key("Nat"));
        assert!(env.types.contains_key("Bool"));
        assert!(env.constructors.contains_key("zero"));
        assert!(env.constructors.contains_key("succ"));
    }
}