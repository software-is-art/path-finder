// ============================================================================
// HOTT FILE LOADER - TRUE DOGFOODING ARCHITECTURE  
// ============================================================================
// This loads and executes pure HoTT files as the source of truth
// Rust VM becomes minimal infrastructure: eliminators, constructors, I/O only

use crate::hott_values::*;
use crate::hott_eliminators::*;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

/// HoTT file loader and executor - the bridge to pure mathematical HoTT
pub struct HottLoader {
    /// Loaded HoTT modules
    modules: HashMap<String, HottModule>,
    /// Function registry from loaded HoTT files
    functions: HashMap<String, HottValue>,
    /// Eliminator engine for executing HoTT eliminators
    eliminator_engine: EliminatorEngine,
}

impl HottLoader {
    /// Create new HoTT loader
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            functions: HashMap::new(),
            eliminator_engine: EliminatorEngine::new(),
        }
    }
    
    /// Load core HoTT infrastructure files
    pub fn load_core_system(&mut self) -> Result<(), LoadError> {
        // Load in dependency order
        self.load_hott_file("../src/core/foundations.hott")?;
        self.load_hott_file("../src/core/eliminators.hott")?;
        self.load_hott_file("../src/core/literals.hott")?;
        self.load_hott_file("../src/parser/parser.hott")?;
        self.load_hott_file("../src/evaluator/evaluator.hott")?;
        
        println!("✅ Loaded pure mathematical HoTT core system");
        Ok(())
    }
    
    /// Load individual HoTT file
    pub fn load_hott_file(&mut self, file_path: &str) -> Result<HottModule, LoadError> {
        let content = fs::read_to_string(file_path)
            .map_err(|e| LoadError::FileNotFound(format!("{}: {}", file_path, e)))?;
            
        println!("📖 Loading HoTT file: {}", file_path);
        
        // Parse the HoTT file into AST (using basic tokenization for now)
        let module = self.parse_hott_module(&content, file_path)?;
        
        // Extract function definitions from the module
        self.extract_functions(&module)?;
        
        let module_name = Path::new(file_path)
            .file_stem()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown")
            .to_string();
            
        self.modules.insert(module_name, module.clone());
        
        Ok(module)
    }
    
    /// Parse HoTT module from source (simplified for dogfooding)
    fn parse_hott_module(&self, content: &str, file_path: &str) -> Result<HottModule, LoadError> {
        // For now, create a basic module structure
        // In full implementation, this would use the HoTT parser we loaded
        let mut declarations = Vec::new();
        let mut imports = Vec::new();
        let mut exports = Vec::new();
        
        // Simple extraction of function definitions (looking for :=)
        for (line_num, line) in content.lines().enumerate() {
            let trimmed = line.trim();
            
            // Skip comments and empty lines
            if trimmed.starts_with("--") || trimmed.is_empty() {
                continue;
            }
            
            // Import statements
            if trimmed.starts_with("import ") {
                let import_name = trimmed[7..].trim().to_string();
                imports.push(import_name);
                continue;
            }
            
            // Function definitions (name : type := body)
            if let Some(assign_pos) = trimmed.find(" := ") {
                let left_part = &trimmed[..assign_pos].trim();
                let body_part = &trimmed[assign_pos + 4..].trim();
                
                if let Some(colon_pos) = left_part.find(" : ") {
                    let func_name = left_part[..colon_pos].trim().to_string();
                    let func_type = left_part[colon_pos + 3..].trim();
                    
                    println!("  📝 Found function: {} : {}", func_name, func_type);
                    
                    // Create a placeholder AST for the function body
                    let body_ast = HottAst::Var(body_part.to_string());
                    
                    // Create function declaration
                    let declaration = HottDeclaration::FunctionDef {
                        name: func_name,
                        type_sig: Some(HottAst::Var(func_type.to_string())),
                        body: body_ast,
                    };
                    
                    declarations.push(declaration);
                }
            }
        }
        
        println!("  ✅ Parsed {} declarations from {}", declarations.len(), file_path);
        
        Ok(HottModule {
            declarations,
            imports,
            exports,
        })
    }
    
    /// Extract function definitions and make them callable
    fn extract_functions(&mut self, module: &HottModule) -> Result<(), LoadError> {
        for declaration in &module.declarations {
            match declaration {
                HottDeclaration::FunctionDef { name, body, .. } => {
                    // Create a HoTT function value that can be executed via eliminators
                    let function_value = HottValue::HottFunction {
                        name: name.clone(),
                        source_body: Box::new(body.clone()),
                    };
                    
                    self.functions.insert(name.clone(), function_value);
                    println!("  ✅ Registered HoTT function: {}", name);
                }
                _ => {
                    // Handle other declaration types as needed
                }
            }
        }
        
        Ok(())
    }
    
    /// Call a loaded HoTT function by name
    pub fn call_hott_function(&self, function_name: &str, args: Vec<HottValue>) -> Result<HottValue, LoadError> {
        let function = self.functions.get(function_name)
            .ok_or_else(|| LoadError::FunctionNotFound(function_name.to_string()))?
            .clone();
            
        // Execute the function using eliminators
        match function {
            HottValue::Closure { params: _, body: _, env: _ } => {
                // For now, simple execution
                // In full implementation, this would use the HoTT evaluator
                Ok(HottValue::constructor(
                    "function-result".to_string(),
                    args,
                    HottType::Universe(0),
                ))
            }
            _ => Err(LoadError::NotAFunction(function_name.to_string()))
        }
    }
    
    /// Get loaded function for direct access
    pub fn get_function(&self, name: &str) -> Option<&HottValue> {
        self.functions.get(name)
    }
    
    /// List all loaded functions
    pub fn list_functions(&self) -> Vec<String> {
        self.functions.keys().cloned().collect()
    }
}

/// Errors that can occur during HoTT file loading
#[derive(Debug, thiserror::Error)]
pub enum LoadError {
    #[error("File not found: {0}")]
    FileNotFound(String),
    #[error("Parse error: {0}")]
    ParseError(String),
    #[error("Function not found: {0}")]
    FunctionNotFound(String),
    #[error("Not a function: {0}")]
    NotAFunction(String),
    #[error("Import error: {0}")]
    ImportError(String),
}

impl Default for HottLoader {
    fn default() -> Self {
        Self::new()
    }
}