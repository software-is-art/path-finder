// ============================================================================
// EFFECT BRIDGE - EXECUTE PURE EFFECT DESCRIPTIONS
// ============================================================================
// Maps PathFinder's pure effect descriptions to actual I/O operations

use std::fs;
use std::path::Path;
use std::collections::HashMap;
use crate::hott_values::HottValue;

/// Effect execution result
#[derive(Debug, Clone)]
pub enum EffectResult {
    Ok(HottValue),
    Error(String),
}

/// Effect handler for executing I/O operations
pub struct EffectBridge {
    /// Working directory for relative paths
    working_dir: String,
    /// Cache for deterministic effects
    effect_cache: HashMap<EffectHash, HottValue>,
}

/// Hash for effect caching
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
struct EffectHash {
    operation: String,
    args: Vec<String>,
}

impl EffectBridge {
    pub fn new(working_dir: String) -> Self {
        Self {
            working_dir,
            effect_cache: HashMap::new(),
        }
    }

    /// Execute an effect description
    pub fn execute_effect(&mut self, effect: &HottValue) -> EffectResult {
        match effect {
            // Pure effect - just return the value
            HottValue::Constructor { name, args, .. } if name == "pure-effect" => {
                if let Some(value) = args.first() {
                    EffectResult::Ok(value.clone())
                } else {
                    EffectResult::Error("Pure effect missing value".to_string())
                }
            }
            
            // I/O effect - dispatch based on operation
            HottValue::Constructor { name, args, .. } if name == "io-effect" => {
                self.execute_io_effect(args)
            }
            
            // Sequential composition
            HottValue::Constructor { name, args, .. } if name == "effect-seq" => {
                self.execute_seq_effect(args)
            }
            
            // Parallel composition
            HottValue::Constructor { name, args, .. } if name == "effect-par" => {
                self.execute_par_effect(args)
            }
            
            // Choice
            HottValue::Constructor { name, args, .. } if name == "effect-choice" => {
                self.execute_choice_effect(args)
            }
            
            _ => EffectResult::Error("Unknown effect type".to_string()),
        }
    }

    /// Execute I/O effect
    fn execute_io_effect(&mut self, args: &[HottValue]) -> EffectResult {
        // Extract operation details
        let (category, operation, op_args, determinism) = match self.parse_io_args(args) {
            Ok(parsed) => parsed,
            Err(e) => return EffectResult::Error(e),
        };

        // Check cache for deterministic effects
        if determinism == "deterministic" {
            let hash = EffectHash {
                operation: format!("{}.{}", category, operation),
                args: op_args.iter().map(|v| format!("{:?}", v)).collect(),
            };
            
            if let Some(cached) = self.effect_cache.get(&hash) {
                return EffectResult::Ok(cached.clone());
            }
        }

        // Execute the operation
        let result = match (category.as_str(), operation.as_str()) {
            ("file", "read") => self.read_file(&op_args),
            ("file", "write") => self.write_file(&op_args),
            ("file", "exists") => self.file_exists(&op_args),
            ("print", "println") => self.print_line(&op_args),
            ("error", "throw") => EffectResult::Error(self.extract_string(&op_args[0])),
            _ => EffectResult::Error(format!("Unknown operation: {}.{}", category, operation)),
        };

        // Cache deterministic results
        if determinism == "deterministic" {
            if let EffectResult::Ok(ref value) = result {
                let hash = EffectHash {
                    operation: format!("{}.{}", category, operation),
                    args: op_args.iter().map(|v| format!("{:?}", v)).collect(),
                };
                self.effect_cache.insert(hash, value.clone());
            }
        }

        result
    }

    /// Read file contents
    fn read_file(&self, args: &[HottValue]) -> EffectResult {
        if args.is_empty() {
            return EffectResult::Error("read-file requires path argument".to_string());
        }

        let path = self.extract_string(&args[0]);
        let full_path = self.resolve_path(&path);

        match fs::read_to_string(&full_path) {
            Ok(content) => EffectResult::Ok(self.make_string_value(content)),
            Err(e) => EffectResult::Error(format!("Failed to read {}: {}", full_path, e)),
        }
    }

    /// Write file contents
    fn write_file(&self, args: &[HottValue]) -> EffectResult {
        if args.len() < 2 {
            return EffectResult::Error("write-file requires path and content".to_string());
        }

        let path = self.extract_string(&args[0]);
        let content = self.extract_string(&args[1]);
        let full_path = self.resolve_path(&path);

        // Create parent directories if needed
        if let Some(parent) = Path::new(&full_path).parent() {
            if let Err(e) = fs::create_dir_all(parent) {
                return EffectResult::Error(format!("Failed to create directories: {}", e));
            }
        }

        match fs::write(&full_path, content) {
            Ok(_) => EffectResult::Ok(self.make_unit_value()),
            Err(e) => EffectResult::Error(format!("Failed to write {}: {}", full_path, e)),
        }
    }

    /// Check if file exists
    fn file_exists(&self, args: &[HottValue]) -> EffectResult {
        if args.is_empty() {
            return EffectResult::Error("file-exists requires path argument".to_string());
        }

        let path = self.extract_string(&args[0]);
        let full_path = self.resolve_path(&path);
        let exists = Path::new(&full_path).exists();
        
        EffectResult::Ok(self.make_bool_value(exists))
    }

    /// Print line to stdout
    fn print_line(&self, args: &[HottValue]) -> EffectResult {
        if args.is_empty() {
            println!();
        } else {
            let text = self.extract_string(&args[0]);
            println!("{}", text);
        }
        EffectResult::Ok(self.make_unit_value())
    }

    /// Execute sequential effects
    fn execute_seq_effect(&mut self, args: &[HottValue]) -> EffectResult {
        if args.len() < 2 {
            return EffectResult::Error("effect-seq requires two arguments".to_string());
        }

        // Execute first effect
        match self.execute_effect(&args[0]) {
            EffectResult::Ok(value1) => {
                // Apply continuation to result
                // TODO: This needs proper function application
                self.execute_effect(&args[1])
            }
            err => err,
        }
    }

    /// Execute parallel effects
    fn execute_par_effect(&mut self, args: &[HottValue]) -> EffectResult {
        if args.len() < 2 {
            return EffectResult::Error("effect-par requires two arguments".to_string());
        }

        // Execute both effects
        let result1 = self.execute_effect(&args[0]);
        let result2 = self.execute_effect(&args[1]);

        match (result1, result2) {
            (EffectResult::Ok(v1), EffectResult::Ok(v2)) => {
                // Return pair of results
                EffectResult::Ok(self.make_pair_value(v1, v2))
            }
            (EffectResult::Error(e), _) => EffectResult::Error(e),
            (_, EffectResult::Error(e)) => EffectResult::Error(e),
        }
    }

    /// Execute choice effect
    fn execute_choice_effect(&mut self, args: &[HottValue]) -> EffectResult {
        if args.len() < 2 {
            return EffectResult::Error("effect-choice requires two arguments".to_string());
        }

        // Try first effect
        match self.execute_effect(&args[0]) {
            ok @ EffectResult::Ok(_) => ok,
            EffectResult::Error(_) => {
                // First failed, try second
                self.execute_effect(&args[1])
            }
        }
    }

    // ========================================================================
    // HELPER FUNCTIONS
    // ========================================================================

    /// Parse I/O effect arguments
    fn parse_io_args(&self, args: &[HottValue]) -> Result<(String, String, Vec<HottValue>, String), String> {
        if args.len() < 4 {
            return Err("io-effect requires 4 arguments".to_string());
        }

        let category = self.extract_string(&args[0]);
        let operation = self.extract_string(&args[1]);
        let op_args = self.extract_list(&args[2]);
        let determinism = self.extract_constructor_name(&args[3]);

        Ok((category, operation, op_args, determinism))
    }

    /// Extract string from string-value constructor
    fn extract_string(&self, value: &HottValue) -> String {
        match value {
            HottValue::Constructor { name, args, .. } if name == "string-value" => {
                // TODO: Extract actual string from PathFinder string representation
                "placeholder".to_string()
            }
            HottValue::String(s) => s.clone(),
            _ => format!("{:?}", value),
        }
    }

    /// Extract list from PathFinder list
    fn extract_list(&self, value: &HottValue) -> Vec<HottValue> {
        match value {
            HottValue::Constructor { name, args, .. } => {
                match name.as_str() {
                    "nil" => vec![],
                    "cons" => {
                        if args.len() >= 2 {
                            let mut result = vec![args[0].clone()];
                            result.extend(self.extract_list(&args[1]));
                            result
                        } else {
                            vec![]
                        }
                    }
                    _ => vec![value.clone()],
                }
            }
            _ => vec![value.clone()],
        }
    }

    /// Extract constructor name
    fn extract_constructor_name(&self, value: &HottValue) -> String {
        match value {
            HottValue::Constructor { name, .. } => name.clone(),
            _ => "unknown".to_string(),
        }
    }

    /// Resolve path relative to working directory
    fn resolve_path(&self, path: &str) -> String {
        if Path::new(path).is_absolute() {
            path.to_string()
        } else {
            format!("{}/{}", self.working_dir, path)
        }
    }

    /// Create unit value
    fn make_unit_value(&self) -> HottValue {
        HottValue::Constructor {
            name: "unit".to_string(),
            args: vec![],
            value_type: crate::hott_values::HottType::Universe(0),
        }
    }

    /// Create boolean value
    fn make_bool_value(&self, b: bool) -> HottValue {
        HottValue::Constructor {
            name: if b { "true" } else { "false" }.to_string(),
            args: vec![],
            value_type: crate::hott_values::HottType::Universe(0),
        }
    }

    /// Create string value
    fn make_string_value(&self, s: String) -> HottValue {
        // TODO: Convert to PathFinder string representation
        HottValue::String(s)
    }

    /// Create pair value
    fn make_pair_value(&self, first: HottValue, second: HottValue) -> HottValue {
        HottValue::Constructor {
            name: "pair".to_string(),
            args: vec![first, second],
            value_type: crate::hott_values::HottType::Universe(0),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pure_effect() {
        let mut bridge = EffectBridge::new(".".to_string());
        let value = HottValue::String("hello".to_string());
        let effect = HottValue::Constructor {
            name: "pure-effect".to_string(),
            args: vec![value.clone()],
            value_type: crate::hott_values::HottType::Universe(0),
        };

        match bridge.execute_effect(&effect) {
            EffectResult::Ok(result) => assert_eq!(result, value),
            EffectResult::Error(e) => panic!("Unexpected error: {}", e),
        }
    }
}