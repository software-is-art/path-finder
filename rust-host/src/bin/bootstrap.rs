// ============================================================================
// PATHFINDER BOOTSTRAP - MINIMAL HOST FOR SELF-HOSTING
// ============================================================================
// This is the minimal Rust program needed to bootstrap PathFinder.
// It provides:
// 1. S-expression parsing
// 2. Effect execution (file I/O)
// 3. Initial type environment
// 4. Module loading

use pathfinder_rust_host::{
    sexp_parser::{SExpParser, SExp},
    effect_bridge::EffectBridge,
    initial_env::InitialEnvironment,
    hott_values::{HottValue, HottType, HottAst},
    bootstrap_vm::BootstrapVM,
};
use std::path::PathBuf;
use std::env;
use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about = "PathFinder Bootstrap - Minimal host for self-hosting")]
struct Args {
    /// Path to bootstrap module (default: src/bootstrap.sexp)
    #[arg(short, long, default_value = "src/bootstrap.sexp")]
    bootstrap: String,
    
    /// Working directory for module resolution
    #[arg(short, long, default_value = ".")]
    working_dir: String,
    
    /// Enable debug output
    #[arg(short, long)]
    debug: bool,
    
    /// Run REPL after bootstrap
    #[arg(short, long)]
    repl: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    
    println!("PathFinder Bootstrap v0.1.0");
    println!("Working directory: {}", args.working_dir);
    
    // Initialize components
    let mut vm = BootstrapVM::new();
    let mut effect_bridge = EffectBridge::new(args.working_dir.clone());
    let sexp_parser = SExpParser::new();
    
    // Load initial environment
    println!("Loading initial environment...");
    let initial_env = InitialEnvironment::new();
    let env = initial_env.to_pathfinder_env();
    
    // TODO: Set initial environment in VM
    // For now, the VM starts with empty environment
    
    // Load bootstrap module
    println!("Loading bootstrap module: {}", args.bootstrap);
    let bootstrap_effect = create_load_module_effect(&args.bootstrap);
    
    // Execute the bootstrap
    match effect_bridge.execute_effect(&bootstrap_effect) {
        pathfinder_rust_host::effect_bridge::EffectResult::Ok(module_content) => {
            println!("Bootstrap module loaded successfully");
            
            // Parse the module
            let content = extract_string(&module_content);
            match sexp_parser.parse(&content) {
                Ok(sexps) => {
                    if args.debug {
                        println!("Parsed {} top-level forms", sexps.len());
                    }
                    
                    // Convert S-expressions to HoTT AST and evaluate
                    for (i, sexp) in sexps.iter().enumerate() {
                        if args.debug {
                            println!("Evaluating form {}: {:?}", i + 1, sexp);
                        }
                        
                        // Check if this is a define or type form
                        if let SExp::List(elements) = sexp {
                            if let Some(SExp::Atom(head)) = elements.first() {
                                if head == "define" && elements.len() >= 3 {
                                    // Handle define specially
                                    if let SExp::Atom(name) = &elements[1] {
                                        match evaluate_sexp(&mut vm, &mut effect_bridge, &elements[2]) {
                                            Ok(value) => {
                                                if args.debug {
                                                    println!("Defined {}: {:?}", name, value);
                                                }
                                                // Store in global environment
                                                // For now, just continue
                                            }
                                            Err(e) => {
                                                eprintln!("Error evaluating form {}: {}", i + 1, e);
                                                return Err(e.into());
                                            }
                                        }
                                        continue;
                                    }
                                } else if head == "type" && elements.len() >= 3 {
                                    // Handle type declaration
                                    if let SExp::Atom(name) = &elements[1] {
                                        match evaluate_sexp(&mut vm, &mut effect_bridge, &elements[2]) {
                                            Ok(type_value) => {
                                                if args.debug {
                                                    println!("Type {}: {:?}", name, type_value);
                                                }
                                                // Type declarations are just for documentation
                                                // The actual definition comes later
                                            }
                                            Err(e) => {
                                                eprintln!("Error evaluating type {}: {}", i + 1, e);
                                                return Err(e.into());
                                            }
                                        }
                                        continue;
                                    }
                                } else if head == "data" && elements.len() >= 3 {
                                    // Handle data type declaration
                                    if let SExp::Atom(name) = &elements[1] {
                                        if args.debug {
                                            println!("Data type {}: {} constructors", name, 
                                                elements.iter().skip(3)
                                                    .filter(|e| matches!(e, SExp::List(l) if l.first() == Some(&SExp::Atom("case".to_string()))))
                                                    .count());
                                        }
                                        // Data declarations establish constructors
                                        // For now, just continue
                                        continue;
                                    }
                                }
                            }
                        }
                        
                        // Evaluate other forms normally
                        match evaluate_sexp(&mut vm, &mut effect_bridge, sexp) {
                            Ok(value) => {
                                if args.debug {
                                    println!("Result: {:?}", value);
                                }
                            }
                            Err(e) => {
                                eprintln!("Error evaluating form {}: {}", i + 1, e);
                                return Err(e.into());
                            }
                        }
                    }
                    
                    println!("Bootstrap completed successfully!");
                    
                    // Start REPL if requested
                    if args.repl {
                        run_repl(&mut vm, &mut effect_bridge, &sexp_parser)?;
                    }
                }
                Err(e) => {
                    eprintln!("Failed to parse bootstrap module: {}", e);
                    return Err(e.into());
                }
            }
        }
        pathfinder_rust_host::effect_bridge::EffectResult::Error(e) => {
            eprintln!("Failed to load bootstrap module: {}", e);
            return Err(e.into());
        }
    }
    
    Ok(())
}

/// Create effect to load a module
fn create_load_module_effect(path: &str) -> HottValue {
    HottValue::Constructor {
        name: "io-effect".to_string(),
        args: vec![
            HottValue::String("file".to_string()),
            HottValue::String("read".to_string()),
            HottValue::Constructor {
                name: "cons".to_string(),
                args: vec![
                    HottValue::String(path.to_string()),
                    HottValue::Constructor {
                        name: "nil".to_string(),
                        args: vec![],
                        value_type: HottType::Universe(0),
                    },
                ],
                value_type: HottType::Universe(0),
            },
            HottValue::Constructor {
                name: "deterministic".to_string(),
                args: vec![],
                value_type: HottType::Universe(0),
            },
        ],
        value_type: HottType::Universe(0),
    }
}

/// Extract string from PathFinder value
fn extract_string(value: &HottValue) -> String {
    match value {
        HottValue::String(s) => s.clone(),
        _ => format!("{:?}", value),
    }
}

/// Evaluate an S-expression
fn evaluate_sexp(
    vm: &mut BootstrapVM,
    effect_bridge: &mut EffectBridge,
    sexp: &SExp,
) -> Result<HottValue, String> {
    // Convert S-expression to HoTT AST
    let ast = sexp_to_hott_ast(sexp)?;
    
    // Evaluate with VM
    match vm.eval(ast) {
        Ok(value_ptr) => {
            // Get the actual value
            if let Some(value) = vm.get_value(value_ptr) {
                // Check if result is an effect
                if is_effect(value) {
                    // Execute the effect
                    match effect_bridge.execute_effect(value) {
                        pathfinder_rust_host::effect_bridge::EffectResult::Ok(result) => Ok(result),
                        pathfinder_rust_host::effect_bridge::EffectResult::Error(e) => Err(e),
                    }
                } else {
                    Ok(value.clone())
                }
            } else {
                Err("Failed to get value from pointer".to_string())
            }
        }
        Err(e) => Err(format!("Evaluation error: {:?}", e)),
    }
}

/// Convert S-expression to HoTT AST
fn sexp_to_hott_ast(sexp: &SExp) -> Result<HottAst, String> {
    match sexp {
        SExp::Atom(s) => {
            // Check for special atoms
            match s.as_str() {
                "Universe" => {
                    // Default to Type0
                    Ok(HottAst::Universe(0))
                }
                "Type0" => Ok(HottAst::Universe(0)),
                "Type1" => Ok(HottAst::Universe(1)),
                "Type2" => Ok(HottAst::Universe(2)),
                "Type3" => Ok(HottAst::Universe(3)),
                "Value" => {
                    // Value is a general type variable, treat as Type0 for now
                    Ok(HottAst::Universe(0))
                }
                _ => Ok(HottAst::Var(s.clone())),
            }
        }
        SExp::String(s) => Ok(HottAst::Literal(make_pathfinder_string(s))),
        SExp::Number(n) => Ok(HottAst::Literal(make_pathfinder_nat(*n as u64))),
        SExp::List(elements) => {
            if elements.is_empty() {
                return Ok(HottAst::Literal(HottValue::Constructor {
                    name: "nil".to_string(),
                    args: vec![],
                    value_type: HottType::Universe(0),
                }));
            }
            
            // Check for special forms
            if let SExp::Atom(head) = &elements[0] {
                match head.as_str() {
                    "import" => convert_import(&elements[1..]),
                    "define" => convert_define(&elements[1..]),
                    "type" => convert_type(&elements[1..]),
                    "data" => convert_data(&elements[1..]),
                    "fn" => convert_fn(&elements[1..]),
                    "let" => convert_let(&elements[1..]),
                    "match" => convert_match(&elements[1..]),
                    "Universe" => {
                        // Handle (Universe n)
                        if elements.len() == 2 {
                            match &elements[1] {
                                SExp::Number(n) => Ok(HottAst::Universe(*n as usize)),
                                _ => Err("Universe requires numeric level".to_string()),
                            }
                        } else {
                            Err("Universe requires exactly one argument".to_string())
                        }
                    }
                    "->" => {
                        // Handle function types (-> A B) or (-> A B C) etc.
                        if elements.len() < 3 {
                            return Err("-> requires at least two arguments".to_string());
                        }
                        
                        // Build nested Pi types from right to left
                        let mut result = sexp_to_hott_ast(&elements[elements.len() - 1])?;
                        
                        for i in (1..elements.len() - 1).rev() {
                            result = HottAst::PiType {
                                var_name: format!("_x{}", i), // Anonymous variable
                                domain: Box::new(sexp_to_hott_ast(&elements[i])?),
                                codomain: Box::new(result),
                            };
                        }
                        
                        Ok(result)
                    }
                    "IdType" => {
                        // Handle identity type (IdType A x y)
                        if elements.len() != 4 {
                            return Err("IdType requires exactly 3 arguments".to_string());
                        }
                        Ok(HottAst::IdType {
                            type_expr: Box::new(sexp_to_hott_ast(&elements[1])?),
                            left: Box::new(sexp_to_hott_ast(&elements[2])?),
                            right: Box::new(sexp_to_hott_ast(&elements[3])?),
                        })
                    }
                    _ => convert_application(elements),
                }
            } else {
                convert_application(elements)
            }
        }
    }
}

/// Check if value is an effect
fn is_effect(value: &HottValue) -> bool {
    match value {
        HottValue::Constructor { name, .. } => {
            matches!(name.as_str(), "pure-effect" | "io-effect" | "effect-seq" | "effect-par" | "effect-choice")
        }
        _ => false,
    }
}

/// Run the REPL
fn run_repl(
    vm: &mut BootstrapVM,
    effect_bridge: &mut EffectBridge,
    parser: &SExpParser,
) -> Result<(), Box<dyn std::error::Error>> {
    use std::io::{self, Write};
    
    println!("\nPathFinder REPL - Type :help for help, :quit to exit");
    
    loop {
        print!("> ");
        io::stdout().flush()?;
        
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        
        let input = input.trim();
        if input.is_empty() {
            continue;
        }
        
        // Handle REPL commands
        if input.starts_with(':') {
            match input {
                ":quit" | ":exit" => break,
                ":help" => print_help(),
                ":env" => print_environment(vm),
                _ => println!("Unknown command: {}", input),
            }
            continue;
        }
        
        // Parse and evaluate
        match parser.parse(input) {
            Ok(sexps) => {
                for sexp in sexps {
                    match evaluate_sexp(vm, effect_bridge, &sexp) {
                        Ok(value) => println!("{:?}", value),
                        Err(e) => eprintln!("Error: {}", e),
                    }
                }
            }
            Err(e) => eprintln!("Parse error: {}", e),
        }
    }
    
    Ok(())
}

/// Print REPL help
fn print_help() {
    println!("PathFinder REPL Commands:");
    println!("  :help    - Show this help");
    println!("  :quit    - Exit the REPL");
    println!("  :env     - Show current environment");
    println!("\nExamples:");
    println!("  (+ 2 3)");
    println!("  (define x 42)");
    println!("  (import \"core/types\")");
}

/// Print current environment
fn print_environment(vm: &BootstrapVM) {
    println!("Current environment:");
    // TODO: Implement environment printing
    println!("  (environment display not yet implemented)");
}

// ============================================================================
// S-EXPRESSION CONVERSION HELPERS
// ============================================================================

fn make_pathfinder_string(s: &str) -> HottValue {
    // Convert Rust string to PathFinder string (list of chars)
    let mut result = HottValue::Constructor {
        name: "empty-string".to_string(),
        args: vec![],
        value_type: HottType::Universe(0),
    };
    
    for ch in s.chars().rev() {
        let char_val = HottValue::Constructor {
            name: "char".to_string(),
            args: vec![make_pathfinder_nat(ch as u64)],
            value_type: HottType::Universe(0),
        };
        
        result = HottValue::Constructor {
            name: "string-cons".to_string(),
            args: vec![char_val, result],
            value_type: HottType::Universe(0),
        };
    }
    
    result
}

fn make_pathfinder_nat(n: u64) -> HottValue {
    let mut result = HottValue::Constructor {
        name: "zero".to_string(),
        args: vec![],
        value_type: HottType::Universe(0),
    };
    
    for _ in 0..n {
        result = HottValue::Constructor {
            name: "succ".to_string(),
            args: vec![result],
            value_type: HottType::Universe(0),
        };
    }
    
    result
}

fn convert_import(args: &[SExp]) -> Result<HottAst, String> {
    if args.is_empty() {
        return Err("import requires module path".to_string());
    }
    
    // For now, just return the module path as a variable
    sexp_to_hott_ast(&args[0])
}

fn convert_define(args: &[SExp]) -> Result<HottAst, String> {
    if args.len() < 2 {
        return Err("define requires name and value".to_string());
    }
    
    // For top-level define, we just evaluate the value
    // The bootstrap will handle storing it in the environment
    sexp_to_hott_ast(&args[1])
}

fn convert_type(args: &[SExp]) -> Result<HottAst, String> {
    if args.len() < 2 {
        return Err("type requires name and type".to_string());
    }
    
    // For now, just return the type expression
    sexp_to_hott_ast(&args[1])
}

fn convert_data(args: &[SExp]) -> Result<HottAst, String> {
    // For now, data declarations are handled outside the normal evaluation
    // Just return a placeholder
    if args.len() < 2 {
        return Err("data requires name and universe".to_string());
    }
    
    // Return the universe level as a placeholder
    sexp_to_hott_ast(&args[1])
}

fn convert_fn(args: &[SExp]) -> Result<HottAst, String> {
    if args.len() < 2 {
        return Err("fn requires parameters and body".to_string());
    }
    
    // Extract parameters
    let params = match &args[0] {
        SExp::List(params) => {
            let mut param_names = Vec::new();
            for p in params {
                match p {
                    SExp::Atom(name) => param_names.push(name.clone()),
                    _ => return Err("fn parameters must be atoms".to_string()),
                }
            }
            param_names
        }
        SExp::Atom(s) => vec![s.clone()],
        _ => return Err("fn requires parameter list or atom".to_string()),
    };
    
    if params.is_empty() {
        return Err("fn requires at least one parameter".to_string());
    }
    
    // Build nested lambdas from right to left
    let mut body = sexp_to_hott_ast(&args[1])?;
    
    for param in params.into_iter().rev() {
        body = HottAst::Lambda {
            param,
            param_type: None,
            body: Box::new(body),
        };
    }
    
    Ok(body)
}

fn convert_let(args: &[SExp]) -> Result<HottAst, String> {
    // TODO: Implement let conversion
    Err("let not yet implemented".to_string())
}

fn convert_match(args: &[SExp]) -> Result<HottAst, String> {
    // TODO: Implement match conversion
    Err("match not yet implemented".to_string())
}

fn convert_application(elements: &[SExp]) -> Result<HottAst, String> {
    if elements.is_empty() {
        return Err("empty application".to_string());
    }
    
    let mut result = sexp_to_hott_ast(&elements[0])?;
    
    for arg in &elements[1..] {
        result = HottAst::App {
            func: Box::new(result),
            arg: Box::new(sexp_to_hott_ast(arg)?),
        };
    }
    
    Ok(result)
}