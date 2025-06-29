# PathFinder LISP Development Guide

## Quick Start

```bash
# Setup development environment
make dev-setup

# Development workflow
make check    # Quick lint + test
make ci       # Full CI pipeline
```

## Development Environment

### Prerequisites
- [Devbox](https://www.jetify.com/devbox/) for reproducible environment
- Git for version control

### Environment Setup
```bash
# Enter development environment
devbox shell

# Build the Rust bootstrap
cd rust-host
cargo build

# Run self-hosting tests
cargo test
```

## Available Commands

### Core Development
```bash
# Bootstrap development
cd rust-host && cargo build    # Build the bootstrap VM
cd rust-host && cargo run      # Run PathFinder with bootstrap
cd rust-host && cargo test     # Run self-hosting tests

# PathFinder core files
ls src/                         # View .sexp source files
ls src/parser/parser.sexp       # PathFinder parser (self-hosted)
ls src/core/evaluator.sexp      # PathFinder evaluator (self-hosted)
```

### Code Quality
```bash
devbox run check-fmt   # Check formatting without changes
devbox run ci          # Full CI pipeline
devbox run clean       # Remove compiled files
```

### Convenience (via Makefile)
```bash
make help              # Show all available commands
make check             # Quick development check
make dev-setup         # Complete development setup
make release-check     # Full release validation
```

### Project Structure

```
path-finder/
├── src/                    # Core source code in .sexp format
│   ├── bootstrap.sexp     # Bootstrap initialization ✅
│   ├── lexer/             # Lexical analysis ✅
│   │   └── lexer.sexp     # S-expression tokenizer
│   ├── parser/            # Syntax analysis ✅
│   │   ├── parser.sexp    # Self-hosted parser
│   │   └── module-parser.sexp # Module parsing
│   ├── types/             # Complete HoTT type system ✅
│   │   ├── types.sexp     # Core HoTT types
│   │   ├── families.sexp  # Type families
│   │   ├── string.sexp    # String operations
│   │   ├── list.sexp      # List operations
│   │   └── equality.sexp  # Equality types
│   ├── evaluator/         # HoTT-based evaluation engine ✅
│   │   └── values.sexp    # HoTT runtime values
│   ├── typecheck/         # Type checking ✅
│   │   ├── bidirectional-inference.sexp # Type checking
│   │   └── inference.sexp # Type inference
│   ├── effects/           # Effect system ✅
│   │   └── effects.sexp   # Pure HoTT effects
│   └── core/              # HoTT foundation ✅
│       ├── foundations.sexp # Mathematical foundations
│       ├── evaluator.sexp # Self-hosted evaluator
│       └── modules.sexp   # Module system
├── rust-host/             # Bootstrap implementation ✅
│   ├── src/               # Rust bootstrap
│   │   ├── bootstrap_vm.rs # Minimal VM
│   │   ├── sexp_parser.rs  # S-expression parser
│   │   └── effect_bridge.rs # I/O bridge
│   └── Cargo.toml         # Rust dependencies
├── docs/                  # Documentation
│   ├── values-as-proofs.md # HoTT foundations
│   └── pure-hott-cache-system.md # Cache system
├── *.md                   # Project documentation
└── devbox.json           # Development environment configuration
```

## Development Environment Details

### Bootstrap Configuration (`rust-host/Cargo.toml`)

```toml
[package]
name = "pathfinder-bootstrap"
version = "0.1.0"
edition = "2021"

[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
log = "0.4"
env_logger = "0.10"

[[bin]]
name = "bootstrap"
path = "src/bin/bootstrap.rs"
```

### Environment Configuration (`devbox.json`)

The development environment is configured using Devbox with:
- **Racket Minimal** v8.16 - Core Racket runtime
- **Node.js** v20 - JavaScript runtime for task management
- **Git** - Version control
- **GNU Make** - Build automation

### Package Management (`package.json`)

Project dependencies are managed through npm:
- **task-master-ai** - AI-powered task management system
- **Node.js scripts** - Convenient task management commands

Custom scripts provide convenient commands for development workflow.

## Testing Framework

### Primary Framework: Rust Tests

```rust
// Self-hosting tests in rust-host/src/bootstrap_vm.rs
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_self_hosting() {
        // Verifies PathFinder can parse and evaluate itself
        let result = bootstrap_pathfinder();
        assert!(result.is_ok());
    }
}
```

### Test Organization

- **Unit Tests** - Individual component testing
- **Integration Tests** - End-to-end language features  
- **Property Tests** - Type system properties
- **Example Programs** - Real-world usage validation

### Running Tests

```bash
# Self-hosting tests in Rust bootstrap
cd rust-host && cargo test

# Specific self-hosting tests
cd rust-host && cargo test test_parser_deps
cd rust-host && cargo test test_evaluator_deps  
cd rust-host && cargo test test_self_hosting

# View test output with details
cd rust-host && cargo test -- --nocapture
```

## Development Workflow

### 1. Environment Activation

Always work within the devbox environment:
```bash
devbox shell  # Activates environment with welcome message
devbox run setup  # Install dependencies (first time only)
```

### 2. Task Management Workflow

PathFinder LISP uses AI-powered task management for organized development:

```bash
# View current project status
npm run tasks

# See what to work on next
npm run next

# Start working on a task
npm run status 1 in-progress

# View specific task details
npm run task 1

# Mark task as complete
npm run status 1 done
```

### 3. Module Development

Each major component (lexer, parser, type checker, etc.) should be:
- Implemented as separate .sexp modules in PathFinder
- Verified through self-hosting tests
- Documented with type signatures
- Integrated through the module system

### 4. Bootstrap-Driven Development

```bash
# Run the PathFinder bootstrap
cd rust-host && cargo run

# Debug bootstrap execution
cd rust-host && RUST_LOG=debug cargo run

# Test specific PathFinder modules
cd rust-host && cargo test test_module_loading
```

### 5. Syntax Checking

```bash
# Check syntax without running
devbox run build
```

## Code Style Guidelines

### Naming Conventions

- **Functions**: `kebab-case` (e.g., `parse-expression`)
- **Constants**: `UPPER-CASE` (e.g., `MAX-RECURSION-DEPTH`)
- **Types**: `PascalCase` (e.g., `ASTNode`)
- **Predicates**: end with `?` (e.g., `symbol?`)

### Module Structure

```lisp
;; PathFinder .sexp module structure

;; Type declarations
(type parse-expression (-> String ASTNode))

;; Function definitions
(define parse-expression (fn (str)
  ;; Implementation using PathFinder syntax
  ...))

;; Export declarations
(export parse-expression)
```

## Current Implementation Status

### Completed ✅
- **Development Environment** - Complete Devbox setup with Racket toolchain
- **S-Expression Lexer** - Full tokenization of parentheses, symbols, numbers, booleans, strings, comments
- **S-Expression Parser** - Recursive descent parser building proper AST nodes
- **Complete HoTT Type System** - Path computation, univalence axiom, dependent types, universe hierarchy
- **HoTT-based Evaluator** - Environment-based interpreter with proper HoTT value representation
- **Type Checker** - Integration of HoTT type checking with path and equivalence types
- **Runtime Values** - Path values, equivalence values, and proper HoTT constructors
- **Interactive REPL** - Working Read-Eval-Print Loop with HoTT value display
- **Comprehensive Testing** - 89 tests covering all implemented features
- **Built-in Operations** - Arithmetic, comparison, and path computation functions

### HoTT Features Implemented
- **Path Computation** - Identity types with reflexivity, concatenation, inverse, transport, congruence
- **Univalence Axiom** - `(A ≃ B) ≃ (Id Type A B)` with equivalence types
- **Universe Hierarchy** - `Type₀ : Type₁ : Type₂ : ...` with proper level management
- **Dependent Types** - Π-types, Σ-types, sum types, inductive types
- **Higher Structure** - 2-paths, 3-paths, truncation levels, h-types
- **J-eliminator** - Path induction for dependent elimination

### Future Development
- **Effect System** - Algebraic effects with HoTT integration
- **Standard Library** - Extended HoTT-based functions and types
- **Cubical Features** - Computational univalence and higher inductive types

## Performance Considerations

### Compilation

- Use `cargo build` for bootstrap compilation
- Use `cargo test` for verification
- Profile with `cargo build --release` for optimized builds

### Memory Management

- Leverage Racket's garbage collector
- Use immutable data structures by default
- Consider performance optimization in later development phases

### Troubleshooting

### Common Issues

1. **"cargo: command not found"**
   - Ensure you're in the devbox environment: `devbox shell`
   - Rust toolchain should be available in devbox

2. **"task-master: command not found"**
   - Run `devbox run setup` to install Node.js dependencies
   - Use `npm run tasks` instead of direct `task-master` commands

3. **Missing dependencies**
   - The Rust bootstrap includes all necessary dependencies
   - Check `rust-host/Cargo.toml` for dependency versions
   - Run `cargo build` to fetch dependencies

4. **Build failures**
   - Check syntax with `devbox run build`
   - Verify all `require` statements are correct

5. **Environment issues**
   - Restart devbox environment: `exit` then `devbox shell`
   - Update devbox: `devbox update`
   - Reinstall dependencies: `devbox run setup`

### Getting Help

- Check `devbox run` for available commands
- Use `npm run` for task management commands
- Use `cargo --help` for Rust toolchain options
- Run `npm run next` to see what to work on next
- Refer to task files in `.taskmaster/tasks/` for implementation guidance
- View task dashboard with `npm run tasks`