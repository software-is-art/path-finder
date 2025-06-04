# PathFinder LISP

A HoTT-based functional programming language with algebraic effects, implemented in Racket.

## Overview

PathFinder LISP is an experimental functional programming language that combines:

- **Homotopy Type Theory (HoTT)** foundations for advanced type system
- **Algebraic Effects** for composable and modular effect handling
- **S-Expression Syntax** for homoiconic program representation
- **Interactive REPL** for exploratory programming

## Features

🎯 **Complete HoTT Foundations**
- **Path Computation**: Identity types with reflexivity, concatenation, inverse, transport, and congruence
- **Univalence Axiom**: `(A ≃ B) ≃ (Id Type A B)` with equivalence types and path induction
- **Universe Hierarchy**: `Type₀ : Type₁ : Type₂ : ...` with proper level management
- **Dependent Types**: Π-types, Σ-types, sum types, and inductive types
- **Higher Structure**: 2-paths, 3-paths, truncation levels, and h-types

🔧 **Language Implementation**
- S-expression based syntax with comprehensive parser
- Complete HoTT-based type system with runtime type checking
- Environment-based evaluator with proper HoTT value representation
- Interactive Read-Eval-Print Loop (REPL) with mathematical notation

🔧 **Development Features**
- Reproducible development environment with Devbox
- Built-in testing framework
- Comprehensive documentation system
- Command-line interface for file execution

## Quick Start

### Prerequisites

- [Devbox](https://www.jetify.com/devbox/) - For reproducible development environment

### Installation

```bash
# Clone the repository
git clone <repository-url>
cd path-finder

# Enter the development environment
devbox shell
```

### Basic Usage

```bash
# Start the interactive REPL
devbox run repl

# Check version
devbox run version

# Run a PathFinder file (once implemented)
devbox run run examples/hello.pf
```

### Example Session

```lisp
pathfinder> (+ 2 3)
(succ (succ (succ (succ (succ zero)))))

pathfinder> (define x 42)
(succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ zero))))))))))))))))))))))))))))))))))))))))))))

pathfinder> (* x 2)
(succ (succ ... (succ zero) ...))

pathfinder> (= 5 5)
true

pathfinder> (< 3 5) 
true

pathfinder> (if (< 3 5) true false)
true

pathfinder> (exit)
Goodbye!
```

## Development Environment

### Available Commands

```bash
devbox run build       # Check syntax and compile
devbox run run          # Start PathFinder LISP interpreter  
devbox run repl         # Start interactive REPL
devbox run version      # Show version information
devbox run test         # Run comprehensive test suite (89 tests)
devbox run fmt          # Format all Racket code
devbox run lint         # Run static analysis
```

### Project Structure

```
path-finder/
├── src/                      # Core source code
│   ├── main.rkt             # Main entry point and CLI
│   ├── lexer/               # Lexical analysis
│   │   ├── lexer.rkt        # S-expression tokenizer
│   │   └── tokens.rkt       # Token definitions
│   ├── parser/              # Syntax analysis  
│   │   ├── parser.rkt       # Recursive descent parser
│   │   └── ast.rkt          # Abstract syntax tree nodes
│   ├── evaluator/           # Evaluation engine
│   │   ├── evaluator.rkt    # Environment-based interpreter
│   │   └── values.rkt       # HoTT runtime values and operations
│   ├── typecheck/           # Type checking
│   │   └── typechecker.rkt  # HoTT-based type checker
│   ├── types/               # Type system
│   │   └── types.rkt        # Complete HoTT type system with path computation
│   ├── effects/             # Effect system (planned)
│   └── stdlib/              # Standard library (planned)
├── tests/                   # Test suites (89 tests)
│   ├── lexer-parser-test.rkt    # Lexer and parser tests
│   ├── evaluator-test.rkt       # Evaluation engine tests
│   ├── types-test.rkt           # Type system tests
│   ├── path-univalence-test.rkt # Path computation and univalence tests
│   └── main-test.rkt            # Integration tests
├── docs/                    # Documentation
├── examples/                # Sample programs (planned)
├── devbox.json             # Environment configuration
├── Makefile                # Build automation
├── info.rkt                # Package metadata
└── README.md               # This file
```

## Language Design Goals

### Type System
- **Dependent Types** - Types that depend on values
- **Univalence** - HoTT's fundamental principle
- **Path Types** - Representing equality as paths
- **Higher Inductive Types** - For advanced mathematical structures

### Effect System
- **Algebraic Effects** - Modular effect definitions
- **Effect Handlers** - Composable effect interpretation
- **Effect Polymorphism** - Generic programming over effects
- **Resource Management** - Safe resource handling

### Syntax Design
- **S-Expressions** - Minimal, regular syntax
- **Hygienic Macros** - Safe metaprogramming
- **Unicode Support** - Mathematical notation
- **Pattern Matching** - Destructuring data

## Development Status

This is an early-stage experimental language. Current implementation status:

### Completed Features ✅

#### **Complete HoTT Implementation**
- **Path Computation** - Identity types with reflexivity, concatenation, inverse, transport, congruence
- **Univalence Axiom** - `(A ≃ B) ≃ (Id Type A B)` with equivalence types and path induction  
- **Universe Hierarchy** - `Type₀ : Type₁ : Type₂ : ...` with proper level management
- **Dependent Types** - Π-types (dependent functions), Σ-types (dependent pairs), sum types
- **Inductive Types** - Natural numbers and booleans as proper HoTT constructions
- **Higher Structure** - 2-paths, 3-paths, truncation levels, contractible/proposition/set types
- **J-eliminator** - Path induction for dependent elimination over identity types

#### **Core Language Implementation**
- **Development Environment** - Devbox setup with Racket toolchain
- **S-Expression Lexer** - Complete tokenization (parentheses, symbols, numbers, booleans, strings, comments)
- **S-Expression Parser** - Recursive descent parser building proper AST
- **HoTT-based Evaluator** - Environment-based interpreter with proper HoTT value representation
- **Type Checker** - Integration of HoTT type checking with path and equivalence types
- **Interactive REPL** - Working Read-Eval-Print Loop with HoTT value display
- **Built-in Operations** - Arithmetic and comparison with proper HoTT natural numbers and booleans
- **Lambda Functions** - First-class functions with closures and proper typing
- **Conditional Expressions** - if/then/else evaluation with HoTT boolean values
- **Variable Definitions** - define for creating bindings with type integration

### Language Features Working Now
```lisp
;; HoTT Natural Numbers and Arithmetic
(+ 1 2 3)                    ; => (succ (succ (succ (succ (succ (succ zero))))))
(* 2 3)                      ; => (succ (succ (succ (succ (succ (succ zero))))))
(< 3 5)                      ; => true
(= 5 5)                      ; => true

;; Variable definitions with HoTT values
(define x 42)                ; => (succ (succ ... zero))
(define y (* x 2))           ; => HoTT natural number value

;; Lambda functions with HoTT types
(define square (lambda (x) (* x x)))
(square 5)                   ; => (succ (succ ... zero)) [25 in HoTT representation]

;; Conditional expressions with HoTT booleans
(if (> 10 5) true false)     ; => true
(if (< 3 5) 42 0)           ; => (succ (succ ... zero)) [42]

;; Function composition with HoTT values
(define add1 (lambda (x) (+ x 1)))
(define double (lambda (x) (* x 2)))
(double (add1 5))            ; => (succ (succ ... zero)) [12]

;; Path computation operations (built-in functions available)
;; (refl value)              ; Create reflexivity path
;; (path-concat p q)         ; Concatenate compatible paths
;; (path-inverse p)          ; Invert path direction
;; (transport pred path val) ; Transport values along paths
;; (cong func path)          ; Apply functions to paths
;; (ua equiv)                ; Apply univalence axiom
```

### Future Development 🚧
- **Effect System** - Algebraic effects runtime with HoTT integration (planned)
- **Standard Library** - Extended HoTT-based core functions and types (planned)
- **Advanced Path Syntax** - Direct syntax for path expressions (planned)
- **Cubical Features** - Computational univalence and higher inductive types (planned)
- **Error Messages** - Improved error reporting with HoTT type information (planned)
- **Performance Optimization** - Efficient path computation and normalization (planned)

## Contributing

### Setup for Contributors

1. Install [Devbox](https://www.jetify.com/devbox/)
2. Clone the repository
3. Run `devbox shell` to enter the development environment
4. See `DEVELOPMENT.md` for detailed development guidelines

### Task Management

This project uses [Task Master](https://github.com/eyaltoledano/claude-task-master) for task management:

```bash
task-master list          # View current tasks
task-master next          # See next task to work on
task-master show <id>     # View specific task details
```

### Code Style

- Follow Racket conventions for module structure
- Use `kebab-case` for function names
- Include contracts for all public functions
- Write comprehensive tests for new features

## Theoretical Foundation

PathFinder LISP draws inspiration from:

- **Homotopy Type Theory** - Univalent foundations of mathematics
- **Cubical Type Theory** - Computational interpretation of univalence  
- **Algebraic Effects** - Modular effect handling
- **Linear Logic** - Resource-aware computation

## Documentation

- `DEVELOPMENT.md` - Development environment and workflow
- `docs/` - Language specification and tutorials (coming soon)
- `examples/` - Sample programs and use cases (coming soon)

## License

Licensed under either of:
- Apache License, Version 2.0
- MIT License

at your option.

## Acknowledgments

Built with:
- [Racket](https://racket-lang.org/) - Implementation language
- [Devbox](https://www.jetify.com/devbox/) - Development environment
- [Task Master](https://github.com/eyaltoledano/claude-task-master) - Project management

Inspired by research in:
- Homotopy Type Theory
- Algebraic Effects and Handlers
- Programming Language Theory

---

**Note**: PathFinder LISP is experimental research software. APIs and language features may change significantly during development.