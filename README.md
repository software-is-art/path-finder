# PathFinder LISP

> **⚠️ Experimental Research Language**: PathFinder LISP is an active research project exploring advanced type theory concepts. While the core functionality works, expect significant API changes and incomplete features. This is ideal for researchers, PL enthusiasts, and those interested in HoTT foundations.

A revolutionary HoTT-based functional programming language with a 3-tier effect system and distributed proof computation, implemented in Racket.

## Overview

PathFinder LISP is a groundbreaking functional programming language that combines:

- **Homotopy Type Theory (HoTT)** foundations with dependent types and proof-carrying values
- **3-Tier Effect System** for compile-time, runtime, and distributed computation
- **Distributed Proof Cache** enabling transparent global mathematical commons
- **Content-Addressable Computation** where proofs computed anywhere can be reused everywhere
- **S-Expression Syntax** for homoiconic program representation
- **Interactive REPL** for exploratory programming with mathematical guarantees

## Features

🎯 **Revolutionary Architecture**
- **Tier 0**: Distributed proof cache with content-addressable computation
- **Tier 1**: Compile-time computational proofs with dependent safety
- **Tier 2**: Algebraic effects for compile-time operations
- **Tier 3**: Unified runtime effects with capability handlers
- **Mathematical Commons**: Global network of shared proofs and computations

🧮 **Complete HoTT Foundations**
- **Path Computation**: Identity types with reflexivity, concatenation, inverse, transport, and congruence
- **Univalence Axiom**: `(A ≃ B) ≃ (Id Type A B)` with equivalence types and path induction
- **Universe Hierarchy**: `Type₀ : Type₁ : Type₂ : ...` with proper level management
- **Dependent Types**: Π-types, Σ-types, sum types, and inductive types
- **Proof-Carrying Values**: Types that carry computational evidence of safety constraints
- **Higher Structure**: 2-paths, 3-paths, truncation levels, and h-types

🔧 **Advanced Type System**
- **Content-Addressable Proofs**: Transparent proof reuse across network boundaries
- **Dependent Safety**: NonEmptyList, BoundedArray with compile-time guarantees
- **Type Families**: Adaptive specialization with multi-context effects
- **Effect-Based Error Handling**: No nullable types, only mathematical guarantees
- **Generic Effects**: Multi-context handlers (compile-time, runtime, test, universal)

🔧 **Language Implementation**
- S-expression based syntax with comprehensive parser
- Complete HoTT-based type system with proof obligations
- Environment-based evaluator with proper HoTT value representation
- Interactive Read-Eval-Print Loop (REPL) with mathematical notation
- MCP server for advanced S-expression manipulation and formatting

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
devbox run test         # Run comprehensive test suite (89+ tests)
devbox run fmt          # Format all Racket code
devbox run lint         # Run static analysis
```

### Project Structure

```
path-finder/
├── src/                          # Core source code
│   ├── main.rkt                 # Main entry point and CLI
│   ├── lexer/                   # Lexical analysis
│   │   ├── lexer.rkt            # S-expression tokenizer
│   │   └── tokens.rkt           # Token definitions
│   ├── parser/                  # Syntax analysis  
│   │   ├── parser.rkt           # Recursive descent parser
│   │   └── ast.rkt              # Abstract syntax tree nodes
│   ├── evaluator/               # Evaluation engine
│   │   ├── evaluator.rkt        # Environment-based interpreter
│   │   └── values.rkt           # HoTT runtime values and operations
│   ├── typecheck/               # Type checking and effect verification
│   │   ├── typechecker.rkt      # HoTT-based type checker
│   │   ├── typechecker-new.rkt  # Enhanced type checker with effect integration
│   │   ├── effect-checker.rkt   # Multi-tier effect constraint verification
│   │   └── test-match.rkt       # Pattern matching support
│   ├── types/                   # Advanced type system
│   │   ├── types.rkt            # Core HoTT type system with path computation
│   │   ├── type-families.rkt    # Parameterized types with adaptive specialization
│   │   ├── dependent-safety.rkt # NonEmptyList and safety infrastructure
│   │   ├── bounded-arrays.rkt   # Tier 1 compile-time bounds checking
│   │   ├── list-type.rkt        # Generic list operations
│   │   └── list-type-generic.rkt # Multi-context list type families
│   ├── effects/                 # 3-Tier effect system
│   │   └── generic-effects.rkt  # Multi-context effect handlers
│   ├── core/                    # HoTT foundation
│   │   ├── hott-ast.rkt         # HoTT-specific AST extensions
│   │   ├── hott-evaluator.rkt   # HoTT evaluation semantics
│   │   ├── hott-literals.rkt    # HoTT literal value handling
│   │   ├── hott-literals-pure.rkt # Pure HoTT literal operations
│   │   └── host-bridge.rkt      # Host language integration
│   └── stdlib/                  # Standard library (in development)
├── tests/                       # Comprehensive test suite (89+ tests)
│   ├── lexer-parser-test.rkt        # Lexer and parser tests
│   ├── evaluator-test.rkt           # Evaluation engine tests
│   ├── types-test.rkt               # Type system tests
│   ├── path-univalence-test.rkt     # Path computation and univalence tests
│   ├── bounded-arrays-test.rkt      # Tier 1 bounds checking tests
│   ├── dependent-safety-test.rkt    # NonEmptyList and safety tests
│   ├── generic-effects-test.rkt     # Multi-context effect tests
│   ├── type-families-test.rkt       # Type family tests
│   ├── effect-aware-typechecker-test.rkt # Effect-type integration tests
│   └── main-test.rkt                # Integration tests
├── examples/                    # Advanced HoTT demonstrations
│   ├── dependent-safety-demo.rkt    # Proof-carrying value examples
│   ├── effect-types-demo.rkt        # Multi-tier effect demonstrations
│   ├── generic-effects-demo.rkt     # Effect handler examples
│   ├── type-family-examples.rkt     # Adaptive type specialization
│   ├── unified-effects-demo.rkt     # Cross-tier effect usage
│   └── values-as-proofs-demo.rkt    # Computational evidence examples
├── docs/                        # Theoretical documentation
│   └── values-as-proofs.md      # HoTT foundations and proof-carrying values
├── scripts/                     # Development utilities
├── devbox.json                  # Environment configuration
├── Makefile                     # Build automation
├── info.rkt                     # Package metadata
└── README.md                    # This file
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

**🚨 Current Status: Early Research Phase**
- **Core Language**: Basic functionality working (arithmetic, functions, conditionals)
- **Type System**: HoTT foundations and dependent types implemented
- **Effect System**: Architecture designed, implementation in progress
- **Distributed Proofs**: Conceptual design complete, implementation pending

**⚡ What Works Right Now:**
- Interactive REPL with HoTT natural numbers and booleans
- Basic arithmetic and comparison operations  
- Lambda functions with closures
- Variable definitions and conditional expressions
- Complete test suite (89+ tests passing)

**🔬 What's Experimental:**
- Advanced type features (dependent safety, bounded arrays)
- Multi-tier effect system (partially implemented)
- HoTT path computation (foundational work complete)
- Distributed proof cache (design phase)

Current implementation status:

### Completed Features ✅

#### **3-Tier Effect System Architecture**
- **Tier 1**: Pure computational proofs with compile-time verification
- **Tier 2**: Algebraic effects for compile-time operations
- **Tier 3**: Unified algebraic effects with runtime & capability handlers
- **Multi-Context Handlers**: Universal, specific contexts, and context lists
- **Execution Context Switching**: Automatic handler resolution across contexts
- **Effect-Aware Type Checking**: Integration across all execution environments

#### **Dependent Safety Infrastructure**
- **Proof-Carrying Values**: Values bundled with computational evidence
- **NonEmptyList**: Compile-time guaranteed non-empty collections
- **BoundedArray**: Tier 1 compile-time bounds checking with mathematical proofs
- **Path-Based Safety**: Provably total operations with termination guarantees
- **Type Families**: Parameterized types with adaptive specialization
- **Effect-Based Error Handling**: Mathematical guarantees instead of nullable types

#### **Complete HoTT Implementation**
- **Path Computation** - Identity types with reflexivity, concatenation, inverse, transport, congruence
- **Univalence Axiom** - `(A ≃ B) ≃ (Id Type A B)` with equivalence types and path induction  
- **Universe Hierarchy** - `Type₀ : Type₁ : Type₂ : ...` with proper level management
- **Dependent Types** - Π-types (dependent functions), Σ-types (dependent pairs), sum types
- **Inductive Types** - Natural numbers and booleans as proper HoTT constructions
- **Higher Structure** - 2-paths, 3-paths, truncation levels, contractible/proposition/set types
- **J-eliminator** - Path induction for dependent elimination over identity types

#### **Advanced Type System Implementation**
- **Tier-Aware Type Families**: Adaptive specialization across execution contexts
- **Generic Effects**: Multi-context effect handlers with universal/specific resolution
- **Bounded Arrays**: Compile-time bounds checking with Tier 1 proofs
- **Dependent Safety**: NonEmptyList with mathematical non-empty guarantees
- **Effect-Checker**: Verification of effect constraints across all tiers
- **Proof Construction**: Automatic generation of safety evidence and constraint proofs

#### **Core Language Implementation**
- **Development Environment** - Devbox setup with Racket toolchain
- **S-Expression Lexer** - Complete tokenization (parentheses, symbols, numbers, booleans, strings, comments)
- **S-Expression Parser** - Recursive descent parser building proper AST
- **HoTT-based Evaluator** - Environment-based interpreter with proper HoTT value representation
- **Effect-Aware Type Checker** - Integration of HoTT type checking with multi-tier effects
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

### In Development 🚧

#### **Tier 0: Distributed Proof Cache (Task 27)**
The next major milestone is implementing a global mathematical commons - a distributed, content-addressable proof cache that extends our 3-tier architecture:

- **Content-Addressable Proofs**: Proofs identified by their mathematical content, not location
- **Transparent Network Computation**: Automatic discovery and reuse of proofs computed anywhere
- **Zero-Configuration Distribution**: No explicit process management or network topology concerns
- **Mathematical Commons**: Global network of shared mathematical knowledge
- **Proof Reuse Optimization**: Skip computation if equivalent proofs exist in the distributed cache

This revolutionary feature will make PathFinder the first programming language with a global, shared mathematical foundation where proofs computed by anyone become available to everyone.

### Future Development 🚧
- **Effect System Runtime** - Complete algebraic effects implementation with distributed handlers
- **Standard Library Expansion** - Extended HoTT-based core functions and types
- **Advanced Path Syntax** - Direct syntax for path expressions and higher groupoid operations
- **Cubical Features** - Computational univalence and higher inductive types
- **Enhanced Error Messages** - Context-aware error reporting with proof suggestions
- **Performance Optimization** - Efficient path computation, normalization, and proof caching

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