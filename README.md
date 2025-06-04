# PathFinder LISP

A HoTT-based functional programming language with algebraic effects, implemented in Racket.

## Overview

PathFinder LISP is an experimental functional programming language that combines:

- **Homotopy Type Theory (HoTT)** foundations for advanced type system
- **Algebraic Effects** for composable and modular effect handling
- **S-Expression Syntax** for homoiconic program representation
- **Interactive REPL** for exploratory programming

## Features

🎯 **Core Language Features**
- S-expression based syntax with hygienic macros
- Strong static type system based on HoTT principles
- First-class support for algebraic effects and handlers
- Interactive Read-Eval-Print Loop (REPL)

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
5
pathfinder> (define x 42)
42
pathfinder> (* x 2)
84
pathfinder> (define square (lambda (n) (* n n)))
#<closure>
pathfinder> (square 7)
49
pathfinder> (if (< 3 5) "yes" "no")
"yes"
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
devbox run test         # Run comprehensive test suite (74 tests)
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
│   │   └── evaluator.rkt    # Environment-based interpreter
│   ├── types/               # Type system
│   │   └── types.rkt        # HoTT-based type definitions
│   ├── effects/             # Effect system (planned)
│   └── stdlib/              # Standard library (planned)
├── tests/                   # Test suites (74 tests)
│   ├── lexer-parser-test.rkt # Lexer and parser tests
│   ├── evaluator-test.rkt   # Evaluation engine tests
│   ├── types-test.rkt       # Type system tests
│   └── main-test.rkt        # Integration tests
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
- **Development Environment** - Devbox setup with Racket toolchain
- **S-Expression Lexer** - Complete tokenization (parentheses, symbols, numbers, booleans, strings, comments)
- **S-Expression Parser** - Recursive descent parser building proper AST
- **Core Evaluation Engine** - Environment-based interpreter with lexical scoping
- **Basic Type System** - Primitive types (Nat, Bool, String) and function types
- **Interactive REPL** - Working Read-Eval-Print Loop with persistent environment
- **Built-in Functions** - Arithmetic (+, -, *, /) and comparison (=, <, >) operators
- **Lambda Functions** - First-class functions with closures
- **Conditional Expressions** - if/then/else evaluation
- **Variable Definitions** - define for creating bindings

### Language Features Working Now
```lisp
;; Arithmetic and comparisons
(+ 1 2 3)                    ; => 6
(< 3 5)                      ; => #t

;; Variable definitions
(define pi 3.14159)          ; => 3.14159

;; Lambda functions and closures
(define square (lambda (x) (* x x)))
(square 5)                   ; => 25

;; Conditional expressions
(if (> 10 5) "big" "small")  ; => "big"

;; Function composition
(define add1 (lambda (x) (+ x 1)))
(define double (lambda (x) (* x 2)))
(double (add1 5))            ; => 12
```

### In Progress 🚧
- **Type Checking** - HoTT-based type checker integration
- **Effect System** - Algebraic effects runtime (planned)
- **Standard Library** - Extended core functions and types (planned)
- **Error Messages** - Improved error reporting and diagnostics (planned)

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