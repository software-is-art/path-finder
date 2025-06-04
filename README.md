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
pathfinder> (define x 42)
pathfinder> (+ x 8)
50
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
devbox run test         # Run test suite
```

### Project Structure

```
path-finder/
├── src/                    # Core source code
│   ├── main.rkt           # Main entry point
│   ├── lexer/             # Lexical analysis
│   ├── parser/            # Syntax analysis  
│   ├── types/             # Type system
│   ├── effects/           # Effect system
│   ├── evaluator/         # Evaluation engine
│   └── stdlib/            # Standard library
├── tests/                 # Test suites
├── docs/                  # Documentation
├── examples/              # Sample programs
├── devbox.json           # Environment configuration
├── info.rkt              # Package metadata
└── README.md             # This file
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

- ✅ **Development Environment** - Devbox setup complete
- ✅ **Basic Infrastructure** - REPL and CLI framework
- 🚧 **Core Parser** - S-expression parsing (in progress)
- 🚧 **Type System** - HoTT-based type checker (planned)
- 🚧 **Effect System** - Algebraic effects runtime (planned)
- 🚧 **Standard Library** - Core functions and types (planned)

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