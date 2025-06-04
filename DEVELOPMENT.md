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

# Install Racket dependencies
devbox run setup

# Install git hooks
make install-hooks
```

## Available Commands

### Core Development
```bash
devbox run build       # Check syntax and compile
devbox run test        # Run comprehensive test suite  
devbox run fmt         # Format all Racket code
devbox run lint        # Run static analysis
devbox run repl        # Start interactive REPL
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
├── src/                    # Core source code
│   ├── main.rkt           # Main entry point and CLI ✅
│   ├── lexer/             # Lexical analysis ✅
│   │   ├── lexer.rkt      # S-expression tokenizer
│   │   └── tokens.rkt     # Token definitions
│   ├── parser/            # Syntax analysis ✅
│   │   ├── parser.rkt     # Recursive descent parser
│   │   └── ast.rkt        # Abstract syntax tree nodes
│   ├── types/             # Complete HoTT type system ✅
│   │   └── types.rkt      # Path computation, univalence, dependent types
│   ├── evaluator/         # HoTT-based evaluation engine ✅
│   │   ├── evaluator.rkt  # Environment-based interpreter
│   │   └── values.rkt     # HoTT runtime values and operations
│   ├── typecheck/         # Type checking ✅
│   │   └── typechecker.rkt # HoTT-based type checker
│   ├── effects/           # Effect system (planned)
│   └── stdlib/            # Standard library (planned)
├── tests/                 # Test suites (89 tests) ✅
│   ├── lexer-parser-test.rkt    # Lexer and parser tests
│   ├── evaluator-test.rkt       # Evaluation engine tests
│   ├── types-test.rkt           # Type system tests
│   ├── path-univalence-test.rkt # Path computation and univalence tests
│   └── main-test.rkt            # Integration tests
├── docs/                  # Documentation
│   ├── manual.scrbl       # User manual (to be implemented)
│   └── internals.scrbl    # Implementation docs (to be implemented)
├── examples/              # Sample programs
├── scripts/               # Build/utility scripts
├── devbox.json           # Development environment configuration
├── info.rkt              # Package metadata
└── compiled/             # Generated files (git-ignored)
```

## Development Environment Details

### Package Configuration (`info.rkt`)

```racket
#lang info
(define collection "pathfinder")
(define deps '("base" "rackunit" "typed-racket"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("docs/manual.scrbl" ())))
(define pkg-desc "PathFinder LISP: HoTT-based functional language with algebraic effects")
(define version "0.1.0")
(define pkg-authors '("PathFinder Development Team"))
(define license '(Apache-2.0 OR MIT))
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

### Primary Framework: RackUnit

```racket
#lang racket/base
(require rackunit)

;; Example test structure
(define-test-suite lexer-tests
  (test-case "tokenize simple symbols"
    (check-equal? (tokenize "(a b c)")
                  (list lparen 'a 'b 'c rparen))))
```

### Test Organization

- **Unit Tests** - Individual component testing
- **Integration Tests** - End-to-end language features  
- **Property Tests** - Type system properties
- **Example Programs** - Real-world usage validation

### Running Tests

```bash
# Within devbox environment - runs all 89 tests
devbox run test

# Run specific test files
racket tests/path-univalence-test.rkt
racket tests/types-test.rkt
racket tests/evaluator-test.rkt

# Run all tests with detailed output
raco test tests/
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
- Implemented as separate Racket modules
- Thoroughly unit tested
- Documented with contracts
- Integrated incrementally

### 4. REPL-Driven Development

```bash
# Start the PathFinder REPL
devbox run repl

# Or run specific files
racket src/main.rkt --version
racket src/main.rkt --interactive
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

```racket
#lang racket/base

;; Imports
(require racket/contract
         racket/match
         racket/file)

;; Exports with contracts
(provide/contract
 [parse-expression (-> string? ast-node?)])

;; Implementation
(define (parse-expression str) ...)
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

- Use `racket -c` for syntax checking
- Consider full Racket installation for advanced compilation features
- Profile with `racket/profile` when performance becomes critical

### Memory Management

- Leverage Racket's garbage collector
- Use immutable data structures by default
- Consider performance optimization in later development phases

### Troubleshooting

### Common Issues

1. **"racket: command not found"**
   - Ensure you're in the devbox environment: `devbox shell`

2. **"task-master: command not found"**
   - Run `devbox run setup` to install Node.js dependencies
   - Use `npm run tasks` instead of direct `task-master` commands

3. **Missing dependencies**
   - The minimal Racket installation includes core packages
   - For advanced features, consider installing full Racket separately
   - Run `npm install` if task management commands fail

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
- Use `racket --help` for Racket-specific options
- Run `npm run next` to see what to work on next
- Refer to task files in `.taskmaster/tasks/` for implementation guidance
- View task dashboard with `npm run tasks`