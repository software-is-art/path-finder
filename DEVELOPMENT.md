# PathFinder LISP Development Environment

## Implementation Language Choice

**Primary Language:** Racket (`#lang racket/base`)  
**Alternative:** Rhombus with shrubbery/forest macros

### Rationale

PathFinder LISP is implemented in Racket for the following reasons:

1. **Native S-expression Support** - Racket's homoiconic nature makes parsing and AST manipulation natural
2. **Advanced Macro System** - Essential for implementing PathFinder's hygienic macro system
3. **Language-Oriented Programming** - Racket excels at creating new languages
4. **Typed Racket Integration** - Provides foundation for advanced type system implementation
5. **Built-in REPL** - Simplifies interactive development environment
6. **Syntax Manipulation Tools** - Excellent support for metaprogramming

### Rhombus Alternative

If using Rhombus:
- Leverage shrubbery/forest macros for more flexible syntax manipulation
- Better support for syntax experimentation and language evolution
- More powerful macro system for domain-specific constructs

## Development Environment Setup

### Prerequisites

1. **Devbox** - Install from https://www.jetify.com/devbox/
   ```bash
   curl -fsSL https://get.jetify.com/devbox | bash
   ```

2. **Development Tools** (optional, but recommended)
   - **DrRacket** (full Racket IDE - install separately if needed)
   - **VS Code** with Racket LSP extension
   - **Emacs** with racket-mode
   - **Vim/Neovim** with vim-racket

### Quick Start

1. **Clone and Enter Environment**
   ```bash
   git clone <repository-url>
   cd path-finder
   devbox shell
   ```

2. **Install Dependencies**
   ```bash
   devbox run setup
   # Installs task-master-ai and other Node.js dependencies
   ```

3. **Verify Installation**
   ```bash
   devbox run version
   # Should output: PathFinder LISP v0.1.0
   
   npm run tasks
   # Should show project task dashboard
   ```

4. **Run the Interpreter**
   ```bash
   devbox run repl
   ```

### Available Commands

```bash
# Core development commands
devbox run build       # Check syntax and compile
devbox run run          # Start PathFinder LISP interpreter
devbox run repl         # Start interactive REPL
devbox run version      # Show version information

# Task management commands
npm run tasks           # View project task dashboard
npm run next            # Show next task to work on
npm run task <id>       # Show specific task details
npm run status <id> <status>  # Update task status
npm run expand <id>     # Break down task into subtasks

# Testing and setup
devbox run test         # Run test suite (when implemented)
devbox run setup        # Install dependencies and setup environment
```

### Project Structure

```
path-finder/
├── src/                    # Core source code
│   ├── main.rkt           # Main entry point
│   ├── lexer/             # Lexical analysis (to be implemented)
│   ├── parser/            # Syntax analysis (to be implemented)
│   ├── types/             # Type system (to be implemented)
│   ├── effects/           # Effect system (to be implemented)
│   ├── evaluator/         # Evaluation engine (to be implemented)
│   └── stdlib/            # Standard library (to be implemented)
├── tests/                 # Test suites
│   ├── unit/              # Unit tests
│   ├── integration/       # Integration tests
│   └── examples/          # Test programs
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
# Within devbox environment
devbox run test

# Manual execution (once tests are implemented)
find tests -name '*.rkt' -exec racket {} \;
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

### Completed (Task 21)
- ✅ Development environment setup with Devbox
- ✅ Project structure established
- ✅ Basic REPL and command-line interface
- ✅ Version control initialization
- ✅ Build system configuration

### Next Tasks
- **Task 1** - S-expression lexer and parser implementation
- **Task 2** - Core evaluator with basic data types
- **Task 3** - Advanced evaluation features (conditionals, functions)
- **Task 4** - Type system foundation

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