# Contributing to PathFinder LISP

Thank you for your interest in contributing to PathFinder LISP! This is an experimental research language exploring the intersection of Homotopy Type Theory, dependent types, and effect systems.

## 🎯 Getting Started

1. **Fork and clone the repository**
   ```bash
   git clone https://github.com/YOUR-USERNAME/path-finder.git
   cd path-finder
   ```

2. **Set up the development environment**
   ```bash
   devbox shell  # Automatically sets up Racket and dependencies
   devbox run test  # Run tests to ensure setup works
   ```

3. **Start the REPL to explore**
   ```bash
   devbox run repl
   ```

## 🔬 Areas for Contribution

### For Newcomers
- **Documentation**: Improve examples, clarify concepts, fix typos
- **Tests**: Add test cases for existing functionality
- **Examples**: Create compelling demos showcasing PathFinder's features
- **Bug fixes**: Help fix failing tests or runtime issues

### For PL Enthusiasts  
- **Type System**: Extend dependent type features
- **Effect System**: Implement missing Tier 2/3 effect handlers
- **Standard Library**: Build out core data structures with safety proofs
- **Parser Extensions**: Add syntactic sugar for common patterns

### For Researchers
- **HoTT Features**: Implement higher inductive types, univalence computing
- **Distributed Proofs**: Design and implement the Tier 0 proof cache
- **Formal Verification**: Prove properties about the type system
- **Compilation**: Explore compilation to efficient targets

## 📝 Development Workflow

1. **Check existing work**
   ```bash
   task-master list  # See current tasks
   task-master next  # Get suggested next task
   ```

2. **Create a feature branch**
   ```bash
   git checkout -b feature/your-feature-name
   ```

3. **Make your changes**
   - Follow existing code style (Racket conventions)
   - Add tests for new functionality
   - Update documentation as needed

4. **Run tests and checks**
   ```bash
   devbox run test   # Run test suite
   devbox run lint   # Check code quality
   devbox run fmt    # Format code
   ```

5. **Submit a pull request**
   - Describe what problem you're solving
   - Include examples if adding features
   - Reference any related issues

## 🏗️ Architecture Overview

```
src/
├── core/          # HoTT foundations
├── typecheck/     # Type checking and effects
├── evaluator/     # Interpreter
├── parser/        # S-expression parser
├── types/         # Advanced type features
└── effects/       # 3-tier effect system
```

Key concepts to understand:
- **HoTT Values**: Everything is represented as inductive constructions
- **Effect Tiers**: Compile-time (1), algebraic (2), runtime (3)
- **Dependent Types**: Types can depend on values
- **Proof-Carrying Values**: Safety proofs bundled with data

## 💡 Design Principles

1. **Mathematical Rigor**: Prefer correctness over performance
2. **Compile-Time Safety**: Catch errors through proofs, not runtime checks
3. **Effect Transparency**: All effects visible in types
4. **No Nulls**: Use effect system instead of Option types
5. **Global Proof Sharing**: Proofs computed once, used everywhere

## 🐛 Reporting Issues

When reporting bugs:
1. Include a minimal reproducible example
2. Show expected vs. actual behavior
3. Mention your environment (OS, Racket version)
4. Check if it's already reported

## 📚 Learning Resources

- [HoTT Book](https://homotopytypetheory.org/book/) - Theoretical foundations
- [Rust Book](https://doc.rust-lang.org/book/) - Bootstrap implementation language
- `docs/values-as-proofs.md` - PathFinder's approach to dependent types
- `examples/` directory - Working code examples

## 🤝 Code of Conduct

- Be respectful and constructive
- Welcome newcomers and help them learn
- Focus on technical merit, not personal attributes
- Remember this is a research project - expect rough edges

## ❓ Questions?

- Open an issue for questions about the codebase
- Use discussions for theoretical/design questions
- Check existing issues first - someone might have asked already

## 🎉 Recognition

Contributors will be recognized in:
- Git history (use meaningful commit messages!)
- README acknowledgments for significant contributions
- Academic papers if your work leads to publications

Welcome to the PathFinder community! We're excited to explore the future of type-safe programming together.