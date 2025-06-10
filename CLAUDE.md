# Development Guide for Claude

### **A Note on Formal Writing Style**

When documenting or discussing this project, please maintain a modest but confident tone. Avoid superlatives, hyperbole, and overly strong claims. Our work should be presented with clarity and professional confidence, allowing its technical merits to speak for themselves.

## PathFinder LISP: Core Development Principles

Our development approach is centered on a key insight: a strong foundation in Homotopy Type Theory (HoTT) simplifies or eliminates the need for many conventional programming language features. Instead of accumulating features as workarounds, we leverage PathFinder's HoTT constructs to find more direct and mathematically sound solutions.

Our goal is to demonstrate that a focused core is more effective than a wide array of features. When developing, always consider the HoTT-native solution first.

### **HoTT-First Implementation Guidelines**

1.  **Use Type Families for Generics:** All generic operations (e.g., equality, ordering) should be implemented using our tier-aware type family system. Avoid ad-hoc polymorphism.
2.  **Use Identity Types for Equality:** Functions that check for equality should return identity type proofs, not boolean values. This provides richer, verifiable information.
3.  **Use Universe Polymorphism for Generic Functions:** Define generic functions using explicit type parameters over universes (`Type₀`, `Type₁`, etc.) rather than creating monomorphic implementations for each type.
4.  **Use Pure HoTT Effects for I/O:** All computational behaviors, including I/O, should be modeled as pure effects. This separates the description of an effect from its execution, ensuring mathematical purity.

### **Alternative Approaches: How HoTT Replaces Conventional Features**

PathFinder's design avoids certain traditional language features by providing more fundamental alternatives.

| Conventional Feature | PathFinder's Alternative | Rationale |
| :--- | :--- | :--- |
| **Option/Result Types** | Effects and Dependent Types | Offers more precise and composable ways to handle potential failures or variations. |
| **Macro Systems** | Type Families and Tier System | Provides a mathematically principled way to handle metaprogramming without staging issues. |
| **Pattern Matching** | HoTT Eliminators and Induction | Ensures that functions are total by construction and can carry proofs. |
| **Complex Generics** | Universe Polymorphism | Offers a clean, mathematical approach without the complexity of systems like C++ templates. |

### **Development Workflow**

1.  **Design:** Frame the problem in HoTT terms. Identify the necessary type families, universe levels, identity proofs, and effects.
2.  **Implement:** Use PathFinder's existing HoTT constructs to build the solution.
3.  **Test:** Verify the HoTT properties of your implementation, including universe polymorphism, path coherence, and effect composition.

### **Development Red Flags**

Be mindful of implementation patterns that diverge from our core philosophy:

* Using hard-coded type dispatch instead of type families.
* Returning boolean values for equality instead of identity type proofs.
* Implementing exception handling instead of using the pure effects system.
* Relying on host language data structures instead of HoTT constructor values.
* Writing manual memoization instead of using the built-in content-addressable cache.

When in doubt, ask: "Is this feature a fundamental need, or is it a workaround for a weak foundation?" If it's a workaround, seek the HoTT-native solution instead.