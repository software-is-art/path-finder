# PathFinder LISP Development Guidelines for Claude

## Revolutionary Insight: HoTT Eliminates Traditional Language Complexity

PathFinder LISP represents a **paradigm shift**: when you have solid HoTT foundations, most traditional programming language features become **unnecessary complications**. Our sophisticated HoTT implementation (tier-aware type families, universe polymorphism, pure effects, identity types) eliminates entire categories of language complexity that other languages accumulate as workarounds.

## Core Philosophy: HoTT Minimalism Over Feature Accumulation

PathFinder demonstrates that **less can be more** when foundations are mathematically sound. Instead of adding traditional language features, **leverage PathFinder's pure HoTT constructs** to solve problems more elegantly than conventional approaches.

## HoTT-Native Implementation Principles

### 1. **Use Type Families for All Generics**
- **Never implement ad-hoc polymorphism** - use the tier-aware type family system in `src/types/type-families.rkt`
- **Register type families** for any generic operation (equality, ordering, serialization, etc.)
- **Leverage tier-aware instantiation**: Tier 1 (compile-time), Tier 2 (type-resolution effects), Tier 3 (runtime dispatch)

```racket
;; ✅ HoTT-native way
(define Equal-family 
  (make-type-family 'Equal 1 
    (λ (A) (make-decidable-equality-type A))))

;; ❌ Traditional way - avoid this
(define (generic-equal? x y) 
  (cond [(nat? x) (nat-equal? x y)] ...))
```

### 2. **Use Identity Types for Equality and Proofs**
- **Always return identity type proofs** rather than just Bool values
- **Leverage existing path operations**: `make-refl`, `path-concat`, `path-inverse`, `transport`, `cong`
- **Use the J-eliminator** for path induction when needed

```racket
;; ✅ HoTT-native equality with proof
(equal-with-proof : (A : Type₀) → A → A → (Id A x y) + ¬(Id A x y))

;; ❌ Traditional boolean equality - avoid this  
(equal? : A → A → Bool)
```

### 3. **Use Universe Polymorphism for Generic Functions**
- **Explicit type parameters** using Π-types over universes
- **Universe hierarchy**: `Type0`, `Type1`, `Type2` already implemented
- **Dependent types** for precise specifications

```racket
;; ✅ Universe polymorphic
(generic-map : (A B : Type₀) → (A → B) → List A → List B)

;; ❌ Monomorphic implementations - avoid duplicating for each type
```

### 4. **Use Effect System for Computational Behavior**
- **Pure HoTT effects** for type-level computation
- **Effect-aware function types** for operations with requirements
- **Algebraic effects** for control flow and error handling

```racket
;; ✅ Effect-aware generic function
(divide : Nat → Nat → Nat) ⟨DivisionByZero⟩

;; ❌ Exception-based error handling - use effects instead
```

## PathFinder's Advanced HoTT Features (Use These!)

### **Tier-Aware Type Families** (`src/types/type-families.rkt`)
- **Compile-time specialization** when types are static
- **Runtime polymorphism** when types are dynamic  
- **Zero-cost abstractions** for known type parameters
- **Automatic cache management** for type instantiations

### **Identity Types and Path Operations** (`src/types/types.rkt`)
- **Path construction**: `make-refl`, `path-concat`, `path-inverse`
- **Path elimination**: J-eliminator with `make-j-eliminator`
- **Transport and congruence**: `make-transport`, `make-cong`
- **Univalence axiom**: `univalence-forward`, `univalence-reverse`

### **Universe Hierarchy** (`src/types/types.rkt`)
- **Explicit universe levels**: `Type0 : Type1 : Type2`
- **Universe polymorphism** via Π-types
- **Cumulative hierarchy** for type inclusion

### **Pure HoTT Effects System** (`src/effects/`)
- **Effects as mathematical objects** (not computational side-effects)
- **Algebraic effect handlers** for pure functional control flow
- **Effect inference and checking** integrated with type system

### **Content-Addressable Caching** (`src/core/hott-cache.rkt`)
- **Automatic memoization** of pure computations
- **Tier promotion** for performance optimization
- **Persistent cache** across compilation units

## Implementation Workflow

### 1. **Design Phase**: Think in HoTT Terms
- What **type family** does this operation belong to?
- What **universe level** should it live in?
- What **identity type proofs** should it construct?
- What **effects** does it require?

### 2. **Implementation Phase**: Use PathFinder's HoTT Constructs
- **Register type families** first
- **Define Π-types** for function signatures  
- **Construct identity type proofs** for correctness
- **Use effect system** for computational behavior

### 3. **Testing Phase**: Verify HoTT Properties
- **Test universe polymorphism** with different type parameters
- **Verify path coherence** for identity type operations
- **Check tier performance** (compile-time vs runtime)
- **Validate effect tracking** and handler composition

## Specific File Locations

### Core HoTT Implementation
- **Type system**: `src/types/types.rkt` - universe hierarchy, identity types, Π/Σ types
- **Type families**: `src/types/type-families.rkt` - tier-aware generic programming
- **Values**: `src/evaluator/values.rkt` - runtime values with path operations
- **HoTT evaluator**: `src/core/hott-evaluator.rkt` - pure HoTT computations

### Effects System  
- **Generic effects**: `src/effects/generic-effects.rkt` - effect definitions
- **Effect checker**: `src/typecheck/effect-checker.rkt` - effect inference
- **Pure HoTT effects**: `src/effects/pure-hott-effects.rkt` - mathematical effects

### Advanced Features
- **Caching**: `src/core/hott-cache.rkt` - content-addressable memoization
- **Tier promotion**: `src/core/tier-promotion.rkt` - performance optimization
- **Dependent safety**: `src/types/dependent-safety.rkt` - proof-carrying code

## Examples of Dogfooding Done Right

### **List Implementation** (`src/types/list-type.rkt`)
✅ Uses HoTT constructor values, structural recursion proofs, type-safe operations

### **Natural Number Arithmetic** (`src/core/hott-evaluator.rkt`) 
✅ Pure HoTT Peano arithmetic with identity type equality

### **Generic Effects System** (`src/effects/generic-effects.rkt`)
✅ Effects as first-class mathematical objects, not side-effects

## Red Flags: When You're Not Dogfooding

- ❌ **Hard-coded type dispatch** instead of type families
- ❌ **Boolean return values** instead of identity type proofs  
- ❌ **Exception handling** instead of algebraic effects
- ❌ **Ad-hoc polymorphism** instead of universe polymorphism
- ❌ **Racket-native data structures** instead of HoTT constructor values
- ❌ **Manual memoization** instead of content-addressable caching

## Remember: We Have World-Class HoTT Infrastructure

PathFinder's HoTT implementation is **more advanced than most research proof assistants**. The tier-aware type family system, universe polymorphism, pure effects, and content-addressable caching are cutting-edge features that should be leveraged for every new language feature.

**When in doubt, ask: "How would this be implemented in pure HoTT theory?" Then use PathFinder's existing infrastructure to implement exactly that.**

## 🚨 CRITICAL INSIGHT: What HoTT Makes Obsolete

### **Traditional Features PathFinder Doesn't Need:**

#### **1. Option/Result Types → Use Effects + Dependent Types**
```rust
// ❌ Traditional approach
fn divide(a: i32, b: i32) -> Result<i32, DivisionError>
fn find_user(id: u32) -> Option<User>
```

```hott
-- ✅ HoTT approach: Effects are superior
divide : Nat → Nat → Nat ⟨DivisionByZero⟩
find-user : UserId → User ⟨NotFound⟩

-- Or dependent types with proofs
find-user : (id : UserId) → (proof : user-exists id) → User
```

**Why HoTT is better:** Effects are more precise, composable, and mathematical than generic sum types.

#### **2. Macro Systems → Use Type Families + Tier System**
```lisp
;; ❌ Traditional macros
(defmacro when (condition &body body)
  `(if ,condition (progn ,@body)))
```

```racket
;; ✅ HoTT approach: Type families with tier-aware instantiation
(define Control-family
  (make-type-family 'Control 2 control-instantiation))
;; Tier 1: Compile-time expansion
;; Tier 2: Type-resolution effects  
;; Tier 3: Runtime dispatch
```

**Why HoTT is better:** No staging issues, mathematically principled, automatic optimization.

#### **3. Pattern Matching → Use HoTT Eliminators**
```haskell
-- ❌ Traditional pattern matching
case list of
  []     -> defaultValue
  (x:xs) -> processHead x xs
```

```hott
-- ✅ HoTT approach: J-eliminator and induction principles
list-eliminator : (P : List T → Type₀) → 
                  P nil → 
                  ((x : T) → (xs : List T) → P xs → P (cons x xs)) →
                  (l : List T) → P l
```

**Why HoTT is better:** More principled, total by construction, proof-carrying.

#### **4. Complex Generics → Use Universe Polymorphism**
```cpp
// ❌ Traditional C++ templates
template<typename T, typename F>
auto map(const std::vector<T>& vec, F func) -> std::vector<decltype(func(T{}))>
```

```hott
-- ✅ HoTT approach: Universe polymorphic functions
map : (A B : Type₀) → (A → B) → List A → List B
```

**Why HoTT is better:** Clean, mathematical, no template metaprogramming complexity.

### **The Pattern: HoTT Eliminates Workarounds**

| Traditional Problem | Traditional Solution | HoTT Solution | Why HoTT Wins |
|-------------------|---------------------|---------------|---------------|
| Null pointers | `Option<T>` | Dependent types + proofs | More precise |
| Error handling | `Result<T,E>` | Algebraic effects | More compositional |
| Code generation | Macros | Type families + tiers | No staging issues |
| Runtime reflection | Metaprogramming | Tier-aware caching | Automatic |
| Generic programming | Templates/type classes | Universe polymorphism | Mathematically clean |
| Memory safety | Ownership systems | Dependent types | Proof-carrying |

## PathFinder IL: Purity Over Syntax Sugar

### **Key Decision: IL Stays Pure**
- **PathFinder IL** should remain mathematically pure and explicit
- **User-friendly syntax** can be implemented as frontends that compile to IL
- **Don't compromise IL purity** for beginner-friendliness - build sugar layers instead

### **What This Means:**
```racket
;; ✅ Keep IL explicit and pure
(define List-constructors
  (list (type-constructor "nil" '() 'List)
        (type-constructor "cons" (list 'T 'List) 'List)))
(define List (inductive-type "List" List-constructors))

;; ✅ User syntax can compile to this
;; deftype List T = Nil | Cons T (List T)  
;; ↓ compiles to IL above
```

## Updated Development Strategy

### **STOP Adding Traditional Features**
- ❌ Don't implement Option/Result types
- ❌ Don't build macro systems  
- ❌ Don't add complex pattern matching syntax
- ❌ Don't create traditional generic systems

### **START Demonstrating HoTT Superiority**
- ✅ Show how effects replace Option/Result
- ✅ Demonstrate type families replacing macros
- ✅ Prove HoTT eliminators are more powerful than pattern matching
- ✅ Document the mathematical elegance

### **Focus Areas:**
1. **Complete the algebraic effect system** (Task #7)
2. **Polish existing HoTT constructs** 
3. **Create compelling examples** showing HoTT superiority
4. **Document the paradigm shift** for users coming from traditional languages

## The Revolutionary Claim

**PathFinder proves that most programming language complexity is accidental.** When you have:
- Solid mathematical foundations (HoTT)
- Sophisticated type system (universe hierarchy, type families) 
- Tier-aware execution (compile-time ↔ runtime bridge)
- Content-addressable caching (automatic optimization)

...you don't need the complexity that other languages accumulate. **PathFinder's minimalist HoTT core is more powerful than kitchen-sink languages.**

**When in doubt, ask: "Is this a fundamental need, or a workaround for weak foundations?" If it's a workaround, find the HoTT-native solution instead.**