**Product Requirements Document: "PathFinder LISP" \- A HoTT-based Practical Programming Language**  
Version: 0.4  
Date: 2025-06-01  
Author: Gemini AI & User  
1\. Introduction  
PathFinder LISP is a statically-typed, functional programming language with a LISP-based syntax, specifically drawing inspiration from the Scheme philosophy of minimalism and powerful, hygienic metaprogramming. It is designed to bring the power and safety of Homotopy Type Theory (HoTT) to practical software development and facilitate deep conceptual exploration of HoTT in a programmable context. PathFinder LISP aims to provide exceptionally strong compile-time guarantees by treating types not just as classifications of data, but as mathematical spaces with inherent notions of equality (paths) and equivalence. A core design principle is that all operations produce constructor values - data constructors create fundamental type instances while constructor-producing functions build complex values with computational evidence. Every type has canonical data constructors (like zero, next for Nat) and operations that produce constructor values (like addition, which builds Nat constructor values with arithmetic proofs). The Scheme-like LISP syntax is chosen to maximize ease of parsing, accelerate prototyping of HoTT concepts, and leverage hygienic macros for robust language extension and experimentation.  
2\. Vision  
To empower developers and researchers to build robust, verifiable, and highly abstract software by leveraging the deep connections between type theory and homotopy theory. PathFinder LISP aims to make advanced concepts of program structure and equivalence an integral part of the programming experience, encouraging a clean separation between effectful construction logic and pure computational logic, all within an extensible, conceptually transparent, and minimalist LISP environment.  
**3\. Goals**

* **Strong Static Guarantees:** Leverage HoTT to catch a wide range of errors at compile-time.  
* **Expressive Type System:** Directly support dependent types, universes, product types, sum types, and identity types, with syntax amenable to Scheme-like LISP's structure.  
* **Intrinsic Equality:** Make the HoTT concept of "equality as paths" a first-class citizen, allowing reasoning about program equivalence.  
* **Constructor Values:** Every operation produces constructor values with computational evidence. Data constructors build fundamental instances, while constructor-producing functions build complex values with proofs.  
* **Evidence-Carrying Values:** All values carry mathematical proofs of their properties and construction history, ensuring safety through computational evidence rather than runtime checks.  
* **Metaprogrammatic Power & Extensibility:** Utilize LISP's homoiconicity and a Scheme-like hygienic macro system to allow for powerful syntactic abstractions, experimentation with new HoTT-related language features, and definition of domain-specific constructs.  
* **Conceptual Clarity & Rapid Prototyping:** The minimalist LISP syntax should aid in directly representing, manipulating, and rapidly iterating on type-theoretic constructs during language development and advanced use.  
* **Modularity:** Provide mechanisms for organizing code into reusable modules/packages, typical of Scheme-like LISP systems.  
* **Encourage Sound Design:** The effect system should encourage developers to handle construction logic upstream, separating it from core, potentially pure, business logic.

**4\. Non-Goals**

* Performance as a Primary Driver (Initial Version): Focus is on correctness, expressiveness, safety, and conceptual exploration.  
* Extensive Concurrency Model (Initial Version).  
* Direct Interoperability with Existing Mainstream Languages (Initial Version).  
* Beginner-Friendliness for Programmers Unfamiliar with LISP or Type Theory: A learning curve is expected and embraced for the conceptual exploration phase.  
* Comprehensive Standard Library (Initial Version): A core library will be provided, with foundational types defined from first principles using data constructors and constructor-producing functions.  
* Highly Optimized Compiler (Initial Version): Focus is on implementing the core HoTT semantics within the LISP framework.

**5\. Target Audience**

* Programming language researchers and designers exploring HoTT, effect systems, and the application of metaprogramming to type theory.  
* Developers and researchers working on high-assurance systems where verifiability and strong foundations are key.  
* Programmers familiar with LISP dialects (especially Scheme) and functional programming languages with advanced type systems (e.g., Racket, Haskell, Agda, Idris).  
* Mathematicians and logicians interested in computational perspectives on HoTT.

**6\. Core Concepts (HoTT for Programmers)**

* **Types as Spaces:** Types are like topological spaces; values are "points" in these spaces. (HoTT Book Ch. 2).  
* **Equality as Paths (Id A x y):** The proposition x \= y (for x, y : A) is itself a type, (Id A x y). An element of this type is a "path" or "proof" demonstrating the equality. (refl A x) is the trivial path (reflexivity). (HoTT Book Ch. 1.12).  
* **Functions as Continuous Maps:** Functions preserve the path structure between types. (HoTT Book Ch. 2.2).  
* **Canonical Instantiation Function (CIF):**  
  * Every type definition (deftype T (params...) ...) implicitly defines its CIF, invoked as (T args...).  
  * The act of calling this CIF is a computational effect.  
  * If the CIF has constraints (e.g., a :where clause) that might not be met, or if its underlying structure involves effectful CIFs, it implicitly performs the ConstructionFailure effect.  
  * Pure CIFs (provably no constraints to fail, using only pure underlying structures, and no declared user effects) perform no effects, and their calls directly yield an instance.  
* **Univalence Axiom:** "Isomorphic types are equal." If two types A and B are equivalent ((≃ A B)), then the type (Id Type A B) is inhabited. (HoTT Book Ch. 2.10).  
* **Propositions as Types:** Logical propositions are represented as types. A proposition is true if its corresponding type is inhabited. (HoTT Book Ch. 1.11).

**7\. Language Features: Syntax and Semantics**  
**7.1. Basic Syntax**

* Scheme-like LISP S-expression syntax: (operator operand1 operand2 ...).  
* Comments: ; line comment. \#| block comment |\# (or dialect-specific convention).  
* Keywords: Symbols, typically prefixed with : for keyword arguments or special markers within forms.

**7.2. Lexical Structure**

* Atoms: Symbols (e.g., my-variable, \+, true), numbers, strings.  
* Lists: (...).  
* Identifiers: Symbols. PathFinder LISP will be **case-sensitive** for identifiers.

**7.3. Core Definitions**

* **define for values and functions:**  
  Lisp  
  (define identifier : Type expression)

  (define (identifier (param1 : Type1) (param2 : Type2) ...) : ReturnType  
          (:performs (Effect1 Effect2 ...)) ; Optional  
          expression-body)

* **deftype for type definitions (defines the CIF):**  
  * The deftype form defines both the type and its CIF. The type name itself is used as the constructor function.  
  * Parameters to deftype are the parameters to its CIF.  
  * CIFs are implicitly effectful with ConstructionFailure if a :where clause could fail or if underlying CIFs are effectful. Inferred by the compiler.  
  * Additional user-defined effects are declared via :performs.

  Lisp  
    (deftype TypeName ((param1 : ParamType1) ...)  
        underlying-structure-definition ; e.g., (struct ((field1 Type1)) (enum ...))  
        (:where (constraint1 ...) (constraint2 ...)) ; Optional  
        (:performs (UserEffect1 UserEffect2 ...)))    ; Optional

  * **Example (Effectful CIF):**  
    Lisp  
    ; (Array A) CIF assumed pure for this example  
    (deftype NonEmptyVec ((A : Type) (n : Nat))  
        (Array A)  
        (:where (not (Id Nat n Nat.zero))) ; Constraint: n \> 0  
    )  
    ; This CIF implicitly performs {ConstructionFailure}

  * **Example (Pure CIF):**  
    Lisp  
    (deftype Point ((x : Nat) (y : Nat))  
        (\* Nat Nat) ; Product type as underlying structure  
    )

  * **Instantiation (calling the CIF):**  
    * Pure CIFs:  
      Lisp  
      (define p : Point (Point (Nat.from-int 3\) (Nat.from-int 4)))

    * Effectful CIFs:  
      Lisp  
      (try  
        (define my-vec : (NonEmptyVec Int 5\) (NonEmptyVec Int (Nat.from-int 5)))  
        (:catch (e : ConstructionError) (...))  
      )

  * underlying-structure-definition can use forms like (struct ((field-name : FieldType) ...)) or (enum Variant1 (Variant2 (field : FieldType)) ...), or refer to existing types.  
  * ConstructionError is a built-in sum type.

**7.4. Types** (Syntax examples)

* **Universes:** Type (or U0), (Type 1\) (or U1), etc. (Type : (Type 1)). (HoTT Book Ch. 1.3)  
* **Function Types (Π-types):** (-\> A B C). (Π ((x : A)) (B x)). (HoTT Book Ch. 1.2, 1.4)  
* **Product Types (Σ-types):** (\* A B). (Σ ((x : A)) (B x)). (HoTT Book Ch. 1.5, 1.6)  
* **Sum Types (Coproducts):** Via deftype with an (enum ...) form. (HoTT Book Ch. 1.7)  
* **Identity Types:** (Id A x y). (HoTT Book Ch. 1.12)

**7.5. Terms and Values**

* Literals, Symbols, Lambdas: (lambda ((x : A)) body).  
* Application: (f arg1 arg2).  
* CIF calls: (TypeName arg1 ...).  
* perform: (perform EffectName.operation arg1 ...).  
* match for pattern matching and effect handlers.

7.6. Canonical Instantiation Function (CIF) \- Semantics  
(Logic as in PRD v0.2, Section 7.6, adapted to LISP invocation style).  
A CIF is pure if the compiler can determine that for all well-typed inputs, its :where clause will not fail, all underlying CIFs used are pure, and it performs no other declared effects. Pure CIF calls do not require try...handle.  
7.6.1. Defining Foundational Types with CIFs (LISP examples)  
Demonstrates defining types from first principles. These are typically pure CIFs.

* **Bool:**  
  Lisp  
  (deftype Bool () (enum true false)) \[cite: 566\]  
  ; (define t : Bool Bool.true)

* **Option:**  
  Lisp  
  (deftype Option ((A : Type))  
      (enum (None) (Some (value : A)))) \[cite: 567\]  
  ; (define s : (Option Int) (Option.Some Int 10))

* **Result (user-defined):**  
  Lisp  
  (deftype Result ((V : Type) (E : Type))  
      (enum (Ok (value : V)) (Err (error : E)))) \[cite: 569\]  
  ; (define ok-res : (Result Int String) (Result.Ok Int String 100))

* **Nat (Peano Naturals):**  
  Lisp  
  (deftype Nat ()  
      (enum zero (succ (pred : Nat)))) \[cite: 571\]  
  ; (define one : Nat (Nat.succ Nat.zero))

* **List:**  
  Lisp  
  (deftype List ((A : Type))  
      (enum Nil (Cons (head : A) (tail : (List A))))) \[cite: 573\]

**7.7. Equality, 7.8. Univalence, 7.9. Pattern Matching and Recursion, 7.10. Modules and Namespacing**

* To be defined following LISP conventions and HoTT principles.  
* Pattern matching via (match ...) will also be used for effect handlers. Definitional equality (≡ in HoTT book) will be handled by the compiler's reduction engine. Propositional equality (Id) is a type manipulated by the programmer.

8\. Algebraic Effects  
8.1. Overview and Motivation  
PathFinder LISP employs an algebraic effect system to separate computational logic from effect handling. Effects allow for non-local control flow type-safely; handlers interpret effects.  
**8.2. Declaring Effects**

Lisp

(defeffect MyEffectCapability  
    (operation1 (p1 : Type1) : ReturnType1) \[cite: 577\]  
    (operation2 (p2 : Type2) (p3 : Type3) : ReturnType2)) \[cite: 578\]

8.3. Performing Effects  
Functions (and CIFs) declare effects via :performs.

Lisp

(define (foo (x : Int)) : String  
        (:performs (MyEffectCapability AnotherEffect))  
        (let ((temp (perform MyEffectCapability.operation1 x))) \[cite: 579\]  
             (perform AnotherEffect.someOp temp))) \[cite: 579\]

(perform ...) suspends computation, transferring control to the handler.

**8.4. The ConstructionFailure Effect**

* Built-in (conceptual declaration): (defeffect ConstructionFailure (error : ConstructionError)).  
* Implicitly performed by a CIF on constraint failure or underlying CIF failure. Not declared in :performs; inferred by the compiler.  
* ConstructionError sum type (defined with deftype):  
  Lisp  
  (deftype ConstructionError ()  
      (enum (ConstraintViolated (constraint-description : String) (failed-proposition-proof : Type)) ; Type is the actual failed proposition if representable \[cite: 584\]  
            (UnderlyingConstructionFailed (type-name : String) (cause : ConstructionError)) \[cite: 585\]  
            ; ... other potential reasons  
      )  
  )

**8.5. Handling Effects**

Lisp

(try  
    (computation-that-might-perform-effects)  
  (:handle (MyEffectCapability op)  
    (match op  
      ((operation1 payload k) ; 'k' is the continuation \[cite: 587\]  
       (let ((result (handle-operation1 payload)))  
         (resume k result)))   ; Resume the computation \[cite: 588\]  
      ((operation2 (p1 p2) k)  
       (let ((result (handle-operation2 p1 p2)))  
         (resume k result))))) \[cite: 589, 590\]  
  (:handle (AnotherEffect op) (...))  
)

; Simplified syntax for ConstructionFailure:  
(try  
  (define my-instance (MyConstrainedType args)) \[cite: 591\]  
  (use my-instance)  
  (:catch (err : ConstructionError) \[cite: 591\]  
    (print (+ "Failed: " (toString err)))  
    (fallback-value))) \[cite: 592\]

Handlers define operation meanings. (resume k value) passes value back. Handlers for ConstructionFailure might provide alternatives.

8.6. Effect-Based Error Handling  
Details how ConstructionFailure serves as PathFinder LISP's primary mechanism for handling errors from CIF calls. (Result V E) is a valuable user-defined type for explicit success/failure data in pure computations.  
9\. Standard Library (Initial Ideas)  
All types defined using CIFs; no compiler/runtime built-ins outside this mechanism.

* Core data structures: (Option A), (Result V E), (List A), (Vec A n) (all via deftype).  
* Basic Nat, Int operations.  
* Id manipulation utilities: (id-symm p), (id-trans p q), (id-ap f p), (id-transport P p u) (HoTT Book Chapter 2).  
* Basic propositions: True (e.g., Unit type), False (e.g., Void or empty enum) (HoTT Book Table 1).  
* Logical connectives (macros): (And A B) for (\* A B), (Or A B) for (+ A B) or (Sum A B), (Not A) for (-\> A False) (HoTT Book Table 1).  
* Propositional truncation: (PropTrunc A) (HoTT Book Chapter 3.7).

10\. Higher Inductive Types (HITs) \- Future Consideration  
While powerful for types like S¹, HITs add significant complexity. To be considered beyond 1.0. Their CIFs would involve path constructors. LISP's hygienic macro system would be exceptionally well-suited for experimenting with HIT definitions and their complex induction principles.  
**11\. Open Questions/Future Considerations**

* Ergonomic handling of algebraic effects in general (e.g., more concise handler syntax).  
* Interaction between ConstructionFailure and user-defined effects if a CIF performs both.  
* Performance of algebraic effects.  
* Exploring the full potential of LISP's hygienic macros for user-defined type formers, HoTT-specific DSLs, and proof/reasoning utilities.  
* Defining an optional, more ML-like surface syntax that desugars to the LISP core if broader appeal becomes a goal in later stages.  
* How to best represent and make usable the higher-order nature of principles like path induction (HoTT Book Ch. 1.12.1) or HIT induction principles within LISP.  
* The precise mechanism for universe polymorphism and handling universe levels (Type, (Type 1), etc.) within the LISP syntax.  
* REPL-driven development experience and debugging for a HoTT-based language.

---

