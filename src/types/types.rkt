#lang racket/base

(require racket/contract
         racket/match
         racket/string
         racket/list
         json
         racket/file)

(provide (all-defined-out))

;; PathFinder LISP HoTT-based Type System
;; Implementing proper Homotopy Type Theory foundations

;; Base type structure
(struct hott-type () #:transparent)

;; Universe hierarchy: Type₀ : Type₁ : Type₂ : ...
(struct universe hott-type (level) #:transparent)

;; Basic type formers from HoTT

;; Π-types (dependent function types): (Π (x : A) B(x))
(struct pi-type hott-type (var-name domain codomain) #:transparent)

;; Σ-types (dependent pair types): (Σ (x : A) B(x))  
(struct sigma-type hott-type (var-name first-type second-type) #:transparent)

;; Sum types: A + B
(struct sum-type hott-type (left-type right-type) #:transparent)

;; Identity types: Id A x y (paths/equality)
(struct identity-type hott-type (type-expr left-term right-term) #:transparent)

;; Unit type: 𝟙 (has exactly one element)
(struct unit-type hott-type () #:transparent)

;; Empty type: 𝟘 (has no elements)
(struct empty-type hott-type () #:transparent)

;; Inductive type definitions
(struct inductive-type hott-type (name constructors) #:transparent)

;; Effect-aware types: types that carry effect requirements
(struct effect-type hott-type (base-type required-effects optional-effects) #:transparent)

;; Effect sets for tracking requirements
(struct effect-set (effects) #:transparent)

;; Effect row types (extensible effect records)
(struct effect-row (effects tail) #:transparent)

;; Constructor for inductive types
(struct type-constructor (name param-types result-type) #:transparent)

;; Type contracts - will be updated later
(define hott-type/c 
  (or/c universe? pi-type? sigma-type? sum-type? identity-type? 
        unit-type? empty-type? inductive-type? effect-type?))

;; Predefined universe levels
(define Type0 (universe 0))
(define Type1 (universe 1))
(define Type2 (universe 2))

;; Predefined fundamental types
(define Unit (unit-type))
(define Empty (empty-type))

;; Constructor helpers for common types

;; Simple function type (non-dependent): A → B
(define/contract (make-function-type domain codomain)
  (-> hott-type/c hott-type/c pi-type?)
  (pi-type "_" domain codomain))

;; Effect-aware function type: A →{E} B (function requiring effects E)
(define/contract (make-effect-function-type domain codomain required-effects)
  (-> hott-type/c hott-type/c effect-set? effect-type?)
  (effect-type (pi-type "_" domain codomain) required-effects (effect-set '())))

;; Create effect set
(define/contract (make-effect-set . effects)
  (->* () () #:rest (listof symbol?) effect-set?)
  (effect-set effects))

;; Create effect row
(define/contract (make-effect-row effects [tail #f])
  (->* ((listof symbol?)) ((or/c effect-row? #f)) effect-row?)
  (effect-row effects tail))

;; Effect type operations
(define/contract (effect-union e1 e2)
  (-> effect-set? effect-set? effect-set?)
  (effect-set (remove-duplicates (append (effect-set-effects e1) (effect-set-effects e2)))))

(define/contract (effect-subset? e1 e2)
  (-> effect-set? effect-set? boolean?)
  (andmap (lambda (eff) (member eff (effect-set-effects e2))) (effect-set-effects e1)))

(define/contract (effect-empty? e)
  (-> effect-set? boolean?)
  (null? (effect-set-effects e)))

;; Check if two effect sets are equal
(define/contract (effect-equal? e1 e2)
  (-> effect-set? effect-set? boolean?)
  (and (effect-subset? e1 e2) (effect-subset? e2 e1)))

;; Product type (non-dependent): A × B  
(define/contract (make-product-type first second)
  (-> hott-type/c hott-type/c sigma-type?)
  (sigma-type "_" first second))

;; Dependent function type: (Π (x : A) B(x))
(define/contract (make-pi-type var-name domain codomain)
  (-> string? hott-type/c hott-type/c pi-type?)
  (pi-type var-name domain codomain))

;; Dependent pair type: (Σ (x : A) B(x))
(define/contract (make-sigma-type var-name first second)
  (-> string? hott-type/c hott-type/c sigma-type?)
  (sigma-type var-name first second))

;; Sum type: A + B
(define/contract (make-sum-type left right)
  (-> hott-type/c hott-type/c sum-type?)
  (sum-type left right))

;; Identity type: Id A x y
(define/contract (make-identity-type type-expr left right)
  (-> hott-type/c any/c any/c identity-type?)
  (identity-type type-expr left right))

;; Inductive type definitions

;; Natural numbers: Nat with constructors zero and next
(define Nat-constructors
  (list (type-constructor "zero" '() 'Nat)
        (type-constructor "next" '(Nat) 'Nat)))

(define Nat (inductive-type "Nat" Nat-constructors))

;; Boolean type: Bool with constructors true and false  
(define Bool-constructors
  (list (type-constructor "true" '() 'Bool)
        (type-constructor "false" '() 'Bool)))

(define Bool (inductive-type "Bool" Bool-constructors))


;; List type: (List A) with constructors nil and cons
(define/contract (make-list-type element-type)
  (-> hott-type/c inductive-type?)
  (let ([list-name (string-append "List-" (type->string element-type))])
    (inductive-type list-name
                   (list (type-constructor "nil" '() list-name)
                         (type-constructor "cons" 
                                         (list element-type list-name) 
                                         list-name)))))

;; Type equality in HoTT context
;; Note: This is definitional equality, not the identity type
(define/contract (hott-type-equal? t1 t2)
  (-> hott-type/c hott-type/c boolean?)
  (match* (t1 t2)
    ;; Universe levels
    [((universe level1) (universe level2)) (= level1 level2)]
    
    ;; Π-types are equal if domains and codomains are equal
    [((pi-type var1 dom1 cod1) (pi-type var2 dom2 cod2))
     (and (hott-type-equal? dom1 dom2)
          (hott-type-equal? cod1 cod2))]
    
    ;; Σ-types 
    [((sigma-type var1 first1 second1) (sigma-type var2 first2 second2))
     (and (hott-type-equal? first1 first2)
          (hott-type-equal? second1 second2))]
    
    ;; Sum types
    [((sum-type left1 right1) (sum-type left2 right2))
     (and (hott-type-equal? left1 left2)
          (hott-type-equal? right1 right2))]
    
    ;; Identity types
    [((identity-type type1 l1 r1) (identity-type type2 l2 r2))
     (and (hott-type-equal? type1 type2)
          (equal? l1 l2) (equal? r1 r2))]
    
    ;; Unit and Empty types
    [((unit-type) (unit-type)) #t]
    [((empty-type) (empty-type)) #t]
    
    ;; Inductive types
    [((inductive-type name1 _) (inductive-type name2 _))
     (string=? name1 name2)]
    
    ;; Effect types
    [((effect-type base1 req1 opt1) (effect-type base2 req2 opt2))
     (and (hott-type-equal? base1 base2)
          (effect-equal? req1 req2)
          (effect-equal? opt1 opt2))]
    
    ;; Different type constructors are never equal
    [(_ _) #f]))


;; Check if a type lives in a particular universe
(define/contract (type-in-universe? t universe-level)
  (-> hott-type/c exact-nonnegative-integer? boolean?)
  (match t
    ;; Base types live in Type₀
    [(unit-type) (= universe-level 0)]
    [(empty-type) (= universe-level 0)]
    [(inductive-type _ _) (= universe-level 0)]
    
    ;; Function types: if A : Type_i and B : Type_j, then A → B : Type_(max i j)
    [(pi-type _ domain codomain)
     (let ([dom-level (type-universe-level domain)]
           [cod-level (type-universe-level codomain)])
       (= universe-level (max dom-level cod-level)))]
    
    ;; Similar for other type formers
    [(sigma-type _ first second)
     (let ([first-level (type-universe-level first)]
           [second-level (type-universe-level second)])
       (= universe-level (max first-level second-level)))]
    
    [(sum-type left right)
     (let ([left-level (type-universe-level left)]
           [right-level (type-universe-level right)])
       (= universe-level (max left-level right-level)))]
    
    [_ #f]))

;; Get the universe level of a type
(define/contract (type-universe-level t)
  (-> hott-type/c exact-nonnegative-integer?)
  (match t
    [(universe level) (+ level 1)] ; Type_i : Type_(i+1)
    [(unit-type) 0]
    [(empty-type) 0] 
    [(inductive-type _ _) 0]
    [(pi-type _ domain codomain)
     (max (type-universe-level domain) (type-universe-level codomain))]
    [(sigma-type _ first second)
     (max (type-universe-level first) (type-universe-level second))]
    [(sum-type left right)
     (max (type-universe-level left) (type-universe-level right))]
    [(identity-type type-expr _ _)
     (type-universe-level type-expr)]))

;; Type validation
(define/contract (valid-hott-type? t)
  (-> any/c boolean?)
  (and (hott-type? t) #t))

;; ============================================================================
;; PATH COMPUTATION AND UNIVALENCE
;; ============================================================================

;; Path/identity type structure for computation
(struct path-value (type start end proof) #:transparent)

;; Reflexivity: refl : (a : A) → Id A a a
(define/contract (make-refl type term)
  (-> hott-type/c any/c path-value?)
  (path-value type term term 'refl))

;; Path operations for HoTT computation

;; Transport: transport P p : P(x) → P(y) given p : Id A x y
;; This is the fundamental operation for moving along paths
(struct transport-operation (path-type predicate proof) #:transparent)

(define/contract (make-transport path-type predicate proof)
  (-> identity-type? any/c path-value? transport-operation?)
  (transport-operation path-type predicate proof))

;; Congruence: cong f p : Id B (f x) (f y) given f : A → B and p : Id A x y
(struct cong-operation (function path-proof) #:transparent)

(define/contract (make-cong function path-proof)
  (-> any/c path-value? cong-operation?)
  (cong-operation function path-proof))

;; Path induction (J-eliminator)
;; J : (C : (x y : A) → Id A x y → Type) → 
;;     ((x : A) → C x x (refl x)) →
;;     (x y : A) → (p : Id A x y) → C x y p
(struct j-eliminator (motive base-case) #:transparent)

(define/contract (make-j-eliminator motive base-case)
  (-> any/c any/c j-eliminator?)
  (j-eliminator motive base-case))

;; ============================================================================
;; EQUIVALENCES AND UNIVALENCE
;; ============================================================================

;; Quasi-inverse structure for equivalences
(struct quasi-inverse (func inverse left-inv right-inv) #:transparent)

;; Equivalence type: A ≃ B
(struct equivalence-type hott-type (type-a type-b) #:transparent)

(define/contract (make-equivalence-type type-a type-b)
  (-> hott-type/c hott-type/c equivalence-type?)
  (equivalence-type type-a type-b))

;; Equivalence value: actual equivalence between types
(struct equivalence-value (type-a type-b function quasi-inv) #:transparent)

(define/contract (make-equivalence type-a type-b func qinv)
  (-> hott-type/c hott-type/c any/c quasi-inverse? equivalence-value?)
  (equivalence-value type-a type-b func qinv))

;; Identity equivalence: idEquiv : A ≃ A
(define/contract (make-identity-equivalence type-a)
  (-> hott-type/c equivalence-value?)
  (let ([id-func (lambda (x) x)]
        [qinv (quasi-inverse (lambda (x) x) (lambda (x) x) 'refl-left 'refl-right)])
    (equivalence-value type-a type-a id-func qinv)))

;; Univalence axiom: (A ≃ B) ≃ (Id Type A B)
;; This is the fundamental axiom that makes HoTT "univalent"
(struct univalence-axiom () #:transparent)

(define UA (univalence-axiom))

;; ua : (A ≃ B) → (Id Type A B) - univalence direction
(define/contract (univalence-forward equiv)
  (-> equivalence-value? path-value?)
  (path-value Type1 (equivalence-value-type-a equiv) 
              (equivalence-value-type-b equiv) 'ua-path))

;; ua⁻¹ : (Id Type A B) → (A ≃ B) - reverse univalence
(define/contract (univalence-reverse path)
  (-> path-value? equivalence-value?)
  (make-identity-equivalence (path-value-start path))) ; Simplified

;; Path algebra operations

;; Path concatenation: p ∙ q where p : x = y and q : y = z gives p ∙ q : x = z
(define/contract (path-concat p q)
  (-> path-value? path-value? path-value?)
  (unless (equal? (path-value-end p) (path-value-start q))
    (error "Cannot concatenate paths: end of first path must equal start of second"))
  (path-value (path-value-type p) 
              (path-value-start p) 
              (path-value-end q)
              (list 'concat (path-value-proof p) (path-value-proof q))))

;; Path inverse: p⁻¹ where p : x = y gives p⁻¹ : y = x
(define/contract (path-inverse p)
  (-> path-value? path-value?)
  (path-value (path-value-type p)
              (path-value-end p)
              (path-value-start p) 
              (list 'inverse (path-value-proof p))))

;; Path application to functions (action on paths)
(define/contract (path-apply func p)
  (-> any/c path-value? path-value?)
  (path-value (path-value-type p) ; Simplified - should compute result type
              (func (path-value-start p))
              (func (path-value-end p))
              (list 'ap func (path-value-proof p))))

;; Higher-dimensional paths (2-paths, 3-paths, etc.)
(struct higher-path (dimension base-path target-path proof) #:transparent)

;; ============================================================================
;; SAFETY PROOFS AND DEPENDENT PREDICATES
;; ============================================================================

;; Inequality proofs for safety conditions
;; These are specific identity types that prove ordering relationships

;; Non-equality proof: a ≠ b (equivalent to ¬(Id A a b))
(struct neq-proof (type left right evidence) #:transparent)

;; Less-than proof: a < b  
(struct lt-proof (left right evidence) #:transparent)

;; Less-than-or-equal proof: a ≤ b
(struct lte-proof (left right evidence) #:transparent)

;; Greater-than proof: a > b
(struct gt-proof (left right evidence) #:transparent)

;; Greater-than-or-equal proof: a ≥ b  
(struct gte-proof (left right evidence) #:transparent)

;; Bounds proof: 0 ≤ i < n (for array indexing)
(struct bounds-proof (index bound lower-evidence upper-evidence) #:transparent)

;; Non-zero proof: n ≠ 0 (for division safety)
(struct non-zero-proof (value evidence) #:transparent)

;; Non-empty proof: length l > 0 (for list operations)
(struct non-empty-proof (container length-evidence) #:transparent)

;; ============================================================================
;; TIER 1: COMPUTATIONAL PROOFS (Mathematical Constructor-Producing Functions)
;; ============================================================================
;; In HoTT, proofs ARE computations. These structures contain both the proof
;; and the computed result, enabling compile-time computation injection.

;; Computational division proof: Contains both safety proof and computed result
(struct division-computation-proof (dividend divisor non-zero-proof computed-result) #:transparent)

;; Computational arithmetic proof: Contains operation and computed result  
(struct arithmetic-computation-proof (operation operands computed-result) #:transparent)

;; Computational bounds proof: Contains index validity and computed access
(struct bounds-computation-proof (index bound bounds-proof computed-element) #:transparent)

;; Generic computational proof wrapper
(struct computational-proof (predicate args computed-result evidence) #:transparent)

;; ============================================================================
;; TIER 2: ALGEBRAIC EFFECTS FOR COMPILE-TIME OPERATIONS
;; ============================================================================
;; These structures represent algebraic effects that signal compile-time operations
;; Effects are handled by the build system, not hardcoded as I/O operations

;; ============================================================================
;; GENERIC EFFECT SYSTEM
;; ============================================================================
;; Effects are user-defined, not hardcoded. The type system only knows about
;; the abstract concept of effects, operations, and handlers.

;; Generic effect definition - users define their own effects
(struct effect-definition (name operations) #:transparent)

;; Effect operation signature
(struct effect-operation (name input-types output-type) #:transparent)

;; Generic effect instance - represents any effect operation call
(struct effect-instance (effect-name operation-name arguments) #:transparent)

;; Effect handler definition
;; handler-type can be: symbol (single context), list (multiple contexts), or 'universal
(struct effect-handler (effect-name handler-type implementations) #:transparent)

;; Effect composition is now handled by pure HoTT effects system
;; See: src/effects/pure-hott-effects.rkt

;; Effect result handling moved to pure HoTT effects system
;; See: src/effects/effect-executor.rkt

;; Effect registry for tracking user-defined effects
(struct effect-registry (effects handlers) #:transparent)

;; Create empty effect registry
(define/contract (make-effect-registry)
  (-> effect-registry?)
  (effect-registry (make-hash) (make-hash)))

;; Register a user-defined effect
(define/contract (register-effect! registry effect-def)
  (-> effect-registry? effect-definition? void?)
  (hash-set! (effect-registry-effects registry) 
             (effect-definition-name effect-def) 
             effect-def))

;; Register an effect handler
(define/contract (register-handler! registry handler)
  (-> effect-registry? effect-handler? void?)
  (hash-set! (effect-registry-handlers registry)
             (cons (effect-handler-effect-name handler) 
                   (effect-handler-handler-type handler))
             handler))

;; Create effect definition (user-facing)
(define/contract (defeffect name . operations)
  (->* (symbol?) () #:rest (listof effect-operation?) effect-definition?)
  (effect-definition name operations))

;; Create effect operation (user-facing)
(define/contract (defop name input-types output-type)
  (-> symbol? (listof hott-type/c) hott-type/c effect-operation?)
  (effect-operation name input-types output-type))

;; Create effect instance (when calling an effect operation)
(define/contract (make-effect-instance effect-name op-name args)
  (-> symbol? symbol? (listof any/c) effect-instance?)
  (effect-instance effect-name op-name args))

;; ============================================================================
;; UNIFIED ALGEBRAIC EFFECT SYSTEM (Unison-style)
;; ============================================================================
;; All effects (Tier 2 and Tier 3) use the same algebraic structure
;; The difference is in the handlers: compile-time vs runtime vs capability-based

;; Effect capabilities: structured permissions for effect operations
(struct effect-capability (name permissions context constraints) #:transparent)

;; Effect handler types that determine when/how effects are resolved
(define-values (COMPILE-TIME-HANDLER RUNTIME-HANDLER CAPABILITY-HANDLER TEST-HANDLER)
  (values 'compile-time 'runtime 'capability 'test))

;; Base effect struct
(struct effect () #:transparent)

;; Extended effect types with handler information
(struct handled-effect effect (handler-type handler-context) #:transparent)

;; Capability-based effects (Unison-style)
(struct capability-file-effect effect (capability-token) #:transparent)
(struct capability-network-effect effect (capability-token) #:transparent)
(struct capability-console-effect effect (capability-token) #:transparent)
(struct capability-process-effect effect (capability-token) #:transparent)
(struct capability-async-effect effect (capability-token) #:transparent)

;; Effect result constants (already defined above as struct)
(define-values (EFFECT-SUCCESS EFFECT-FAILURE EFFECT-PENDING)
  (values 'success 'failure 'pending))


;; Effect result creation moved to pure HoTT effects system
;; See: src/effects/effect-executor.rkt

;; Effect capabilities with permissions
(define/contract (make-effect-capability name permissions [context '()] [constraints '()])
  (->* (symbol? (listof symbol?)) ((listof any/c) (listof any/c)) effect-capability?)
  (effect-capability name permissions context constraints))

;; Check if capability allows operation
(define/contract (capability-allows? capability operation)
  (-> effect-capability? symbol? boolean?)
  (member operation (effect-capability-permissions capability)))

;; Safety proof constructors

(define/contract (make-neq-proof type left right evidence)
  (-> hott-type/c any/c any/c any/c neq-proof?)
  (neq-proof type left right evidence))

(define/contract (make-lt-proof left right evidence)
  (-> any/c any/c any/c lt-proof?)
  (lt-proof left right evidence))

(define/contract (make-non-zero-proof value evidence)
  (-> any/c any/c non-zero-proof?)
  (non-zero-proof value evidence))

(define/contract (make-bounds-proof index bound lower upper)
  (-> any/c any/c any/c any/c bounds-proof?)
  (bounds-proof index bound lower upper))

;; ============================================================================
;; TIER 1: COMPUTATIONAL PROOF CONSTRUCTORS
;; ============================================================================

;; Construct a computational division proof - the proof IS the computation
(define/contract (make-computational-division-proof dividend divisor)
  (-> exact-nonnegative-integer? exact-nonnegative-integer? (or/c division-computation-proof? #f))
  (let ([nz-proof (try-prove-non-zero divisor)])
    (if nz-proof
        ;; Proof construction IS the computation - we compute the result here
        (let ([computed-result (quotient dividend divisor)])
          (division-computation-proof dividend divisor nz-proof computed-result))
        #f)))

;; ============================================================================
;; GENERIC EFFECT CONSTRUCTORS
;; ============================================================================

;; Create a generic effect handler
(define/contract (defhandler effect-name handler-type . implementations)
  (->* (symbol? (or/c symbol? (listof symbol?))) () #:rest (listof (cons/c symbol? procedure?)) effect-handler?)
  (effect-handler effect-name handler-type implementations))

;; Generic effect invocation
(define/contract (invoke-effect registry effect-name op-name . args)
  (->* (effect-registry? symbol? symbol?) () #:rest (listof any/c) effect-instance?)
  (let ([effect-def (hash-ref (effect-registry-effects registry) effect-name #f)])
    (if effect-def
        (make-effect-instance effect-name op-name args)
        (error "Unknown effect: " effect-name))))

;; Check if an effect is registered
(define/contract (effect-registered? registry effect-name)
  (-> effect-registry? symbol? boolean?)
  (hash-has-key? (effect-registry-effects registry) effect-name))

;; Get effect operation type signature
(define/contract (get-effect-operation registry effect-name op-name)
  (-> effect-registry? symbol? symbol? (or/c effect-operation? #f))
  (let ([effect-def (hash-ref (effect-registry-effects registry) effect-name #f)])
    (if effect-def
        (findf (lambda (op) (eq? (effect-operation-name op) op-name))
               (effect-definition-operations effect-def))
        #f)))

;; ============================================================================
;; UNIFIED EFFECT CONSTRUCTORS (All Tiers)
;; ============================================================================
;; These create effects that can be handled by different handler types

;; File operations with capability checking
(define/contract (make-file-operation-effect operation file-path [capability #f] [handler-type RUNTIME-HANDLER])
  (->* (symbol? string?) ((or/c effect-capability? #f) symbol?) effect?)
  (let ([effect-data (hash 'operation operation 'path file-path 'capability capability)])
    (if capability
        (capability-file-effect 'file-io effect-data #f capability)
        (handled-effect 'file-io effect-data #f handler-type #f))))

;; Network operations with capability checking  
(define/contract (make-network-operation-effect operation url [payload #f] [capability #f] [handler-type RUNTIME-HANDLER])
  (->* (symbol? string?) (any/c (or/c effect-capability? #f) symbol?) effect?)
  (let ([effect-data (hash 'operation operation 'url url 'payload payload 'capability capability)])
    (if capability
        (capability-network-effect 'network-io effect-data #f capability)
        (handled-effect 'network-io effect-data #f handler-type #f))))

;; Console I/O with capability checking
(define/contract (make-console-operation-effect operation message [capability #f] [handler-type RUNTIME-HANDLER])
  (->* (symbol? string?) ((or/c effect-capability? #f) symbol?) effect?)
  (let ([effect-data (hash 'operation operation 'message message 'capability capability)])
    (if capability
        (capability-console-effect 'console-io effect-data #f capability)
        (handled-effect 'console-io effect-data #f handler-type #f))))

;; Async operations with capability checking
(define/contract (make-async-operation-effect operation computation [capability #f] [handler-type RUNTIME-HANDLER])
  (->* (symbol? any/c) ((or/c effect-capability? #f) symbol?) effect?)
  (let ([effect-data (hash 'operation operation 'computation computation 'capability capability)])
    (if capability
        (capability-async-effect 'async-io effect-data #f capability)
        (handled-effect 'async-io effect-data #f handler-type #f))))

;; Smart constructors for different handler types

;; Tier 1: Pure computational (no effects) - for reference
;; Already implemented in computational proofs

;; Tier 2: Compile-time effects (build fails if unresolved)
(define/contract (make-compile-time-file-read path)
  (-> string? effect?)
  (make-file-operation-effect 'read path #f COMPILE-TIME-HANDLER))

(define/contract (make-compile-time-resource-load resource-type path)
  (-> symbol? string? effect?)
  (let ([effect-data (hash 'resource-type resource-type 'path path)])
    (handled-effect 'resource-load effect-data #f COMPILE-TIME-HANDLER #f)))

;; Tier 3: Runtime effects (dynamic, can fail)
(define/contract (make-runtime-file-read path [fallback #f])
  (->* (string?) (any/c) effect?)
  (let ([effect-data (hash 'operation 'read 'path path 'fallback fallback)])
    (handled-effect 'file-io effect-data #f RUNTIME-HANDLER #f)))

(define/contract (make-runtime-user-input prompt)
  (-> string? effect?)
  (let ([effect-data (hash 'operation 'input 'prompt prompt)])
    (handled-effect 'user-io effect-data #f RUNTIME-HANDLER #f)))

(define/contract (make-runtime-network-request url method [payload #f])
  (->* (string? symbol?) (any/c) effect?)
  (let ([effect-data (hash 'operation method 'url url 'payload payload)])
    (handled-effect 'network-io effect-data #f RUNTIME-HANDLER #f)))

;; Capability-based effects (Unison-style)
(define/contract (make-capability-file-read path file-capability)
  (-> string? effect-capability? effect?)
  (if (capability-allows? file-capability 'read)
      (make-file-operation-effect 'read path file-capability CAPABILITY-HANDLER)
      (error "Capability does not allow file read operations")))

(define/contract (make-capability-network-get url network-capability)
  (-> string? effect-capability? effect?)
  (if (capability-allows? network-capability 'http-get)
      (make-network-operation-effect 'GET url #f network-capability CAPABILITY-HANDLER)
      (error "Capability does not allow network GET operations")))

;; Runtime effects are now user-defined through the generic effect system
;; Users create their own effect definitions using the generic effect constructors above

;; Construct a computational arithmetic proof - computation happens during proof construction  
(define/contract (make-computational-arithmetic-proof operation operands)
  (-> symbol? (listof exact-nonnegative-integer?) arithmetic-computation-proof?)
  (let ([computed-result 
         (match operation
           ['add (apply + operands)]
           ['mult (apply * operands)]
           ['sub (max 0 (apply - operands))]  ; Natural subtraction (truncated at 0)
           [_ (error "Unknown arithmetic operation")])])
    (arithmetic-computation-proof operation operands computed-result)))

;; Generic computational proof constructor
(define/contract (make-computational-proof predicate args computation-fn)
  (-> symbol? (listof any/c) (-> (listof any/c) any/c) computational-proof?)
  (let ([computed-result (computation-fn args)])
    (computational-proof predicate args computed-result 'computational-evidence)))

;; Extract computed result from computational proof or effect result
(define/contract (extract-computed-result proof-or-result)
  (-> any/c any/c)
  (match proof-or-result
    ;; Tier 1: Computational proofs
    [(division-computation-proof _ _ _ result) result]
    [(arithmetic-computation-proof _ _ result) result]
    [(bounds-computation-proof _ _ _ result) result]
    [(computational-proof _ _ result _) result]
    ;; Effect results now handled by pure HoTT effects system
    ;; Direct effects (should be handled, but provide fallback)
    [(effect-instance effect-name op-name args)
     (error (format "Unhandled effect: ~a.~a" effect-name op-name))]
    [_ (error "Not a computational proof or effect result: " proof-or-result)]))

;; ============================================================================
;; PROOF CONSTRUCTION TACTICS
;; ============================================================================

;; Automatic proof construction for simple, decidable cases

;; Try to construct a proof that a ≠ b for natural numbers
(define/contract (try-prove-nat-neq a b)
  (-> exact-nonnegative-integer? exact-nonnegative-integer? (or/c non-zero-proof? #f))
  (if (not (= a b))
      (make-non-zero-proof (abs (- a b)) 'trivial-arithmetic)
      #f))

;; Try to construct a proof that a < b for natural numbers  
(define/contract (try-prove-nat-lt a b)
  (-> exact-nonnegative-integer? exact-nonnegative-integer? (or/c lt-proof? #f))
  (if (< a b)
      (make-lt-proof a b 'trivial-arithmetic)
      #f))

;; Try to construct a proof that n ≠ 0
(define/contract (try-prove-non-zero n)
  (-> exact-nonnegative-integer? (or/c non-zero-proof? #f))
  (if (> n 0)
      (make-non-zero-proof n 'trivial-arithmetic)
      #f))

;; Try to construct a bounds proof for array indexing
(define/contract (try-prove-bounds index bound)
  (-> exact-nonnegative-integer? exact-nonnegative-integer? (or/c bounds-proof? #f))
  (if (and (>= index 0) (< index bound))
      (make-bounds-proof index bound 'trivial-bounds 'trivial-bounds)
      #f))

;; Generic proof construction dispatcher
(define/contract (try-construct-proof predicate . args)
  (->* (symbol?) () #:rest (listof any/c) any/c)
  (match predicate
    ['non-zero 
     (when (= (length args) 1)
       (let ([n (first args)])
         (when (exact-nonnegative-integer? n)
           (try-prove-non-zero n))))]
    ['nat-lt
     (when (= (length args) 2)
       (let ([a (first args)] [b (second args)])
         (when (and (exact-nonnegative-integer? a) (exact-nonnegative-integer? b))
           (try-prove-nat-lt a b))))]
    ['bounds
     (when (= (length args) 2)
       (let ([i (first args)] [n (second args)])
         (when (and (exact-nonnegative-integer? i) (exact-nonnegative-integer? n))
           (try-prove-bounds i n))))]
    [_ #f]))

(define/contract (make-2path base-path target-path proof)
  (-> path-value? path-value? any/c higher-path?)
  (higher-path 2 base-path target-path proof))

;; Function extensionality: if f(x) = g(x) for all x, then f = g
(define/contract (function-extensionality pointwise-equality)
  (-> any/c path-value?)
  (path-value (pi-type "_" 'A 'B) 'f 'g 
              (list 'funext pointwise-equality)))

;; Contractibility and truncation levels
(struct contractible-type (type center contraction) #:transparent)

(define/contract (make-contractible-type type center contraction)
  (-> hott-type/c any/c any/c contractible-type?)
  (contractible-type type center contraction))

;; Proposition (h-level 1): a type where any two elements are equal
(struct proposition-type (type) #:transparent)

;; Set (h-level 2): a type where any two paths are equal  
(struct set-type (type) #:transparent)

;; Update the type contract to include new structures  
(define extended-hott-type/c
  (or/c universe? pi-type? sigma-type? sum-type? identity-type? 
        unit-type? empty-type? inductive-type? equivalence-type?
        contractible-type? proposition-type? set-type? effect-type?))

;; Pretty printing for HoTT types
(define/contract (type->string t)
  (-> extended-hott-type/c string?)
  (match t
    [(universe level) (string-append "Type" (number->string level))]
    [(pi-type "_" domain codomain) 
     (string-append "(" (type->string domain) " → " (type->string codomain) ")")]
    [(pi-type var-name domain codomain)
     (string-append "(Π (" var-name " : " (type->string domain) ") " (type->string codomain) ")")]
    [(sigma-type "_" first second)
     (string-append "(" (type->string first) " × " (type->string second) ")")]
    [(sigma-type var-name first second)
     (string-append "(Σ (" var-name " : " (type->string first) ") " (type->string second) ")")]
    [(sum-type left right)
     (string-append "(" (type->string left) " + " (type->string right) ")")]
    [(identity-type type-expr left right)
     (string-append "(Id " (type->string type-expr) " " (format "~a" left) " " (format "~a" right) ")")]
    [(unit-type) "𝟙"]
    [(empty-type) "𝟘"]
    [(inductive-type name _) name]
    [(equivalence-type type-a type-b) 
     (string-append (type->string type-a) " ≃ " (type->string type-b))]
    [(contractible-type type _ _) 
     (string-append "isContr(" (type->string type) ")")]
    [(proposition-type type) 
     (string-append "isProp(" (type->string type) ")")]
    [(set-type type) 
     (string-append "isSet(" (type->string type) ")")]
    [(effect-type base-type required optional)
     (string-append (type->string base-type) 
                    " →{" (string-join (map symbol->string (effect-set-effects required)) ",") "}"
                    (if (effect-empty? optional) 
                        ""
                        (string-append " [" (string-join (map symbol->string (effect-set-effects optional)) ",") "]")))]))

;; Extended pretty printing for new types
(define/contract (path-value->string p)
  (-> path-value? string?)
  (let ([type-str (if (hott-type? (path-value-type p))
                      (type->string (path-value-type p))
                      (format "~a" (path-value-type p)))]
        [start-str (format "~a" (path-value-start p))]
        [end-str (format "~a" (path-value-end p))])
    (string-append "path[" type-str " : " start-str " = " end-str "]")))

(define/contract (equivalence->string equiv)
  (-> equivalence-value? string?)
  (string-append (type->string (equivalence-value-type-a equiv))
                 " ≃ " 
                 (type->string (equivalence-value-type-b equiv))))

;; Helper predicates are auto-generated by struct definitions

(define/contract (is-refl-path? p)
  (-> path-value? boolean?)
  (and (equal? (path-value-start p) (path-value-end p))
       (eq? (path-value-proof p) 'refl)))

;; Path computation rules

;; Reflexivity computation: refl ∙ p = p and p ∙ refl = p  
(define/contract (normalize-path-concat p q)
  (-> path-value? path-value? path-value?)
  (cond
    [(is-refl-path? p) q]
    [(is-refl-path? q) p] 
    [else (path-concat p q)]))

;; Inverse computation: (p⁻¹)⁻¹ = p and p ∙ p⁻¹ = refl
(define/contract (normalize-path-inverse p)
  (-> path-value? path-value?)
  (match (path-value-proof p)
    [(list 'inverse inner-proof) 
     (path-value (path-value-type p) (path-value-end p) (path-value-start p) inner-proof)]
    [_ (path-inverse p)]))