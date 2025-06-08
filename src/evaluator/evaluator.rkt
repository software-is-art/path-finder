#lang racket/base

(require racket/contract
         racket/match
         racket/list
         racket/string
         "../parser/ast.rkt"
         "../types/types.rkt"
         "../effects/pure-hott-effects.rkt"
         "../effects/effect-executor.rkt"
         "values.rkt"
         "../core/hott-ast.rkt"
         "../core/hott-evaluator.rkt"
         "../core/hott-literals-pure.rkt"
         "../core/host-bridge.rkt"
         "../core/hott-cache.rkt"
         "../core/hott-cache-persistence.rkt")

(provide evaluate
         make-environment
         make-global-environment
         environment?
         env-lookup
         initialize-evaluator-cache
         shutdown-evaluator-cache
         global-hott-cache
         compute-operation-cache-key
         zero-value
         reset-global-cache!)

;; Environment for lexical scoping
(struct environment (bindings parent) #:transparent)

;; ============================================================================
;; TRANSPARENT CACHE INTEGRATION
;; ============================================================================

;; Global computation cache (pure HoTT) - initialized from persistence
(define global-hott-cache (make-empty-cache))

;; Initialize cache system with persistent storage
(define/contract (initialize-evaluator-cache)
  (-> void?)
  (set! global-hott-cache (initialize-persistent-cache))
  (printf "Evaluator cache initialized with persistence.~n"))

;; Shutdown cache system and save to persistent storage
(define/contract (shutdown-evaluator-cache)
  (-> void?)
  (shutdown-persistent-cache global-hott-cache)
  (printf "Evaluator cache shutdown complete.~n"))

;; Reset global cache to empty state (for demos/testing)
(define/contract (reset-global-cache!)
  (-> void?)
  (set! global-hott-cache (make-empty-cache)))

;; Cache-aware evaluation wrapper
(define/contract (evaluate-with-cache operation args env evaluation-thunk)
  (-> string? (listof value/c) environment? (-> value/c) value/c)
  ;; Try cache lookup first
  (let* ([cache-key (compute-operation-cache-key operation args)]
         [cached-result (hott-cache-lookup cache-key global-hott-cache)])
    (match cached-result
      [(constructor-value "some" (list value) _)
       ;; Cache hit - return cached value (Tier promotion!)
       value]
      [(constructor-value "none" _ _)
       ;; Cache miss - evaluate and cache result
       (let ([result (evaluation-thunk)])
         ;; Cache the result if it's deterministic
         (when (operation-cacheable? operation)
           (set! global-hott-cache 
                 (hott-cache-insert cache-key result global-hott-cache)))
         result)]
      [_ 
       ;; Fallback to normal evaluation
       (evaluation-thunk)])))

;; Compute cache key for an operation
(define/contract (compute-operation-cache-key operation args)
  (-> string? (listof value/c) constructor-value?)
  ;; Create a composite key from operation name and arguments
  (let ([op-addr (compute-content-address 
                   (inductive-type "String" '()) 
                   (pure-racket-string->hott-string operation))]
        [args-addr (compute-content-address
                     (inductive-type "List" '())
                     (list-from-racket-list args))])
    ;; Combine into a single content address
    (compute-content-address 
      (inductive-type "OperationKey" '())
      (constructor-value "operation-key" (list op-addr args-addr) 
                        (inductive-type "OperationKey" '())))))

;; Check if operation should be cached
(define/contract (operation-cacheable? operation)
  (-> string? boolean?)
  (not (not (member operation '("+" "-" "*" "/" "<" ">" "=" "read-file" "parse-json")))))

;; Convert Racket list to HoTT list value
(define/contract (list-from-racket-list racket-list)
  (-> (listof value/c) constructor-value?)
  (if (null? racket-list)
      (constructor-value "nil" '() (inductive-type "List" '()))
      (constructor-value "cons" 
                        (list (first racket-list)
                              (list-from-racket-list (rest racket-list)))
                        (inductive-type "List" '()))))

;; Environment operations
(define/contract (make-environment [parent #f])
  (->* () ((or/c environment? #f)) environment?)
  (environment (make-hash) parent))

(define/contract (env-lookup env name)
  (-> environment? string? (or/c value/c #f))
  (or (hash-ref (environment-bindings env) name #f)
      (and (environment-parent env)
           (env-lookup (environment-parent env) name))))

(define/contract (env-define! env name value)
  (-> environment? string? value/c void?)
  (hash-set! (environment-bindings env) name value))

(define/contract (env-extend env params args)
  (-> environment? (listof string?) (listof value/c) environment?)
  (let ([new-env (make-environment env)])
    (for ([param params] [arg args])
      (env-define! new-env param arg))
    new-env))

;; HoTT-native arithmetic functions with transparent caching
(define/contract (nat-add a b)
  (-> value/c value/c value/c)
  (evaluate-with-cache "+" (list a b) (make-environment)
                      (lambda () (hott-add a b))))

(define/contract (nat-mult a b)
  (-> value/c value/c value/c)
  (evaluate-with-cache "*" (list a b) (make-environment)
                      (lambda () (hott-mult a b))))

(define/contract (nat-sub a b)
  (-> value/c value/c value/c)
  (evaluate-with-cache "-" (list a b) (make-environment)
                      (lambda () (hott-sub a b))))

(define/contract (nat-equal? a b)
  (-> value/c value/c value/c)
  (hott-equal? a b))

(define/contract (nat-less? a b)
  (-> value/c value/c value/c)
  (hott-less? a b))

;; Additional natural number query and comparison operations (HoTT-native)
(define/contract (nat-is-zero? a)
  (-> value/c value/c)
  (hott-equal? a zero-value))

(define/contract (nat-predecessor a)
  (-> value/c value/c)
  (hott-pred a))

(define/contract (nat-less-equal? a b)
  (-> value/c value/c value/c)
  ;; a <= b is equivalent to a < b OR a = b
  (hott-or (hott-less? a b) (hott-equal? a b)))

(define/contract (nat-greater? a b)
  (-> value/c value/c value/c)
  ;; a > b is equivalent to b < a
  (hott-less? b a))

(define/contract (nat-greater-equal? a b)
  (-> value/c value/c value/c)
  ;; a >= b is equivalent to b <= a
  (hott-or (hott-less? b a) (hott-equal? a b)))

;; ============================================================================
;; PROOF-AWARE SAFE OPERATIONS
;; ============================================================================

;; Safe division using pure HoTT (no Racket conversion)
(define/contract (safe-nat-divide dividend divisor proof)
  (-> value/c value/c any/c value/c)
  ;; For now, we trust that the proof is valid (proof checking will be added later)
  ;; In a full implementation, we would verify the proof here
  (let ([is-zero (hott-equal? divisor zero-value)])
    (if (and (constructor-value? is-zero)
             (string=? (constructor-value-constructor-name is-zero) "true"))
        (error "Division by zero - proof should have prevented this!")
        ;; Use HoTT division (placeholder - implement HoTT division)
        (hott-divide dividend divisor))))

;; Proof construction for simple division cases (pure HoTT)
(define/contract (try-prove-safe-divide dividend divisor)
  (-> value/c value/c (or/c any/c #f))
  (let ([is-zero (hott-equal? divisor zero-value)])
    (if (and (constructor-value? is-zero)
             (string=? (constructor-value-constructor-name is-zero) "false"))
        ;; Construct a simple proof that divisor ≠ 0
        (hott-proof-non-zero divisor)
        #f)))

;; HoTT division (placeholder - needs proper implementation)
(define/contract (hott-divide dividend divisor)
  (-> value/c value/c value/c)
  ;; For now, use computational division as placeholder
  (computational-divide dividend divisor))

;; HoTT proof that a value is non-zero (placeholder)
(define/contract (hott-proof-non-zero n)
  (-> value/c any/c)
  ;; Placeholder proof construction
  `(proof-non-zero ,n))

;; Auto-safe division: tries to construct proof automatically
(define/contract (auto-safe-divide dividend divisor)
  (-> value/c value/c value/c)
  (let ([proof (try-prove-safe-divide dividend divisor)])
    (if proof
        (safe-nat-divide dividend divisor proof)
        (error "Cannot prove divisor is non-zero"))))

;; ============================================================================
;; TIER 1: COMPUTATIONAL FUNCTIONS (Mathematical Operations)
;; ============================================================================
;; These constructor-producing functions use computational proofs where proof construction IS computation
;; Results are computed at compile time and injected at runtime

;; Computational division function - HoTT-native safe division that produces constructor values
(define/contract (computational-divide dividend divisor)
  (-> value/c value/c value/c)
  ;; Use the auto-safe-divide that checks for zero divisor
  (auto-safe-divide dividend divisor))

;; ============================================================================
;; GENERIC EFFECT SYSTEM
;; ============================================================================
;; The evaluator doesn't know about specific effects - only how to handle them

;; Perform effect operation using Pure HoTT Effects
(define/contract (perform-effect-operation effect-name-value op-name-value . arg-values)
  (->* (value/c value/c) () #:rest (listof value/c) value/c)
  (unless (and (constructor-value? effect-name-value) (constructor-value? op-name-value))
    (error "perform-effect requires HoTT constructor values for effect and operation names"))
  ;; Create and execute pure HoTT effect description
  (let ([effect-desc (io-effect-description effect-name-value op-name-value arg-values 'deterministic)])
    ;; Execute effect with cache integration
    (let ([result (cached-effect-executor effect-desc global-hott-cache)])
      ;; Update global cache and return value
      (set! global-hott-cache (effect-result-cache result))
      (effect-result-value result))))

;; Pure HoTT Effects System - old generic effects system removed
;; All effects are now pure mathematical descriptions executed through host bridge


;; ============================================================================
;; ARITHMETIC OPERATIONS (Tier 1 Computational Functions)
;; ============================================================================

;; Computational arithmetic functions - using HoTT arithmetic directly 
;; (proof construction IS computation in HoTT, all produce constructor values)
(define/contract (computational-add a b)
  (-> value/c value/c value/c)
  ;; In HoTT, computation IS proof construction
  (hott-add a b))

(define/contract (computational-mult a b)
  (-> value/c value/c value/c)
  (hott-mult a b))

(define/contract (computational-sub a b)
  (-> value/c value/c value/c)
  (hott-sub a b))

;; ============================================================================
;; PATH AND EQUIVALENCE BUILT-IN OPERATIONS  
;; ============================================================================

;; refl : (A : Type) → (x : A) → Id A x x
(define/contract (make-refl-builtin type-val term-val)
  (-> value/c value/c value/c)
  ;; For now, assume type information is available at runtime
  (make-refl-value Nat term-val)) ; Simplified

;; Path concatenation: (p : Id A x y) → (q : Id A y z) → Id A x z
(define/contract (path-concat-builtin p q)
  (-> value/c value/c value/c)
  (unless (and (path-runtime-value? p) (path-runtime-value? q))
    (error "path-concat requires path values"))
  (make-path-concat-value p q))

;; Path inverse: (p : Id A x y) → Id A y x
(define/contract (path-inverse-builtin p)
  (-> value/c value/c)
  (unless (path-runtime-value? p)
    (error "path-inverse requires a path value"))
  (make-path-inverse-value p))

;; Transport: (P : A → Type) → (p : Id A x y) → P(x) → P(y)
(define/contract (transport-builtin predicate path val)
  (-> value/c value/c value/c value/c)
  (unless (path-runtime-value? path)
    (error "transport requires a path value"))
  (transport-value path predicate val))

;; Congruence: (f : A → B) → (p : Id A x y) → Id B (f x) (f y)
(define/contract (cong-builtin func path)
  (-> value/c value/c value/c)
  (unless (path-runtime-value? path)
    (error "cong requires a path value"))
  (cong-value func path))

;; Univalence: (e : A ≃ B) → Id Type A B
(define/contract (univalence-builtin equiv)
  (-> value/c value/c)
  (unless (equivalence-runtime-value? equiv)
    (error "ua requires an equivalence value"))
  (univalence-apply equiv))

;; Natural number constructor functions
(define/contract (zero-constructor unit-arg)
  (-> value/c value/c)
  zero-value)

(define/contract (next-constructor n)
  (-> value/c value/c)
  (succ-value n))

;; Boolean constructor functions  
(define/contract (true-constructor unit-arg)
  (-> value/c value/c)
  true-value)

(define/contract (false-constructor unit-arg)
  (-> value/c value/c)
  false-value)

;; Boolean logical operations (HoTT-native)
(define/contract (bool-and a b)
  (-> value/c value/c value/c)
  (hott-and a b))

(define/contract (bool-or a b)
  (-> value/c value/c value/c)
  (hott-or a b))

(define/contract (bool-not a)
  (-> value/c value/c)
  (hott-not a))


;; Built-in functions with proper HoTT types (HoTT-native)
(define builtin-environment
  (let ([env (make-environment)])
    ;; HoTT builtin environment functions are wrapped as builtin-values
    (for ([(name func) (in-hash hott-builtin-environment)])
      (env-define! env name (builtin-value name func (inductive-type "Function" '()))))
    
    ;; Arithmetic operations: Nat → Nat → Nat (wrapper for compatibility)
    (let ([nat-nat-nat (make-function-type (make-product-type Nat Nat) Nat)]
          [nat-nat-bool (make-function-type (make-product-type Nat Nat) Bool)])
      (env-define! env "+" (builtin-value "+" nat-add nat-nat-nat))
      (env-define! env "*" (builtin-value "*" nat-mult nat-nat-nat))
      (env-define! env "-" (builtin-value "-" nat-sub nat-nat-nat))
      (env-define! env "=" (builtin-value "=" nat-equal? nat-nat-bool))
      (env-define! env "<" (builtin-value "<" nat-less? nat-nat-bool))
      (env-define! env "<=" (builtin-value "<=" nat-less-equal? nat-nat-bool))
      (env-define! env ">" (builtin-value ">" nat-greater? nat-nat-bool))
      (env-define! env ">=" (builtin-value ">=" nat-greater-equal? nat-nat-bool)))

    ;; Natural number query and unary operations
    (let ([nat-bool (make-function-type Nat Bool)]
          [nat-nat (make-function-type Nat Nat)])
      (env-define! env "zero?" (builtin-value "zero?" nat-is-zero? nat-bool))
      (env-define! env "prior" (builtin-value "prior" nat-predecessor nat-nat)))
    
    ;; Constructor functions for inductive types
    (let ([unit-nat (make-function-type Unit Nat)]
          [nat-nat (make-function-type Nat Nat)]
          [unit-bool (make-function-type Unit Bool)])
      (env-define! env "zero" (builtin-value "zero" zero-constructor unit-nat))
      (env-define! env "next" (builtin-value "next" next-constructor nat-nat))
      (env-define! env "true" (builtin-value "true" true-constructor unit-bool))  
      (env-define! env "false" (builtin-value "false" false-constructor unit-bool)))

    ;; Boolean logical operations: Bool → Bool → Bool and Bool → Bool
    (let ([bool-bool-bool (make-function-type (make-product-type Bool Bool) Bool)]
          [bool-bool (make-function-type Bool Bool)])
      (env-define! env "and" (builtin-value "and" bool-and bool-bool-bool))
      (env-define! env "or" (builtin-value "or" bool-or bool-bool-bool))
      (env-define! env "not" (builtin-value "not" bool-not bool-bool)))


    ;; Division with automatic safety proofs (integrated Tier 1/2 safety)
    (let ([nat-nat-nat (make-function-type (make-product-type Nat Nat) Nat)])
      (env-define! env "/" (builtin-value "/" auto-safe-divide nat-nat-nat)))

    ;; Note: Tier 1 computational operations are now integrated into the main +, -, *, / operators
    ;; The 3-tier system works transparently - users don't need separate function names

    ;; Pure HoTT Effects - direct effect construction functions
    (let ([string-type (inductive-type "String" '())]
          [effect-type (inductive-type "EffectDescription" '())])
      ;; Effect construction functions (pure HoTT)
      (env-define! env "file-read" 
                   (builtin-value "file-read" file-read-effect 
                                 (make-function-type string-type effect-type)))
      (env-define! env "console-print" 
                   (builtin-value "console-print" console-print-effect
                                 (make-function-type string-type effect-type)))
      (env-define! env "environment-get" 
                   (builtin-value "environment-get" environment-get-effect
                                 (make-function-type string-type effect-type))))
    
    ;; Path and equivalence operations - simplified types for now
    (let ([path-type (make-function-type Nat Nat)]) ; Simplified
      (env-define! env "reflexivity" (builtin-value "reflexivity" make-refl-builtin path-type))
      (env-define! env "path-concat" (builtin-value "path-concat" path-concat-builtin path-type))
      (env-define! env "path-inverse" (builtin-value "path-inverse" path-inverse-builtin path-type))
      (env-define! env "transport" (builtin-value "transport" transport-builtin path-type))
      (env-define! env "congruence" (builtin-value "congruence" cong-builtin path-type))
      (env-define! env "univalence" (builtin-value "univalence" univalence-builtin path-type)))
    env))

;; Create a new environment that extends the builtin environment
(define/contract (make-global-environment)
  (-> environment?)
  (make-environment builtin-environment))

;; Main evaluation function
(define/contract (evaluate ast [env (make-global-environment)])
  (->* (ast-node/c) (environment?) value/c)
  (match ast
    ;; Literals - now already HoTT values from parser!
    [(number-atom hott-value) hott-value]
    [(boolean-atom hott-value) hott-value]
    [(string-atom hott-value) hott-value]  ; Keep HoTT strings as constructor values
    
    ;; HoTT AST nodes - delegate to HoTT evaluator
    [(? hott-ast-node? hott-ast)
     (evaluate-hott-ast hott-ast (hash))]
    
    ;; Symbol lookup
    [(symbol-atom name)
     (let ([value (env-lookup env name)])
       (if value
           value
           (error "Undefined variable: " name)))]
    
    ;; S-expressions (function calls and special forms)
    [(sexpr elements)
     (if (null? elements)
         '() ; empty list
         (let ([first-elem (first elements)])
           (match first-elem
             ;; Special forms
             [(symbol-atom "def")
              (when (< (length elements) 3)
                (error "def requires at least 2 arguments"))
              (let* ([name-elem (second elements)]
                     [value-elem (third elements)])
                (match name-elem
                  [(symbol-atom name)
                   (let ([value (evaluate value-elem env)])
                     (env-define! env name value)
                     value)]
                  [_ (error "def expects a symbol as first argument")]))]
             
             [(symbol-atom "fn")
              (when (< (length elements) 3)
                (error "fn requires at least 2 arguments"))
              (let* ([params-elem (second elements)]
                     [body-elems (drop elements 2)]) ; Multiple body expressions
                (match params-elem
                  [(sexpr param-nodes)
                   (let ([params (map (lambda (node)
                                       (match node
                                         [(symbol-atom name) name]
                                         [_ (error "fn parameters must be symbols")]))
                                     param-nodes)])
                     (closure-value params body-elems env))]
                  [_ (error "fn parameters must be a list")]))]
             
             [(symbol-atom "if")
              (when (not (= (length elements) 4))
                (error "if requires exactly 3 arguments"))
              (let* ([cond-elem (second elements)]
                     [then-elem (third elements)]
                     [else-elem (fourth elements)]
                     [cond-value (evaluate cond-elem env)])
                ;; Pure HoTT boolean test - no conversion to Racket
                (cond
                  [(and (constructor-value? cond-value)
                        (string=? (constructor-value-constructor-name cond-value) "true"))
                   (evaluate then-elem env)]
                  [(and (constructor-value? cond-value)
                        (string=? (constructor-value-constructor-name cond-value) "false"))
                   (evaluate else-elem env)]
                  [else (error "if condition must be a boolean, got: " cond-value)]))]
             
             ;; Function calls
             [_
              (let* ([func (evaluate first-elem env)]
                     [args (map (lambda (arg) (evaluate arg env)) (rest elements))])
                (match func
                  [(closure-value params body-list closure-env)
                   (when (not (= (length params) (length args)))
                     (error "Wrong number of arguments"))
                   (let ([extended-env (env-extend closure-env params args)])
                     ;; Evaluate all body expressions, return the last one
                     (let loop ([exprs body-list] [result unit])
                       (if (null? exprs)
                           result
                           (loop (rest exprs) (evaluate (first exprs) extended-env)))))]
                  
                  [(builtin-value name proc _)
                   ;; Handle various argument counts for built-in functions
                   (cond
                     ;; Zero-argument constructors: automatically inject unit to maintain "no direct terms" philosophy
                     ;; This allows syntax like (true) while preserving the constructor abstraction Unit → Bool
                     [(= (length args) 0) (proc unit)]  ; Pass unit for zero-argument constructors
                     [(= (length args) 1) (proc (first args))]  ; Unary functions/constructors  
                     [(= (length args) 2) (proc (first args) (second args))]  ; Binary operations
                     [else (error "Built-in function" name "expects 0-2 arguments, got" (length args))])]
                  
                  [_ (error "Cannot call non-function value: " func)]))])))]
    
    ;; ============================================================================
    ;; PATTERN MATCHING EVALUATION
    ;; ============================================================================
    
    ;; Match expressions
    [(match-expr scrutinee cases)
     (let ([scrutinee-value (evaluate scrutinee env)])
       (evaluate-match-cases scrutinee-value cases env))]
    
    [_ (error "Unknown AST node type: " ast)]))

;; ============================================================================
;; PATTERN MATCHING EVALUATION FUNCTIONS
;; ============================================================================

;; Evaluate match cases against a scrutinee value
(define/contract (evaluate-match-cases scrutinee-value cases env)
  (-> value/c (listof match-case?) environment? value/c)
  (if (null? cases)
      (error "No matching pattern found (non-exhaustive match)")
      (let ([first-case (first cases)])
        (let ([pattern (match-case-pattern first-case)]
              [body (match-case-body first-case)])
          (let ([match-result (try-match-pattern pattern scrutinee-value env)])
            (if match-result
                ;; Pattern matched, evaluate body with extended environment
                (evaluate body match-result)
                ;; Pattern didn't match, try next case
                (evaluate-match-cases scrutinee-value (rest cases) env)))))))

;; Try to match a pattern against a value, return extended environment if successful, #f if not
(define/contract (try-match-pattern pattern value env)
  (-> pattern-node/c value/c environment? (or/c environment? #f))
  (match pattern
    ;; Wildcard pattern always matches
    [(wildcard-pattern) env]
    
    ;; Variable pattern always matches and binds the variable
    [(variable-pattern name)
     (let ([new-env (make-environment env)])
       (env-define! new-env name value)
       new-env)]
    
    ;; Literal patterns must match exactly
    [(literal-pattern literal-value)
     (if (literal-matches? literal-value value)
         env
         #f)]
    
    ;; All HoTT inductive type patterns now use general constructor patterns
    ;; This handles zero, true, false, none, some, next, etc. uniformly
    [(constructor-pattern constructor-name sub-patterns)
     (if (constructor-value? value)
         (let ([value-constructor (constructor-value-constructor-name value)]
               [value-args (constructor-value-args value)])
           (if (and (string=? constructor-name value-constructor)
                    (= (length sub-patterns) (length value-args)))
               ;; Try to match all sub-patterns
               (try-match-sub-patterns sub-patterns value-args env)
               #f))
         #f)]
    
    [_ (error "Unknown pattern type: " pattern)]))

;; Try to match a list of sub-patterns against a list of values
(define/contract (try-match-sub-patterns patterns values env)
  (-> (listof pattern-node/c) (listof value/c) environment? (or/c environment? #f))
  (if (null? patterns)
      env  ; All patterns matched
      (let ([first-pattern (first patterns)]
            [first-value (first values)])
        (let ([partial-env (try-match-pattern first-pattern first-value env)])
          (if partial-env
              ;; First pattern matched, try the rest
              (try-match-sub-patterns (rest patterns) (rest values) partial-env)
              ;; First pattern didn't match
              #f)))))

;; Check if a literal value matches a runtime value (Pure HoTT comparison)
(define/contract (literal-matches? literal-value runtime-value)
  (-> any/c value/c boolean?)
  (cond
    ;; If literal-value is already a HoTT constructor (from new AST)
    [(constructor-value? literal-value)
     (and (constructor-value? runtime-value)
          (string=? (constructor-value-constructor-name literal-value)
                    (constructor-value-constructor-name runtime-value))
          (hott-equal-values? literal-value runtime-value))]
    
    ;; Legacy support for Racket literals (will be phased out)
    [(number? literal-value)
     (and (constructor-value? runtime-value)
          (nat-value? runtime-value)
          (hott-nat-equals-racket-number? runtime-value literal-value))]
    
    [(boolean? literal-value)
     (and (constructor-value? runtime-value)
          (bool-value? runtime-value)
          (hott-bool-equals-racket-boolean? runtime-value literal-value))]
    
    [else #f]))

;; Pure HoTT equality check
(define/contract (hott-equal-values? v1 v2)
  (-> constructor-value? constructor-value? boolean?)
  (let ([equal-effect (hott-equal? v1 v2)])
    (and (constructor-value? equal-effect)
         (string=? (constructor-value-constructor-name equal-effect) "true"))))

;; Helper: Check if HoTT nat equals Racket number (for legacy patterns)
(define/contract (hott-nat-equals-racket-number? hott-nat racket-num)
  (-> constructor-value? exact-nonnegative-integer? boolean?)
  (let ([racket-as-hott (pure-racket-number->hott-nat racket-num)])
    (hott-equal-values? hott-nat racket-as-hott)))

;; Helper: Check if HoTT bool equals Racket boolean (for legacy patterns)
(define/contract (hott-bool-equals-racket-boolean? hott-bool racket-bool)
  (-> constructor-value? boolean? boolean?)
  (let ([racket-as-hott (pure-racket-boolean->hott-bool racket-bool)])
    (hott-equal-values? hott-bool racket-as-hott)))