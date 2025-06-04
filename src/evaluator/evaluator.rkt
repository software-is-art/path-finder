#lang racket/base

(require racket/contract
         racket/match
         racket/list
         racket/string
         "../parser/ast.rkt"
         "../types/types.rkt"
         "values.rkt")

(provide evaluate
         make-environment
         make-global-environment
         environment?)

;; Environment for lexical scoping
(struct environment (bindings parent) #:transparent)

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

;; Built-in arithmetic functions for natural numbers
(define/contract (nat-add a b)
  (-> value/c value/c value/c)
  (let ([n1 (nat-value->racket-number a)]
        [n2 (nat-value->racket-number b)])
    (racket-number->nat-value (+ n1 n2))))

(define/contract (nat-mult a b)
  (-> value/c value/c value/c)
  (let ([n1 (nat-value->racket-number a)]
        [n2 (nat-value->racket-number b)])
    (racket-number->nat-value (* n1 n2))))

(define/contract (nat-sub a b)
  (-> value/c value/c value/c)
  (let ([n1 (nat-value->racket-number a)]
        [n2 (nat-value->racket-number b)])
    (racket-number->nat-value (max 0 (- n1 n2)))))

(define/contract (nat-equal? a b)
  (-> value/c value/c value/c)
  (let ([n1 (nat-value->racket-number a)]
        [n2 (nat-value->racket-number b)])
    (racket-boolean->bool-value (= n1 n2))))

(define/contract (nat-less? a b)
  (-> value/c value/c value/c)
  (let ([n1 (nat-value->racket-number a)]
        [n2 (nat-value->racket-number b)])
    (racket-boolean->bool-value (< n1 n2))))

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

;; Built-in functions with proper HoTT types
(define builtin-environment
  (let ([env (make-environment)])
    ;; Arithmetic operations: Nat → Nat → Nat
    (let ([nat-nat-nat (make-function-type (make-product-type Nat Nat) Nat)]
          [nat-nat-bool (make-function-type (make-product-type Nat Nat) Bool)])
      (env-define! env "+" (builtin-value "+" nat-add nat-nat-nat))
      (env-define! env "*" (builtin-value "*" nat-mult nat-nat-nat))
      (env-define! env "-" (builtin-value "-" nat-sub nat-nat-nat))
      (env-define! env "=" (builtin-value "=" nat-equal? nat-nat-bool))
      (env-define! env "<" (builtin-value "<" nat-less? nat-nat-bool)))
    
    ;; Path and equivalence operations - simplified types for now
    (let ([path-type (make-function-type Nat Nat)]) ; Simplified
      (env-define! env "refl" (builtin-value "refl" make-refl-builtin path-type))
      (env-define! env "path-concat" (builtin-value "path-concat" path-concat-builtin path-type))
      (env-define! env "path-inverse" (builtin-value "path-inverse" path-inverse-builtin path-type))
      (env-define! env "transport" (builtin-value "transport" transport-builtin path-type))
      (env-define! env "cong" (builtin-value "cong" cong-builtin path-type))
      (env-define! env "ua" (builtin-value "ua" univalence-builtin path-type)))
    env))

;; Create a new environment that extends the builtin environment
(define/contract (make-global-environment)
  (-> environment?)
  (make-environment builtin-environment))

;; Main evaluation function
(define/contract (evaluate ast [env (make-global-environment)])
  (->* (ast-node/c) (environment?) value/c)
  (match ast
    ;; Literals - convert to HoTT values
    [(number-atom value) (racket-number->nat-value value)]
    [(boolean-atom value) (racket-boolean->bool-value value)]
    [(string-atom value) (error "String literals not yet implemented in HoTT system")]
    
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
             [(symbol-atom "define")
              (when (< (length elements) 3)
                (error "define requires at least 2 arguments"))
              (let* ([name-elem (second elements)]
                     [value-elem (third elements)])
                (match name-elem
                  [(symbol-atom name)
                   (let ([value (evaluate value-elem env)])
                     (env-define! env name value)
                     value)]
                  [_ (error "define expects a symbol as first argument")]))]
             
             [(symbol-atom "lambda")
              (when (< (length elements) 3)
                (error "lambda requires at least 2 arguments"))
              (let* ([params-elem (second elements)]
                     [body-elems (drop elements 2)]) ; Multiple body expressions
                (match params-elem
                  [(sexpr param-nodes)
                   (let ([params (map (lambda (node)
                                       (match node
                                         [(symbol-atom name) name]
                                         [_ (error "lambda parameters must be symbols")]))
                                     param-nodes)])
                     (closure-value params body-elems env))]
                  [_ (error "lambda parameters must be a list")]))]
             
             [(symbol-atom "if")
              (when (not (= (length elements) 4))
                (error "if requires exactly 3 arguments"))
              (let* ([cond-elem (second elements)]
                     [then-elem (third elements)]
                     [else-elem (fourth elements)]
                     [cond-value (evaluate cond-elem env)])
                ;; Convert HoTT boolean to Racket boolean for if test
                (let ([cond-bool (if (bool-value? cond-value)
                                     (bool-value->racket-boolean cond-value)
                                     (error "if condition must be a boolean"))])
                  (if cond-bool
                      (evaluate then-elem env)
                      (evaluate else-elem env))))]
             
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
                   ;; Handle binary operations by taking pairs of arguments
                   (cond
                     [(= (length args) 2) (proc (first args) (second args))]
                     [else (error "Built-in function" name "expects 2 arguments, got" (length args))])]
                  
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
    
    ;; HoTT-specific patterns
    [(zero-pattern)
     (if (and (constructor-value? value)
              (string=? (constructor-value-constructor-name value) "zero"))
         env
         #f)]
    
    [(successor-pattern sub-pattern)
     (if (and (constructor-value? value)
              (string=? (constructor-value-constructor-name value) "succ")
              (= (length (constructor-value-args value)) 1))
         (try-match-pattern sub-pattern (first (constructor-value-args value)) env)
         #f)]
    
    [(true-pattern)
     (if (and (constructor-value? value)
              (string=? (constructor-value-constructor-name value) "true"))
         env
         #f)]
    
    [(false-pattern)
     (if (and (constructor-value? value)
              (string=? (constructor-value-constructor-name value) "false"))
         env
         #f)]
    
    ;; General constructor patterns: (constructor-name sub-patterns...)
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

;; Check if a literal value matches a runtime value
(define/contract (literal-matches? literal-value runtime-value)
  (-> any/c value/c boolean?)
  (cond
    [(number? literal-value)
     (and (constructor-value? runtime-value)
          (nat-value? runtime-value)
          (= literal-value (nat-value->racket-number runtime-value)))]
    
    [(boolean? literal-value)
     (and (constructor-value? runtime-value)
          (bool-value? runtime-value)
          (eq? literal-value (bool-value->racket-boolean runtime-value)))]
    
    [(string? literal-value)
     ;; String literals not yet implemented in HoTT system
     #f]
    
    [else #f]))