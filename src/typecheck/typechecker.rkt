#lang racket/base

(require racket/contract
         racket/match
         racket/list
         "../parser/ast.rkt"
         "../types/types.rkt")

(provide type-check
         make-type-environment
         make-global-type-environment
         type-environment?)

;; Type checking environment for HoTT-based PathFinder LISP

;; Type environment maps variables to their types
(struct type-environment (bindings parent) #:transparent)

;; Type environment operations
(define/contract (make-type-environment [parent #f])
  (->* () ((or/c type-environment? #f)) type-environment?)
  (type-environment (make-hash) parent))

(define/contract (type-env-lookup env name)
  (-> type-environment? string? (or/c hott-type/c #f))
  (or (hash-ref (type-environment-bindings env) name #f)
      (and (type-environment-parent env)
           (type-env-lookup (type-environment-parent env) name))))

(define/contract (type-env-define! env name type)
  (-> type-environment? string? hott-type/c void?)
  (hash-set! (type-environment-bindings env) name type))

;; Built-in type environment
(define builtin-type-environment
  (let ([env (make-type-environment)])
    ;; Built-in arithmetic operations
    (let ([nat-nat-nat (make-function-type (make-product-type Nat Nat) Nat)]
          [nat-nat-bool (make-function-type (make-product-type Nat Nat) Bool)])
      (type-env-define! env "+" nat-nat-nat)
      (type-env-define! env "*" nat-nat-nat)
      (type-env-define! env "-" nat-nat-nat)
      (type-env-define! env "=" nat-nat-bool)
      (type-env-define! env "<" nat-nat-bool))
    
    ;; Path and equivalence operations - simplified types for now
    (let ([path-type (make-function-type Nat Nat)]  ; Simplified
          [equiv-type (make-function-type Nat Nat)]) ; Simplified
      (type-env-define! env "refl" path-type)
      (type-env-define! env "path-concat" path-type)
      (type-env-define! env "path-inverse" path-type)
      (type-env-define! env "transport" path-type)
      (type-env-define! env "cong" path-type)
      (type-env-define! env "ua" equiv-type))
    env))

(define/contract (make-global-type-environment)
  (-> type-environment?)
  (make-type-environment builtin-type-environment))

;; Main type checking function
(define/contract (type-check ast [env (make-global-type-environment)])
  (->* (ast-node/c) (type-environment?) hott-type/c)
  (match ast
    ;; Literals have their corresponding inductive types
    [(number-atom _) Nat]
    [(boolean-atom _) Bool]
    [(string-atom _) (error "String literals not yet implemented in HoTT type system")]
    
    ;; Variable lookup
    [(symbol-atom name)
     (let ([type (type-env-lookup env name)])
       (if type
           type
           (error "Undefined variable: " name)))]
    
    ;; S-expressions
    [(sexpr elements)
     (if (null? elements)
         Unit  ; empty list has unit type
         (let ([first-elem (first elements)])
           (match first-elem
             ;; define special form
             [(symbol-atom "define")
              (when (< (length elements) 3)
                (error "define requires at least 2 arguments"))
              (let* ([name-elem (second elements)]
                     [value-elem (third elements)])
                (match name-elem
                  [(symbol-atom name)
                   (let ([value-type (type-check value-elem env)])
                     (type-env-define! env name value-type)
                     value-type)]
                  [_ (error "define expects a symbol as first argument")]))]
             
             ;; lambda special form
             [(symbol-atom "lambda")
              (when (< (length elements) 3)
                (error "lambda requires at least 2 arguments"))
              ;; Simplified lambda typing - assume all params are Nat for now
              (let* ([params-elem (second elements)]
                     [body-elem (third elements)])
                (match params-elem
                  [(sexpr param-nodes)
                   (let* ([param-count (length param-nodes)]
                          [domain (if (= param-count 1) Nat (make-product-type Nat Nat))]
                          [codomain Nat])  ; Simplified - assume returns Nat
                     (make-function-type domain codomain))]
                  [_ (error "lambda parameters must be a list")]))]
             
             ;; if special form
             [(symbol-atom "if")
              (when (not (= (length elements) 4))
                (error "if requires exactly 3 arguments"))
              (let* ([cond-elem (second elements)]
                     [then-elem (third elements)]
                     [else-elem (fourth elements)]
                     [cond-type (type-check cond-elem env)]
                     [then-type (type-check then-elem env)]
                     [else-type (type-check else-elem env)])
                ;; Condition must be Bool
                (unless (hott-type-equal? cond-type Bool)
                  (error "if condition must have type Bool"))
                ;; Then and else branches must have same type
                (unless (hott-type-equal? then-type else-type)
                  (error "if branches must have same type"))
                then-type)]
             
             ;; Function application
             [_
              (let* ([func-type (type-check first-elem env)]
                     [arg-types (map (lambda (arg) (type-check arg env)) (rest elements))])
                (match func-type
                  [(pi-type _ domain codomain)
                   ;; Simplified type checking - just return codomain
                   codomain]
                  [_ (error "Cannot apply non-function type")]))])))]
    
    ;; Match expressions  
    [(match-expr scrutinee cases)
     (typecheck-match-expr scrutinee cases env)]
     
    [_ (error "Unknown AST node type in type checking: " ast)]))

;; ============================================================================
;; PATTERN MATCHING TYPE CHECKING
;; ============================================================================

;; Type check match expression
(define/contract (typecheck-match-expr scrutinee cases env)
  (-> ast-node/c (listof match-case?) type-environment? hott-type/c)
  ;; 1. Type check the scrutinee
  (let ([scrutinee-type (type-check scrutinee env)])
    ;; 2. Check exhaustiveness and unreachability
    (check-exhaustiveness (map match-case-pattern cases) scrutinee-type)
    (check-unreachability (map match-case-pattern cases))
    ;; 3. Type check each case and collect body types
    (let ([body-types (map (lambda (case)
                            (typecheck-match-case case scrutinee-type env))
                          cases)])
      ;; 4. Compute least upper bound of all body types
      (compute-lub body-types))))

;; Type check a single match case
(define/contract (typecheck-match-case case scrutinee-type env)
  (-> match-case? hott-type/c type-environment? hott-type/c)
  (let ([pattern (match-case-pattern case)]
        [body (match-case-body case)])
    ;; Type check pattern and get extended environment
    (let ([extended-env (typecheck-pattern pattern scrutinee-type env)])
      ;; Type check body in extended environment
      (type-check body extended-env))))

;; Type check pattern and return extended type environment
(define/contract (typecheck-pattern pattern scrutinee-type env)
  (-> pattern-node/c hott-type/c type-environment? type-environment?)
  (match pattern
    ;; Wildcard pattern: matches any type, no new bindings
    [(wildcard-pattern) env]
    
    ;; Variable pattern: binds variable to scrutinee type
    [(variable-pattern name)
     (let ([new-env (make-type-environment env)])
       (type-env-define! new-env name scrutinee-type)
       new-env)]
    
    ;; Literal patterns: must match scrutinee type
    [(literal-pattern value)
     (let ([literal-type (literal-value->type value)])
       (unless (hott-type-equal? literal-type scrutinee-type)
         (error "Pattern literal type mismatch: expected" scrutinee-type "got" literal-type))
       env)]
    
    ;; HoTT-specific patterns
    [(zero-pattern)
     (unless (hott-type-equal? scrutinee-type Nat)
       (error "Zero pattern requires Nat type, got" scrutinee-type))
     env]
    
    [(successor-pattern sub-pattern)
     (unless (hott-type-equal? scrutinee-type Nat)
       (error "Successor pattern requires Nat type, got" scrutinee-type))
     (typecheck-pattern sub-pattern Nat env)]
    
    [(true-pattern)
     (unless (hott-type-equal? scrutinee-type Bool)
       (error "True pattern requires Bool type, got" scrutinee-type))
     env]
    
    [(false-pattern)
     (unless (hott-type-equal? scrutinee-type Bool)
       (error "False pattern requires Bool type, got" scrutinee-type))
     env]
    
    ;; Constructor patterns (simplified for now)
    [(constructor-pattern constructor-name sub-patterns)
     ;; For now, just check that all sub-patterns type check
     ;; Full constructor type checking will be implemented later
     (let loop ([patterns sub-patterns] [current-env env])
       (if (null? patterns)
           current-env
           (loop (rest patterns) 
                 (typecheck-pattern (first patterns) scrutinee-type current-env))))]
    
    [_ (error "Unknown pattern type in type checking: " pattern)]))

;; Convert literal value to its type
(define/contract (literal-value->type value)
  (-> any/c hott-type/c)
  (cond
    [(number? value) Nat]
    [(boolean? value) Bool]
    [(string? value) (error "String literals not supported in HoTT type system")]
    [else (error "Unknown literal type: " value)]))

;; Compute least upper bound of types (simplified)
(define/contract (compute-lub types)
  (-> (listof hott-type/c) hott-type/c)
  (if (null? types)
      Unit  ; Empty case list has unit type
      (let ([first-type (first types)])
        ;; For now, require all types to be equal
        ;; TODO: Implement proper LUB computation
        (for ([type (rest types)])
          (unless (hott-type-equal? first-type type)
            (error "Match case body types must be equal: expected" first-type "got" type)))
        first-type)))

;; ============================================================================
;; EXHAUSTIVENESS AND UNREACHABILITY CHECKING
;; ============================================================================

;; Check if match patterns are exhaustive for the given scrutinee type
(define/contract (check-exhaustiveness patterns scrutinee-type)
  (-> (listof pattern-node/c) hott-type/c void?)
  (cond
    ;; Boolean exhaustiveness: need both #t and #f cases, or wildcard
    [(hott-type-equal? scrutinee-type Bool)
     (check-boolean-exhaustiveness patterns)]
    
    ;; Natural number exhaustiveness: need zero and successor cases, or wildcard
    [(hott-type-equal? scrutinee-type Nat)
     (check-nat-exhaustiveness patterns)]
    
    ;; For other types, we'll assume exhaustive for now
    ;; TODO: Add sum type exhaustiveness when deftype is implemented
    [else (void)]))

;; Check boolean exhaustiveness
(define/contract (check-boolean-exhaustiveness patterns)
  (-> (listof pattern-node/c) void?)
  (let ([has-true? (has-pattern-covering-true? patterns)]
        [has-false? (has-pattern-covering-false? patterns)]
        [has-wildcard? (has-wildcard-or-variable? patterns)])
    (unless (or has-wildcard? (and has-true? has-false?))
      (error "Non-exhaustive boolean match: missing cases for" 
             (cond
               [(not has-true?) "#t"]
               [(not has-false?) "#f"]
               [else "unknown"])))))

;; Check natural number exhaustiveness  
(define/contract (check-nat-exhaustiveness patterns)
  (-> (listof pattern-node/c) void?)
  (let ([has-zero? (has-pattern-covering-zero? patterns)]
        [has-successor? (has-pattern-covering-successor? patterns)]
        [has-wildcard? (has-wildcard-or-variable? patterns)])
    (unless (or has-wildcard? (and has-zero? has-successor?))
      (error "Non-exhaustive natural number match: missing cases for"
             (cond
               [(not has-zero?) "zero"]
               [(not has-successor?) "successor"]
               [else "unknown"])))))

;; Check if patterns contain wildcard or variable pattern
(define/contract (has-wildcard-or-variable? patterns)
  (-> (listof pattern-node/c) boolean?)
  (ormap (lambda (p) 
           (or (wildcard-pattern? p) 
               (variable-pattern? p)))
         patterns))

;; Check if patterns cover true case
(define/contract (has-pattern-covering-true? patterns)
  (-> (listof pattern-node/c) boolean?)
  (ormap (lambda (p)
           (or (true-pattern? p)
               (and (literal-pattern? p) 
                    (eq? (literal-pattern-value p) #t))))
         patterns))

;; Check if patterns cover false case
(define/contract (has-pattern-covering-false? patterns)
  (-> (listof pattern-node/c) boolean?)
  (ormap (lambda (p)
           (or (false-pattern? p)
               (and (literal-pattern? p) 
                    (eq? (literal-pattern-value p) #f))))
         patterns))

;; Check if patterns cover zero case
(define/contract (has-pattern-covering-zero? patterns)
  (-> (listof pattern-node/c) boolean?)
  (ormap (lambda (p)
           (or (zero-pattern? p)
               (and (literal-pattern? p)
                    (eqv? (literal-pattern-value p) 0))))
         patterns))

;; Check if patterns cover successor case
(define/contract (has-pattern-covering-successor? patterns)
  (-> (listof pattern-node/c) boolean?)
  (ormap (lambda (p)
           (successor-pattern? p))
         patterns))

;; Check for unreachable patterns
(define/contract (check-unreachability patterns)
  (-> (listof pattern-node/c) void?)
  (check-patterns-sequence patterns))

;; Check patterns in sequence for unreachability
(define/contract (check-patterns-sequence patterns)
  (-> (listof pattern-node/c) void?)
  (let loop ([remaining patterns] [index 0])
    (unless (null? remaining)
      (let ([current (first remaining)]
            [preceding (take patterns index)])
        ;; Check if current pattern is unreachable due to preceding patterns
        (when (pattern-unreachable? current preceding)
          (error "Unreachable pattern at position" index ": pattern" 
                 (pattern->string current) "is covered by preceding patterns"))
        (loop (rest remaining) (+ index 1))))))

;; Check if a pattern is unreachable given preceding patterns
(define/contract (pattern-unreachable? pattern preceding-patterns)
  (-> pattern-node/c (listof pattern-node/c) boolean?)
  (cond
    ;; Any pattern after wildcard or variable is unreachable
    [(has-wildcard-or-variable? preceding-patterns) #t]
    
    ;; Specific literal pattern is unreachable if same literal already covered
    [(literal-pattern? pattern)
     (has-literal-pattern? preceding-patterns (literal-pattern-value pattern))]
    
    ;; Specific constructor pattern is unreachable if same constructor covered
    [(zero-pattern? pattern)
     (has-pattern-covering-zero? preceding-patterns)]
    
    [(true-pattern? pattern) 
     (has-pattern-covering-true? preceding-patterns)]
    
    [(false-pattern? pattern)
     (has-pattern-covering-false? preceding-patterns)]
    
    ;; For other patterns, assume reachable for now
    [else #f]))

;; Check if preceding patterns contain a specific literal value
(define/contract (has-literal-pattern? patterns value)
  (-> (listof pattern-node/c) any/c boolean?)
  (ormap (lambda (p)
           (and (literal-pattern? p)
                (equal? (literal-pattern-value p) value)))
         patterns))

;; Helper function to convert patterns back to strings for error messages
;; (This references the pattern->string function from ast.rkt)

;; Simplified type system for now - we'll expand this with proper HoTT features later