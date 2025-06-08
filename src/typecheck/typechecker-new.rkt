#lang racket/base

(require racket/contract
         racket/match
         racket/list
         "../parser/ast.rkt"
         "../types/types.rkt")

(provide type-check
         make-type-environment
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
                  [_ (error "Cannot apply non-function type")]))]))))]
    
    ;; Match expressions
    [(match-expr scrutinee cases)
     (let* ([scrutinee-type (type-check scrutinee env)]
            [case-types (map (lambda (case) (type-check-match-case case scrutinee-type env)) cases)])
       ;; Check exhaustiveness
       (check-pattern-exhaustiveness scrutinee-type cases)
       ;; Check unreachability  
       (check-pattern-unreachability scrutinee-type cases)
       ;; All cases must have the same type
       (unless (all-types-equal? case-types)
         (error "All match cases must have the same type"))
       (first case-types))]
    
    [_ (error "Unknown AST node type during type checking: " ast)]))

;; ============================================================================
;; PATTERN MATCHING TYPE CHECKING HELPERS
;; ============================================================================

;; Type check a single match case
(define/contract (type-check-match-case match-case scrutinee-type env)
  (-> match-case? hott-type/c type-environment? hott-type/c)
  (let ([pattern (match-case-pattern match-case)]
        [body (match-case-body match-case)])
    ;; Type check the pattern against the scrutinee type and get variable bindings
    (let ([pattern-env (type-check-pattern pattern scrutinee-type env)])
      ;; Type check the body in the extended environment
      (type-check body pattern-env))))

;; Type check a pattern against an expected type, return extended environment
(define/contract (type-check-pattern pattern expected-type env)
  (-> pattern-node/c hott-type/c type-environment? type-environment?)
  (match pattern
    ;; Wildcard pattern matches any type
    [(wildcard-pattern) env]
    
    ;; Variable pattern binds the variable to the expected type
    [(variable-pattern name)
     (let ([new-env (make-type-environment env)])
       (type-env-define! new-env name expected-type)
       new-env)]
    
    ;; Literal patterns must match the expected type
    [(literal-pattern value)
     (let ([literal-type (cond
                           [(number? value) Nat]
                           [(boolean? value) Bool]
                           [(string? value) (error "String literals not implemented")]
                           [else (error "Unknown literal type")])])
       (unless (hott-type-equal? literal-type expected-type)
         (error "Literal pattern type mismatch"))
       env)]
    
    ;; HoTT-specific patterns
    [(zero-pattern)
     (unless (hott-type-equal? expected-type Nat)
       (error "Zero pattern can only match Nat type"))
     env]
    
    [(next-pattern sub-pattern)
     (unless (hott-type-equal? expected-type Nat)
       (error "Next pattern can only match Nat type"))
     ;; The sub-pattern must also match Nat type
     (type-check-pattern sub-pattern Nat env)]
    
    [(true-pattern)
     (unless (hott-type-equal? expected-type Bool)
       (error "True pattern can only match Bool type"))
     env]
    
    [(false-pattern)
     (unless (hott-type-equal? expected-type Bool)
       (error "False pattern can only match Bool type"))
     env]
    
    ;; General constructor patterns
    [(constructor-pattern constructor-name sub-patterns)
     ;; For now, assume constructor patterns match their expected type
     ;; In a full implementation, we'd look up constructor signatures
     env]
    
    [_ (error "Unknown pattern type during type checking: " pattern)]))

;; ============================================================================
;; EXHAUSTIVENESS AND UNREACHABILITY CHECKING
;; ============================================================================

;; Check if pattern matching is exhaustive for a given type
(define/contract (check-pattern-exhaustiveness scrutinee-type cases)
  (-> hott-type/c (listof match-case?) void?)
  (let ([required-patterns (generate-required-patterns scrutinee-type)]
        [covered-patterns (extract-covered-patterns cases)])
    (let ([missing-patterns (set-subtract required-patterns covered-patterns)])
      (unless (set-empty? missing-patterns)
        (error "Non-exhaustive pattern match. Missing patterns: " missing-patterns)))))

;; Check for unreachable patterns
(define/contract (check-pattern-unreachability scrutinee-type cases)
  (-> hott-type/c (listof match-case?) void?)
  ;; For each pattern, check if it's reachable given the patterns that come before it
  (let loop ([remaining-cases cases] [covered-so-far (set)])
    (unless (null? remaining-cases)
      (let* ([current-case (first remaining-cases)]
             [current-pattern (match-case-pattern current-case)]
             [current-pattern-set (pattern-to-pattern-set current-pattern)])
        ;; Check if this pattern is completely covered by previous patterns
        (when (set-subset? current-pattern-set covered-so-far)
          (error "Unreachable pattern: " current-pattern))
        ;; Add this pattern to the covered set and continue
        (loop (rest remaining-cases) (set-union covered-so-far current-pattern-set))))))

;; Generate the set of all required patterns for a type to be exhaustive
(define/contract (generate-required-patterns type)
  (-> hott-type/c (set/c symbol?))
  (cond
    [(hott-type-equal? type Nat)
     (set 'zero 'next)]
    [(hott-type-equal? type Bool)
     (set 'true 'false)]
    [else
     ;; For unknown types, assume they need at least one pattern (wildcard or variable)
     (set 'any)]))

;; Extract the set of patterns covered by the match cases
(define/contract (extract-covered-patterns cases)
  (-> (listof match-case?) (set/c symbol?))
  (apply set-union 
         (map (lambda (case) (pattern-to-pattern-set (match-case-pattern case))) cases)))

;; Convert a pattern to a set of pattern symbols it covers
(define/contract (pattern-to-pattern-set pattern)
  (-> pattern-node/c (set/c symbol?))
  (match pattern
    [(wildcard-pattern) (set 'any 'zero 'next 'true 'false)] ; Wildcard covers everything
    [(variable-pattern _) (set 'any 'zero 'next 'true 'false)] ; Variable covers everything
    [(zero-pattern) (set 'zero)]
    [(next-pattern _) (set 'next)]
    [(true-pattern) (set 'true)]
    [(false-pattern) (set 'false)]
    [(literal-pattern value)
     (cond
       [(and (number? value) (= value 0)) (set 'zero)]
       [(number? value) (set 'next)] ; Non-zero numbers are next
       [(eq? value #t) (set 'true)]
       [(eq? value #f) (set 'false)]
       [else (set)])]
    [(constructor-pattern name _) (set (string->symbol name))]
    [_ (set)]))

;; Check if all types in a list are equal
(define/contract (all-types-equal? types)
  (-> (listof hott-type/c) boolean?)
  (or (null? types)
      (null? (rest types))
      (and (hott-type-equal? (first types) (second types))
           (all-types-equal? (rest types)))))