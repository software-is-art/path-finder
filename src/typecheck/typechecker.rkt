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
                  [_ (error "Cannot apply non-function type")]))])))]))

;; Simplified type system for now - we'll expand this with proper HoTT features later