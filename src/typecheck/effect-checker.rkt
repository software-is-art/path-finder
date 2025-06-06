#lang racket/base

(require racket/contract
         racket/match
         racket/list
         "../parser/ast.rkt"
         "../types/types.rkt"
         "typechecker.rkt")

(provide effect-check
         infer-effects
         effect-type-check
         make-effect-environment
         effect-unify
         effect-generalize)

;; Effect-aware type environment that tracks both types and effect requirements
(struct effect-environment (type-env effect-context handlers) #:transparent)

;; Effect context tracks what effects are available/required in scope
(struct effect-context (required available handlers) #:transparent)

;; Effect type checking result
(struct effect-check-result (type effects constraints) #:transparent)

;; Create effect environment
(define/contract (make-effect-environment [type-env (make-global-type-environment)])
  (->* () (type-environment?) effect-environment?)
  (effect-environment type-env 
                      (effect-context (make-effect-set) (make-effect-set) '())
                      '()))

;; Infer effects required by an expression
(define/contract (infer-effects ast env)
  (-> ast-node/c effect-environment? effect-set?)
  (match ast
    ;; Literals require no effects
    [(number-atom _) (make-effect-set)]
    [(boolean-atom _) (make-effect-set)]
    [(string-atom _) (make-effect-set)]
    
    ;; Variables might have effect types
    [(symbol-atom name) 
     (let ([type (type-env-lookup (effect-environment-type-env env) name)])
       (if (and type (effect-type? type))
           (effect-type-required-effects type)
           (make-effect-set)))]
    
    ;; S-expressions - check for 'perform' calls
    [(sexpr elements)
     (if (null? elements)
         (make-effect-set)
         (match (first elements)
           ;; Generic effect performance: (perform "EffectName" "operation" args...)
           [(symbol-atom "perform")
            (if (>= (length elements) 3)
                (match (second elements)
                  [(string-atom effect-name)
                   ;; Add the effect to the required set
                   (make-effect-set (string->symbol effect-name))]
                  [_ (make-effect-set)])
                (make-effect-set))]
           ;; Other function calls
           [_
            (let ([func-effects (infer-effects (first elements) env)]
                  [arg-effects (apply effect-union 
                                     (map (lambda (arg) (infer-effects arg env)) 
                                          (rest elements)))])
              (effect-union func-effects arg-effects))]))]
    
    ;; Match expressions: union of scrutinee and all case effects
    [(match-expr scrutinee cases)
     (let ([scrutinee-effects (infer-effects scrutinee env)])
       (apply effect-union 
              scrutinee-effects
              (map (lambda (case) 
                     (infer-effects (match-case-body case) env))
                   cases)))]
    
    [_ (make-effect-set)]))

;; Effect-aware type checking
(define/contract (effect-type-check ast env)
  (-> ast-node/c effect-environment? effect-check-result?)
  (let ([base-type (type-check ast (effect-environment-type-env env))]
        [effects (infer-effects ast env)])
    (effect-check-result base-type effects '())))

;; Check if effects are properly handled
(define/contract (effect-check ast env)
  (-> ast-node/c effect-environment? (or/c effect-check-result? string?))
  (let ([result (effect-type-check ast env)])
    (let ([required-effects (effect-check-result-effects result)]
          [available-effects (effect-context-available (effect-environment-effect-context env))])
      (if (effect-subset? required-effects available-effects)
          result
          (format "Unhandled effects: ~a" 
                  (filter (lambda (eff) (not (member eff (effect-set-effects available-effects))))
                          (effect-set-effects required-effects)))))))

;; Effect unification for type inference
(define/contract (effect-unify e1 e2)
  (-> effect-set? effect-set? (or/c effect-set? #f))
  ;; For now, simple union - could be more sophisticated
  (effect-union e1 e2))

;; Effect generalization for polymorphic effects
(define/contract (effect-generalize effects env)
  (-> effect-set? effect-environment? effect-set?)
  ;; For now, just return the effects - could implement effect polymorphism
  effects)

;; Extend environment with effect handler
(define/contract (extend-with-handler env effect-type handler)
  (-> effect-environment? symbol? any/c effect-environment?)
  (let ([new-context (effect-context 
                      (effect-context-required (effect-environment-effect-context env))
                      (effect-union (effect-context-available (effect-environment-effect-context env))
                                   (make-effect-set effect-type))
                      (cons (cons effect-type handler) 
                            (effect-context-handlers (effect-environment-effect-context env))))])
    (effect-environment (effect-environment-type-env env)
                        new-context
                        (effect-environment-handlers env))))

;; Check effect handler coverage
(define/contract (check-handler-coverage required-effects available-handlers)
  (-> effect-set? (listof (cons/c symbol? any/c)) boolean?)
  (andmap (lambda (eff) 
            (assoc eff available-handlers))
          (effect-set-effects required-effects)))

;; Effect-aware function type inference
(define/contract (infer-function-effects params body env)
  (-> (listof string?) ast-node/c effect-environment? effect-set?)
  ;; Create extended environment with parameters
  (let ([extended-env env]) ; Simplified - would extend with params
    (infer-effects body extended-env)))

;; Smart constructors for common effect types

;; Pure function type (no effects)
(define/contract (make-pure-function-type domain codomain)
  (-> hott-type/c hott-type/c hott-type/c)
  (make-function-type domain codomain))

;; File I/O function type
(define/contract (make-file-io-function-type domain codomain)
  (-> hott-type/c hott-type/c effect-type?)
  (make-effect-function-type domain codomain (make-effect-set 'file-read 'file-write)))

;; Network I/O function type
(define/contract (make-network-function-type domain codomain)
  (-> hott-type/c hott-type/c effect-type?)
  (make-effect-function-type domain codomain (make-effect-set 'http-get 'http-post)))

;; Build-time computation function type
(define/contract (make-build-time-function-type domain codomain)
  (-> hott-type/c hott-type/c effect-type?)
  (make-effect-function-type domain codomain (make-effect-set 'build-time 'compile-time)))

;; Effect subsumption: can we use a function requiring fewer effects in place of one requiring more?
(define/contract (effect-subsumes? smaller larger)
  (-> effect-set? effect-set? boolean?)
  (effect-subset? smaller larger))

;; Effect composition for sequential operations
(define/contract (effect-sequence-type effects)
  (-> (listof effect-set?) effect-set?)
  (apply effect-union effects))

;; Effect choice for alternative operations  
(define/contract (effect-choice-type effects)
  (-> (listof effect-set?) effect-set?)
  ;; For choice, we need the intersection (common effects)
  (if (null? effects)
      (make-effect-set)
      (let loop ([effs (rest effects)] [acc (first effects)])
        (if (null? effs)
            acc
            (loop (rest effs) 
                  (effect-set (filter (lambda (e) (member e (effect-set-effects (first effs))))
                                     (effect-set-effects acc))))))))