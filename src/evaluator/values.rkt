#lang racket/base

(require racket/match
         racket/string
         "../types/types.rkt")

(provide (all-defined-out))

;; Runtime values for PathFinder LISP HoTT-based evaluation

;; Base value type
(struct value () #:transparent)

;; Constructor applications (for inductive types)
(struct constructor-value value (constructor-name args type) #:transparent)

;; Function closures 
(struct closure-value value (params body env) #:transparent)

;; Built-in function values
(struct builtin-value value (name proc type) #:transparent)

;; Unit value (the unique inhabitant of 𝟙)
(struct unit-value value () #:transparent)

;; Path values (inhabitants of identity types)
(struct path-runtime-value value (type start end proof) #:transparent)

;; Equivalence values (inhabitants of equivalence types)
(struct equivalence-runtime-value value (type-a type-b function quasi-inv) #:transparent)

;; String values (for Tier 2 effects)
(struct string-value value (content) #:transparent)

;; Effect values (algebraic effects that signal operations)
(struct effect-value value (effect) #:transparent)

;; All effects now use the generic effect-value struct

;; Values have no inhabitants for Empty type (𝟘)

;; HoTT-native value predicates (replacing Racket contracts)
(define (hott-value? x)
  (or (constructor-value? x) (closure-value? x) (builtin-value? x) (unit-value? x)
      (path-runtime-value? x) (equivalence-runtime-value? x) (string-value? x) (effect-value? x)))

;; Type contract using HoTT dependent types
(define value/c hott-value?)

;; Predefined values for HoTT inductive types

;; Natural number values
(define zero-value (constructor-value "zero" '() Nat))

(define (succ-value n)
  (unless (hott-value? n)
    (error "succ-value: argument must be a value" n))
  (constructor-value "next" (list n) Nat))

;; Boolean values  
(define true-value (constructor-value "true" '() Bool))
(define false-value (constructor-value "false" '() Bool))

;; Unit value
(define unit (unit-value))

;; Helper to create natural number values from Racket numbers
;; ============================================================================
;; PURE HOTT VALUE SYSTEM - NO RACKET DEPENDENCIES
;; ============================================================================
;; All Racket conversion functions moved to host-bridge.rkt for self-hosting

;; Type checking for values
(define (value-has-type? val type)
  (unless (hott-value? val)
    (error "value-has-type?: first argument must be a value" val))
  (unless (extended-hott-type/c type)
    (error "value-has-type?: second argument must be a HoTT type" type))
  (match* (val type)
    ;; Constructor values check against their inductive type
    [((constructor-value _ _ val-type) type)
     (hott-type-equal? val-type type)]
    
    ;; Unit value has unit type
    [((unit-value) (unit-type)) #t]
    
    ;; Closures have function types (we'd need more sophisticated checking)
    [((closure-value _ _ _) (pi-type _ _ _)) #t]
    
    ;; Built-ins have their declared type
    [((builtin-value _ _ val-type) type)
     (hott-type-equal? val-type type)]
    
    ;; Path values have identity types
    [((path-runtime-value path-type start end _) (identity-type id-type id-left id-right))
     (and (hott-type-equal? path-type id-type)
          (equal? start id-left)
          (equal? end id-right))]
    
    ;; Equivalence values have equivalence types
    [((equivalence-runtime-value val-type-a val-type-b _ _) (equivalence-type eq-type-a eq-type-b))
     (and (hott-type-equal? val-type-a eq-type-a)
          (hott-type-equal? val-type-b eq-type-b))]
    
    [(_ _) #f]))

;; HoTT-style value eliminator for value->string
(define (value-eliminator val constructor-case closure-case builtin-case unit-case string-case effect-case path-case equiv-case)
  "HoTT eliminator for value types - total by construction"
  (cond
    [(constructor-value? val) 
     (let ([name (constructor-value-constructor-name val)]
           [args (constructor-value-args val)])
       (constructor-case name args (constructor-value-type val)))]
    [(closure-value? val) (closure-case (closure-value-params val) (closure-value-body val) (closure-value-env val))]
    [(builtin-value? val) (builtin-case (builtin-value-name val) (builtin-value-proc val) (builtin-value-type val))]
    [(unit-value? val) (unit-case)]
    [(string-value? val) (string-case (string-value-content val))]
    [(effect-value? val) (effect-case (effect-value-effect val))]
    [(path-runtime-value? val) (path-case (path-runtime-value-type val) (path-runtime-value-start val) 
                                         (path-runtime-value-end val) (path-runtime-value-proof val))]
    [(equivalence-runtime-value? val) (equiv-case (equivalence-runtime-value-type-a val) (equivalence-runtime-value-type-b val)
                                                 (equivalence-runtime-value-function val) (equivalence-runtime-value-quasi-inv val))]
    [else (error "value-eliminator: unknown value type" val)]))

;; Pretty printing for values using HoTT eliminator
(define (value->string val)
  (unless (hott-value? val)
    (error "value->string: argument must be a value" val))
  (value-eliminator val
    ;; constructor-value case
    (lambda (name args type)
      (if (null? args)
          name
          (string-append "(" name " " 
                        (string-join (map value->string args) " ") 
                        ")")))
    ;; closure-value case  
    (lambda (params body env) "#<closure>")
    ;; builtin-value case
    (lambda (name proc type) (string-append "#<builtin:" name ">"))
    ;; unit-value case
    (lambda () "unit")
    ;; string-value case
    (lambda (content) (string-append "\"" content "\""))
    ;; effect-value case
    (lambda (effect) (string-append "#<effect:" (format "~a" effect) ">"))
    ;; path-runtime-value case
    (lambda (type start end proof)
      (string-append "path[" (type->string type) " : " 
                    (value->string start) " = " (value->string end) "]"))
    ;; equivalence-runtime-value case
    (lambda (type-a type-b func quasi-inv)
      (string-append (type->string type-a) " ≃ " (type->string type-b)))))

;; Check if value is a natural number
(define (nat-value? val)
  (unless (hott-value? val)
    (error "nat-value?: argument must be a value" val))
  (and (constructor-value? val)
       (let ([name (constructor-value-constructor-name val)])
         (or (string=? name "zero") (string=? name "next")))))

;; Check if value is a boolean
(define (bool-value? val)
  (unless (hott-value? val)
    (error "bool-value?: argument must be a value" val))
  (and (constructor-value? val)
       (let ([name (constructor-value-constructor-name val)])
         (or (string=? name "true") (string=? name "false")))))

;; ============================================================================
;; PATH AND EQUIVALENCE VALUES
;; ============================================================================

;; Create reflexivity path
(define (make-refl-value type term)
  (unless (hott-type? type)
    (error "make-refl-value: type must be a HoTT type" type))
  (unless (hott-value? term)
    (error "make-refl-value: term must be a value" term))
  (path-runtime-value type term term 'refl))

;; Create path concatenation
(define (make-path-concat-value p q)
  (unless (path-runtime-value? p)
    (error "make-path-concat-value: first argument must be a path-runtime-value" p))
  (unless (path-runtime-value? q)
    (error "make-path-concat-value: second argument must be a path-runtime-value" q))
  (unless (equal? (path-runtime-value-end p) (path-runtime-value-start q))
    (error "Cannot concatenate paths: end of first must equal start of second"))
  (path-runtime-value (path-runtime-value-type p)
                      (path-runtime-value-start p)
                      (path-runtime-value-end q)
                      (list 'concat (path-runtime-value-proof p) 
                           (path-runtime-value-proof q))))

;; Create path inverse
(define (make-path-inverse-value p)
  (unless (path-runtime-value? p)
    (error "make-path-inverse-value: argument must be a path-runtime-value" p))
  (path-runtime-value (path-runtime-value-type p)
                      (path-runtime-value-end p)
                      (path-runtime-value-start p)
                      (list 'inverse (path-runtime-value-proof p))))

;; Transport operation: move value along a path
(define (transport-value path predicate val)
  (unless (path-runtime-value? path)
    (error "transport-value: first argument must be a path-runtime-value" path))
  (unless (hott-value? val)
    (error "transport-value: third argument must be a value" val))
  ;; Simplified implementation - just return the value for now
  val)

;; Congruence: apply function to path
(define (cong-value func path)
  (unless (path-runtime-value? path)
    (error "cong-value: second argument must be a path-runtime-value" path))
  (path-runtime-value (path-runtime-value-type path) ; Simplified
                      (func (path-runtime-value-start path))
                      (func (path-runtime-value-end path))
                      (list 'ap func (path-runtime-value-proof path))))

;; Create identity equivalence
(define (make-identity-equiv-value type-a)
  (unless (hott-type? type-a)
    (error "make-identity-equiv-value: argument must be a HoTT type" type-a))
  (let ([id-func (lambda (x) x)])
    (equivalence-runtime-value type-a type-a id-func 'id-quasi-inverse)))

;; Apply univalence axiom to equivalence
(define (univalence-apply equiv)
  (unless (equivalence-runtime-value? equiv)
    (error "univalence-apply: argument must be an equivalence-runtime-value" equiv))
  (path-runtime-value Type1 
                      (equivalence-runtime-value-type-a equiv)
                      (equivalence-runtime-value-type-b equiv)
                      'ua-path))

;; Predicates are auto-generated by struct definitions

;; Check if path is reflexivity
(define (is-refl-runtime-path? p)
  (unless (path-runtime-value? p)
    (error "is-refl-runtime-path?: argument must be a path-runtime-value" p))
  (and (equal? (path-runtime-value-start p) (path-runtime-value-end p))
       (eq? (path-runtime-value-proof p) 'refl)))