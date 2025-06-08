#lang racket/base

(require racket/contract
         racket/match
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

;; Value contracts
(define value/c
  (or/c constructor-value? closure-value? builtin-value? unit-value? 
        path-runtime-value? equivalence-runtime-value? string-value? effect-value?))

;; Predefined values for HoTT inductive types

;; Natural number values
(define zero-value (constructor-value "zero" '() Nat))

(define/contract (succ-value n)
  (-> value/c constructor-value?)
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
(define/contract (value-has-type? val type)
  (-> value/c extended-hott-type/c boolean?)
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

;; Pretty printing for values
(define/contract (value->string val)
  (-> value/c string?)
  (match val
    [(constructor-value name args _)
     (if (null? args)
         name
         (string-append "(" name " " 
                       (string-join (map value->string args) " ") 
                       ")"))]
    [(closure-value _ _ _) "#<closure>"]
    [(builtin-value name _ _) (string-append "#<builtin:" name ">")]
    [(unit-value) "unit"]
    [(string-value content) (string-append "\"" content "\"")]
    [(effect-value effect) (string-append "#<effect:" (format "~a" effect) ">")]
    [(path-runtime-value type start end proof)
     (string-append "path[" (type->string type) " : " 
                   (value->string start) " = " (value->string end) "]")]
    [(equivalence-runtime-value type-a type-b _ _)
     (string-append (type->string type-a) " ≃ " (type->string type-b))]))

;; Check if value is a natural number
(define/contract (nat-value? val)
  (-> value/c boolean?)
  (and (constructor-value? val)
       (let ([name (constructor-value-constructor-name val)])
         (or (string=? name "zero") (string=? name "next")))))

;; Check if value is a boolean
(define/contract (bool-value? val)
  (-> value/c boolean?)
  (and (constructor-value? val)
       (let ([name (constructor-value-constructor-name val)])
         (or (string=? name "true") (string=? name "false")))))

;; ============================================================================
;; PATH AND EQUIVALENCE VALUES
;; ============================================================================

;; Create reflexivity path
(define/contract (make-refl-value type term)
  (-> hott-type/c value/c path-runtime-value?)
  (path-runtime-value type term term 'refl))

;; Create path concatenation
(define/contract (make-path-concat-value p q)
  (-> path-runtime-value? path-runtime-value? path-runtime-value?)
  (unless (equal? (path-runtime-value-end p) (path-runtime-value-start q))
    (error "Cannot concatenate paths: end of first must equal start of second"))
  (path-runtime-value (path-runtime-value-type p)
                      (path-runtime-value-start p)
                      (path-runtime-value-end q)
                      (list 'concat (path-runtime-value-proof p) 
                           (path-runtime-value-proof q))))

;; Create path inverse
(define/contract (make-path-inverse-value p)
  (-> path-runtime-value? path-runtime-value?)
  (path-runtime-value (path-runtime-value-type p)
                      (path-runtime-value-end p)
                      (path-runtime-value-start p)
                      (list 'inverse (path-runtime-value-proof p))))

;; Transport operation: move value along a path
(define/contract (transport-value path predicate val)
  (-> path-runtime-value? any/c value/c value/c)
  ;; Simplified implementation - just return the value for now
  val)

;; Congruence: apply function to path
(define/contract (cong-value func path)
  (-> any/c path-runtime-value? path-runtime-value?)
  (path-runtime-value (path-runtime-value-type path) ; Simplified
                      (func (path-runtime-value-start path))
                      (func (path-runtime-value-end path))
                      (list 'ap func (path-runtime-value-proof path))))

;; Create identity equivalence
(define/contract (make-identity-equiv-value type-a)
  (-> hott-type/c equivalence-runtime-value?)
  (let ([id-func (lambda (x) x)])
    (equivalence-runtime-value type-a type-a id-func 'id-quasi-inverse)))

;; Apply univalence axiom to equivalence
(define/contract (univalence-apply equiv)
  (-> equivalence-runtime-value? path-runtime-value?)
  (path-runtime-value Type1 
                      (equivalence-runtime-value-type-a equiv)
                      (equivalence-runtime-value-type-b equiv)
                      'ua-path))

;; Predicates are auto-generated by struct definitions

;; Check if path is reflexivity
(define/contract (is-refl-runtime-path? p)
  (-> path-runtime-value? boolean?)
  (and (equal? (path-runtime-value-start p) (path-runtime-value-end p))
       (eq? (path-runtime-value-proof p) 'refl)))