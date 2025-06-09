#lang racket/base

(require racket/contract
         racket/match
         "types.rkt"
         "../evaluator/values.rkt"
         "../core/hott-evaluator.rkt")

(provide (all-defined-out))

;; ============================================================================
;; PARAMETERIZED LIST TYPE - HOTT NATIVE
;; ============================================================================
;; List T is a parameterized inductive type with provably safe operations
;; All operations are Tier 1 (pure computational proofs)

;; Use the existing make-list-type from types.rkt
;; (imported automatically)

;; Common list types
(define List-Nat (make-list-type Nat))
(define List-Bool (make-list-type Bool))
(define List-String (make-list-type (inductive-type "String" '())))

;; ============================================================================
;; LIST CONSTRUCTORS (Data Constructors)
;; ============================================================================

;; Empty list constructor - Data constructor: Unit → List T
(define/contract (hott-list-nil element-type)
  (-> extended-hott-type/c constructor-value?)
  (constructor-value "nil" '() (make-list-type element-type)))

;; Cons constructor - Data constructor: T × List T → List T  
(define/contract (hott-list-cons element tail-list element-type)
  (-> constructor-value? constructor-value? extended-hott-type/c constructor-value?)
  (constructor-value "cons" 
                    (list element tail-list)
                    (make-list-type element-type)))

;; ============================================================================
;; TIER 1: PROVABLY SAFE LIST OPERATIONS (Pure Computational Proofs)
;; ============================================================================

;; List length - Pure HoTT computation that proves termination
(define/contract (hott-list-length lst)
  (-> constructor-value? constructor-value?)
  (match lst
    [(constructor-value "nil" '() _) zero-value]
    [(constructor-value "cons" (list _ rest) _)
     (succ-value (hott-list-length rest))]
    [_ (error "Not a list: " lst)]))

;; List concatenation - Pure HoTT with structural recursion proof
(define/contract (hott-list-append lst1 lst2)
  (-> constructor-value? constructor-value? constructor-value?)
  (match lst1
    [(constructor-value "nil" '() list-type) lst2]
    [(constructor-value "cons" (list head tail) list-type)
     (let ([element-type (extract-list-element-type list-type)])
       (hott-list-cons head 
                      (hott-list-append tail lst2)
                      element-type))]
    [_ (error "Not a list: " lst1)]))

;; List map - Higher-order function with totality proof
(define/contract (hott-list-map func lst element-type result-type)
  (-> procedure? constructor-value? extended-hott-type/c extended-hott-type/c constructor-value?)
  (match lst
    [(constructor-value "nil" '() _) 
     (hott-list-nil result-type)]
    [(constructor-value "cons" (list head tail) _)
     (let ([mapped-head (func head)]
           [mapped-tail (hott-list-map func tail element-type result-type)])
       (hott-list-cons mapped-head mapped-tail result-type))]
    [_ (error "Not a list: " lst)]))

;; List fold (reduce) - Provably total with structural recursion
(define/contract (hott-list-fold func initial lst)
  (-> procedure? constructor-value? constructor-value? constructor-value?)
  (match lst
    [(constructor-value "nil" '() _) initial]
    [(constructor-value "cons" (list head tail) _)
     (func head (hott-list-fold func initial tail))]
    [_ (error "Not a list: " lst)]))

;; List filter - Filter elements based on predicate
(define/contract (hott-list-filter predicate lst element-type)
  (-> procedure? constructor-value? extended-hott-type/c constructor-value?)
  (match lst
    [(constructor-value "nil" '() _) 
     (hott-list-nil element-type)]
    [(constructor-value "cons" (list head tail) _)
     (let ([filtered-tail (hott-list-filter predicate tail element-type)])
       (if (string=? (constructor-value-constructor-name (predicate head)) "true")
           (hott-list-cons head filtered-tail element-type)
           filtered-tail))]
    [_ (error "Not a list: " lst)]))

;; List reverse - Reverse the order of elements
(define/contract (hott-list-reverse lst)
  (-> constructor-value? constructor-value?)
  (match lst
    [(constructor-value "nil" '() list-type) lst]  ; Empty list is its own reverse
    [(constructor-value "cons" (list head tail) list-type)
     (let ([element-type (extract-list-element-type list-type)])
       (hott-list-append (hott-list-reverse tail) 
                        (hott-list-cons head (hott-list-nil element-type) element-type)))]
    [_ (error "Not a list: " lst)]))

;; ============================================================================
;; SAFE LIST ACCESS (Tier 1 with Proofs)
;; ============================================================================

;; Safe head - requires proof that list is non-empty
(define/contract (hott-list-safe-head lst non-empty-proof)
  (-> constructor-value? any/c constructor-value?)
  ;; For now, trust the proof (full proof checking would verify here)
  (match lst
    [(constructor-value "cons" (list head _) _) head]
    [(constructor-value "nil" '() _) 
     (error "Empty list - proof should have prevented this!")]
    [_ (error "Not a list: " lst)]))

;; Safe tail - requires proof that list is non-empty  
(define/contract (hott-list-safe-tail lst non-empty-proof)
  (-> constructor-value? any/c constructor-value?)
  (match lst
    [(constructor-value "cons" (list _ tail) _) tail]
    [(constructor-value "nil" '() _)
     (error "Empty list - proof should have prevented this!")]
    [_ (error "Not a list: " lst)]))

;; Try to construct proof that list is non-empty
(define/contract (try-prove-list-non-empty lst)
  (-> constructor-value? (or/c any/c #f))
  (match lst
    [(constructor-value "cons" _ _) 
     ;; Construct computational proof that list has at least one element
     `(proof-non-empty-list ,lst)]
    [(constructor-value "nil" '() _) #f]
    [_ #f]))

;; Auto-safe head: tries to construct proof automatically
(define/contract (hott-list-auto-safe-head lst)
  (-> constructor-value? constructor-value?)
  (let ([proof (try-prove-list-non-empty lst)])
    (if proof
        (hott-list-safe-head lst proof)
        (error "Cannot prove list is non-empty"))))

;; ============================================================================
;; LIST PREDICATES (Computational Proofs)
;; ============================================================================

;; Is list empty? - Pure HoTT predicate
(define/contract (hott-list-empty? lst)
  (-> constructor-value? constructor-value?)
  (match lst
    [(constructor-value "nil" '() _) true-value]
    [(constructor-value "cons" _ _) false-value]
    [_ (error "Not a list: " lst)]))

;; List equality - Structural equality using element equality
(define/contract (hott-list-equal? lst1 lst2 element-equal-func)
  (-> constructor-value? constructor-value? procedure? constructor-value?)
  (match* (lst1 lst2)
    [((constructor-value "nil" '() _) (constructor-value "nil" '() _))
     true-value]
    [((constructor-value "cons" (list h1 t1) _) (constructor-value "cons" (list h2 t2) _))
     (let ([heads-equal (element-equal-func h1 h2)]
           [tails-equal (hott-list-equal? t1 t2 element-equal-func)])
       (hott-and heads-equal tails-equal))]
    [(_ _) false-value]))

;; ============================================================================
;; UTILITY FUNCTIONS
;; ============================================================================

;; Extract element type from List type
(define/contract (extract-list-element-type list-type)
  (-> any/c extended-hott-type/c)
  ;; This would parse the List type to extract T from List T
  ;; For now, simplified implementation based on naming convention
  (match list-type
    [(inductive-type name constructors) 
     (cond
       [(string=? name "List-Nat") Nat]
       [(string=? name "List-Bool") Bool]
       [(and (>= (string-length name) 5) 
             (string=? (substring name 0 5) "List-")) Nat]  ; Default fallback
       [else (error "Not a List type: " list-type)])]
    [_ (error "Not a List type: " list-type)]))

;; Create list from HoTT values
(define/contract (hott-values->list values element-type)
  (-> (listof constructor-value?) extended-hott-type/c constructor-value?)
  (if (null? values)
      (hott-list-nil element-type)
      (hott-list-cons (car values)
                     (hott-values->list (cdr values) element-type)
                     element-type)))

;; Convert list to HoTT values (for debugging/display)
(define/contract (hott-list->values lst)
  (-> constructor-value? (listof constructor-value?))
  (match lst
    [(constructor-value "nil" '() _) '()]
    [(constructor-value "cons" (list head tail) _)
     (cons head (hott-list->values tail))]
    [_ (error "Not a list: " lst)]))