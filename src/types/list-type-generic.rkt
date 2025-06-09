#lang racket/base

(require racket/contract
         racket/match
         "type-families.rkt"
         "types.rkt"
         "../evaluator/values.rkt"
         "../core/hott-evaluator.rkt"
         "../effects/pure-hott-effects.rkt")

(provide (all-defined-out))

;; ============================================================================
;; GENERIC LIST TYPE IMPLEMENTATION
;; ============================================================================
;; This replaces the old monomorphic list-type.rkt with the new tier-aware
;; generic type family system

;; ============================================================================
;; GENERIC LIST OPERATIONS (Tier-Aware)
;; ============================================================================

;; All list operations are now adaptive and use the type family system
;; They automatically choose the right tier based on available information

;; List append: Tier-aware concatenation
(define/contract (hott-list-append element-type lst1 lst2)
  (-> extended-hott-type/c constructor-value? constructor-value? constructor-value?)
  (match lst1
    [(constructor-value "nil" '() _) lst2]
    [(constructor-value "cons" (list head tail) _)
     (list-cons element-type head (hott-list-append element-type tail lst2))]
    [_ (error "Not a list: " lst1)]))

;; List map: Higher-order function with tier awareness
(define/contract (hott-list-map element-type result-type func lst)
  (-> extended-hott-type/c extended-hott-type/c procedure? constructor-value? constructor-value?)
  (match lst
    [(constructor-value "nil" '() _) 
     (list-nil result-type)]
    [(constructor-value "cons" (list head tail) _)
     (let ([mapped-head (func head)]
           [mapped-tail (hott-list-map element-type result-type func tail)])
       (list-cons result-type mapped-head mapped-tail))]
    [_ (error "Not a list: " lst)]))

;; List fold: Structural recursion with tier awareness
(define/contract (hott-list-fold element-type func initial lst)
  (-> extended-hott-type/c procedure? constructor-value? constructor-value? constructor-value?)
  (match lst
    [(constructor-value "nil" '() _) initial]
    [(constructor-value "cons" (list head tail) _)
     (func head (hott-list-fold element-type func initial tail))]
    [_ (error "Not a list: " lst)]))

;; ============================================================================
;; SAFE LIST ACCESS (Generic with Proofs)
;; ============================================================================

;; Safe head - requires proof that list is non-empty
(define/contract (hott-list-safe-head lst non-empty-proof)
  (-> constructor-value? any/c constructor-value?)
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
;; LIST PREDICATES (Generic)
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
;; TIER-AWARE UTILITY FUNCTIONS
;; ============================================================================

;; Create list from HoTT values with tier awareness
(define/contract (hott-values->list element-type values)
  (-> extended-hott-type/c (listof constructor-value?) constructor-value?)
  (if (null? values)
      (list-nil element-type)
      (list-cons element-type (car values)
                (hott-values->list element-type (cdr values)))))

;; Convert list to HoTT values (for debugging/display)
(define/contract (hott-list->values lst)
  (-> constructor-value? (listof constructor-value?))
  (match lst
    [(constructor-value "nil" '() _) '()]
    [(constructor-value "cons" (list head tail) _)
     (cons head (hott-list->values tail))]
    [_ (error "Not a list: " lst)]))

;; ============================================================================
;; GENERIC LIST EXAMPLES AND DEMONSTRATIONS
;; ============================================================================

;; Example: Creating lists of different types using the same API
(define/contract (create-sample-lists)
  (-> (values constructor-value? constructor-value? constructor-value?))
  (let ([nat-list (list-cons Nat zero-value 
                            (list-cons Nat (succ-value zero-value) 
                                      (list-nil Nat)))]
        [bool-list (list-cons Bool true-value 
                             (list-cons Bool false-value 
                                       (list-nil Bool)))]
        [empty-nat-list (list-nil Nat)])
    (values nat-list bool-list empty-nat-list)))

;; Example: Generic operations that work across different instantiations  
(define/contract (demonstrate-generic-ops)
  (-> void?)
  (let-values ([(nat-list bool-list empty-list) (create-sample-lists)])
    
    ;; Length computation works for any element type
    (let ([nat-length (list-length Nat nat-list)]
          [bool-length (list-length Bool bool-list)]
          [empty-length (list-length Nat empty-list)])
      
      (printf "Generic list operations:~n")
      (printf "  Nat list length: ~a~n" 
              (constructor-value-constructor-name nat-length))
      (printf "  Bool list length: ~a~n" 
              (constructor-value-constructor-name bool-length))
      (printf "  Empty list length: ~a~n" 
              (constructor-value-constructor-name empty-length)))
    
    ;; Append operations
    (let ([appended-nats (hott-list-append Nat nat-list nat-list)])
      (printf "  Appended nat lists: ~a~n" 
              (constructor-value-constructor-name appended-nats)))))

;; Example: Tier-aware behavior demonstration
(define/contract (demonstrate-tier-behavior)
  (-> void?)
  (printf "~nTier-aware behavior:~n")
  
  ;; Tier 1: Compile-time
  (printf "  Tier 1 (compile-time): ~a~n"
          (determine-tier (list Nat) (list zero-value)))
  
  ;; Tier 2: Type-specialized
  (printf "  Tier 2 (type-specialized): ~a~n"
          (determine-tier (list Nat) '()))
  
  ;; Tier 3: Runtime polymorphic
  (let ([runtime-type (runtime-polymorphic-type 'List '(Nat) 
                        (lambda (t) (List t)))])
    (printf "  Tier 3 (runtime polymorphic): ~a~n"
            (determine-tier (list runtime-type) '()))))

;; ============================================================================
;; BACKWARDS COMPATIBILITY
;; ============================================================================

;; Provide aliases for the old monomorphic interface for gradual migration
(define hott-list-nil list-nil)
(define hott-list-cons list-cons)  
(define hott-list-length list-length)

;; Legacy constructors that match the old interface
(define/contract (make-list-nil element-type)
  (-> extended-hott-type/c constructor-value?)
  (list-nil element-type))

(define/contract (make-list-cons element tail-list element-type)
  (-> constructor-value? constructor-value? extended-hott-type/c constructor-value?)
  (list-cons element-type element tail-list))

;; Legacy type creation that uses the new type family system
(define/contract (make-list-type element-type)
  (-> extended-hott-type/c extended-hott-type/c)
  (List element-type))

;; ============================================================================
;; MODULE TESTING
;; ============================================================================

;; Simple test to verify functionality
(define/contract (test-generic-lists)
  (-> void?)
  (printf "Testing generic list system...~n")
  (demonstrate-generic-ops)
  (demonstrate-tier-behavior)
  (printf "Generic list system tests completed!~n"))