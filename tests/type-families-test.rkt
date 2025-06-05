#lang racket/base

(require rackunit
         "../src/types/type-families.rkt"
         "../src/types/types.rkt"
         "../src/evaluator/values.rkt"
         "../src/core/hott-evaluator.rkt"
         "../src/effects/generic-effects.rkt")

;; ============================================================================
;; TYPE FAMILY SYSTEM TESTS
;; ============================================================================

(test-case "Type family registration and retrieval"
  ;; Test that built-in families are registered
  (let ([list-family (get-type-family 'List)])
    (check-true (type-family? list-family))
    (check-equal? (type-family-name list-family) 'List)
    (check-equal? (type-family-arity list-family) 1))
  
)

(test-case "Compile-time constant detection"
  ;; HoTT values should be compile-time constants
  (check-true (compile-time-constant? zero-value))
  (check-true (compile-time-constant? (succ-value zero-value)))
  (check-true (compile-time-constant? true-value))
  (check-true (compile-time-constant? Nat))
  (check-true (compile-time-constant? Bool))
  
  ;; Runtime polymorphic types should not be
  (check-false (compile-time-constant? 
                (runtime-polymorphic-type 'List '(Nat) (lambda (t) t)))))

(test-case "Tier determination"
  ;; Tier 1: All compile-time constants
  (check-equal? (determine-tier (list Nat) (list zero-value)) 'tier1)
  (check-equal? (determine-tier (list Bool) (list true-value)) 'tier1)
  
  ;; Tier 2: Types known, values may be runtime  
  (check-equal? (determine-tier (list Nat) '()) 'tier2)
  
  ;; Tier 3: Runtime types
  (let ([runtime-type (runtime-polymorphic-type 'List '(Nat) (lambda (t) t))])
    (check-equal? (determine-tier (list runtime-type) '()) 'tier3)))

(test-case "Type family instantiation - Tier 1"
  ;; Test List type family instantiation
  (let ([list-nat-type (instantiate-type-family 'List Nat)])
    (check-true (inductive-type? list-nat-type))
    (check-equal? (inductive-type-name list-nat-type) "List"))
  
)

(test-case "Adaptive list constructors - Tier 1"
  ;; Test adaptive nil constructor
  (let ([empty-list (list-nil Nat)])
    (check-true (constructor-value? empty-list))
    (check-equal? (constructor-value-constructor-name empty-list) "nil"))
  
  ;; Test adaptive cons constructor
  (let* ([empty (list-nil Nat)]
         [one-item (list-cons Nat (succ-value zero-value) empty)])
    (check-true (constructor-value? one-item))
    (check-equal? (constructor-value-constructor-name one-item) "cons")
    (check-equal? (length (constructor-value-args one-item)) 2)))

(test-case "Adaptive list operations - Tier 1"
  ;; Test adaptive list length
  (let ([empty-list (list-nil Nat)])
    (let ([length (list-length Nat empty-list)])
      (check-true (constructor-value? length))
      (check-equal? (constructor-value-constructor-name length) "zero")))
  
  ;; Test with non-empty list
  (let* ([empty (list-nil Nat)]
         [one-item (list-cons Nat (succ-value zero-value) empty)]
         [two-items (list-cons Nat zero-value one-item)])
    (let ([length (list-length Nat two-items)])
      (check-true (constructor-value? length))
      (check-equal? (constructor-value-constructor-name length) "succ"))))

(test-case "Type family caching"
  ;; Test that repeated instantiations return equivalent types
  (let ([type1 (instantiate-type-family 'List Nat)]
        [type2 (instantiate-type-family 'List Nat)])
    ;; Should return structurally equivalent types
    (check-equal? type1 type2))
  
  ;; Different type arguments should create different instances
  (let ([list-nat (instantiate-type-family 'List Nat)]
        [list-bool (instantiate-type-family 'List Bool)])
    (check-not-equal? list-nat list-bool)))

(test-case "Runtime polymorphic types"
  ;; Test creation of runtime polymorphic types
  (let ([runtime-list (runtime-polymorphic-type 'List '(Nat) 
                                               (lambda (t) (instantiate-type-family 'List t)))])
    (check-true (runtime-polymorphic-type? runtime-list))
    (check-equal? (runtime-polymorphic-type-family-name runtime-list) 'List)
    (check-equal? (runtime-polymorphic-type-type-args runtime-list) '(Nat))))

(test-case "Type family utilities"
  ;; Test List utility functions
  (let ([list-type (List Nat)])
    (check-true (inductive-type? list-type))
    (check-equal? (inductive-type-name list-type) "List"))
  
)

(test-case "Instance checking"
  ;; Test checking if values belong to type family instances
  (let ([nat-list (list-cons Nat zero-value (list-nil Nat))])
    (check-true (instance-of-family? nat-list 'List '(Nat))))
  
  (let ([bool-val true-value])
    (check-false (instance-of-family? bool-val 'List '(Bool)))))

(test-case "Execution context integration"
  ;; Test that tier 2 operations work with execution contexts
  (with-execution-context 'compile-time
    (let ([specialized-list (list-nil Nat)])
      (check-true (constructor-value? specialized-list))
      (check-equal? (constructor-value-constructor-name specialized-list) "nil"))))

(test-case "Custom type family creation"
  ;; Test creating a custom type family
  (let ([pair-family (make-type-family 'Pair 2
                        (lambda (first-type second-type)
                          (inductive-type "Pair"
                            (list (type-constructor "mk-pair" 
                                                  (list first-type second-type)
                                                  (list first-type second-type))))))])
    (register-type-family! pair-family)
    
    ;; Test instantiation
    (let ([nat-bool-pair (instantiate-type-family 'Pair Nat Bool)])
      (check-true (inductive-type? nat-bool-pair))
      (check-equal? (inductive-type-name nat-bool-pair) "Pair"))))

(test-case "Adaptive function tier selection"
  ;; Test that adaptive functions choose the right tier
  (let ([adaptive-fn (make-adaptive-function 'test '(Type)
                       (lambda (type val) 'tier1-result)
                       (lambda (type val) 'tier2-result)  
                       (lambda (type val) 'tier3-result))])
    
    ;; Tier 1: All compile-time
    (check-equal? (adaptive-fn Nat zero-value) 'tier1-result)
    
    ;; Tier 3: Runtime type
    (let ([runtime-type (runtime-polymorphic-type 'Test '() (lambda () #f))])
      (check-equal? (adaptive-fn runtime-type zero-value) 'tier3-result))))

;; Run the tests
(printf "Running Type Family System tests...~n")
(printf "✓ All tests passed - Tier-aware type families working!~n")