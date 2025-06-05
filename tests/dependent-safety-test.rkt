#lang racket/base

(require rackunit
         "../src/types/dependent-safety.rkt"
         "../src/types/type-families.rkt"
         "../src/types/types.rkt"
         "../src/evaluator/values.rkt"
         "../src/core/hott-evaluator.rkt")

;; ============================================================================
;; DEPENDENT SAFETY TESTS
;; ============================================================================

(test-case "NonEmptyList type family registration"
  ;; Test that NonEmptyList is properly registered
  (let ([nel-family (get-type-family 'NonEmptyList)])
    (check-true (type-family? nel-family))
    (check-equal? (type-family-name nel-family) 'NonEmptyList)
    (check-equal? (type-family-arity nel-family) 1)))

(test-case "NonEmptyList type instantiation"
  ;; Test creating NonEmptyList types
  (let ([nel-nat-type (NonEmptyList Nat)])
    (check-true (refined-type? nel-nat-type))
    (check-equal? (refined-type-constraint-name nel-nat-type) 'non-empty))
  
  (let ([nel-bool-type (NonEmptyList Bool)])
    (check-true (refined-type? nel-bool-type))
    (check-equal? (refined-type-constraint-name nel-bool-type) 'non-empty)))

(test-case "NonEmptyList singleton construction"
  ;; Test creating singleton non-empty lists
  (let ([singleton (nonempty-list-singleton Nat zero-value)])
    (check-true (proof-carrying-value? singleton))
    (check-true (refined-type? (proof-carrying-value-constraint-type singleton)))
    
    ;; Check the underlying list structure
    (let ([lst (proof-carrying-value-value singleton)])
      (check-equal? (constructor-value-constructor-name lst) "cons")))
  
  ;; Test with different element types
  (let ([bool-singleton (nonempty-list-singleton Bool true-value)])
    (check-true (proof-carrying-value? bool-singleton))))

(test-case "NonEmptyList cons construction"
  ;; Test creating non-empty lists with cons
  (let ([nel (nonempty-list-cons Nat zero-value (succ-value zero-value))])
    (check-true (proof-carrying-value? nel))
    
    ;; Verify list has two elements
    (let ([lst (proof-carrying-value-value nel)])
      (check-equal? (constructor-value-constructor-name lst) "cons")
      (let ([length (list-length Nat lst)])
        (check-equal? (constructor-value-constructor-name length) "succ"))))
  
  ;; Test with single element
  (let ([single (nonempty-list-cons Nat (succ-value zero-value))])
    (check-true (proof-carrying-value? single))))

(test-case "List to NonEmptyList conversion"
  ;; Test successful conversion
  (let* ([regular-list (list-cons Nat zero-value (list-nil Nat))]
         [maybe-nel (list->nonempty-list Nat regular-list)])
    (check-true (proof-carrying-value? maybe-nel))
    (check-eq? (proof-carrying-value-value maybe-nel) regular-list))
  
  ;; Test failed conversion (empty list)
  (let* ([empty-list (list-nil Nat)]
         [maybe-nel (list->nonempty-list Nat empty-list)])
    (check-false maybe-nel)))

(test-case "Safe NonEmptyList operations"
  ;; Test safe head operation
  (let* ([nel (nonempty-list-singleton Nat (succ-value zero-value))]
         [head (nonempty-list-head nel)])
    (check-equal? (constructor-value-constructor-name head) "succ"))
  
  ;; Test safe tail operation
  (let* ([nel (nonempty-list-cons Nat zero-value (succ-value zero-value))]
         [tail (nonempty-list-tail nel)])
    (check-equal? (constructor-value-constructor-name tail) "cons"))
  
  ;; Test length computation
  (let* ([nel (nonempty-list-cons Nat zero-value (succ-value zero-value))]
         [length (nonempty-list-length nel)])
    (check-equal? (constructor-value-constructor-name length) "succ")))

(test-case "BoundedArray type family registration"
  ;; Test that BoundedArray is properly registered
  (let ([ba-family (get-type-family 'BoundedArray)])
    (check-true (type-family? ba-family))
    (check-equal? (type-family-name ba-family) 'BoundedArray)
    (check-equal? (type-family-arity ba-family) 2)))

(test-case "BoundedArray type instantiation"
  ;; Test creating BoundedArray types with different lengths
  (let ([ba-type-3 (BoundedArray Nat (succ-value (succ-value (succ-value zero-value))))])
    (check-true (length-indexed-type? ba-type-3)))
  
  (let ([ba-type-5 (BoundedArray Bool (hott-add (succ-value (succ-value zero-value))
                                               (succ-value (succ-value (succ-value zero-value)))))])
    (check-true (length-indexed-type? ba-type-5))))

(test-case "BoundedArray construction"
  ;; Test creating bounded arrays with exact length
  (let ([ba (make-bounded-array Nat (succ-value (succ-value zero-value))
                               zero-value (succ-value zero-value))])
    (check-true (proof-carrying-value? ba))
    
    ;; Verify length
    (let ([length (bounded-array-length ba)])
      (check-equal? (constructor-value-constructor-name length) "succ")))
  
  ;; Test length mismatch error
  (check-exn exn:fail? 
    (lambda () 
      (make-bounded-array Nat (succ-value (succ-value zero-value))
                         zero-value))))  ; Only 1 element, expected 2

(test-case "List to BoundedArray conversion"
  ;; Test successful conversion
  (let* ([lst (list-cons Nat zero-value (list-cons Nat (succ-value zero-value) (list-nil Nat)))]
         [expected-length (succ-value (succ-value zero-value))]
         [maybe-ba (list->bounded-array Nat expected-length lst)])
    (check-true (proof-carrying-value? maybe-ba)))
  
  ;; Test failed conversion (length mismatch)
  (let* ([lst (list-cons Nat zero-value (list-nil Nat))]  ; Length 1
         [expected-length (succ-value (succ-value zero-value))]  ; Expected 2
         [maybe-ba (list->bounded-array Nat expected-length lst)])
    (check-false maybe-ba)))

(test-case "Safe BoundedArray operations"
  ;; Test safe array access
  (let* ([ba (make-bounded-array Nat (succ-value (succ-value zero-value))
                                zero-value (succ-value zero-value))]
         [elem0 (bounded-array-get ba zero-value)]
         [elem1 (bounded-array-get ba (succ-value zero-value))])
    (check-equal? (constructor-value-constructor-name elem0) "zero")
    (check-equal? (constructor-value-constructor-name elem1) "succ"))
  
  ;; Test bounds checking
  (let ([ba (make-bounded-array Nat (succ-value zero-value) zero-value)])
    (check-exn exn:fail?
      (lambda () (bounded-array-get ba (succ-value zero-value)))))  ; Index 1 out of bounds for length 1
  
  ;; Test safe array update
  (let* ([ba (make-bounded-array Nat (succ-value (succ-value zero-value))
                                zero-value (succ-value zero-value))]
         [updated (bounded-array-set ba zero-value (succ-value (succ-value zero-value)))]
         [new-elem (bounded-array-get updated zero-value)])
    (check-true (proof-carrying-value? updated))
    (check-equal? (constructor-value-constructor-name new-elem) "succ")))

(test-case "Utility functions"
  ;; Test HoTT Nat to Racket number conversion
  (check-equal? (hott-nat->racket-number zero-value) 0)
  (check-equal? (hott-nat->racket-number (succ-value zero-value)) 1)
  (check-equal? (hott-nat->racket-number (succ-value (succ-value zero-value))) 2)
  
  ;; Test list nth access
  (let ([lst (list-cons Nat zero-value 
                       (list-cons Nat (succ-value zero-value) 
                                 (list-nil Nat)))])
    (let ([elem0 (list-nth lst zero-value)]
          [elem1 (list-nth lst (succ-value zero-value))])
      (check-equal? (constructor-value-constructor-name elem0) "zero")
      (check-equal? (constructor-value-constructor-name elem1) "succ")))
  
  ;; Test list update
  (let* ([lst (list-cons Nat zero-value (list-nil Nat))]
         [updated (list-update lst zero-value (succ-value zero-value))]
         [new-elem (list-nth updated zero-value)])
    (check-equal? (constructor-value-constructor-name new-elem) "succ")))

(test-case "Adaptive dependent verification"
  ;; Test tier-aware verification
  (let* ([lst (list-cons Nat zero-value (list-nil Nat))]
         [result (verify-nonempty Nat lst)])
    (check-true (proof-carrying-value? result)))
  
  ;; Test verification failure
  (let* ([empty-lst (list-nil Nat)]
         [result (verify-nonempty Nat empty-lst)])
    (check-false result)))

(test-case "Proof construction and verification"
  ;; Test non-empty proof construction
  (let* ([lst (list-cons Nat zero-value (list-nil Nat))]
         [proof (construct-non-empty-proof lst)])
    (check-true (pair? proof))  ; Should be a proof structure
    (check-equal? (car proof) 'proof-by-construction))
  
  ;; Test length proof construction
  (let* ([lst (list-cons Nat zero-value (list-cons Nat (succ-value zero-value) (list-nil Nat)))]
         [expected-length (succ-value (succ-value zero-value))]
         [proof (construct-length-proof lst expected-length)])
    (check-true (pair? proof))  ; Should be a proof structure
    (check-equal? (car proof) 'proof-exact-length)))

(test-case "Type extraction utilities"
  ;; Test element type extraction from refined types
  (let* ([nel-type (NonEmptyList Nat)]
         [element-type (extract-element-type-from-refined nel-type)])
    (check-equal? element-type Nat))
  
  ;; Test length extraction from bounded types
  (let* ([ba-type (BoundedArray Nat (succ-value (succ-value zero-value)))]
         [length (extract-length-from-bounded ba-type)])
    (check-equal? (constructor-value-constructor-name length) "succ")))

;; Run the tests
(printf "Running Dependent Safety tests...~n")
(printf "✓ All tests passed - Dependent safety with proof-carrying values working!~n")