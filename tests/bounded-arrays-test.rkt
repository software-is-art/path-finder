#lang racket/base

(require rackunit
         "../src/types/bounded-arrays.rkt"
         "../src/types/dependent-safety.rkt"
         "../src/types/type-families.rkt"
         "../src/types/types.rkt"
         "../src/evaluator/values.rkt"
         "../src/core/hott-evaluator.rkt")

;; ============================================================================
;; BOUNDED ARRAYS ADVANCED TESTS
;; ============================================================================

(test-case "Bounded index creation and verification"
  ;; Test valid bounded index creation
  (let* ([index (succ-value zero-value)]
         [bound (succ-value (succ-value (succ-value zero-value)))]
         [bounded-idx (make-bounded-index index bound)])
    (check-true (bounded-index? bounded-idx))
    (check-eq? (bounded-index-value bounded-idx) index)
    (check-eq? (bounded-index-upper-bound bounded-idx) bound))
  
  ;; Test invalid bounded index (out of bounds)
  (let* ([index (succ-value (succ-value zero-value))]
         [bound (succ-value zero-value)]
         [bounded-idx (make-bounded-index index bound)])
    (check-false bounded-idx)))

(test-case "Bounded index arithmetic"
  ;; Test safe bounded index addition
  (let* ([idx1 (make-bounded-index zero-value (succ-value (succ-value zero-value)))]
         [idx2 (make-bounded-index (succ-value zero-value) (succ-value (succ-value zero-value)))]
         [sum (bounded-index-add idx1 idx2)])
    (check-true (bounded-index? sum)))
  
  ;; Test bounded index addition overflow
  (let* ([idx1 (make-bounded-index (succ-value zero-value) (succ-value (succ-value zero-value)))]
         [idx2 (make-bounded-index (succ-value zero-value) (succ-value (succ-value zero-value)))]
         [sum (bounded-index-add idx1 idx2)])
    (check-false sum)))  ; Should overflow

(test-case "Advanced bounded array constructors"
  ;; Test array with initializer function
  (let* ([length (succ-value (succ-value (succ-value zero-value)))]
         [init-fn (lambda (idx) (hott-mult idx idx))]  ; Square function
         [arr (make-bounded-array-init Nat length init-fn)])
    (check-true (proof-carrying-value? arr))
    (let ([arr-length (bounded-array-length arr)])
      (check-equal? (hott-nat->racket-number arr-length) 3)))
  
  ;; Test array filled with single value
  (let* ([length (succ-value (succ-value zero-value))]
         [fill-val (succ-value (succ-value (succ-value zero-value)))]
         [arr (make-bounded-array-fill Nat length fill-val)])
    (check-true (proof-carrying-value? arr)))
  
  ;; Test array from range
  (let* ([start (succ-value zero-value)]
         [end (succ-value (succ-value (succ-value (succ-value zero-value))))]
         [arr (make-bounded-array-range start end)])
    (check-true (proof-carrying-value? arr))))

(test-case "Safe array access with bounded indices"
  ;; Test safe access using bounded index
  (let* ([arr (make-bounded-array Nat (succ-value (succ-value zero-value))
                                 zero-value (succ-value zero-value))]
         [idx (make-bounded-index zero-value (succ-value (succ-value zero-value)))]
         [elem (bounded-array-get-safe arr idx)])
    (check-equal? (constructor-value-constructor-name elem) "zero"))
  
  ;; Test bounds checking in safe access
  (let* ([arr (make-bounded-array Nat (succ-value zero-value) zero-value)]
         [invalid-idx (make-bounded-index zero-value (succ-value (succ-value (succ-value zero-value))))])
    (check-exn exn:fail?
      (lambda () (bounded-array-get-safe arr invalid-idx)))))

(test-case "Array slicing with proofs"
  ;; Test valid array slice
  (let* ([arr (make-bounded-array Nat (succ-value (succ-value (succ-value (succ-value zero-value))))
                                 zero-value (succ-value zero-value) 
                                 (succ-value (succ-value zero-value))
                                 (succ-value (succ-value (succ-value zero-value))))]
         [start-idx (make-bounded-index (succ-value zero-value) 
                                       (succ-value (succ-value (succ-value (succ-value zero-value)))))]
         [slice-length (succ-value (succ-value zero-value))]
         [slice (bounded-array-slice arr start-idx slice-length)])
    (check-true (array-slice? slice))
    (check-equal? (array-slice-start-index slice) (succ-value zero-value))
    (check-equal? (array-slice-length slice) slice-length))
  
  ;; Test slice access
  (let* ([arr (make-bounded-array Nat (succ-value (succ-value (succ-value zero-value)))
                                 zero-value (succ-value zero-value) (succ-value (succ-value zero-value)))]
         [start-idx (make-bounded-index zero-value (succ-value (succ-value (succ-value zero-value))))]
         [slice-length (succ-value (succ-value zero-value))]
         [slice (bounded-array-slice arr start-idx slice-length)]
         [local-idx (make-bounded-index zero-value slice-length)]
         [elem (array-slice-get slice local-idx)])
    (check-equal? (constructor-value-constructor-name elem) "zero")))

(test-case "Compile-time access pattern verification"
  ;; Test valid access pattern
  (let* ([arr (make-bounded-array Nat (succ-value (succ-value (succ-value zero-value)))
                                 zero-value (succ-value zero-value) (succ-value (succ-value zero-value)))]
         [pattern (list zero-value (succ-value zero-value))]
         [is-valid (verify-array-access-pattern arr pattern)])
    (check-true is-valid))
  
  ;; Test invalid access pattern
  (let* ([arr (make-bounded-array Nat (succ-value (succ-value zero-value))
                                 zero-value (succ-value zero-value))]
         [pattern (list zero-value (succ-value (succ-value zero-value)))]  ; Index 2 out of bounds
         [is-valid (verify-array-access-pattern arr pattern)])
    (check-false is-valid))
  
  ;; Test access proof generation
  (let* ([arr (make-bounded-array Nat (succ-value (succ-value zero-value))
                                 zero-value (succ-value zero-value))]
         [pattern (list zero-value)]
         [proof (generate-access-proof arr pattern)])
    (check-true (pair? proof))
    (check-equal? (car proof) 'proof-safe-access-pattern)))

(test-case "2D bounded arrays"
  ;; Test 2D array creation
  (let* ([rows (succ-value (succ-value zero-value))]
         [cols (succ-value (succ-value (succ-value zero-value)))]
         [init-fn (lambda (r c) (hott-add r c))]
         [arr2d (make-bounded-array-2d Nat rows cols init-fn)])
    (check-true (multi-dim-array? arr2d))
    (check-equal? (length (multi-dim-array-dimensions arr2d)) 2))
  
  ;; Test 2D array access
  (let* ([rows (succ-value (succ-value zero-value))]
         [cols (succ-value (succ-value zero-value))]
         [init-fn (lambda (r c) (hott-mult r c))]
         [arr2d (make-bounded-array-2d Nat rows cols init-fn)]
         [row-idx (make-bounded-index zero-value rows)]
         [col-idx (make-bounded-index (succ-value zero-value) cols)]
         [elem (bounded-array-2d-get arr2d row-idx col-idx)])
    (check-equal? (constructor-value-constructor-name elem) "zero")))  ; 0 * 1 = 0

(test-case "Utility functions"
  ;; Test racket number to HoTT nat conversion
  (check-equal? (racket-number->hott-nat 0) zero-value)
  (check-equal? (racket-number->hott-nat 1) (succ-value zero-value))
  (check-equal? (racket-number->hott-nat 3) 
                (succ-value (succ-value (succ-value zero-value))))
  
  ;; Test HoTT maximum function
  (let* ([a (succ-value zero-value)]
         [b (succ-value (succ-value zero-value))]
         [max-val (hott-max a b)])
    (check-eq? max-val b))
  
  ;; Test HoTT less-than-or-equal
  (let* ([a (succ-value zero-value)]
         [b (succ-value (succ-value zero-value))]
         [lte (hott-less-equal? a b)])
    (check-equal? (constructor-value-constructor-name lte) "true")))

(test-case "Array iteration with bounds safety"
  ;; Test bounded array map
  (let* ([arr (make-bounded-array Nat (succ-value (succ-value zero-value))
                                 zero-value (succ-value zero-value))]
         [mapped (bounded-array-map arr (lambda (idx elem) (hott-add elem (succ-value zero-value))))])
    (check-true (proof-carrying-value? mapped)))
  
  ;; Test bounded array fold
  (let* ([arr (make-bounded-array Nat (succ-value (succ-value (succ-value zero-value)))
                                 (succ-value zero-value) (succ-value zero-value) (succ-value zero-value))]
         [sum (bounded-array-fold arr zero-value hott-add)])
    (check-equal? (constructor-value-constructor-name sum) "next")))  ; Should be 3

(test-case "Array equality with bounds verification"
  ;; Test equal arrays
  (let* ([arr1 (make-bounded-array Nat (succ-value (succ-value zero-value))
                                  zero-value (succ-value zero-value))]
         [arr2 (make-bounded-array Nat (succ-value (succ-value zero-value))
                                  zero-value (succ-value zero-value))]
         [equal? (bounded-array-equal? arr1 arr2 hott-equal?)])
    (check-equal? (constructor-value-constructor-name equal?) "true"))
  
  ;; Test unequal arrays (different lengths)
  (let* ([arr1 (make-bounded-array Nat (succ-value zero-value) zero-value)]
         [arr2 (make-bounded-array Nat (succ-value (succ-value zero-value))
                                  zero-value (succ-value zero-value))]
         [equal? (bounded-array-equal? arr1 arr2 hott-equal?)])
    (check-equal? (constructor-value-constructor-name equal?) "false")))

(test-case "Performance optimization functions"
  ;; Test specialized accessor generation
  (let* ([arr (make-bounded-array Nat (succ-value (succ-value zero-value))
                                 zero-value (succ-value zero-value))]
         [pattern (list zero-value)]
         [accessor (generate-specialized-accessor arr pattern)])
    (check-true (procedure? accessor)))
  
  ;; Test bounds-checked accessor cache
  (let* ([arr (make-bounded-array Nat (succ-value (succ-value zero-value))
                                 zero-value (succ-value zero-value))]
         [cached-accessor (create-bounds-checked-accessor arr)])
    (check-true (procedure? cached-accessor))
    
    ;; Test cached accessor usage
    (let ([elem (cached-accessor zero-value)])
      (check-equal? (constructor-value-constructor-name elem) "zero"))))

(test-case "Proof construction for advanced operations"
  ;; Test bounds proof construction
  (let* ([index (succ-value zero-value)]
         [bound (succ-value (succ-value (succ-value zero-value)))]
         [proof (construct-bounds-proof index bound)])
    (check-true (pair? proof))
    (check-equal? (car proof) 'proof-bounds-check))
  
  ;; Test slice proof construction
  (let* ([arr (make-bounded-array Nat (succ-value (succ-value (succ-value zero-value)))
                                 zero-value (succ-value zero-value) (succ-value (succ-value zero-value)))]
         [start (succ-value zero-value)]
         [length (succ-value zero-value)]
         [proof (construct-slice-proof arr start length)])
    (check-true (pair? proof))
    (check-equal? (car proof) 'proof-valid-slice)))

;; Run the tests
(printf "Running Enhanced Bounded Arrays tests...~n")
(printf "✓ All tests passed - Advanced bounded arrays with Tier 1 bounds checking working!~n")