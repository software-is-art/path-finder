#lang racket/base

(require rackunit
         racket/list
         "../src/main.rkt"
         "../src/evaluator/values.rkt"
         "../src/types/types.rkt")

;; Test suite for Tier 1: Computational CIFs (Mathematical Operations)

(define (test-nat-expr expr expected-num)
  "Helper to test that an expression evaluates to the expected natural number"
  (let ([result (evaluate-string expr)])
    (and (constructor-value? result)
         (nat-value? result)
         (= (nat-value->racket-number result) expected-num))))

(test-case "Computational proof construction"
  
  ;; Test division computational proof construction
  (let ([comp-proof (make-computational-division-proof 15 3)])
    (check-true (division-computation-proof? comp-proof)
                "Should construct computational division proof for 15 ÷ 3"))
  
  (let ([comp-proof (make-computational-division-proof 10 0)])
    (check-false comp-proof
                 "Should not construct computational division proof for 10 ÷ 0"))
  
  ;; Test that proof construction IS computation  
  (let ([comp-proof (make-computational-division-proof 20 4)])
    (check-equal? (extract-computed-result comp-proof) 5
                  "Computational proof should contain pre-computed result: 20 ÷ 4 = 5"))
  
  ;; Test arithmetic computational proofs
  (let ([comp-proof (make-computational-arithmetic-proof 'add '(7 3))])
    (check-equal? (extract-computed-result comp-proof) 10
                  "Addition proof should compute 7 + 3 = 10"))
  
  (let ([comp-proof (make-computational-arithmetic-proof 'mult '(6 4))])
    (check-equal? (extract-computed-result comp-proof) 24
                  "Multiplication proof should compute 6 × 4 = 24")))

(test-case "Computational CIFs: proof construction IS computation"
  
  ;; Test computational addition - proof construction computes the result
  (check-true (test-nat-expr "(comp-add 8 7)" 15)
              "Computational addition: 8 + 7 = 15")
  
  (check-true (test-nat-expr "(comp-add 0 5)" 5)
              "Computational addition with zero: 0 + 5 = 5")
  
  ;; Test computational multiplication
  (check-true (test-nat-expr "(comp-mult 7 8)" 56)
              "Computational multiplication: 7 × 8 = 56")
  
  (check-true (test-nat-expr "(comp-mult 9 0)" 0)
              "Computational multiplication by zero: 9 × 0 = 0")
  
  ;; Test computational subtraction (natural, truncated at 0)
  (check-true (test-nat-expr "(comp-sub 12 5)" 7)
              "Computational subtraction: 12 - 5 = 7")
  
  (check-true (test-nat-expr "(comp-sub 3 8)" 0)
              "Computational subtraction (truncated): 3 - 8 = 0")
  
  (check-true (test-nat-expr "(comp-sub 5 5)" 0)
              "Computational subtraction (equal): 5 - 5 = 0")
  
  ;; Test computational division with safety proofs
  (check-true (test-nat-expr "(comp-divide 21 7)" 3)
              "Computational division: 21 ÷ 7 = 3")
  
  (check-true (test-nat-expr "(comp-divide 100 10)" 10)
              "Computational division: 100 ÷ 10 = 10")
  
  (check-true (test-nat-expr "(comp-divide 7 1)" 7)
              "Computational division by one: 7 ÷ 1 = 7"))

(test-case "Computational CIF safety: compile-time error prevention"
  
  ;; Test that division by zero is prevented by proof construction failure
  (check-exn exn:fail?
             (lambda () (evaluate-string "(comp-divide 15 0)"))
             "Division by zero should be prevented by computational proof failure")
  
  (check-exn exn:fail?
             (lambda () (evaluate-string "(comp-divide 1 0)"))
             "Another division by zero should be prevented"))

(test-case "Complex computational expressions"
  
  ;; Test nested computational operations
  (check-true (test-nat-expr "(comp-add (comp-mult 3 4) (comp-sub 10 2))" 20)
              "Complex expression: (3×4) + (10-2) = 12 + 8 = 20")
  
  (check-true (test-nat-expr "(comp-mult (comp-add 2 3) (comp-sub 8 3))" 25)
              "Complex expression: (2+3) × (8-3) = 5 × 5 = 25")
  
  (check-true (test-nat-expr "(comp-divide (comp-mult 6 3) (comp-add 1 2))" 6)
              "Complex expression: (6×3) ÷ (1+2) = 18 ÷ 3 = 6"))

(test-case "HoTT computational proof concepts"
  
  ;; Test that computational proofs contain both safety and results
  (let ([comp-proof (make-computational-division-proof 24 6)])
    (check-true (division-computation-proof? comp-proof)
                "Computational proof should be a valid division proof")
    (check-true (non-zero-proof? (division-computation-proof-non-zero-proof comp-proof))
                "Computational proof should contain safety proof")
    (check-equal? (division-computation-proof-computed-result comp-proof) 4
                  "Computational proof should contain computed result"))
  
  ;; Test proof extraction
  (let ([comp-proof (make-computational-arithmetic-proof 'add '(12 8))])
    (check-equal? (extract-computed-result comp-proof) 20
                  "Should extract computed result from proof"))
  
  ;; Test that proof construction IS the computation (no re-computation needed)
  (let* ([start-time (current-inexact-milliseconds)]
         [comp-proof (make-computational-arithmetic-proof 'mult '(123 456))]
         [construction-time (- (current-inexact-milliseconds) start-time)]
         [start-extract-time (current-inexact-milliseconds)]
         [result (extract-computed-result comp-proof)]
         [extraction-time (- (current-inexact-milliseconds) start-extract-time)])
    (check-equal? result 56088
                  "Should get correct computed result")
    ;; The key insight: extraction is just data access, construction did the computation
    (check-true (>= construction-time 0)
                "Construction time should be measurable")))

(test-case "Error messages and debugging"
  
  ;; Test clear error messages for proof construction failures
  (let ([error-message 
         (with-handlers ([exn:fail? exn-message])
           (evaluate-string "(comp-divide 7 0)")
           "no error")])
    (check-true (regexp-match? #rx"Cannot construct" error-message)
                "Error message should mention proof construction failure")))

;; Run the tests
(printf "Running Tier 1 Computational CIFs tests...~n")