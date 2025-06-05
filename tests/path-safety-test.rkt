#lang racket/base

(require rackunit
         racket/list
         "../src/main.rkt"
         "../src/evaluator/values.rkt"
         "../src/types/types.rkt")

;; Test suite for Path-Based Safety Infrastructure

(define (test-nat-expr expr expected-num)
  "Helper to test that an expression evaluates to the expected natural number"
  (let ([result (evaluate-string expr)])
    (and (constructor-value? result)
         (nat-value? result)
         (= (nat-value->racket-number result) expected-num))))

(test-case "Proof construction tactics"
  
  ;; Test non-zero proof construction
  (let ([proof (try-prove-non-zero 5)])
    (check-true (non-zero-proof? proof)
                "Should construct proof that 5 ≠ 0"))
  
  (let ([proof (try-prove-non-zero 0)])
    (check-false proof
                 "Should not construct proof that 0 ≠ 0"))
  
  ;; Test less-than proof construction
  (let ([proof (try-prove-nat-lt 3 7)])
    (check-true (lt-proof? proof)
                "Should construct proof that 3 < 7"))
  
  (let ([proof (try-prove-nat-lt 7 3)])
    (check-false proof
                 "Should not construct proof that 7 < 3"))
  
  ;; Test bounds proof construction
  (let ([proof (try-prove-bounds 2 5)])
    (check-true (bounds-proof? proof)
                "Should construct proof that 2 is in bounds [0, 5)"))
  
  (let ([proof (try-prove-bounds 5 5)])
    (check-false proof
                 "Should not construct proof that 5 is in bounds [0, 5)")))

(test-case "Auto-safe division with proof construction"
  
  ;; Test successful safe division
  (check-true (test-nat-expr "(auto-safe-divide 10 2)" 5)
              "10 ÷ 2 should be 5")
  
  (check-true (test-nat-expr "(auto-safe-divide 15 3)" 5)
              "15 ÷ 3 should be 5")
  
  (check-true (test-nat-expr "(auto-safe-divide 20 4)" 5)
              "20 ÷ 4 should be 5")
  
  (check-true (test-nat-expr "(auto-safe-divide 7 1)" 7)
              "7 ÷ 1 should be 7")
  
  ;; Test division by zero prevention
  (check-exn exn:fail?
             (lambda () (evaluate-string "(auto-safe-divide 10 0)"))
             "Division by zero should be prevented by proof system")
  
  (check-exn exn:fail?
             (lambda () (evaluate-string "(auto-safe-divide 5 0)"))
             "Another division by zero should be prevented"))

(test-case "Path-based safety concepts"
  
  ;; Test that proof construction works for various cases
  (check-true (non-zero-proof? (try-prove-non-zero 1))
              "Should prove 1 ≠ 0")
  (check-true (non-zero-proof? (try-prove-non-zero 100))
              "Should prove 100 ≠ 0")
  (check-false (try-prove-non-zero 0)
               "Should not prove 0 ≠ 0")
  
  ;; Test that less-than proofs work correctly
  (check-true (lt-proof? (try-prove-nat-lt 0 1))
              "Should prove 0 < 1")
  (check-true (lt-proof? (try-prove-nat-lt 5 10))
              "Should prove 5 < 10")
  (check-false (try-prove-nat-lt 10 5)
               "Should not prove 10 < 5")
  (check-false (try-prove-nat-lt 5 5)
               "Should not prove 5 < 5"))

(test-case "Integration with HoTT foundations"
  
  ;; Test that path-based proofs integrate with existing HoTT infrastructure
  (let ([refl-path (make-refl Nat 5)])
    (check-true (path-value? refl-path)
                "Should create reflexivity path"))
  
  ;; Test proof construction with computed values
  (check-true (test-nat-expr "(auto-safe-divide (+ 8 2) (+ 1 1))" 5)
              "Safe division should work with computed divisors: (8+2) ÷ (1+1) = 5")
  
  (check-true (test-nat-expr "(auto-safe-divide (* 3 4) (+ 2 1))" 4)
              "Safe division should work with complex expressions: (3*4) ÷ (2+1) = 4"))

(test-case "Error messages and debugging"
  
  ;; Test that error messages are clear
  (let ([error-message 
         (with-handlers ([exn:fail? exn-message])
           (evaluate-string "(auto-safe-divide 7 0)")
           "no error")])
    (check-true (regexp-match? #rx"Cannot prove" error-message)
                "Error message should mention proof failure"))
  
  ;; Test edge cases
  (check-true (test-nat-expr "(auto-safe-divide 0 1)" 0)
              "0 ÷ 1 should be 0")
  
  (check-true (test-nat-expr "(auto-safe-divide 1 1)" 1)
              "1 ÷ 1 should be 1"))

;; Run the tests
(printf "Running Path-Based Safety Infrastructure tests...~n")