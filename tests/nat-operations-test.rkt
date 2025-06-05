#lang racket/base

(require rackunit
         "../src/main.rkt"
         "../src/evaluator/values.rkt")

;; Test suite for Nat type and natural number operations

(define (test-nat-expr expr expected-num)
  "Helper to test that an expression evaluates to the expected natural number"
  (let ([result (evaluate-string expr)])
    (and (constructor-value? result)
         (nat-value? result)
         (= (nat-value->racket-number result) expected-num))))

(define (test-bool-expr expr expected-bool)
  "Helper to test that an expression evaluates to the expected boolean value"
  (let ([result (evaluate-string expr)])
    (and (constructor-value? result)
         (bool-value? result)
         (eq? (bool-value->racket-boolean result) expected-bool))))

(test-case "Natural number constructors and basic operations"
  
  ;; Test constructors
  (check-true (test-nat-expr "(zero)" 0)
              "zero constructor should create 0")
  (check-true (test-nat-expr "(successor (zero))" 1)
              "successor of zero should be 1")
  (check-true (test-nat-expr "(successor (successor (zero)))" 2)
              "nested successors should work")
  
  ;; Test arithmetic operations
  (check-true (test-nat-expr "(+ 2 3)" 5)
              "2 + 3 should be 5")
  (check-true (test-nat-expr "(* 3 4)" 12)
              "3 * 4 should be 12")
  (check-true (test-nat-expr "(- 7 3)" 4)
              "7 - 3 should be 4")
  (check-true (test-nat-expr "(- 3 7)" 0)
              "3 - 7 should be 0 (natural subtraction)"))

(test-case "Natural number query operations"
  
  ;; Test is-zero? operation
  (check-true (test-bool-expr "(is-zero? (zero))" #t)
              "zero should be identified as zero")
  (check-true (test-bool-expr "(is-zero? 0)" #t)
              "literal 0 should be identified as zero")
  (check-true (test-bool-expr "(is-zero? 5)" #f)
              "5 should not be identified as zero")
  (check-true (test-bool-expr "(is-zero? (successor (zero)))" #f)
              "successor of zero should not be zero")
  
  ;; Test predecessor operation
  (check-true (test-nat-expr "(predecessor 5)" 4)
              "predecessor of 5 should be 4")
  (check-true (test-nat-expr "(predecessor 1)" 0)
              "predecessor of 1 should be 0")
  (check-true (test-nat-expr "(predecessor 0)" 0)
              "predecessor of 0 should be 0 (base case)")
  (check-true (test-nat-expr "(predecessor (successor (successor (zero))))" 1)
              "predecessor should work with constructor notation"))

(test-case "Natural number comparison operations"
  
  ;; Test equality
  (check-true (test-bool-expr "(= 5 5)" #t)
              "5 should equal 5")
  (check-true (test-bool-expr "(= 3 7)" #f)
              "3 should not equal 7")
  
  ;; Test less than
  (check-true (test-bool-expr "(< 3 7)" #t)
              "3 should be less than 7")
  (check-true (test-bool-expr "(< 7 3)" #f)
              "7 should not be less than 3")
  (check-true (test-bool-expr "(< 5 5)" #f)
              "5 should not be less than 5")
  
  ;; Test less than or equal
  (check-true (test-bool-expr "(<= 3 7)" #t)
              "3 should be less than or equal to 7")
  (check-true (test-bool-expr "(<= 5 5)" #t)
              "5 should be less than or equal to 5")
  (check-true (test-bool-expr "(<= 7 3)" #f)
              "7 should not be less than or equal to 3")
  
  ;; Test greater than
  (check-true (test-bool-expr "(> 7 3)" #t)
              "7 should be greater than 3")
  (check-true (test-bool-expr "(> 3 7)" #f)
              "3 should not be greater than 7")
  (check-true (test-bool-expr "(> 5 5)" #f)
              "5 should not be greater than 5")
  
  ;; Test greater than or equal
  (check-true (test-bool-expr "(>= 7 3)" #t)
              "7 should be greater than or equal to 3")
  (check-true (test-bool-expr "(>= 5 5)" #t)
              "5 should be greater than or equal to 5")
  (check-true (test-bool-expr "(>= 3 7)" #f)
              "3 should not be greater than or equal to 7"))

(test-case "Complex natural number expressions"
  
  ;; Test combining operations
  (check-true (test-bool-expr "(< (+ 2 3) (* 2 3))" #t)
              "2+3 should be less than 2*3")
  (check-true (test-bool-expr "(is-zero? (- 5 5))" #t)
              "5-5 should be zero")
  (check-true (test-nat-expr "(predecessor (+ 3 2))" 4)
              "predecessor of 3+2 should be 4")
  
  ;; Test with constructors
  (check-true (test-bool-expr "(= (successor (zero)) 1)" #t)
              "successor of zero should equal 1")
  (check-true (test-bool-expr "(> (successor (successor (zero))) (zero))" #t)
              "2 should be greater than 0"))

(test-case "Natural number pattern matching integration"
  
  ;; Test pattern matching with query results
  (let ([result (evaluate-string "(match (is-zero? 0) ((true) 42) ((false) 0))")])
    (check-equal? (nat-value->racket-number result) 42
                  "Pattern matching on is-zero? result should work"))
  
  (let ([result (evaluate-string "(match (< 3 7) ((true) 1) ((false) 0))")])
    (check-equal? (nat-value->racket-number result) 1
                  "Pattern matching on comparison result should work"))
  
  (let ([result (evaluate-string "(match (predecessor 5) ((zero) 0) (n n))")])
    (check-equal? (nat-value->racket-number result) 4
                  "Pattern matching on predecessor result should work")))

;; Run the tests
(printf "Running Nat operations tests...~n")