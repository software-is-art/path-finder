#lang racket/base

(require rackunit
         "../src/main.rkt"
         "../src/evaluator/values.rkt")

;; Test suite for Bool type and boolean logical operations

(define (test-bool-expr expr expected-bool)
  "Helper to test that an expression evaluates to the expected boolean value"
  (let ([result (evaluate-string expr)])
    (and (constructor-value? result)
         (bool-value? result)
         (eq? (bool-value->racket-boolean result) expected-bool))))

(test-case "Boolean logical operations"
  
  ;; Test 'and' operation
  (check-true (test-bool-expr "(and (true) (true))" #t)
              "true AND true should be true")
  (check-true (test-bool-expr "(and (true) (false))" #f)
              "true AND false should be false")
  (check-true (test-bool-expr "(and (false) (true))" #f)
              "false AND true should be false")
  (check-true (test-bool-expr "(and (false) (false))" #f)
              "false AND false should be false")
  
  ;; Test 'or' operation
  (check-true (test-bool-expr "(or (true) (true))" #t)
              "true OR true should be true")
  (check-true (test-bool-expr "(or (true) (false))" #t)
              "true OR false should be true")
  (check-true (test-bool-expr "(or (false) (true))" #t)
              "false OR true should be true")
  (check-true (test-bool-expr "(or (false) (false))" #f)
              "false OR false should be false")
  
  ;; Test 'not' operation
  (check-true (test-bool-expr "(not (true))" #f)
              "NOT true should be false")
  (check-true (test-bool-expr "(not (false))" #t)
              "NOT false should be true")
  
  ;; Test nested boolean operations
  (check-true (test-bool-expr "(and (or (true) (false)) (not (false)))" #t)
              "Complex expression: (true OR false) AND (NOT false) should be true")
  (check-true (test-bool-expr "(or (and (true) (false)) (not (true)))" #f)
              "Complex expression: (true AND false) OR (NOT true) should be false")
  
  ;; Test boolean operations with boolean literals
  (check-true (test-bool-expr "(and #t #t)" #t)
              "Boolean literals: #t AND #t should be true")
  (check-true (test-bool-expr "(or #f #t)" #t)
              "Boolean literals: #f OR #t should be true")
  (check-true (test-bool-expr "(not #f)" #t)
              "Boolean literals: NOT #f should be true"))

(test-case "Boolean pattern matching with logical operations"
  
  ;; Test pattern matching with boolean results from logical operations
  (let ([result (evaluate-string "(match (and (true) (false)) ((true) 1) ((false) 0))")])
    (check-equal? (nat-value->racket-number result) 0
                  "Pattern matching on AND result should work"))
  
  (let ([result (evaluate-string "(match (or (false) (true)) ((true) 42) ((false) 0))")])
    (check-equal? (nat-value->racket-number result) 42
                  "Pattern matching on OR result should work"))
  
  (let ([result (evaluate-string "(match (not (false)) ((true) 1) ((false) 0))")])
    (check-equal? (nat-value->racket-number result) 1
                  "Pattern matching on NOT result should work")))

;; Run the tests
(printf "Running Bool operations tests...~n")