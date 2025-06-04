#lang racket/base

(require "../src/main.rkt"
         "../src/evaluator/evaluator.rkt"
         "../src/evaluator/values.rkt"
         "../src/types/types.rkt"
         "../src/parser/ast.rkt"
         rackunit)

;; Tests for PathFinder LISP HoTT-based Evaluator

;; Helper to evaluate string and check result
(define (eval-string-to-nat str expected-num)
  (let ([result (evaluate-string str)])
    (and (nat-value? result)
         (= (nat-value->racket-number result) expected-num))))

(define (eval-string-to-bool str expected-bool)
  (let ([result (evaluate-string str)])
    (and (bool-value? result)
         (eq? (bool-value->racket-boolean result) expected-bool))))

;; Basic literal evaluation tests
(test-case "evaluate natural number literals"
  (check-true (eval-string-to-nat "0" 0))
  (check-true (eval-string-to-nat "1" 1))
  (check-true (eval-string-to-nat "42" 42)))

(test-case "evaluate boolean literals" 
  (check-true (eval-string-to-bool "#t" #t))
  (check-true (eval-string-to-bool "#f" #f)))

;; Arithmetic operations
(test-case "natural number addition"
  (check-true (eval-string-to-nat "(+ 1 2)" 3))
  (check-true (eval-string-to-nat "(+ 0 5)" 5))
  (check-true (eval-string-to-nat "(+ 10 20)" 30)))

(test-case "natural number multiplication"
  (check-true (eval-string-to-nat "(* 2 3)" 6))
  (check-true (eval-string-to-nat "(* 0 5)" 0))
  (check-true (eval-string-to-nat "(* 4 4)" 16)))

(test-case "natural number subtraction (with truncation)"
  (check-true (eval-string-to-nat "(- 5 3)" 2))
  (check-true (eval-string-to-nat "(- 10 4)" 6))
  (check-true (eval-string-to-nat "(- 3 5)" 0))) ; truncated to 0

;; Comparison operations
(test-case "natural number equality"
  (check-true (eval-string-to-bool "(= 5 5)" #t))
  (check-true (eval-string-to-bool "(= 3 7)" #f))
  (check-true (eval-string-to-bool "(= 0 0)" #t)))

(test-case "natural number less than"
  (check-true (eval-string-to-bool "(< 3 5)" #t))
  (check-true (eval-string-to-bool "(< 5 3)" #f))
  (check-true (eval-string-to-bool "(< 4 4)" #f)))

;; Variable definition and lookup
(test-case "variable definition and lookup"
  (let ([env (make-global-environment)])
    (evaluate (parse (tokenize "(define x 42)")) env)
    (let ([result (evaluate (parse (tokenize "x")) env)])
      (check-true (nat-value? result))
      (check-equal? (nat-value->racket-number result) 42))))

;; If expressions
(test-case "if with true condition"
  (check-true (eval-string-to-nat "(if #t 1 2)" 1)))

(test-case "if with false condition"  
  (check-true (eval-string-to-nat "(if #f 1 2)" 2)))

(test-case "if with computed condition"
  (check-true (eval-string-to-nat "(if (< 3 5) 100 200)" 100)))

;; Lambda expressions and function calls
(test-case "lambda creation and application"
  (let ([env (make-global-environment)])
    (evaluate (parse (tokenize "(define double (lambda (x) (* x 2)))")) env)
    (let ([result (evaluate (parse (tokenize "(double 7)")) env)])
      (check-true (nat-value? result))
      (check-equal? (nat-value->racket-number result) 14))))

(test-case "lambda with multiple parameters"
  (let ([env (make-global-environment)])
    (evaluate (parse (tokenize "(define add (lambda (x y) (+ x y)))")) env)
    (let ([result (evaluate (parse (tokenize "(add 8 5)")) env)])
      (check-true (nat-value? result))
      (check-equal? (nat-value->racket-number result) 13))))

;; Value type checking
(test-case "values have correct HoTT types"
  (let ([nat-val (racket-number->nat-value 5)]
        [bool-val (racket-boolean->bool-value #t)])
    (check-true (value-has-type? nat-val Nat))
    (check-true (value-has-type? bool-val Bool))
    (check-false (value-has-type? nat-val Bool))
    (check-false (value-has-type? bool-val Nat))))

;; Pretty printing for HoTT values
(test-case "value pretty printing"
  (check-equal? (value->string zero-value) "zero")
  (check-equal? (value->string (succ-value zero-value)) "(succ zero)")
  (check-equal? (value->string true-value) "true")
  (check-equal? (value->string false-value) "false"))