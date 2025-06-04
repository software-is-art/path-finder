#lang racket/base

(require "../src/main.rkt"
         "../src/evaluator/evaluator.rkt"
         "../src/parser/ast.rkt"
         rackunit)

;; Basic literal evaluation tests
(test-case "evaluate number literal"
  (check-equal? (evaluate-string "42") 42))

(test-case "evaluate boolean true"
  (check-equal? (evaluate-string "#t") #t))

(test-case "evaluate boolean false"
  (check-equal? (evaluate-string "#f") #f))

(test-case "evaluate string literal"
  (check-equal? (evaluate-string "\"hello\"") "hello"))

;; Arithmetic operations
(test-case "addition"
  (check-equal? (evaluate-string "(+ 1 2)") 3))

(test-case "subtraction"
  (check-equal? (evaluate-string "(- 5 3)") 2))

(test-case "multiplication"
  (check-equal? (evaluate-string "(* 4 3)") 12))

(test-case "division"
  (check-equal? (evaluate-string "(/ 10 2)") 5))

(test-case "nested arithmetic"
  (check-equal? (evaluate-string "(+ (* 2 3) (- 10 5))") 11))

;; Comparison operations
(test-case "equality"
  (check-equal? (evaluate-string "(= 5 5)") #t)
  (check-equal? (evaluate-string "(= 5 3)") #f))

(test-case "less than"
  (check-equal? (evaluate-string "(< 3 5)") #t)
  (check-equal? (evaluate-string "(< 5 3)") #f))

(test-case "greater than"
  (check-equal? (evaluate-string "(> 5 3)") #t)
  (check-equal? (evaluate-string "(> 3 5)") #f))

;; Variable definition and lookup
(test-case "define and lookup variable"
  (let ([env (make-global-environment)])
    (check-equal? (evaluate (parse (tokenize "(define x 42)")) env) 42)
    (check-equal? (evaluate (parse (tokenize "x")) env) 42)))

(test-case "define with expression"
  (let ([env (make-global-environment)])
    (evaluate (parse (tokenize "(define y (+ 3 4))")) env)
    (check-equal? (evaluate (parse (tokenize "y")) env) 7)))

;; If expressions
(test-case "if true condition"
  (check-equal? (evaluate-string "(if #t 1 2)") 1))

(test-case "if false condition"
  (check-equal? (evaluate-string "(if #f 1 2)") 2))

(test-case "if with expression condition"
  (check-equal? (evaluate-string "(if (< 3 5) 100 200)") 100))

;; Lambda expressions and function calls
(test-case "lambda creation and call"
  (let ([env (make-global-environment)])
    (evaluate (parse (tokenize "(define square (lambda (x) (* x x)))")) env)
    (check-equal? (evaluate (parse (tokenize "(square 4)")) env) 16)))

(test-case "lambda with multiple parameters"
  (let ([env (make-global-environment)])
    (evaluate (parse (tokenize "(define add (lambda (x y) (+ x y)))")) env)
    (check-equal? (evaluate (parse (tokenize "(add 3 7)")) env) 10)))

(test-case "closure captures environment"
  (let ([env (make-global-environment)])
    (evaluate (parse (tokenize "(define x 10)")) env)
    (evaluate (parse (tokenize "(define get-x (lambda () x))")) env)
    (check-equal? (evaluate (parse (tokenize "(get-x)")) env) 10)))

;; Error handling tests
(test-case "undefined variable error"
  (check-exn exn:fail? (lambda () (evaluate-string "undefined-var"))))

(test-case "wrong number of arguments"
  (let ([env (make-global-environment)])
    (evaluate (parse (tokenize "(define f (lambda (x) x))")) env)
    (check-exn exn:fail? (lambda () (evaluate (parse (tokenize "(f 1 2)")) env)))))

(test-case "call non-function"
  (let ([env (make-global-environment)])
    (evaluate (parse (tokenize "(define x 42)")) env)
    (check-exn exn:fail? (lambda () (evaluate (parse (tokenize "(x 1)")) env)))))

;; Complex integration tests
(test-case "factorial function"
  (let ([env (make-global-environment)])
    (evaluate (parse (tokenize "(define factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1))))))")) env)
    (check-equal? (evaluate (parse (tokenize "(factorial 5)")) env) 120)))

(test-case "nested function definitions"
  (let ([env (make-global-environment)])
    (evaluate (parse (tokenize "(define outer (lambda (x) (define inner (lambda (y) (+ x y))) (inner 5)))")) env)
    (check-equal? (evaluate (parse (tokenize "(outer 3)")) env) 8)))