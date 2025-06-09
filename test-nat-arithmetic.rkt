#lang racket/base

(require "src/lexer/lexer.rkt"
         "src/parser/parser.rkt"
         "src/evaluator/evaluator.rkt")

(printf "Testing Nat arithmetic operations...~n")

;; Initialize cache
(initialize-evaluator-cache)

;; Test evaluation without type checking (since we know that's the issue)
(define (evaluate-string-no-typecheck input)
  (let* ([tokens (tokenize input)]
         [ast (parse tokens)])
    (evaluate ast (make-global-environment))))

;; Test addition
(printf "Testing addition...~n")
(printf "(+ 2 3): ~a~n" (evaluate-string-no-typecheck "(+ 2 3)"))
(printf "(+ 0 5): ~a~n" (evaluate-string-no-typecheck "(+ 0 5)"))
(printf "(+ 7 0): ~a~n" (evaluate-string-no-typecheck "(+ 7 0)"))

;; Test subtraction (natural number truncation)
(printf "~nTesting subtraction...~n") 
(printf "(- 5 3): ~a~n" (evaluate-string-no-typecheck "(- 5 3)"))
(printf "(- 7 7): ~a~n" (evaluate-string-no-typecheck "(- 7 7)"))
(printf "(- 3 5): ~a~n" (evaluate-string-no-typecheck "(- 3 5)")) ; Should be 0

;; Test using constructor syntax
(printf "~nTesting with constructors...~n")
(printf "(+ (next (zero)) (next (next (zero)))): ~a~n" 
        (evaluate-string-no-typecheck "(+ (next (zero)) (next (next (zero))))"))

(shutdown-evaluator-cache)