#lang racket/base

(require "src/lexer/lexer.rkt"
         "src/parser/parser.rkt"
         "src/evaluator/evaluator.rkt")

(printf "Testing without type checking...~n")

;; Initialize cache
(initialize-evaluator-cache)

;; Test evaluation without type checking (like the working example above)
(define (evaluate-string-no-typecheck input)
  (let* ([tokens (tokenize input)]
         [ast (parse tokens)])
    (evaluate ast (make-global-environment))))

(printf "Testing (zero? (zero)) without type checking: ~a~n" 
        (evaluate-string-no-typecheck "(zero? (zero))"))

(shutdown-evaluator-cache)