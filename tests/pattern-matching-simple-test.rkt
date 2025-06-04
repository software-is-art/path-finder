#lang racket/base

(require "../src/lexer/lexer.rkt"
         "../src/parser/parser.rkt" 
         "../src/parser/ast.rkt"
         "../src/evaluator/evaluator.rkt"
         "../src/evaluator/values.rkt"
         rackunit)

;; Simple tests for pattern matching without type checking

(test-case "simple pattern matching works"
  (let* ([input "(match 0 ((zero) 999) (_ 111))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [result (evaluate ast)])
    (check-true (nat-value? result))
    (check-equal? (nat-value->racket-number result) 999)))

(test-case "pattern matching with successor"
  (let* ([input "(match 3 ((zero) 0) ((successor n) (+ n 1)))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [result (evaluate ast)])
    (check-true (nat-value? result))
    (check-equal? (nat-value->racket-number result) 3)))

(printf "All pattern matching tests passed!\n")