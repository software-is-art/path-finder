#lang racket/base

(require "../src/main.rkt"
         "../src/evaluator/values.rkt"
         rackunit)

;; Basic functionality tests
(test-case "version information"
  (check-equal? pathfinder-version "0.1.0"))

(test-case "main function exists"
  (check-true (procedure? main)))

(test-case "start-repl function exists"  
  (check-true (procedure? start-repl)))

(test-case "evaluate-string function exists"
  (check-true (procedure? evaluate-string)))

(test-case "evaluate-file function exists"
  (check-true (procedure? evaluate-file)))

(test-case "tokenize function exists"
  (check-true (procedure? tokenize)))

(test-case "parse function exists"
  (check-true (procedure? parse)))

;; Test that unimplemented features throw errors as expected  
(test-case "type checker not implemented"
  (check-exn exn:fail? (lambda () (evaluate-string "hello"))))

(test-case "evaluator works with HoTT values"
  (let ([result (evaluate-string "(+ 1 2)")])
    (check-true (constructor-value? result))
    (check-equal? (constructor-value-constructor-name result) "succ")))