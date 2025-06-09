#lang racket/base

(require "src/main.rkt"
         "src/evaluator/evaluator.rkt")

(printf "Testing Nat operations...~n")

;; Initialize the cache system first (like main does)
(initialize-evaluator-cache)

;; Test basic Nat operations
(printf "Testing (zero): ~a~n" (evaluate-string "(zero)"))
(printf "Testing (+ 2 3): ~a~n" (evaluate-string "(+ 2 3)"))
(printf "Testing (< 3 5): ~a~n" (evaluate-string "(< 3 5)"))

;; Test zero? function
(printf "Testing (zero? (zero)): ")
(with-handlers ([exn:fail? (lambda (e) (printf "ERROR: ~a~n" (exn-message e)))])
  (printf "~a~n" (evaluate-string "(zero? (zero))")))

;; Clean shutdown
(shutdown-evaluator-cache)