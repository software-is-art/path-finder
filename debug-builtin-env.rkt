#lang racket/base

(require "src/evaluator/evaluator.rkt"
         "src/evaluator/values.rkt")

(printf "Debugging builtin environment...~n")

;; Initialize cache
(initialize-evaluator-cache)

;; Create a global environment and inspect it
(let ([env (make-global-environment)])
  (printf "Checking if zero? is defined: ~a~n" (env-lookup env "zero?"))
  (printf "Checking if zero is defined: ~a~n" (env-lookup env "zero"))
  (printf "Checking if + is defined: ~a~n" (env-lookup env "+"))
  (printf "Checking if and is defined: ~a~n" (env-lookup env "and")))

(shutdown-evaluator-cache)