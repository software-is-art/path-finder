#lang racket/base

(require "src/main.rkt"
         "src/evaluator/evaluator.rkt"
         "src/evaluator/values.rkt"
         "src/types/types.rkt")

(printf "Debugging zero? function call...~n")

;; Initialize cache
(initialize-evaluator-cache)

;; Test basic functionality that we know works
(printf "Testing basic operations...~n")
(printf "(zero): ~a~n" (evaluate-string "(zero)"))
(printf "(+ 2 3): ~a~n" (evaluate-string "(+ 2 3)"))

;; Test zero? in different ways
(printf "~nTesting zero? function...~n")

;; Method 1: Direct function call in REPL-style environment
(let ([env (make-global-environment)])
  (printf "zero? in environment: ~a~n" (env-lookup env "zero?"))
  
  ;; Create zero value manually and test nat-is-zero? directly
  (let* ([zero-val (constructor-value "zero" '() Nat)]
         [result (nat-is-zero? zero-val)])
    (printf "Direct nat-is-zero? call: ~a~n" result))
  
  ;; Try evaluating with the persistent environment
  (let ([zero-ast (parse (tokenize "(zero)"))]
        [zero-check-ast (parse (tokenize "(zero? (zero))"))])
    (printf "Evaluating (zero) in env: ~a~n" (evaluate zero-ast env))
    (with-handlers ([exn:fail? (lambda (e) (printf "Error evaluating (zero? (zero)): ~a~n" (exn-message e)))])
      (printf "Evaluating (zero? (zero)) in env: ~a~n" (evaluate zero-check-ast env)))))

(shutdown-evaluator-cache)