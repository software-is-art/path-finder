#lang racket/base

(require "src/main.rkt"
         "src/evaluator/evaluator.rkt")

(printf "Testing evaluation pathway...~n")

;; Initialize cache
(initialize-evaluator-cache)

;; Test if the issue is in the evaluation pathway
(printf "Creating environment and testing step by step...~n")

(let ([env (make-global-environment)])
  ;; Test if zero? is accessible
  (printf "zero? lookup: ~a~n" (env-lookup env "zero?"))
  
  ;; Test parsing and evaluation steps
  (printf "~nTesting parsing...~n")
  (let* ([tokens (tokenize "(zero? (zero))")]
         [ast (parse tokens)])
    (printf "Tokens: ~a~n" tokens)
    (printf "AST: ~a~n" ast)
    
    ;; Test evaluation
    (printf "~nTesting evaluation...~n")
    (with-handlers ([exn:fail? (lambda (e) (printf "Evaluation error: ~a~n" (exn-message e)))])
      (let ([result (evaluate ast env)])
        (printf "Result: ~a~n" result)))))

(shutdown-evaluator-cache)