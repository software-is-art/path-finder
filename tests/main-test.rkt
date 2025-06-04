#lang racket/base

(require "../src/main.rkt")

;; PathFinder LISP Test Suite
;; Basic tests to verify the testing framework and core functionality
;; Using simple assertions since rackunit is not available in racket-minimal

(define test-count 0)
(define pass-count 0)

(define (test-assert name condition)
  (set! test-count (+ test-count 1))
  (if condition
      (begin
        (set! pass-count (+ pass-count 1))
        (printf "✓ ~a~n" name))
      (printf "✗ ~a~n" name)))

(define (test-equal name actual expected)
  (test-assert name (equal? actual expected)))

(define (test-procedure name proc)
  (test-assert name (procedure? proc)))

(define (test-exception name thunk)
  (set! test-count (+ test-count 1))
  (with-handlers ([exn:fail? (lambda (e) 
                              (set! pass-count (+ pass-count 1))
                              (printf "✓ ~a (expected exception)~n" name))]
                 [exn? (lambda (e) 
                        (printf "✗ ~a (unexpected exception type)~n" name))])
    (thunk)
    (printf "✗ ~a (no exception thrown)~n" name)))

(printf "Running PathFinder LISP Tests...~n~n")

;; Run tests
(test-equal "version information" pathfinder-version "0.1.0")
(test-procedure "main function exists" main)
(test-procedure "start-repl function exists" start-repl)
(test-procedure "evaluate-string function exists" evaluate-string)
(test-procedure "evaluate-file function exists" evaluate-file)

;; Test that unimplemented features throw errors as expected
(test-exception "placeholder: lexer not implemented"
                (lambda () (evaluate-string "(+ 1 2)")))
(test-exception "placeholder: parser not implemented"
                (lambda () (evaluate-string "42")))
(test-exception "placeholder: type checker not implemented"
                (lambda () (evaluate-string "hello")))
(test-exception "placeholder: evaluator not implemented"
                (lambda () (evaluate-string "(define x 1)")))

;; Report results
(printf "~n=== Test Results ===~n")
(printf "Passed: ~a/~a tests~n" pass-count test-count)
(if (= pass-count test-count)
    (printf "All tests passed! ✓~n")
    (printf "Some tests failed. ✗~n"))