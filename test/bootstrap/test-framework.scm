#!/usr/bin/env guile
!#
;;; Simple test framework for PathFinder bootstrap

(use-modules (ice-9 format))

;; Test state
(define tests-run 0)
(define tests-passed 0)
(define tests-failed 0)
(define current-test-file "")

;; Test assertion functions
(define (test-equal? name expected actual)
  "Test that actual equals expected"
  (set! tests-run (+ tests-run 1))
  (if (equal? expected actual)
      (begin
        (set! tests-passed (+ tests-passed 1))
        (format #t "  ✓ ~a~%" name))
      (begin
        (set! tests-failed (+ tests-failed 1))
        (format #t "  ✗ ~a~%" name)
        (format #t "    Expected: ~a~%" expected)
        (format #t "    Actual:   ~a~%" actual))))

(define (test-true? name condition)
  "Test that condition is true"
  (test-equal? name #t condition))

(define (test-false? name condition)
  "Test that condition is false"
  (test-equal? name #f condition))

(define (test-error? name thunk)
  "Test that thunk throws an error"
  (set! tests-run (+ tests-run 1))
  (catch #t
    (lambda ()
      (thunk)
      (set! tests-failed (+ tests-failed 1))
      (format #t "  ✗ ~a~%" name)
      (format #t "    Expected error but none thrown~%"))
    (lambda (key . args)
      (set! tests-passed (+ tests-passed 1))
      (format #t "  ✓ ~a (error: ~a)~%" name key))))

;; Test runner functions
(define (test-file file-path test-proc)
  "Run tests from a file"
  (set! current-test-file file-path)
  (format #t "~%Running tests from ~a:~%" file-path)
  (test-proc))

(define (test-summary)
  "Print test summary"
  (format #t "~%Test Summary:~%")
  (format #t "============~%")
  (format #t "Total:  ~a~%" tests-run)
  (format #t "Passed: ~a~%" tests-passed)
  (format #t "Failed: ~a~%" tests-failed)
  (format #t "~%")
  (if (= tests-failed 0)
      (format #t "All tests passed! ✓~%")
      (format #t "Some tests failed! ✗~%"))
  (= tests-failed 0))

;; Helper to run PathFinder code and capture output
(define (run-pathfinder-code code)
  "Run PathFinder code and return result"
  (let ((temp-file (tmpnam)))
    (call-with-output-file temp-file
      (lambda (port)
        (display code port)))
    
    (let ((output (with-output-to-string
                    (lambda ()
                      (system* "guile" 
                               "guile-bootstrap/pathfinder-bootstrap.scm" 
                               temp-file)))))
      (delete-file temp-file)
      output)))

;; No need to export when loading directly