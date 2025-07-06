#!/usr/bin/env guile
!#
;;; Main test runner for PathFinder bootstrap tests

(use-modules (ice-9 format))

(define (run-test-file filename)
  "Run a test file and report results"
  (format #t "~%========================================~%")
  (format #t "Running ~a~%" filename)
  (format #t "========================================~%")
  
  (let ((result (system* "guile" filename)))
    (if (= result 0)
        (format #t "~%✓ ~a passed~%" filename)
        (format #t "~%✗ ~a failed (exit code: ~a)~%" filename result))
    result))

(define (main)
  (format #t "PathFinder Bootstrap Test Suite~%")
  (format #t "===============================~%")
  
  (let ((test-files '("test/bootstrap/test-basic.scm"
                      "test/bootstrap/test-modules.scm"
                      "test/bootstrap/test-recursion.scm"))
        (failed 0))
    
    ;; Run each test file
    (for-each
      (lambda (file)
        (let ((result (run-test-file file)))
          (when (not (= result 0))
            (set! failed (+ failed 1)))))
      test-files)
    
    ;; Summary
    (format #t "~%~%========================================~%")
    (format #t "OVERALL SUMMARY~%")
    (format #t "========================================~%")
    (format #t "Total test files: ~a~%" (length test-files))
    (format #t "Failed: ~a~%" failed)
    
    (if (= failed 0)
        (begin
          (format #t "~%All tests passed! ✓~%")
          (exit 0))
        (begin
          (format #t "~%Some tests failed! ✗~%")
          (exit 1)))))

(main)