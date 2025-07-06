#!/usr/bin/env guile
!#
;;; Test module loading for PathFinder bootstrap

(use-modules (ice-9 regex))
(define (find-project-root)
  (let loop ((dir (dirname (current-filename))))
    (if (file-exists? (string-append dir "/guile-bootstrap"))
        dir
        (loop (dirname dir)))))
(define project-root (find-project-root))
(add-to-load-path project-root)
(load (string-append project-root "/test/bootstrap/test-framework.scm"))

;; Create test modules
(define (setup-test-modules)
  ;; Simple module
  (call-with-output-file "test-simple.sexp"
    (lambda (port)
      (display "
(define helper-add
  (fn (x y)
    (nat-elim (fn (_) Nat) x (fn (n rec) (succ rec)) y)))

(export helper-add)
" port)))
  
  ;; Module with relative import
  (call-with-output-file "test-import-simple.sexp"
    (lambda (port)
      (display "
(import \"./test-simple.sexp\")

(define five (helper-add (succ (succ zero)) (succ (succ (succ zero)))))

(export five)
" port))))

(define (cleanup-test-modules)
  (when (file-exists? "test-simple.sexp")
    (delete-file "test-simple.sexp"))
  (when (file-exists? "test-import-simple.sexp")
    (delete-file "test-import-simple.sexp")))

(define (run-tests)
  ;; Test simple module import
  (test-file "Simple Module Import"
    (lambda ()
      (setup-test-modules)
      (let ((output (run-pathfinder-code "
(import \"./test-simple.sexp\")
(define two (succ (succ zero)))
(define three (helper-add two (succ zero)))
(perform (print \"done\"))
")))
        (test-true? "Imports module" (string-contains output "Importing: ./test-simple.sexp"))
        (test-true? "Uses imported function" (string-contains output "Defined: three"))
        (cleanup-test-modules))))
  
  ;; Test nested imports
  (test-file "Nested Module Import"
    (lambda ()
      (setup-test-modules)
      (let ((output (run-pathfinder-code "
(import \"./test-import-simple.sexp\")
(perform (print \"done\"))
")))
        (test-true? "Imports nested module" (string-contains output "Importing: ./test-simple.sexp"))
        (test-true? "Imports parent module" (string-contains output "Importing: ./test-import-simple.sexp"))
        (cleanup-test-modules))))
  
  ;; Test symbol name import
  (test-file "Symbol Name Import"
    (lambda ()
      (let ((output (run-pathfinder-code "
(import minimal-arithmetic)
(perform (print \"done\"))
")))
        ;; Should try to find in standard locations
        (test-true? "Attempts symbol import" 
                   (or (string-contains output "Importing:")
                       (string-contains output "Module not found: minimal-arithmetic"))))))
  
  ;; Test original format
  (test-file "Original Import Format"
    (lambda ()
      (let ((output (run-pathfinder-code "
(import (bootstrap simple-arithmetic))
(perform (print \"done\"))
")))
        (test-true? "Handles (category module) format" 
                   (string-contains output "Importing: bootstrap/simple-arithmetic"))))))

;; Run all tests
(run-tests)
(test-summary)