#!/usr/bin/env guile
!#
;;; Basic tests for PathFinder bootstrap

(use-modules (ice-9 regex))
(define (find-project-root)
  (let loop ((dir (dirname (current-filename))))
    (if (file-exists? (string-append dir "/guile-bootstrap"))
        dir
        (loop (dirname dir)))))
(define project-root (find-project-root))
(add-to-load-path project-root)
(load (string-append project-root "/test/bootstrap/test-framework.scm"))

(define (run-tests)
  ;; Test natural numbers
  (test-file "Natural Numbers" 
    (lambda ()
      (let ((output (run-pathfinder-code "
(define my-zero zero)
(define one (succ zero))
(define two (succ one))
(perform (print \"done\"))
")))
        (test-true? "Creates zero" (string-contains output "Defined: my-zero"))
        (test-true? "Creates successor" (string-contains output "Defined: one"))
        (test-true? "Multiple successors" (string-contains output "Defined: two")))))
  
  ;; Test arithmetic with nat-elim
  (test-file "Arithmetic"
    (lambda ()
      (let ((output (run-pathfinder-code "
(define one (succ zero))
(define two (succ one))
(define add
  (fn (x y)
    (nat-elim (fn (_) Nat)
              x
              (fn (n rec) (succ rec))
              y)))
(define three (add one two))
(perform (print \"done\"))
")))
        (test-true? "Defines addition" (string-contains output "Defined: add"))
        (test-true? "Computes 1+2" (string-contains output "Defined: three")))))
  
  ;; Test closures
  (test-file "Closures"
    (lambda ()
      (let ((output (run-pathfinder-code "
(define make-adder
  (fn (x)
    (fn (y)
      (nat-elim (fn (_) Nat) x (fn (n rec) (succ rec)) y))))
(define add-two (make-adder (succ (succ zero))))
(perform (print \"done\"))
")))
        (test-true? "Creates closure" (string-contains output "Defined: make-adder"))
        (test-true? "Applies partial function" (string-contains output "Defined: add-two")))))
  
  ;; Test effects
  (test-file "Effects"
    (lambda ()
      (let ((output (run-pathfinder-code "(perform (print \"Hello PathFinder!\"))")))
        (test-true? "Print effect works" (string-contains output "Hello PathFinder!")))))
  
  ;; Test booleans
  (test-file "Booleans"
    (lambda ()
      (let ((output (run-pathfinder-code "
(define my-true true)
(define my-false false)
(define not
  (fn (b)
    (bool-elim (fn (_) Bool) true false b)))
(perform (print \"done\"))
")))
        (test-true? "Boolean true" (string-contains output "Defined: my-true"))
        (test-true? "Boolean false" (string-contains output "Defined: my-false"))
        (test-true? "Boolean elimination" (string-contains output "Defined: not"))))))

;; Run all tests
(run-tests)
(test-summary)