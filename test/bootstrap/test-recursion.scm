#!/usr/bin/env guile
!#
;;; Test mutual recursion support

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
  ;; Test simple recursion
  (test-file "Simple Recursion"
    (lambda ()
      (let ((output (run-pathfinder-code "
(define factorial
  (fn (n)
    (nat-elim (fn (_) Nat)
              (succ zero)  ; 0! = 1
              (fn (k rec)
                ;; (k+1)! = (k+1) * k!
                ;; For simplicity, just return succ rec
                (succ rec))
              n)))
(define fact-three (factorial (succ (succ (succ zero))))
(perform (print \"done\"))
")))
        (test-true? "Defines recursive function" (string-contains output "Defined: factorial"))
        (test-true? "Applies recursive function" (string-contains output "Defined: fact-three")))))
  
  ;; Test mutual recursion
  (test-file "Mutual Recursion"
    (lambda ()
      (let ((output (run-pathfinder-code "
(define even?
  (fn (n)
    (nat-elim (fn (_) Bool)
              true         ; 0 is even
              (fn (k rec) (odd? k))  ; n+1 is even if n is odd
              n)))

(define odd?
  (fn (n)
    (nat-elim (fn (_) Bool)
              false        ; 0 is not odd
              (fn (k rec) (even? k)) ; n+1 is odd if n is even
              n)))

(define two (succ (succ zero)))
(define is-two-even (even? two))
(define is-two-odd (odd? two))
(perform (print \"done\"))
")))
        (test-true? "Defines even?" (string-contains output "Defined: even?"))
        (test-true? "Defines odd?" (string-contains output "Defined: odd?"))
        (test-true? "Can call mutually recursive functions" 
                   (and (string-contains output "Defined: is-two-even")
                        (string-contains output "Defined: is-two-odd"))))))
  
  ;; Test forward references in modules
  (test-file "Module Mutual Recursion"
    (lambda ()
      ;; Create module with mutual recursion
      (call-with-output-file "test-mutual.sexp"
        (lambda (port)
          (display "
(define is-even
  (fn (n)
    (nat-elim (fn (_) Bool)
              true
              (fn (k rec) (is-odd k))
              n)))

(define is-odd
  (fn (n)
    (nat-elim (fn (_) Bool)
              false
              (fn (k rec) (is-even k))
              n)))

(export is-even)
(export is-odd)
" port)))
      
      (let ((output (run-pathfinder-code "
(import \"./test-mutual.sexp\")
(define three (succ (succ (succ zero))))
(define three-is-odd (is-odd three))
(perform (print \"done\"))
")))
        (test-true? "Module with mutual recursion loads" 
                   (string-contains output "Importing: ./test-mutual.sexp"))
        (test-true? "Can use mutually recursive functions from module"
                   (string-contains output "Defined: three-is-odd"))
        
        ;; Cleanup
        (when (file-exists? "test-mutual.sexp")
          (delete-file "test-mutual.sexp"))))))

;; Run all tests
(run-tests)
(test-summary)