#!/usr/bin/env guile
!#

;; Test loading PathFinder parser with match support

(add-to-load-path "../../guile-bootstrap")
(use-modules (ice-9 match)
             (ice-9 pretty-print)
             (ice-9 rdelim)
             (srfi srfi-1))

;; Load bootstrap components
(load "../../guile-bootstrap/evaluator.scm")
(load "../../guile-bootstrap/primitives.scm") 
(load "../../guile-bootstrap/parser.scm")
(load "../../guile-bootstrap/effects.scm")
(load "../../guile-bootstrap/environment.scm")

(define (test-load-parser)
  (format #t "~%=== Testing PathFinder Parser Loading ===~%~%")
  
  ;; Try to load the parser module
  (let* ((parser-file "src/parser/parser.sexp")
         (sexps (call-with-input-file parser-file read-all-sexps))
         (env (make-initial-environment)))
    
    (format #t "Loaded ~a expressions from parser.sexp~%~%" (length sexps))
    
    ;; Try to evaluate the first few expressions
    (let loop ((sexps (take sexps 10))  ; Just first 10 for testing
               (env env)
               (count 0))
      (if (null? sexps)
          (format #t "~%Successfully evaluated ~a expressions!~%" count)
          (let ((sexp (car sexps)))
            (format #t "~%Expression ~a: ~a~%" (+ count 1) 
                    (if (pair? sexp) (car sexp) sexp))
            (with-exception-handler
              (lambda (e)
                (format #t "ERROR: ~a~%" e)
                (loop (cdr sexps) env count))
              (lambda ()
                (let* ((ast (parse-pathfinder sexp))
                       (result (pathfinder-eval ast env)))
                  (cond
                    ((and (pair? result) (eq? (car result) 'define-result))
                     (let ((name (cadr result)))
                       (format #t "  Defined: ~a~%" name)
                       (loop (cdr sexps) 
                             (env-extend env name (caddr result))
                             (+ count 1))))
                    (else
                     (format #t "  Result: ~a~%" 
                             (if (> (string-length (format #f "~a" result)) 60)
                                 (string-append 
                                   (substring (format #f "~a" result) 0 57) 
                                   "...")
                                 result))
                     (loop (cdr sexps) env (+ count 1))))))
              #:unwind? #t))))))

(define (read-all-sexps port)
  "Read all s-expressions from a port"
  (let loop ((sexps '()))
    (let ((sexp (read port)))
      (if (eof-object? sexp)
          (reverse sexps)
          (loop (cons sexp sexps))))))

(test-load-parser)