#!/usr/bin/env guile
!#

;; Simple test of match expressions

(use-modules (ice-9 match)
             (srfi srfi-1))

;; Load bootstrap components
(load "../../guile-bootstrap/environment.scm")
(load "../../guile-bootstrap/effects.scm")
(load "../../guile-bootstrap/parser.scm")
(load "../../guile-bootstrap/evaluator.scm")
(load "../../guile-bootstrap/primitives.scm")

(define (test-match)
  (let* ((env (make-initial-environment))
         ;; Parse and evaluate the is-zero function
         (def-expr '(define is-zero
                      (fn (n)
                        (match n
                          (case zero true)
                          (case (succ _) false)))))
         (def-ast (parse-pathfinder def-expr))
         (def-result (pathfinder-eval def-ast env)))
    
    (format #t "Definition result: ~a~%~%" def-result)
    
    ;; Extract the function
    (let* ((is-zero-fn (caddr def-result))
           (new-env (env-extend env "is-zero" is-zero-fn)))
      
      ;; Test calling is-zero with zero
      (format #t "~%Testing (is-zero zero):~%")
      (let* ((test-expr '(is-zero zero))
             (test-ast (parse-pathfinder test-expr))
             (test-result (pathfinder-eval test-ast new-env)))
        (format #t "Result: ~a~%~%" test-result))
      
      ;; Test calling is-zero with (succ zero)
      (format #t "~%Testing (is-zero (succ zero)):~%")
      (let* ((test-expr '(is-zero (succ zero)))
             (test-ast (parse-pathfinder test-expr))
             (test-result (pathfinder-eval test-ast new-env)))
        (format #t "Result: ~a~%~%" test-result)))))

(test-match)