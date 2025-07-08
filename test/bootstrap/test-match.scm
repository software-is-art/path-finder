#!/usr/bin/env guile
!#

;; Test match expressions in the PathFinder bootstrap VM

(use-modules (ice-9 match)
             (srfi srfi-1))

;; Load bootstrap components directly
(load "../../guile-bootstrap/environment.scm")
(load "../../guile-bootstrap/effects.scm")
(load "../../guile-bootstrap/parser.scm")
(load "../../guile-bootstrap/evaluator.scm")
(load "../../guile-bootstrap/primitives.scm")

;; Pretty printer for values
(define (pretty-print-value val)
  (match val
    (('constructor "zero") "zero")
    (('constructor "succ" n) 
     (format #f "(succ ~a)" (pretty-print-value n)))
    (('constructor "true") "true")
    (('constructor "false") "false")
    (('constructor "nil") "nil")
    (('constructor "cons" h t)
     (format #f "(cons ~a ~a)" (pretty-print-value h) (pretty-print-value t)))
    (_ (format #f "~a" val))))

(define test-exprs
  '(
    ;; Test 1: Simple constructor matching
    (define is-zero
      (fn (n)
        (match n
          (case zero true)
          (case (succ _) false))))
    
    ;; Test 2: Variable binding in patterns
    (define pred
      (fn (n)
        (match n
          (case zero zero)
          (case (succ p) p))))
    
    ;; Test 3: List matching
    (define is-empty
      (fn (lst)
        (match lst
          (case nil true)
          (case (cons _ _) false))))
    
    ;; Test 4: Nested patterns
    (define second
      (fn (lst)
        (match lst
          (case nil nil)
          (case (cons _ nil) nil)
          (case (cons _ (cons x _)) x))))
    
    ;; Test expressions
    (print "Testing is-zero:")
    (print (is-zero zero))
    (print (is-zero (succ zero)))
    
    (print "Testing pred:")
    (print (pred zero))
    (print (pred (succ zero)))
    (print (pred (succ (succ zero))))
    
    (print "Testing is-empty:")
    (print (is-empty nil))
    (print (is-empty (cons zero nil)))
    
    (print "Testing second:")
    (print (second nil))
    (print (second (cons zero nil)))
    (print (second (cons zero (cons (succ zero) nil))))
  ))

(define (run-test-file)
  (format #t "~%=== Testing Match Expressions ===~%~%")
  (let ((env (make-initial-environment)))
    (for-each
     (lambda (expr)
       (call-with-current-continuation
        (lambda (k)
          (with-exception-handler
           (lambda (e)
             (format #t "ERROR evaluating: ~a~%" expr)
             (format #t "  ~a~%~%" e)
             (k #f))
           (lambda ()
             (let* ((ast (parse-pathfinder expr))
                    (result (pathfinder-eval ast env)))
               (format #t "Parsed: ~a~%" ast)
               (cond
                ;; Handle define results
                ((and (pair? result) (eq? (car result) 'define-result))
                 (let ((name (cadr result))
                       (value (caddr result)))
                   (format #t "Defined: ~a = ~a~%~%" name value)
                   (set! env (env-extend env name value))))
                ;; Handle constructor results
                ((and (pair? result) (eq? (car result) 'constructor))
                 (format #t "Result: ~a~%~%" (pretty-print-value result)))
                ;; Handle other results
                (else
                 (format #t "Result: ~a~%~%" result)))))))))
     test-exprs)))

(run-test-file)