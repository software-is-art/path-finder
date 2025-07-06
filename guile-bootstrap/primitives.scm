;;; PathFinder primitive operations
;;; Implements the core HoTT eliminators and constructors

(use-modules (ice-9 match)
             (srfi srfi-1))  ; for fold-right

;; This file is loaded by evaluator.scm, so the implementations
;; are already there. This file can contain additional primitive
;; operations or utilities.

;; Convert Peano numbers to Scheme numbers for display
(define (peano->number n)
  "Convert a Peano number to a Scheme number"
  (match n
    (('constructor "zero") 0)
    (('constructor "succ" pred)
     (+ 1 (peano->number pred)))
    (_ (error "Not a Peano number:" n))))

;; Convert Scheme numbers to Peano for testing
(define (number->peano n)
  "Convert a Scheme number to Peano representation"
  (if (= n 0)
      '(constructor "zero")
      `(constructor "succ" ,(number->peano (- n 1)))))

;; Boolean operations (helpers)
(define (pathfinder-not b)
  "Logical NOT using bool-elim"
  (match b
    (('constructor "true") '(constructor "false"))
    (('constructor "false") '(constructor "true"))
    (_ (error "Not a boolean:" b))))

(define (pathfinder-and a b)
  "Logical AND using bool-elim"
  (match a
    (('constructor "true") b)
    (('constructor "false") '(constructor "false"))
    (_ (error "Not a boolean:" a))))

(define (pathfinder-or a b)
  "Logical OR using bool-elim"
  (match a
    (('constructor "true") '(constructor "true"))
    (('constructor "false") b)
    (_ (error "Not a boolean:" a))))

;; List operations
(define (pathfinder-list? val)
  "Check if a value is a list"
  (match val
    (('constructor "nil") #t)
    (('constructor "cons" _ _) #t)
    (_ #f)))

(define (pathfinder-list->scheme-list lst)
  "Convert PathFinder list to Scheme list"
  (match lst
    (('constructor "nil") '())
    (('constructor "cons" head tail)
     (cons head (pathfinder-list->scheme-list tail)))
    (_ (error "Not a PathFinder list:" lst))))

(define (scheme-list->pathfinder-list lst)
  "Convert Scheme list to PathFinder list"
  (if (null? lst)
      '(constructor "nil")
      `(constructor "cons" ,(car lst) ,(scheme-list->pathfinder-list (cdr lst)))))

;; Type checking helpers
(define (is-nat? val)
  "Check if a value is a natural number"
  (match val
    (('constructor "zero") #t)
    (('constructor "succ" pred) (is-nat? pred))
    (_ #f)))

(define (is-bool? val)
  "Check if a value is a boolean"
  (match val
    (('constructor "true") #t)
    (('constructor "false") #t)
    (_ #f)))

;; String helpers for PathFinder strings
(define (make-pathfinder-string str)
  "Convert a Scheme string to PathFinder string representation"
  (if (string-null? str)
      '(constructor "empty-string")
      (let ((chars (string->list str)))
        (fold-right
         (lambda (ch acc)
           `(constructor "string-cons"
                        (constructor "char" ,(number->peano (char->integer ch)))
                        ,acc))
         '(constructor "empty-string")
         chars))))