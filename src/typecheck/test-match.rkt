#lang racket/base

(require racket/match)

;; Test the match form structure
(define (test-match x)
  (match x
    [1 'one]
    [2 'two]
    [_ 'other]))

(define (test-nested x)
  (match x
    [(list a b)
     (match a
       [1 'first-is-one]
       [_ 'first-is-other])]
    [_ 'not-a-list]))

(displayln (test-match 1))
(displayln (test-nested '(1 2)))