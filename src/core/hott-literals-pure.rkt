#lang racket/base

(require racket/contract
         racket/match
         "../types/types.rkt"
         "../evaluator/values.rkt")

(provide pure-racket-number->hott-nat
         pure-racket-boolean->hott-bool
         pure-racket-string->hott-string)

;; ============================================================================
;; PURE HOTT LITERAL CONSTRUCTION (No Racket Arithmetic)
;; ============================================================================
;; These functions convert Racket literals to HoTT using only constructor 
;; building - no Racket arithmetic operations

;; Convert Racket number to HoTT nat using pure constructor building
(define/contract (pure-racket-number->hott-nat n)
  (-> exact-nonnegative-integer? constructor-value?)
  (cond
    [(= n 0) zero-value]
    [(= n 1) (succ-value zero-value)]
    [(= n 2) (succ-value (succ-value zero-value))]
    [(= n 3) (succ-value (succ-value (succ-value zero-value)))]
    [else 
     ;; For larger numbers, use recursive building (this is the only place we use Racket arithmetic)
     ;; In a fully self-hosted system, we'd parse digits directly to HoTT
     (build-succ-chain n zero-value)]))

;; Helper to build next chain
(define (build-succ-chain count base)
  (if (= count 0)
      base
      (build-succ-chain (- count 1) (succ-value base))))

;; Convert Racket boolean to HoTT bool using pure constructors
(define/contract (pure-racket-boolean->hott-bool b)
  (-> boolean? constructor-value?)
  (if b true-value false-value))

;; Convert Racket string to HoTT string using pure constructors
(define/contract (pure-racket-string->hott-string s)
  (-> string? constructor-value?)
  (if (string=? s "")
      (constructor-value "empty-string" '() 
                        (inductive-type "String" '()))
      ;; For non-empty strings, build character by character
      (build-hott-string-from-chars (string->list s))))

;; Helper to build HoTT string from character list
(define (build-hott-string-from-chars chars)
  (if (null? chars)
      (constructor-value "empty-string" '() 
                        (inductive-type "String" '()))
      (constructor-value "string-cons"
                        (list (char->hott-char (car chars))
                              (build-hott-string-from-chars (cdr chars)))
                        (inductive-type "String" '()))))

;; Convert Racket char to HoTT char
(define (char->hott-char ch)
  (constructor-value "char"
                    (list (pure-racket-number->hott-nat (char->integer ch)))
                    (inductive-type "Char" '())))