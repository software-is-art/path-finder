#lang racket/base

(require racket/contract)

(provide (all-defined-out))

;; Token Types for PathFinder LISP S-expressions

;; Token structure
(struct token (type value line column) #:transparent)

;; Token types enumeration
(define token-type/c
  (or/c 'left-paren    ; (
        'right-paren   ; )
        'symbol        ; identifiers (case-sensitive)
        'number        ; integers and floats
        'boolean       ; #t, #f
        'string        ; "..."
        'comment       ; ; ...
        'eof           ; end of file
        'whitespace))  ; spaces, tabs, newlines

;; Contracts for token operations are provided through struct definition

;; Token creation helpers
(define/contract (make-token type value line column)
  (-> token-type/c any/c exact-nonnegative-integer? exact-nonnegative-integer? token?)
  (token type value line column))

(define/contract (token-eof line column)
  (-> exact-nonnegative-integer? exact-nonnegative-integer? token?)
  (token 'eof #f line column))

(define/contract (token-left-paren line column)
  (-> exact-nonnegative-integer? exact-nonnegative-integer? token?)
  (token 'left-paren "(" line column))

(define/contract (token-right-paren line column)
  (-> exact-nonnegative-integer? exact-nonnegative-integer? token?)
  (token 'right-paren ")" line column))

(define/contract (token-symbol value line column)
  (-> string? exact-nonnegative-integer? exact-nonnegative-integer? token?)
  (token 'symbol value line column))

(define/contract (token-number value line column)
  (-> number? exact-nonnegative-integer? exact-nonnegative-integer? token?)
  (token 'number value line column))

(define/contract (token-boolean value line column)
  (-> boolean? exact-nonnegative-integer? exact-nonnegative-integer? token?)
  (token 'boolean value line column))

(define/contract (token-string value line column)
  (-> string? exact-nonnegative-integer? exact-nonnegative-integer? token?)
  (token 'string value line column))

(define/contract (token-comment value line column)
  (-> string? exact-nonnegative-integer? exact-nonnegative-integer? token?)
  (token 'comment value line column))

(define/contract (token-whitespace value line column)
  (-> string? exact-nonnegative-integer? exact-nonnegative-integer? token?)
  (token 'whitespace value line column))