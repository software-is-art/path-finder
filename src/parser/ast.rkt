#lang racket/base

(require racket/contract
         racket/string)

(provide (all-defined-out))

;; Abstract Syntax Tree (AST) nodes for PathFinder LISP S-expressions

;; AST Node types
(struct ast-node () #:transparent)

;; Atomic expressions
(struct atom ast-node (value) #:transparent)

;; Specific atom types
(struct symbol-atom atom () #:transparent)
(struct number-atom atom () #:transparent)
(struct boolean-atom atom () #:transparent)
(struct string-atom atom () #:transparent)

;; S-expression (list) nodes
(struct sexpr ast-node (elements) #:transparent)

;; Contracts for AST node types
(define ast-node/c 
  (or/c symbol-atom? number-atom? boolean-atom? string-atom? sexpr?))

;; Symbol atom creation
(define/contract (make-symbol-atom value)
  (-> string? symbol-atom?)
  (symbol-atom value))

;; Number atom creation
(define/contract (make-number-atom value)
  (-> number? number-atom?)
  (number-atom value))

;; Boolean atom creation
(define/contract (make-boolean-atom value)
  (-> boolean? boolean-atom?)
  (boolean-atom value))

;; String atom creation
(define/contract (make-string-atom value)
  (-> string? string-atom?)
  (string-atom value))

;; S-expression creation
(define/contract (make-sexpr elements)
  (-> (listof ast-node/c) sexpr?)
  (sexpr elements))

;; Empty S-expression
(define/contract (make-empty-sexpr)
  (-> sexpr?)
  (sexpr '()))

;; Utility functions for AST manipulation
(define/contract (get-atom-value node)
  (-> atom? any/c)
  (atom-value node))

(define/contract (symbol-atom-value node)
  (-> symbol-atom? string?)
  (atom-value node))

(define/contract (number-atom-value node)
  (-> number-atom? number?)
  (atom-value node))

(define/contract (boolean-atom-value node)
  (-> boolean-atom? boolean?)
  (atom-value node))

(define/contract (string-atom-value node)
  (-> string-atom? string?)
  (atom-value node))

;; Pretty printing support
(define/contract (ast->string node)
  (-> ast-node/c string?)
  (cond
    [(symbol-atom? node) (symbol-atom-value node)]
    [(number-atom? node) (number->string (number-atom-value node))]
    [(boolean-atom? node) (if (boolean-atom-value node) "#t" "#f")]
    [(string-atom? node) (string-append "\"" (string-atom-value node) "\"")]
    [(sexpr? node) 
     (string-append "(" 
                   (string-join (map ast->string (sexpr-elements node)) " ") 
                   ")")]))

;; AST validation
(define/contract (valid-ast? node)
  (-> any/c boolean?)
  (and (ast-node? node)
       (cond
         [(atom? node) #t]
         [(sexpr? node) (andmap valid-ast? (sexpr-elements node))]
         [else #f])))