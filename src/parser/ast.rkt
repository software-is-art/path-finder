#lang racket/base

(require racket/contract
         racket/string
         "../core/hott-literals-pure.rkt")

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

;; ============================================================================
;; PATTERN MATCHING AST NODES
;; ============================================================================

;; Match expression: (match scrutinee (pattern body) ...)
(struct match-expr ast-node (scrutinee cases) #:transparent)

;; Match case: pattern and body expression
(struct match-case (pattern body) #:transparent)

;; Pattern types for descriptive pattern matching
(struct pattern-node () #:transparent)

;; Literal patterns: 42, true, "hello"
(struct literal-pattern pattern-node (value) #:transparent)

;; Variable patterns: x, name, result
(struct variable-pattern pattern-node (name) #:transparent)

;; Wildcard pattern: _
(struct wildcard-pattern pattern-node () #:transparent)

;; Constructor patterns: (point x y), (some value), (list-with first rest)
(struct constructor-pattern pattern-node (constructor-name sub-patterns) #:transparent)

;; Note: All HoTT inductive type patterns now use the general constructor-pattern mechanism
;; Examples: (zero) -> (constructor-pattern "zero" '())
;;           (some x) -> (constructor-pattern "some" (list (variable-pattern "x")))

;; Contracts for AST node types
(define ast-node/c 
  (or/c symbol-atom? number-atom? boolean-atom? string-atom? sexpr? match-expr?))

(define pattern-node/c
  (or/c literal-pattern? variable-pattern? wildcard-pattern? constructor-pattern?))

;; Symbol atom creation
(define/contract (make-symbol-atom value)
  (-> string? symbol-atom?)
  (symbol-atom value))

;; Number atom creation - converts Racket number to HoTT nat
(define/contract (make-number-atom value)
  (-> exact-nonnegative-integer? number-atom?)
  (number-atom (pure-racket-number->hott-nat value)))

;; Boolean atom creation - converts Racket boolean to HoTT bool
(define/contract (make-boolean-atom value)
  (-> boolean? boolean-atom?)
  (boolean-atom (pure-racket-boolean->hott-bool value)))

;; String atom creation - converts Racket string to HoTT string
(define/contract (make-string-atom value)
  (-> string? string-atom?)
  (string-atom (pure-racket-string->hott-string value)))

;; S-expression creation
(define/contract (make-sexpr elements)
  (-> (listof ast-node/c) sexpr?)
  (sexpr elements))

;; Empty S-expression
(define/contract (make-empty-sexpr)
  (-> sexpr?)
  (sexpr '()))

;; ============================================================================
;; PATTERN MATCHING AST CONSTRUCTORS
;; ============================================================================

;; Match expression creation
(define/contract (make-match-expr scrutinee cases)
  (-> ast-node/c (listof match-case?) match-expr?)
  (match-expr scrutinee cases))

;; Match case creation
(define/contract (make-match-case pattern body)
  (-> pattern-node/c ast-node/c match-case?)
  (match-case pattern body))

;; Pattern constructors
(define/contract (make-literal-pattern value)
  (-> any/c literal-pattern?)
  (literal-pattern value))

(define/contract (make-variable-pattern name)
  (-> string? variable-pattern?)
  (variable-pattern name))

(define/contract (make-wildcard-pattern)
  (-> wildcard-pattern?)
  (wildcard-pattern))

(define/contract (make-constructor-pattern name sub-patterns)
  (-> string? (listof pattern-node/c) constructor-pattern?)
  (constructor-pattern name sub-patterns))

;; HoTT-specific pattern constructors
;; All constructor patterns now use make-constructor-pattern
;; Examples:
;; (make-constructor-pattern "zero" '()) for (zero)
;; (make-constructor-pattern "some" (list (variable-pattern "x"))) for (some x)

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
                   ")")]
    [(match-expr? node)
     (string-append "(match " (ast->string (match-expr-scrutinee node))
                   " " (string-join (map match-case->string (match-expr-cases node)) " ")
                   ")")]))

;; Pattern pretty printing
(define/contract (pattern->string pattern)
  (-> pattern-node/c string?)
  (cond
    [(literal-pattern? pattern) (format "~a" (literal-pattern-value pattern))]
    [(variable-pattern? pattern) (variable-pattern-name pattern)]
    [(wildcard-pattern? pattern) "_"]
    [(constructor-pattern? pattern)
     (string-append "(" (constructor-pattern-constructor-name pattern)
                   (if (null? (constructor-pattern-sub-patterns pattern))
                       ""
                       (string-append " " (string-join 
                                         (map pattern->string (constructor-pattern-sub-patterns pattern)) 
                                         " ")))
                   ")")]
    ;; All patterns now use constructor-pattern, no special cases needed
    [else (error "Unknown pattern type in pattern->string: " pattern)]))

;; Match case pretty printing
(define/contract (match-case->string case)
  (-> match-case? string?)
  (string-append "(" (pattern->string (match-case-pattern case))
                " " (ast->string (match-case-body case)) ")"))

;; AST validation
(define/contract (valid-ast? node)
  (-> any/c boolean?)
  (and (ast-node? node)
       (cond
         [(atom? node) #t]
         [(sexpr? node) (andmap valid-ast? (sexpr-elements node))]
         [(match-expr? node) 
          (and (valid-ast? (match-expr-scrutinee node))
               (andmap valid-match-case? (match-expr-cases node)))]
         [else #f])))

;; Match case validation
(define/contract (valid-match-case? case)
  (-> any/c boolean?)
  (and (match-case? case)
       (valid-pattern? (match-case-pattern case))
       (valid-ast? (match-case-body case))))

;; Pattern validation
(define/contract (valid-pattern? pattern)
  (-> any/c boolean?)
  (and (pattern-node? pattern)
       (cond
         [(literal-pattern? pattern) #t]
         [(variable-pattern? pattern) #t]
         [(wildcard-pattern? pattern) #t]
         [(constructor-pattern? pattern) 
          (andmap valid-pattern? (constructor-pattern-sub-patterns pattern))]
         ;; All special patterns removed - now use constructor-pattern
         [else #f])))