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

;; Special patterns for HoTT inductive types
(struct zero-pattern pattern-node () #:transparent)
(struct successor-pattern pattern-node (sub-pattern) #:transparent)
(struct true-pattern pattern-node () #:transparent)
(struct false-pattern pattern-node () #:transparent)

;; Contracts for AST node types
(define ast-node/c 
  (or/c symbol-atom? number-atom? boolean-atom? string-atom? sexpr? match-expr?))

(define pattern-node/c
  (or/c literal-pattern? variable-pattern? wildcard-pattern? constructor-pattern?
        zero-pattern? successor-pattern? true-pattern? false-pattern?))

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
(define/contract (make-zero-pattern)
  (-> zero-pattern?)
  (zero-pattern))

(define/contract (make-successor-pattern sub-pattern)
  (-> pattern-node/c successor-pattern?)
  (successor-pattern sub-pattern))

(define/contract (make-true-pattern)
  (-> true-pattern?)
  (true-pattern))

(define/contract (make-false-pattern)
  (-> false-pattern?)
  (false-pattern))

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
    [(zero-pattern? pattern) "(zero)"]
    [(successor-pattern? pattern) 
     (string-append "(successor " (pattern->string (successor-pattern-sub-pattern pattern)) ")")]
    [(true-pattern? pattern) "(true)"]
    [(false-pattern? pattern) "(false)"]))

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
         [(zero-pattern? pattern) #t]
         [(successor-pattern? pattern) 
          (valid-pattern? (successor-pattern-sub-pattern pattern))]
         [(true-pattern? pattern) #t]
         [(false-pattern? pattern) #t]
         [else #f])))