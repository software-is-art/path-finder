#lang racket/base

(require racket/contract
         racket/match
         racket/list
         "../types/types.rkt"
         "../evaluator/values.rkt"
         "hott-literals.rkt")

(provide (all-defined-out))

;; ============================================================================
;; HOTT-NATIVE AST (No Racket types stored)
;; ============================================================================

;; Base AST node
(struct hott-ast-node () #:transparent)

;; Atoms that store HoTT representations, not Racket values
(struct hott-nat-atom hott-ast-node (nat-literal) #:transparent)      ; nat-literal, not Racket number
(struct hott-string-atom hott-ast-node (hott-string) #:transparent)   ; HoTT string, not Racket string
(struct hott-bool-atom hott-ast-node (constructor-name) #:transparent) ; 'true or 'false
(struct hott-symbol-atom hott-ast-node (name) #:transparent)           ; Still use symbols for names

;; Composite structures
(struct hott-sexpr hott-ast-node (elements) #:transparent)
(struct hott-match-expr hott-ast-node (scrutinee cases) #:transparent)
(struct hott-match-case (pattern body) #:transparent)

;; Pattern nodes
(struct hott-pattern-node () #:transparent)
(struct hott-wildcard-pattern hott-pattern-node () #:transparent)
(struct hott-variable-pattern hott-pattern-node (name) #:transparent)
(struct hott-literal-pattern hott-pattern-node (literal-node) #:transparent)
(struct hott-constructor-pattern hott-pattern-node (constructor-name sub-patterns) #:transparent)

;; ============================================================================
;; PARSING FROM S-EXPRESSIONS TO HOTT AST
;; ============================================================================

;; Parse Racket s-expr to HoTT AST (bridge function)
(define/contract (parse-to-hott-ast sexpr)
  (-> any/c hott-ast-node?)
  (match sexpr
    ;; Numbers become nat-literals
    [(? exact-nonnegative-integer? n)
     (hott-nat-atom (racket-number->nat-literal n))]
    
    ;; Strings become HoTT strings
    [(? string? s)
     (hott-string-atom (racket-string->hott-string s))]
    
    ;; Booleans become constructor names
    [#t (hott-bool-atom 'true)]
    [#f (hott-bool-atom 'false)]
    
    ;; Symbols remain symbols (for function names, variables)
    [(? symbol? sym)
     (hott-symbol-atom sym)]
    
    ;; Lists become s-expressions
    [(? list? lst)
     (hott-sexpr (map parse-to-hott-ast lst))]
    
    [_ (error "Cannot parse to HoTT AST: " sexpr)]))

;; ============================================================================
;; HOTT AST EVALUATION
;; ============================================================================

;; Evaluate HoTT AST using only HoTT operations
(define/contract (evaluate-hott-ast ast env)
  (-> hott-ast-node? hash? constructor-value?)
  (match ast
    ;; Literals evaluate to their HoTT representations
    [(hott-nat-atom nat-lit)
     (nat-literal->constructor-value nat-lit)]
    
    [(hott-string-atom hott-str)
     (hott-string->constructor-value hott-str)]
    
    [(hott-bool-atom 'true) true-value]
    [(hott-bool-atom 'false) false-value]
    
    ;; Variable lookup
    [(hott-symbol-atom name)
     (let ([val (hash-ref env name #f)])
       (if val
           val
           (error "Undefined variable: " name)))]
    
    ;; S-expressions (function calls)
    [(hott-sexpr elements)
     (if (null? elements)
         (unit-value)
         (let* ([func-ast (first elements)]
                [arg-asts (rest elements)]
                [func (evaluate-hott-ast func-ast env)]
                [args (map (lambda (arg) (evaluate-hott-ast arg env)) arg-asts)])
           (apply-hott-function func args)))]
    
    [_ (error "Unknown HoTT AST node: " ast)]))

;; Convert nat-literal to constructor-value
(define/contract (nat-literal->constructor-value nat-lit)
  (-> nat-literal? constructor-value?)
  ;; Convert from decimal representation to Peano
  (let ([digits (nat-literal-digits nat-lit)])
    (digits->peano digits)))

;; Helper: Convert digit list to Peano number
(define/contract (digits->peano digits)
  (-> (listof symbol?) constructor-value?)
  ;; This would implement proper decimal-to-unary conversion
  ;; For now, simplified:
  (let ([n (digits->number digits)])
    (number->peano n)))

;; Helper: Convert digits to number (for conversion only)
(define/contract (digits->number digits)
  (-> (listof symbol?) exact-nonnegative-integer?)
  (foldl (lambda (digit acc)
           (+ (* acc 10) (digit-symbol->number digit)))
         0
         digits))

(define/contract (digit-symbol->number d)
  (-> symbol? (integer-in 0 9))
  (case d
    ['d0 0] ['d1 1] ['d2 2] ['d3 3] ['d4 4]
    ['d5 5] ['d6 6] ['d7 7] ['d8 8] ['d9 9]))

;; Helper: Convert number to Peano
(define/contract (number->peano n)
  (-> exact-nonnegative-integer? constructor-value?)
  (if (= n 0)
      zero-value
      (succ-value (number->peano (- n 1)))))

;; Convert HoTT string to constructor-value
(define/contract (hott-string->constructor-value hott-str)
  (-> list? constructor-value?)
  ;; The hott-str is already in constructor form
  (parse-string-structure hott-str))

(define/contract (parse-string-structure str-struct)
  (-> list? constructor-value?)
  (match str-struct
    ['(empty-string)
     (constructor-value "empty-string" '() String)]
    [(list 'string-cons char-struct rest-struct)
     (constructor-value "string-cons"
                       (list (parse-char-structure char-struct)
                             (parse-string-structure rest-struct))
                       String)]))

(define/contract (parse-char-structure char-struct)
  (-> list? constructor-value?)
  (match char-struct
    [(list 'char n)
     (constructor-value "char" 
                       (list (if (exact-nonnegative-integer? n)
                                 (number->peano n)
                                 n))
                       Char)]))

;; Apply HoTT function (all functions are HoTT-native)
(define/contract (apply-hott-function func args)
  (-> any/c (listof constructor-value?) constructor-value?)
  (if (procedure? func)
      (apply func args)
      (error "Not a function: " func)))

;; ============================================================================
;; CONTRACT DEFINITIONS
;; ============================================================================

(define hott-ast-node/c
  (or/c hott-nat-atom? hott-string-atom? hott-bool-atom? hott-symbol-atom?
        hott-sexpr? hott-match-expr?))

(define hott-pattern-node/c  
  (or/c hott-wildcard-pattern? hott-variable-pattern? hott-literal-pattern?
        hott-constructor-pattern?))