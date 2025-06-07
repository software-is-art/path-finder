#lang racket/base

(require "../src/lexer/lexer.rkt"
         "../src/parser/parser.rkt" 
         "../src/parser/ast.rkt"
         "../src/evaluator/evaluator.rkt"
         "../src/evaluator/values.rkt"
         "../src/core/host-bridge.rkt"
         racket/list
         rackunit)

;; Tests for PathFinder LISP Descriptive Pattern Matching

;; ============================================================================
;; PATTERN PARSING TESTS
;; ============================================================================

(test-case "parse simple match expression"
  (let* ([input "(match x ((zero) 0) ((successor n) (+ n 1)))"]
         [tokens (tokenize input)]
         [ast (parse tokens)])
    (check-true (match-expr? ast))
    (check-equal? (length (match-expr-cases ast)) 2)))

(test-case "parse wildcard and variable patterns"
  (let* ([input "(match result ((some value) value) (_ (default)))"]
         [tokens (tokenize input)]
         [ast (parse tokens)])
    (check-true (match-expr? ast))
    (let ([cases (match-expr-cases ast)])
      (check-equal? (length cases) 2)
      (check-true (constructor-pattern? (match-case-pattern (first cases))))
      (check-true (wildcard-pattern? (match-case-pattern (second cases)))))))

(test-case "parse constructor patterns"
  (let* ([input "(match person ((person name age) (greet name)))"]
         [tokens (tokenize input)]
         [ast (parse tokens)])
    (check-true (match-expr? ast))
    (let* ([cases (match-expr-cases ast)]
           [pattern (match-case-pattern (first cases))])
      (check-true (constructor-pattern? pattern))
      (check-equal? (constructor-pattern-constructor-name pattern) "person")
      (check-equal? (length (constructor-pattern-sub-patterns pattern)) 2))))

;; ============================================================================
;; PATTERN EVALUATION TESTS
;; ============================================================================

(test-case "zero pattern matching"
  (let* ([input "(match 0 ((zero) 999) (_ 111))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [result (evaluate ast)])
    (check-true (nat-value? result))
    (check-equal? (nat-value->racket-number result) 999)))

(test-case "successor pattern matching with variable binding"
  (let* ([input "(match 3 ((zero) 0) ((successor n) (+ n 1)))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [result (evaluate ast)])
    (check-true (nat-value? result))
    (check-equal? (nat-value->racket-number result) 3)))

(test-case "boolean pattern matching"
  (let* ([input-true "(match #t ((true) 42) ((false) 0))"]
         [tokens-true (tokenize input-true)]
         [ast-true (parse tokens-true)]
         [result-true (evaluate ast-true)]
         [input-false "(match #f ((true) 42) ((false) 0))"]
         [tokens-false (tokenize input-false)]
         [ast-false (parse tokens-false)]
         [result-false (evaluate ast-false)])
    (check-equal? (nat-value->racket-number result-true) 42)
    (check-equal? (nat-value->racket-number result-false) 0)))

(test-case "variable pattern binds any value"
  (let* ([input "(match 42 (x (* x 2)))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [result (evaluate ast)])
    (check-true (nat-value? result))
    (check-equal? (nat-value->racket-number result) 84)))

(test-case "wildcard pattern matches anything"
  (let* ([input "(match 123 (_ 999))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [result (evaluate ast)])
    (check-true (nat-value? result))
    (check-equal? (nat-value->racket-number result) 999)))

(test-case "pattern order matters - first match wins"
  (let* ([input "(match 5 (_ 111) ((successor n) 222))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [result (evaluate ast)])
    (check-equal? (nat-value->racket-number result) 111)))

(test-case "nested successor patterns"
  (let* ([input "(match 2 ((zero) 0) ((successor (zero)) 1) ((successor (successor n)) (+ n 2)))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [result (evaluate ast)])
    (check-equal? (nat-value->racket-number result) 2))) ; 2 = succ(succ(zero)), n = zero, result = 0 + 2 = 2

;; ============================================================================
;; PATTERN MATCHING WITH FUNCTIONS TESTS
;; ============================================================================

(test-case "pattern matching in function body"
  (let* ([input "((lambda (n) (match n ((zero) 42) ((successor x) x))) 3)"]
         [env (make-global-environment)])
    ;; Test pattern matching within a function - should work now
    (let* ([tokens (tokenize input)]
           [ast (parse tokens)]
           [result (evaluate ast env)])
      (check-true (nat-value? result))
      (check-equal? (nat-value->racket-number result) 2))))

(test-case "fibonacci with pattern matching"
  (let* ([fib-def "(define fib (lambda (n) (match n ((zero) 0) ((successor (zero)) 1) ((successor (successor k)) (+ (fib (successor k)) (fib k))))))"]
         [fib-call "(fib 4)"]
         [env (make-global-environment)])
    ;; Define fibonacci function
    (let* ([def-tokens (tokenize fib-def)]
           [def-ast (parse def-tokens)])
      (evaluate def-ast env))
    ;; Call fibonacci function
    (let* ([call-tokens (tokenize fib-call)]
           [call-ast (parse call-tokens)]
           [result (evaluate call-ast env)])
      (check-true (nat-value? result))
      (check-equal? (nat-value->racket-number result) 3)))) ; fib(4) = 3

;; ============================================================================
;; ERROR HANDLING TESTS
;; ============================================================================

(test-case "non-exhaustive pattern matching throws error"
  (let* ([input "(match 5 ((zero) 0))"] ; Missing case for successor
         [tokens (tokenize input)]
         [ast (parse tokens)])
    (check-exn exn:fail? (lambda () (evaluate ast)))))

;; ============================================================================
;; DESCRIPTIVE CONSTRUCTOR PATTERNS TESTS
;; ============================================================================

(test-case "descriptive constructor pattern structure"
  (let* ([input "(match location ((point x y) (+ x y)) ((origin) 0))"]
         [tokens (tokenize input)]
         [ast (parse tokens)])
    (check-true (match-expr? ast))
    (let* ([cases (match-expr-cases ast)]
           [point-pattern (match-case-pattern (first cases))]
           [origin-pattern (match-case-pattern (second cases))])
      (check-true (constructor-pattern? point-pattern))
      (check-equal? (constructor-pattern-constructor-name point-pattern) "point")
      (check-equal? (length (constructor-pattern-sub-patterns point-pattern)) 2)
      (check-true (constructor-pattern? origin-pattern))
      (check-equal? (constructor-pattern-constructor-name origin-pattern) "origin")
      (check-equal? (length (constructor-pattern-sub-patterns origin-pattern)) 0))))

;; ============================================================================
;; LITERAL PATTERN TESTS
;; ============================================================================

(test-case "literal number patterns"
  (let* ([input "(match x (0 \"zero\") (1 \"one\") (_ \"other\"))"]
         [tokens (tokenize input)]
         [ast (parse tokens)])
    (check-true (match-expr? ast))
    (let* ([cases (match-expr-cases ast)]
           [zero-pattern (match-case-pattern (first cases))]
           [one-pattern (match-case-pattern (second cases))])
      (check-true (literal-pattern? zero-pattern))
      (check-equal? (literal-pattern-value zero-pattern) 0)
      (check-true (literal-pattern? one-pattern))
      (check-equal? (literal-pattern-value one-pattern) 1))))

(test-case "literal boolean patterns"
  (let* ([input "(match flag (#t \"true\") (#f \"false\"))"]
         [tokens (tokenize input)]
         [ast (parse tokens)])
    (check-true (match-expr? ast))
    (let* ([cases (match-expr-cases ast)]
           [true-pattern (match-case-pattern (first cases))]
           [false-pattern (match-case-pattern (second cases))])
      (check-true (literal-pattern? true-pattern))
      (check-equal? (literal-pattern-value true-pattern) #t)
      (check-true (literal-pattern? false-pattern))
      (check-equal? (literal-pattern-value false-pattern) #f))))