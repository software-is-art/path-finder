#lang racket/base

(require "../src/lexer/lexer.rkt"
         "../src/parser/parser.rkt" 
         "../src/parser/ast.rkt"
         "../src/typecheck/typechecker.rkt"
         "../src/types/types.rkt"
         rackunit)

;; Tests for Pattern Matching Exhaustiveness and Unreachability Checking

;; ============================================================================
;; EXHAUSTIVENESS TESTS
;; ============================================================================

(test-case "exhaustive Nat patterns pass type checking"
  (let* ([input "(match n ((zero) 0) ((successor x) (+ x 1)))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Add 'n' variable of type Nat to environment
    (type-env-define! env "n" Nat)
    ;; Should not throw an error
    (check-not-exn (lambda () (type-check ast env)))))

(test-case "exhaustive Bool patterns pass type checking"
  (let* ([input "(match flag ((true) 1) ((false) 0))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Add 'flag' variable of type Bool to environment
    (type-env-define! env "flag" Bool)
    ;; Should not throw an error
    (check-not-exn (lambda () (type-check ast env)))))

(test-case "wildcard pattern makes any match exhaustive"
  (let* ([input "(match value (_ 42))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Add 'value' variable of type Nat to environment
    (type-env-define! env "value" Nat)
    ;; Should not throw an error - wildcard covers everything
    (check-not-exn (lambda () (type-check ast env)))))

(test-case "variable pattern makes any match exhaustive"
  (let* ([input "(match number (x (* x 2)))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Add 'number' variable of type Nat to environment
    (type-env-define! env "number" Nat)
    ;; Should not throw an error - variable pattern covers everything
    (check-not-exn (lambda () (type-check ast env)))))

;; ============================================================================
;; NON-EXHAUSTIVENESS TESTS
;; ============================================================================

(test-case "missing zero pattern fails exhaustiveness check"
  (let* ([input "(match n ((successor x) (+ x 1)))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Add 'n' variable of type Nat to environment
    (type-env-define! env "n" Nat)
    ;; Should throw an error - missing zero pattern
    (check-exn exn:fail? (lambda () (type-check ast env)))))

(test-case "missing successor pattern fails exhaustiveness check"
  (let* ([input "(match n ((zero) 0))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Add 'n' variable of type Nat to environment
    (type-env-define! env "n" Nat)
    ;; Should throw an error - missing successor pattern
    (check-exn exn:fail? (lambda () (type-check ast env)))))

(test-case "missing true pattern fails exhaustiveness check"
  (let* ([input "(match flag ((false) 0))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Add 'flag' variable of type Bool to environment
    (type-env-define! env "flag" Bool)
    ;; Should throw an error - missing true pattern
    (check-exn exn:fail? (lambda () (type-check ast env)))))

(test-case "missing false pattern fails exhaustiveness check"
  (let* ([input "(match flag ((true) 1))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Add 'flag' variable of type Bool to environment
    (type-env-define! env "flag" Bool)
    ;; Should throw an error - missing false pattern
    (check-exn exn:fail? (lambda () (type-check ast env)))))

;; ============================================================================
;; UNREACHABILITY TESTS
;; ============================================================================

(test-case "pattern after wildcard is unreachable"
  (let* ([input "(match n (_ 42) ((zero) 0))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Add 'n' variable of type Nat to environment
    (type-env-define! env "n" Nat)
    ;; Should throw an error - zero pattern is unreachable after wildcard
    (check-exn exn:fail? (lambda () (type-check ast env)))))

(test-case "pattern after variable is unreachable"
  (let* ([input "(match n (x (* x 2)) ((successor y) (+ y 1)))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Add 'n' variable of type Nat to environment
    (type-env-define! env "n" Nat)
    ;; Should throw an error - successor pattern is unreachable after variable
    (check-exn exn:fail? (lambda () (type-check ast env)))))

(test-case "specific pattern after general pattern is unreachable"
  (let* ([input "(match flag ((true) 1) ((false) 0) (_ 42))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Add 'flag' variable of type Bool to environment
    (type-env-define! env "flag" Bool)
    ;; Should throw an error - wildcard is unreachable after complete patterns
    (check-exn exn:fail? (lambda () (type-check ast env)))))

;; ============================================================================
;; LITERAL PATTERN TESTS
;; ============================================================================

(test-case "literal zero pattern equivalent to zero constructor"
  (let* ([input "(match n (0 100) ((successor x) (+ x 1)))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Add 'n' variable of type Nat to environment
    (type-env-define! env "n" Nat)
    ;; Should not throw an error - literal 0 covers zero case
    (check-not-exn (lambda () (type-check ast env)))))

(test-case "literal boolean patterns work for exhaustiveness"
  (let* ([input "(match flag (#t 1) (#f 0))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Add 'flag' variable of type Bool to environment
    (type-env-define! env "flag" Bool)
    ;; Should not throw an error - literal booleans cover all Bool cases
    (check-not-exn (lambda () (type-check ast env)))))

(test-case "mixing literal and constructor patterns"
  (let* ([input "(match n (0 100) (1 200) ((successor (successor x)) (+ x 2)))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Add 'n' variable of type Nat to environment
    (type-env-define! env "n" Nat)
    ;; Should not throw an error - covers zero (0), succ(zero) (1), and succ(succ(_))
    (check-not-exn (lambda () (type-check ast env)))))

;; ============================================================================
;; TYPE CHECKING PATTERN MATCHING TESTS
;; ============================================================================

(test-case "pattern variables are properly typed in match body"
  (let* ([input "(match n ((successor x) (+ x 1)) ((zero) 0))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Add 'n' variable of type Nat to environment
    (type-env-define! env "n" Nat)
    ;; Should type check successfully - x is bound to Nat type in the body
    (let ([result-type (type-check ast env)])
      (check-true (hott-type-equal? result-type Nat)))))

(test-case "match case bodies must have consistent types"
  (let* ([input "(match flag ((true) 42) ((false) #t))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Add 'flag' variable of type Bool to environment
    (type-env-define! env "flag" Bool)
    ;; Should throw an error - first case returns Nat, second returns Bool
    (check-exn exn:fail? (lambda () (type-check ast env)))))

(test-case "nested pattern matching type checks correctly"
  (let* ([input "(match n ((zero) 0) ((successor (zero)) 1) ((successor (successor x)) (+ x 2)))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Add 'n' variable of type Nat to environment
    (type-env-define! env "n" Nat)
    ;; Should type check successfully with nested patterns
    (let ([result-type (type-check ast env)])
      (check-true (hott-type-equal? result-type Nat)))))