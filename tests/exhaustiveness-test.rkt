#lang racket/base

(require "../src/lexer/lexer.rkt"
         "../src/parser/parser.rkt" 
         "../src/parser/ast.rkt"
         "../src/typecheck/typechecker.rkt"
         "../src/types/types.rkt"
         rackunit)

;; Tests for Pattern Matching Exhaustiveness and Unreachability Checking

;; ============================================================================
;; EXHAUSTIVENESS TESTS - Boolean
;; ============================================================================

(test-case "boolean exhaustiveness - complete match"
  (let* ([input "(match #t (#t 1) (#f 0))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Should type check successfully - covers both true and false
    (check-not-exn (lambda () (type-check ast env)))))

(test-case "boolean exhaustiveness - wildcard"
  (let* ([input "(match #t (_ 42))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Should type check successfully - wildcard covers all cases
    (check-not-exn (lambda () (type-check ast env)))))

(test-case "boolean exhaustiveness - variable pattern"
  (let* ([input "(match #t (x x))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Should type check successfully - variable covers all cases  
    (check-not-exn (lambda () (type-check ast env)))))

(test-case "boolean exhaustiveness - missing false case"
  (let* ([input "(match #t (#t 1))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Should fail - missing false case
    (check-exn exn:fail? (lambda () (type-check ast env)))))

(test-case "boolean exhaustiveness - missing true case"
  (let* ([input "(match #f (#f 0))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Should fail - missing true case
    (check-exn exn:fail? (lambda () (type-check ast env)))))

;; ============================================================================
;; EXHAUSTIVENESS TESTS - Natural Numbers
;; ============================================================================

(test-case "nat exhaustiveness - complete match"
  (let* ([input "(match 5 ((zero) 0) ((successor n) (+ n 1)))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Should type check successfully - covers zero and successor
    (check-not-exn (lambda () (type-check ast env)))))

(test-case "nat exhaustiveness - wildcard"
  (let* ([input "(match 5 (_ 42))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Should type check successfully - wildcard covers all cases
    (check-not-exn (lambda () (type-check ast env)))))

(test-case "nat exhaustiveness - missing zero case"
  (let* ([input "(match 5 ((successor n) (+ n 1)))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Should fail - missing zero case
    (check-exn exn:fail? (lambda () (type-check ast env)))))

(test-case "nat exhaustiveness - missing successor case"
  (let* ([input "(match 5 ((zero) 0))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Should fail - missing successor case
    (check-exn exn:fail? (lambda () (type-check ast env)))))

;; ============================================================================
;; UNREACHABILITY TESTS
;; ============================================================================

(test-case "unreachability - pattern after wildcard"
  (let* ([input "(match #t (_ 42) (#t 1))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Should fail - true pattern after wildcard is unreachable
    (check-exn exn:fail? (lambda () (type-check ast env)))))

(test-case "unreachability - pattern after variable"
  (let* ([input "(match 5 (x 42) ((zero) 0))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Should fail - zero pattern after variable is unreachable
    (check-exn exn:fail? (lambda () (type-check ast env)))))

(test-case "unreachability - duplicate literal patterns"
  (let* ([input "(match #t (#t 1) (#t 2) (#f 0))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Should fail - second true pattern is unreachable
    (check-exn exn:fail? (lambda () (type-check ast env)))))

(test-case "unreachability - duplicate constructor patterns"
  (let* ([input "(match 0 ((zero) 1) ((zero) 2) ((successor n) 3))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Should fail - second zero pattern is unreachable
    (check-exn exn:fail? (lambda () (type-check ast env)))))

(test-case "reachability - different patterns are reachable"
  (let* ([input "(match #t (#t 1) (#f 0))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Should type check successfully - different patterns are reachable
    (check-not-exn (lambda () (type-check ast env)))))