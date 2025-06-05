#lang racket/base

(require "../src/lexer/lexer.rkt"
         "../src/parser/parser.rkt" 
         "../src/parser/ast.rkt"
         "../src/typecheck/typechecker.rkt"
         "../src/types/types.rkt"
         rackunit)

;; Tests for Pattern Matching Type Checking

(test-case "match expression type checking - simple case"
  (let* ([input "(match 42 (0 \"zero\") (_ \"non-zero\"))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; This should fail due to string literals, but structure is correct
    (check-exn exn:fail? (lambda () (type-check ast env)))))

(test-case "match expression type checking - natural numbers"
  (let* ([input "(match 42 ((zero) 0) ((successor n) (+ n 1)))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Should type check successfully and return Nat
    (let ([result-type (type-check ast env)])
      (check-true (hott-type-equal? result-type Nat)))))

(test-case "match expression type checking - variable binding"
  (let* ([input "(match 5 (x (* x 2)))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Should type check successfully and return Nat
    (let ([result-type (type-check ast env)])
      (check-true (hott-type-equal? result-type Nat)))))

(test-case "match expression type checking - wildcard pattern"
  (let* ([input "(match 123 (_ 999))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Should type check successfully and return Nat
    (let ([result-type (type-check ast env)])
      (check-true (hott-type-equal? result-type Nat)))))

(test-case "match expression type checking - inconsistent body types"
  (let* ([input "(match #t (#t 42) (#f #f))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Should fail because body types don't match (Nat vs Bool)
    (check-exn exn:fail? (lambda () (type-check ast env)))))

(test-case "match expression type checking - literal pattern type mismatch"
  (let* ([input "(match 42 (#t 1) (#f 0))"]
         [tokens (tokenize input)]
         [ast (parse tokens)]
         [env (make-global-type-environment)])
    ;; Should fail because boolean patterns don't match Nat scrutinee
    (check-exn exn:fail? (lambda () (type-check ast env)))))