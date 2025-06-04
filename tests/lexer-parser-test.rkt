#lang racket/base

(require "../src/main.rkt"
         "../src/parser/ast.rkt"
         "../src/lexer/tokens.rkt"
         racket/list
         rackunit)

;; Basic tokenizer tests
(test-case "tokenize simple number"
  (let ([tokens (tokenize "42")])
    (check-equal? (length tokens) 2) ; number + EOF
    (check-equal? (token-type (first tokens)) 'number)
    (check-equal? (token-value (first tokens)) 42)))

(test-case "tokenize simple symbol"
  (let ([tokens (tokenize "hello")])
    (check-equal? (length tokens) 2) ; symbol + EOF
    (check-equal? (token-type (first tokens)) 'symbol)
    (check-equal? (token-value (first tokens)) "hello")))

(test-case "tokenize boolean true"
  (let ([tokens (tokenize "#t")])
    (check-equal? (length tokens) 2) ; boolean + EOF
    (check-equal? (token-type (first tokens)) 'boolean)
    (check-equal? (token-value (first tokens)) #t)))

(test-case "tokenize boolean false"
  (let ([tokens (tokenize "#f")])
    (check-equal? (length tokens) 2) ; boolean + EOF
    (check-equal? (token-type (first tokens)) 'boolean)
    (check-equal? (token-value (first tokens)) #f)))

(test-case "tokenize string literal"
  (let ([tokens (tokenize "\"hello world\"")])
    (check-equal? (length tokens) 2) ; string + EOF
    (check-equal? (token-type (first tokens)) 'string)
    (check-equal? (token-value (first tokens)) "hello world")))

(test-case "tokenize simple S-expression"
  (let ([tokens (tokenize "(+ 1 2)")])
    (check-equal? (length tokens) 6))) ; ( + 1 2 ) EOF

;; Basic parser tests
(test-case "parse number"
  (let ([ast (parse (tokenize "42"))])
    (check-true (number-atom? ast))
    (check-equal? (atom-value ast) 42)))

(test-case "parse symbol"
  (let ([ast (parse (tokenize "hello"))])
    (check-true (symbol-atom? ast))
    (check-equal? (atom-value ast) "hello")))

(test-case "parse boolean true"
  (let ([ast (parse (tokenize "#t"))])
    (check-true (boolean-atom? ast))
    (check-equal? (atom-value ast) #t)))

(test-case "parse string"
  (let ([ast (parse (tokenize "\"test\""))])
    (check-true (string-atom? ast))
    (check-equal? (atom-value ast) "test")))

(test-case "parse simple S-expression"
  (let ([ast (parse (tokenize "(+ 1 2)"))])
    (check-true (sexpr? ast))
    (check-equal? (length (sexpr-elements ast)) 3)))

(test-case "parse nested S-expression"
  (let ([ast (parse (tokenize "(if #t (+ 1 2) 3)"))])
    (check-true (sexpr? ast))
    (check-equal? (length (sexpr-elements ast)) 4)
    (check-true (sexpr? (third (sexpr-elements ast))))))

;; Complex integration tests
(test-case "parse function definition"
  (let ([ast (parse (tokenize "(define (square x) (* x x))"))])
    (check-true (sexpr? ast))
    (check-equal? (length (sexpr-elements ast)) 3)
    (check-true (symbol-atom? (first (sexpr-elements ast))))
    (check-equal? (atom-value (first (sexpr-elements ast))) "define")))

;; Error handling tests
(test-case "parse mismatched parentheses"
  (check-exn exn:fail? (lambda () (parse (tokenize "(+ 1 2")))))

(test-case "parse invalid token"
  (check-exn exn:fail? (lambda () (tokenize "@invalid"))))