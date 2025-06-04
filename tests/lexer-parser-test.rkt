#lang racket/base

(require "../src/main.rkt"
         "../src/parser/ast.rkt"
         "../src/lexer/tokens.rkt"
         racket/list)

;; Simple test framework (same as main-test.rkt)
(define test-count 0)
(define pass-count 0)

(define (test-assert name condition)
  (set! test-count (+ test-count 1))
  (if condition
      (begin
        (set! pass-count (+ pass-count 1))
        (printf "✓ ~a~n" name))
      (printf "✗ ~a~n" name)))

(define (test-equal name actual expected)
  (test-assert name (equal? actual expected)))

(printf "Running Lexer and Parser Tests...~n~n")

;; Basic tokenizer tests
(test-assert "tokenize simple number"
  (let ([tokens (tokenize "42")])
    (and (= (length tokens) 2) ; number + EOF
         (equal? (token-type (first tokens)) 'number)
         (equal? (token-value (first tokens)) 42))))

(test-assert "tokenize simple symbol"
  (let ([tokens (tokenize "hello")])
    (and (= (length tokens) 2) ; symbol + EOF
         (equal? (token-type (first tokens)) 'symbol)
         (equal? (token-value (first tokens)) "hello"))))

(test-assert "tokenize boolean true"
  (let ([tokens (tokenize "#t")])
    (and (= (length tokens) 2) ; boolean + EOF
         (equal? (token-type (first tokens)) 'boolean)
         (equal? (token-value (first tokens)) #t))))

(test-assert "tokenize boolean false"
  (let ([tokens (tokenize "#f")])
    (and (= (length tokens) 2) ; boolean + EOF
         (equal? (token-type (first tokens)) 'boolean)
         (equal? (token-value (first tokens)) #f))))

(test-assert "tokenize string literal"
  (let ([tokens (tokenize "\"hello world\"")])
    (and (= (length tokens) 2) ; string + EOF
         (equal? (token-type (first tokens)) 'string)
         (equal? (token-value (first tokens)) "hello world"))))

(test-assert "tokenize simple S-expression"
  (let ([tokens (tokenize "(+ 1 2)")])
    (= (length tokens) 6))) ; ( + 1 2 ) EOF

;; Basic parser tests
(test-assert "parse number"
  (let ([ast (parse (tokenize "42"))])
    (and (number-atom? ast)
         (equal? (atom-value ast) 42))))

(test-assert "parse symbol"
  (let ([ast (parse (tokenize "hello"))])
    (and (symbol-atom? ast)
         (equal? (atom-value ast) "hello"))))

(test-assert "parse boolean true"
  (let ([ast (parse (tokenize "#t"))])
    (and (boolean-atom? ast)
         (equal? (atom-value ast) #t))))

(test-assert "parse string"
  (let ([ast (parse (tokenize "\"test\""))])
    (and (string-atom? ast)
         (equal? (atom-value ast) "test"))))

(test-assert "parse simple S-expression"
  (let ([ast (parse (tokenize "(+ 1 2)"))])
    (and (sexpr? ast)
         (= (length (sexpr-elements ast)) 3))))

(test-assert "parse nested S-expression"
  (let ([ast (parse (tokenize "(if #t (+ 1 2) 3)"))])
    (and (sexpr? ast)
         (= (length (sexpr-elements ast)) 4)
         (sexpr? (third (sexpr-elements ast))))))

;; Complex integration tests
(test-assert "parse function definition"
  (let ([ast (parse (tokenize "(define (square x) (* x x))"))])
    (and (sexpr? ast)
         (= (length (sexpr-elements ast)) 3)
         (symbol-atom? (first (sexpr-elements ast)))
         (equal? (atom-value (first (sexpr-elements ast))) "define"))))

;; Report results
(printf "~n=== Lexer/Parser Test Results ===~n")
(printf "Passed: ~a/~a tests~n" pass-count test-count)
(if (= pass-count test-count)
    (printf "All lexer/parser tests passed! ✓~n")
    (printf "Some lexer/parser tests failed. ✗~n"))