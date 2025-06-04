#lang racket/base

(require racket/contract
         racket/list
         "../lexer/tokens.rkt"
         "ast.rkt")

(provide parse)

;; Parser state for tracking token position
(struct parser-state (tokens position) #:transparent)

;; Parser state operations
(define/contract (make-parser-state tokens)
  (-> (listof token?) parser-state?)
  (parser-state tokens 0))

(define/contract (current-token state)
  (-> parser-state? (or/c token? #f))
  (let ([pos (parser-state-position state)]
        [tokens (parser-state-tokens state)])
    (if (>= pos (length tokens))
        #f
        (list-ref tokens pos))))

(define/contract (advance-parser state)
  (-> parser-state? parser-state?)
  (parser-state (parser-state-tokens state)
                (+ (parser-state-position state) 1)))

(define/contract (at-parser-end? state)
  (-> parser-state? boolean?)
  (>= (parser-state-position state) (length (parser-state-tokens state))))

;; Skip comments and whitespace tokens
(define/contract (skip-non-essential state)
  (-> parser-state? parser-state?)
  (let ([token (current-token state)])
    (if (or (not token) 
            (not (or (equal? (token-type token) 'comment)
                    (equal? (token-type token) 'whitespace))))
        state
        (skip-non-essential (advance-parser state)))))

;; Parse atomic expressions
(define/contract (parse-atom state)
  (-> parser-state? (values ast-node/c parser-state?))
  (let ([state (skip-non-essential state)])
    (let ([token (current-token state)])
      (if (not token)
          (error "Unexpected end of input while parsing atom")
          (let ([new-state (advance-parser state)])
            (case (token-type token)
              [(symbol) 
               (values (make-symbol-atom (token-value token)) new-state)]
              [(number) 
               (values (make-number-atom (token-value token)) new-state)]
              [(boolean) 
               (values (make-boolean-atom (token-value token)) new-state)]
              [(string) 
               (values (make-string-atom (token-value token)) new-state)]
              [else 
               (error "Expected atomic expression, got: " (token-type token))]))))))

;; Parse S-expressions (lists)
(define/contract (parse-sexpr state)
  (-> parser-state? (values sexpr? parser-state?))
  (let ([state (skip-non-essential state)])
    (let ([token (current-token state)])
      (if (not token)
          (error "Unexpected end of input while parsing S-expression")
          (if (equal? (token-type token) 'left-paren)
              ;; Parse list contents
              (let ([state (advance-parser state)]) ; skip opening paren
                (let-values ([(elements final-state) (parse-sexpr-elements state)])
                  (values (make-sexpr elements) final-state)))
              (error "Expected opening parenthesis, got: " (token-type token)))))))

;; Parse elements inside an S-expression until closing paren
(define/contract (parse-sexpr-elements state)
  (-> parser-state? (values (listof ast-node/c) parser-state?))
  (let loop ([state (skip-non-essential state)] [elements '()])
    (let ([token (current-token state)])
      (cond
        [(not token) 
         (error "Unexpected end of input, expected closing parenthesis")]
        [(equal? (token-type token) 'right-paren)
         ;; End of S-expression
         (values (reverse elements) (advance-parser state))]
        [(equal? (token-type token) 'eof)
         (error "Unexpected end of file, expected closing parenthesis")]
        [else
         ;; Parse next element
         (let-values ([(element new-state) (parse-expression state)])
           (loop new-state (cons element elements)))]))))

;; Parse any expression (atom or S-expression)
(define/contract (parse-expression state)
  (-> parser-state? (values ast-node/c parser-state?))
  (let ([state (skip-non-essential state)])
    (let ([token (current-token state)])
      (if (not token)
          (error "Unexpected end of input while parsing expression")
          (if (equal? (token-type token) 'left-paren)
              (parse-sexpr state)
              (parse-atom state))))))

;; Main parse function - converts list of tokens to AST
(define/contract (parse tokens)
  (-> (listof token?) ast-node/c)
  ;; Filter out EOF tokens first
  (let ([filtered-tokens (filter (lambda (t) (not (equal? (token-type t) 'eof))) tokens)])
    (if (null? filtered-tokens)
        (make-empty-sexpr)
        (let ([state (make-parser-state filtered-tokens)])
          (let-values ([(ast remaining-state) (parse-expression state)])
            ;; Check that we consumed all tokens (except comments/whitespace)
            (let ([remaining-state (skip-non-essential remaining-state)])
              (if (at-parser-end? remaining-state)
                  ast
                  (error "Unexpected tokens after expression"))))))))