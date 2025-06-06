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

;; Parse S-expressions (lists) - with special form detection
(define/contract (parse-sexpr state)
  (-> parser-state? (values ast-node/c parser-state?))
  (let ([state (skip-non-essential state)])
    (let ([token (current-token state)])
      (if (not token)
          (error "Unexpected end of input while parsing S-expression")
          (if (equal? (token-type token) 'left-paren)
              ;; Look ahead to see if this is a special form
              (let ([state-after-paren (advance-parser state)])
                (let ([next-token (current-token (skip-non-essential state-after-paren))])
                  (if (and next-token 
                           (equal? (token-type next-token) 'symbol)
                           (string=? (token-value next-token) "match"))
                      ;; Parse match expression
                      (parse-match-expr state-after-paren)
                      ;; Parse regular S-expression
                      (let-values ([(elements final-state) (parse-sexpr-elements state-after-paren)])
                        (values (make-sexpr elements) final-state)))))
              (error "Expected opening parenthesis, got: " (token-type token)))))))

;; ============================================================================
;; PATTERN MATCHING PARSER FUNCTIONS
;; ============================================================================

;; Parse match expression: (match scrutinee (pattern body) ...)
(define/contract (parse-match-expr state)
  (-> parser-state? (values match-expr? parser-state?))
  (let ([state (skip-non-essential state)])
    ;; Expect and consume "match" keyword
    (let ([token (current-token state)])
      (if (and token (equal? (token-type token) 'symbol) (string=? (token-value token) "match"))
          (let ([state (advance-parser state)])
            ;; Parse scrutinee expression
            (let-values ([(scrutinee state) (parse-expression state)])
              ;; Parse match cases until closing paren
              (let-values ([(cases state) (parse-match-cases state)])
                (values (make-match-expr scrutinee cases) state))))
          (error "Expected 'match' keyword")))))

;; Parse match cases: (pattern body) ...
(define/contract (parse-match-cases state)
  (-> parser-state? (values (listof match-case?) parser-state?))
  (let loop ([state (skip-non-essential state)] [cases '()])
    (let ([token (current-token state)])
      (cond
        [(not token) 
         (error "Unexpected end of input, expected closing parenthesis or match case")]
        [(equal? (token-type token) 'right-paren)
         ;; End of match expression
         (values (reverse cases) (advance-parser state))]
        [(equal? (token-type token) 'left-paren)
         ;; Parse match case
         (let-values ([(match-case new-state) (parse-match-case state)])
           (loop new-state (cons match-case cases)))]
        [else
         (error "Expected match case (starting with '(') or closing parenthesis")]))))

;; Parse single match case: (pattern body)
(define/contract (parse-match-case state)
  (-> parser-state? (values match-case? parser-state?))
  (let ([state (skip-non-essential state)])
    (let ([token (current-token state)])
      (if (equal? (token-type token) 'left-paren)
          (let ([state (advance-parser state)]) ; skip opening paren
            ;; Parse pattern
            (let-values ([(pattern state) (parse-pattern state)])
              ;; Parse body expression
              (let-values ([(body state) (parse-expression state)])
                ;; Expect closing paren
                (let ([state (skip-non-essential state)])
                  (let ([token (current-token state)])
                    (if (equal? (token-type token) 'right-paren)
                        (values (make-match-case pattern body) (advance-parser state))
                        (error "Expected closing parenthesis after match case")))))))
          (error "Expected opening parenthesis for match case")))))

;; Parse pattern
(define/contract (parse-pattern state)
  (-> parser-state? (values pattern-node/c parser-state?))
  (let ([state (skip-non-essential state)])
    (let ([token (current-token state)])
      (cond
        [(not token) (error "Unexpected end of input while parsing pattern")]
        
        ;; Wildcard pattern: _
        [(and (equal? (token-type token) 'symbol) (string=? (token-value token) "_"))
         (values (make-wildcard-pattern) (advance-parser state))]
        
        ;; Literal number pattern
        [(equal? (token-type token) 'number)
         (values (make-literal-pattern (token-value token)) (advance-parser state))]
        
        ;; Literal boolean pattern  
        [(equal? (token-type token) 'boolean)
         (values (make-literal-pattern (token-value token)) (advance-parser state))]
        
        ;; Literal string pattern
        [(equal? (token-type token) 'string)
         (values (make-literal-pattern (token-value token)) (advance-parser state))]
        
        ;; Variable pattern (symbol)
        [(equal? (token-type token) 'symbol)
         (values (make-variable-pattern (token-value token)) (advance-parser state))]
        
        ;; Constructor pattern: (constructor-name sub-patterns...)
        [(equal? (token-type token) 'left-paren)
         (parse-constructor-pattern state)]
        
        [else (error "Invalid pattern: " (token-type token))]))))

;; Parse constructor pattern: (name pattern1 pattern2 ...)
(define/contract (parse-constructor-pattern state)
  (-> parser-state? (values pattern-node/c parser-state?))
  (let ([state (advance-parser state)]) ; skip opening paren
    (let ([state (skip-non-essential state)])
      (let ([token (current-token state)])
        (if (equal? (token-type token) 'symbol)
            (let ([constructor-name (token-value token)]
                  [state (advance-parser state)])
              ;; Check for special HoTT patterns
              (cond
                [else
                 ;; All patterns now use general constructor pattern mechanism
                 ;; This handles zero, true, false, none, some, successor, etc. uniformly
                 (let-values ([(sub-patterns state) (parse-sub-patterns state)])
                   (values (make-constructor-pattern constructor-name sub-patterns) state))]))
            (error "Expected constructor name in pattern"))))))

;; Parse sub-patterns until closing paren
(define/contract (parse-sub-patterns state)
  (-> parser-state? (values (listof pattern-node/c) parser-state?))
  (let loop ([state (skip-non-essential state)] [patterns '()])
    (let ([token (current-token state)])
      (cond
        [(not token) 
         (error "Unexpected end of input, expected closing parenthesis")]
        [(equal? (token-type token) 'right-paren)
         ;; End of constructor pattern
         (values (reverse patterns) (advance-parser state))]
        [else
         ;; Parse next sub-pattern
         (let-values ([(pattern new-state) (parse-pattern state)])
           (loop new-state (cons pattern patterns)))]))))

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