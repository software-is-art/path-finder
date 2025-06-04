#lang racket/base

(require racket/contract
         racket/string
         racket/match
         "tokens.rkt")

(provide tokenize)

;; Lexer state for tracking position
(struct lexer-state (input position line column) #:transparent)

;; Character classification predicates
(define/contract (symbol-start-char? ch)
  (-> char? boolean?)
  (or (char-alphabetic? ch)
      (char=? ch #\+) (char=? ch #\-) (char=? ch #\*) (char=? ch #\/)
      (char=? ch #\=) (char=? ch #\<) (char=? ch #\>) (char=? ch #\!)
      (char=? ch #\?) (char=? ch #\_) (char=? ch #\$)))

(define/contract (symbol-char? ch)
  (-> char? boolean?)
  (or (symbol-start-char? ch)
      (char-numeric? ch)))

(define/contract (whitespace-char? ch)
  (-> char? boolean?)
  (or (char=? ch #\space) (char=? ch #\tab) 
      (char=? ch #\newline) (char=? ch #\return)))

;; Lexer state operations
(define/contract (make-lexer-state input)
  (-> string? lexer-state?)
  (lexer-state input 0 1 1))

(define/contract (advance-position state)
  (-> lexer-state? lexer-state?)
  (let ([ch (current-char state)])
    (if (equal? ch #\newline)
        (lexer-state (lexer-state-input state)
                    (+ (lexer-state-position state) 1)
                    (+ (lexer-state-line state) 1)
                    1)
        (lexer-state (lexer-state-input state)
                    (+ (lexer-state-position state) 1)
                    (lexer-state-line state)
                    (+ (lexer-state-column state) 1)))))

(define/contract (current-char state)
  (-> lexer-state? (or/c char? eof-object?))
  (let ([pos (lexer-state-position state)]
        [input (lexer-state-input state)])
    (if (>= pos (string-length input))
        eof
        (string-ref input pos))))

(define/contract (peek-char state offset)
  (-> lexer-state? exact-nonnegative-integer? (or/c char? eof-object?))
  (let ([pos (+ (lexer-state-position state) offset)]
        [input (lexer-state-input state)])
    (if (>= pos (string-length input))
        eof
        (string-ref input pos))))

(define/contract (at-end? state)
  (-> lexer-state? boolean?)
  (>= (lexer-state-position state) (string-length (lexer-state-input state))))

;; Skip whitespace
(define/contract (skip-whitespace state)
  (-> lexer-state? lexer-state?)
  (let ([ch (current-char state)])
    (if (or (eof-object? ch) (not (whitespace-char? ch)))
        state
        (skip-whitespace (advance-position state)))))

;; Tokenize numbers
(define/contract (tokenize-number state)
  (-> lexer-state? (values token? lexer-state?))
  (let ([start-line (lexer-state-line state)]
        [start-column (lexer-state-column state)])
    (let-values ([(num-str new-state) (read-number-string state)])
      (let ([num (string->number num-str)])
        (if num
            (values (token-number num start-line start-column) new-state)
            (error "Invalid number format: " num-str))))))

(define/contract (read-number-string state)
  (-> lexer-state? (values string? lexer-state?))
  (let loop ([state state] [acc ""])
    (let ([ch (current-char state)])
      (if (or (eof-object? ch) 
              (not (or (char-numeric? ch) (equal? ch #\.))))
          (values acc state)
          (loop (advance-position state) (string-append acc (string ch)))))))

;; Tokenize symbols
(define/contract (tokenize-symbol state)
  (-> lexer-state? (values token? lexer-state?))
  (let ([start-line (lexer-state-line state)]
        [start-column (lexer-state-column state)])
    (let-values ([(symbol-str new-state) (read-symbol-string state)])
      (cond
        [(string=? symbol-str "#t") 
         (values (token-boolean #t start-line start-column) new-state)]
        [(string=? symbol-str "#f") 
         (values (token-boolean #f start-line start-column) new-state)]
        [else 
         (values (token-symbol symbol-str start-line start-column) new-state)]))))

;; Tokenize string literals
(define/contract (tokenize-string state)
  (-> lexer-state? (values token? lexer-state?))
  (let ([start-line (lexer-state-line state)]
        [start-column (lexer-state-column state)])
    ;; Skip opening quote
    (let ([state (advance-position state)])
      (let-values ([(str-content new-state) (read-string-content state)])
        ;; Skip closing quote
        (let ([final-state (advance-position new-state)])
          (values (token-string str-content start-line start-column) final-state))))))

(define/contract (read-string-content state)
  (-> lexer-state? (values string? lexer-state?))
  (let loop ([state state] [acc ""])
    (let ([ch (current-char state)])
      (cond
        [(eof-object? ch) 
         (error "Unterminated string literal")]
        [(char=? ch #\") 
         (values acc state)]
        [(char=? ch #\\)
         ;; Handle escape sequences
         (let ([next-state (advance-position state)])
           (let ([escaped-ch (current-char next-state)])
             (cond
               [(eof-object? escaped-ch) 
                (error "Unterminated string literal")]
               [(char=? escaped-ch #\n) 
                (loop (advance-position next-state) (string-append acc "\n"))]
               [(char=? escaped-ch #\t) 
                (loop (advance-position next-state) (string-append acc "\t"))]
               [(char=? escaped-ch #\r) 
                (loop (advance-position next-state) (string-append acc "\r"))]
               [(char=? escaped-ch #\\) 
                (loop (advance-position next-state) (string-append acc "\\"))]
               [(char=? escaped-ch #\") 
                (loop (advance-position next-state) (string-append acc "\""))]
               [else 
                (loop (advance-position next-state) (string-append acc (string escaped-ch)))])))]
        [else 
         (loop (advance-position state) (string-append acc (string ch)))]))))

;; Tokenize comments
(define/contract (tokenize-comment state)
  (-> lexer-state? (values token? lexer-state?))
  (let ([start-line (lexer-state-line state)]
        [start-column (lexer-state-column state)])
    (let-values ([(comment-content new-state) (read-comment-content state)])
      (values (token-comment comment-content start-line start-column) new-state))))

(define/contract (read-comment-content state)
  (-> lexer-state? (values string? lexer-state?))
  (let loop ([state state] [acc ""])
    (let ([ch (current-char state)])
      (cond
        [(or (eof-object? ch) (char=? ch #\newline))
         (values acc state)]
        [else 
         (loop (advance-position state) (string-append acc (string ch)))]))))

(define/contract (read-symbol-string state)
  (-> lexer-state? (values string? lexer-state?))
  (let loop ([state state] [acc ""])
    (let ([ch (current-char state)])
      (if (or (eof-object? ch) 
              (not (or (symbol-char? ch) (and (char=? ch #\#) (= (string-length acc) 0)))))
          (values acc state)
          (loop (advance-position state) (string-append acc (string ch)))))))

;; Main tokenization function
(define/contract (tokenize-next state)
  (-> lexer-state? (values token? lexer-state?))
  (let ([state (skip-whitespace state)])
    (let ([ch (current-char state)]
          [line (lexer-state-line state)]
          [column (lexer-state-column state)])
      (cond
        [(eof-object? ch) 
         (values (token-eof line column) state)]
        [(equal? ch #\() 
         (values (token-left-paren line column) (advance-position state))]
        [(equal? ch #\)) 
         (values (token-right-paren line column) (advance-position state))]
        [(equal? ch #\") 
         (tokenize-string state)]
        [(equal? ch #\;) 
         (tokenize-comment state)]
        [(char-numeric? ch) 
         (tokenize-number state)]
        [(or (symbol-start-char? ch) (equal? ch #\#))
         (tokenize-symbol state)]
        [else 
         (error "Unexpected character: " ch " at line " line " column " column)]))))

;; Main tokenize function - converts input string to list of tokens
(define/contract (tokenize input)
  (-> string? (listof token?))
  (let loop ([state (make-lexer-state input)] [tokens '()])
    (let-values ([(token new-state) (tokenize-next state)])
      (if (equal? (token-type token) 'eof)
          (reverse (cons token tokens))
          (loop new-state (cons token tokens))))))