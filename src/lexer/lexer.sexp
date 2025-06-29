;; ============================================================================
;; PURE HOTT LEXER IMPLEMENTATION (S-EXPRESSION VERSION)
;; ============================================================================
;; Mathematical lexical analysis using only HoTT constructors
;; Character-by-character tokenization with formal state transitions

(import effects effects)
(import core operations)
(import types string)

;; ============================================================================
;; TOKEN TYPES
;; ============================================================================

;; Complete token classification
(data TokenType U0
  ;; Delimiters
  (case lparen TokenType)
  (case rparen TokenType)
  (case lbracket TokenType)
  (case rbracket TokenType)
  (case lbrace TokenType)
  (case rbrace TokenType)
  
  ;; Operators and symbols
  (case plus TokenType)
  (case minus TokenType)
  (case multiply TokenType)
  (case divide TokenType)
  (case equal TokenType)
  (case less-than TokenType)
  (case greater-than TokenType)
  (case arrow TokenType)
  (case double-arrow TokenType)
  (case colon TokenType)
  (case semicolon TokenType)
  (case comma TokenType)
  (case dot TokenType)
  (case pipe TokenType)
  
  ;; Keywords
  (case lambda-kw TokenType)
  (case fn-kw TokenType)
  (case let-kw TokenType)
  (case in-kw TokenType)
  (case if-kw TokenType)
  (case then-kw TokenType)
  (case else-kw TokenType)
  (case match-kw TokenType)
  (case case-kw TokenType)
  (case data-kw TokenType)
  (case where-kw TokenType)
  (case import-kw TokenType)
  (case type-kw TokenType)
  (case define-kw TokenType)
  
  ;; Literals
  (case number-lit TokenType)
  (case string-lit TokenType)
  (case char-lit TokenType)
  (case bool-lit TokenType)
  
  ;; Identifiers and special
  (case identifier TokenType)
  (case comment TokenType)
  (case whitespace TokenType)
  (case eof TokenType))

;; Token with position information
(data Token U0
  (case token (-> TokenType String Nat Nat Token)))

;; ============================================================================
;; LEXER STATE
;; ============================================================================

;; Lexer state for stateful tokenization
(data LexerState U0
  (case lexer-state (-> String      ;; input
                       Nat          ;; position
                       Nat          ;; line
                       Nat          ;; column
                       (List Token) ;; tokens so far
                       LexerState)))

;; ============================================================================
;; CHARACTER CLASSIFICATION
;; ============================================================================

;; Check if character is operator
(type is-operator? (-> Char Bool))
(define is-operator?
  (fn (ch)
    (member? ch (list char-plus char-minus char-star char-slash
                     char-equal char-less char-greater))))

;; Check if character is delimiter
(type is-delimiter? (-> Char Bool))
(define is-delimiter?
  (fn (ch)
    (member? ch (list char-lparen char-rparen 
                     char-lbracket char-rbracket
                     char-lbrace char-rbrace))))

;; Character constants
(define char-plus (make-char 43))      ;; +
(define char-minus (make-char 45))     ;; -
(define char-star (make-char 42))      ;; *
(define char-slash (make-char 47))     ;; /
(define char-equal (make-char 61))     ;; =
(define char-less (make-char 60))      ;; <
(define char-greater (make-char 62))   ;; >
(define char-lparen (make-char 40))    ;; (
(define char-rparen (make-char 41))    ;; )
(define char-lbracket (make-char 91))  ;; [
(define char-rbracket (make-char 93))  ;; ]
(define char-lbrace (make-char 123))   ;; {
(define char-rbrace (make-char 125))   ;; }
(define char-semicolon (make-char 59)) ;; ;
(define char-colon (make-char 58))     ;; :
(define char-comma (make-char 44))     ;; ,
(define char-dot (make-char 46))       ;; .
(define char-pipe (make-char 124))     ;; |
(define char-quote (make-char 34))     ;; "
(define char-underscore (make-char 95)) ;; _

;; ============================================================================
;; TOKENIZATION
;; ============================================================================

;; Main tokenization function
(type tokenize (-> String (Effect (List Token))))
(define tokenize
  (fn (input)
    (let ((initial-state (lexer-state input zero zero zero nil)))
      (>>= (tokenize-loop initial-state)
           (fn (final-state)
             (match final-state
               (case (lexer-state _ _ _ _ tokens)
                 (pure (reverse tokens)))))))))

;; Tokenization loop
(type tokenize-loop (-> LexerState (Effect LexerState)))
(define tokenize-loop
  (fn (state)
    (match state
      (case (lexer-state input pos line col tokens)
        (if (>= pos (string-length input))
            ;; End of input
            (pure (lexer-state input pos line col 
                              (cons (token eof "" line col) tokens)))
            ;; Process next character
            (let ((ch (string-nth input pos)))
              (cond
                ;; Whitespace
                ((is-whitespace? ch)
                 (tokenize-loop (advance-whitespace state ch)))
                
                ;; Comments
                ((and (char-equal? ch char-semicolon)
                      (char-equal? (string-nth-or input (succ pos) char-space) 
                                  char-semicolon))
                 (tokenize-comment state))
                
                ;; String literals
                ((char-equal? ch char-quote)
                 (tokenize-string state))
                
                ;; Numbers
                ((is-digit? ch)
                 (tokenize-number state))
                
                ;; Identifiers and keywords
                ((or (is-alpha? ch) (char-equal? ch char-underscore))
                 (tokenize-identifier state))
                
                ;; Operators
                ((is-operator? ch)
                 (tokenize-operator state))
                
                ;; Delimiters
                ((is-delimiter? ch)
                 (tokenize-delimiter state))
                
                ;; Special symbols
                ((member? ch (list char-colon char-comma char-dot char-pipe))
                 (tokenize-symbol state))
                
                ;; Unknown character
                (else
                 (error "Unknown character")))))))))

;; Tokenize whitespace
(type advance-whitespace (-> LexerState Char LexerState))
(define advance-whitespace
  (fn (state ch)
    (match state
      (case (lexer-state input pos line col tokens)
        (if (char-equal? ch char-newline)
            (lexer-state input (succ pos) (succ line) zero tokens)
            (lexer-state input (succ pos) line (succ col) tokens))))))

;; Tokenize comment
(type tokenize-comment (-> LexerState (Effect LexerState)))
(define tokenize-comment
  (fn (state)
    (match state
      (case (lexer-state input pos line col tokens)
        (let ((comment-end (find-newline input pos)))
          (let ((comment-text (string-substring input pos comment-end)))
            (pure (lexer-state input comment-end (succ line) zero
                              (cons (token comment comment-text line col) 
                                   tokens)))))))))

;; Tokenize string literal
(type tokenize-string (-> LexerState (Effect LexerState)))
(define tokenize-string
  (fn (state)
    (match state
      (case (lexer-state input pos line col tokens)
        (let ((string-end (find-closing-quote input (succ pos))))
          (if (= string-end (string-length input))
              (error "Unterminated string")
              (let ((string-content (string-substring input (succ pos) string-end)))
                (pure (lexer-state input (succ string-end) line 
                                  (+ col (- string-end pos))
                                  (cons (token string-lit string-content line col)
                                       tokens))))))))))

;; Tokenize number
(type tokenize-number (-> LexerState (Effect LexerState)))
(define tokenize-number
  (fn (state)
    (match state
      (case (lexer-state input pos line col tokens)
        (let ((num-end (find-non-digit input pos)))
          (let ((num-text (string-substring input pos num-end)))
            (pure (lexer-state input num-end line (+ col (- num-end pos))
                              (cons (token number-lit num-text line col)
                                   tokens)))))))))

;; Tokenize identifier or keyword
(type tokenize-identifier (-> LexerState (Effect LexerState)))
(define tokenize-identifier
  (fn (state)
    (match state
      (case (lexer-state input pos line col tokens)
        (let ((id-end (find-non-alnum input pos)))
          (let ((id-text (string-substring input pos id-end)))
            (let ((tok-type (classify-identifier id-text)))
              (pure (lexer-state input id-end line (+ col (- id-end pos))
                                (cons (token tok-type id-text line col)
                                     tokens))))))))))

;; Classify identifier as keyword or identifier
(type classify-identifier (-> String TokenType))
(define classify-identifier
  (fn (text)
    (cond
      ((string-equal? text "fn") fn-kw)
      ((string-equal? text "lambda") lambda-kw)
      ((string-equal? text "let") let-kw)
      ((string-equal? text "in") in-kw)
      ((string-equal? text "if") if-kw)
      ((string-equal? text "then") then-kw)
      ((string-equal? text "else") else-kw)
      ((string-equal? text "match") match-kw)
      ((string-equal? text "case") case-kw)
      ((string-equal? text "data") data-kw)
      ((string-equal? text "where") where-kw)
      ((string-equal? text "import") import-kw)
      ((string-equal? text "type") type-kw)
      ((string-equal? text "define") define-kw)
      ((string-equal? text "true") bool-lit)
      ((string-equal? text "false") bool-lit)
      (else identifier))))

;; ============================================================================
;; HELPER FUNCTIONS
;; ============================================================================

;; Get nth character or default
(type string-nth-or (-> String Nat Char Char))
(define string-nth-or
  (fn (str n default)
    (if (< n (string-length str))
        (string-nth str n)
        default)))

;; Get nth character
(type string-nth (-> String Nat Char))
(define string-nth
  (fn (str n)
    (match (string-drop n str)
      (case empty-string (error "Index out of bounds"))
      (case (string-cons ch _) ch))))

;; Find position of newline
(type find-newline (-> String Nat Nat))
(define find-newline
  (fn (str pos)
    (find-char str pos char-newline)))

;; Find position of character
(type find-char (-> String Nat Char Nat))
(define find-char
  (fn (str pos ch)
    (if (>= pos (string-length str))
        (string-length str)
        (if (char-equal? (string-nth str pos) ch)
            pos
            (find-char str (succ pos) ch)))))

;; String substring
(type string-substring (-> String Nat Nat String))
(define string-substring
  (fn (str start end)
    (string-take (- end start) (string-drop start str))))