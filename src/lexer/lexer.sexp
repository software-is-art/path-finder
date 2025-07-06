;; ============================================================================
;; S-EXPRESSION LEXER FOR PATHFINDER
;; ============================================================================
;; Simple tokenizer for S-expressions to enable self-hosting

(import core foundations)
(import types string)
(import types list)

;; ============================================================================
;; TOKEN TYPES
;; ============================================================================

(data SExpToken U0
  ;; Delimiters
  (case lparen-token SExpToken)
  (case rparen-token SExpToken)
  
  ;; Atoms
  (case symbol-token (-> String SExpToken))
  (case number-token (-> Nat SExpToken))
  (case string-token (-> String SExpToken))
  
  ;; Special
  (case quote-token SExpToken)
  (case eof-token SExpToken))

;; ============================================================================
;; LEXER STATE
;; ============================================================================

(data LexerState U0
  (case lexer-state 
    (-> (input : String)
        (position : Nat)
        (line : Nat)
        (column : Nat)
        LexerState)))

;; ============================================================================
;; CHARACTER CLASSIFICATION
;; ============================================================================

;; Check if character is whitespace
(define is-whitespace?
  (fn (ch)
    (match ch
      (case #\space true)
      (case #\newline true)
      (case #\tab true)
      (case #\return true)
      (case _ false))))

;; Check if character is delimiter
(define is-delimiter?
  (fn (ch)
    (match ch
      (case #\( true)
      (case #\) true)
      (case _ (is-whitespace? ch)))))

;; Check if character starts a symbol
(define is-symbol-start?
  (fn (ch)
    (bool-elim (fn (_) Bool)
               ;; Not delimiter
               (bool-elim (fn (_) Bool)
                          ;; Not digit
                          true
                          false
                          (is-digit? ch))
               false
               (is-delimiter? ch))))

;; Check if character continues a symbol
(define is-symbol-continue?
  (fn (ch)
    (not (is-delimiter? ch))))

;; Check if character is digit
(define is-digit?
  (fn (ch)
    (match ch
      (case #\0 true) (case #\1 true) (case #\2 true) (case #\3 true) (case #\4 true)
      (case #\5 true) (case #\6 true) (case #\7 true) (case #\8 true) (case #\9 true)
      (case _ false))))

;; ============================================================================
;; LEXER HELPERS
;; ============================================================================

;; Get current character
(define current-char
  (fn (state)
    (match state
      (case (lexer-state input pos line col)
        (string-char-at input pos)))))

;; Advance position
(define advance
  (fn (state)
    (match state
      (case (lexer-state input pos line col)
        (let ((ch (current-char state)))
          (match ch
            (case (some #\newline)
              (lexer-state input (succ pos) (succ line) zero))
            (case _
              (lexer-state input (succ pos) line (succ col)))))))))

;; Skip whitespace
(define skip-whitespace
  (fn (state)
    (match (current-char state)
      (case none state)
      (case (some ch)
        (if (is-whitespace? ch)
            (skip-whitespace (advance state))
            state)))))

;; Skip comment (from ; to end of line)
(define skip-comment
  (fn (state)
    (match (current-char state)
      (case none state)
      (case (some ch)
        (match ch
          (case #\newline (advance state))
          (case _ (skip-comment (advance state))))))))

;; Read while predicate holds
(define read-while
  (fn (pred state acc)
    (match (current-char state)
      (case none (pair (reverse-string acc) state))
      (case (some ch)
        (if (pred ch)
            (read-while pred (advance state) (string-cons ch acc))
            (pair (reverse-string acc) state))))))

;; Read string literal
(define read-string
  (fn (state)
    ;; Skip opening quote
    (let ((state1 (advance state)))
      (read-string-chars state1 empty-string))))

(define read-string-chars
  (fn (state acc)
    (match (current-char state)
      (case none (error "Unterminated string"))
      (case (some ch)
        (match ch
          ;; Closing quote
          (case #\" 
            (pair (reverse-string acc) (advance state)))
          ;; Escape sequence
          (case #\\
            (let ((state1 (advance state)))
              (match (current-char state1)
                (case none (error "Unterminated string escape"))
                (case (some esc-ch)
                  (let ((actual-ch (match esc-ch
                                     (case #\n #\newline)
                                     (case #\t #\tab)
                                     (case #\" #\")
                                     (case #\\ #\\)
                                     (case _ esc-ch))))
                    (read-string-chars (advance state1) 
                                      (string-cons actual-ch acc)))))))
          ;; Regular character
          (case _
            (read-string-chars (advance state) (string-cons ch acc))))))))

;; Read number
(define read-number
  (fn (state)
    (let ((result (read-while is-digit? state empty-string)))
      (match result
        (case (pair digits new-state)
          (pair (string-to-nat digits) new-state))))))

;; Read symbol
(define read-symbol
  (fn (state)
    (read-while is-symbol-continue? state empty-string)))

;; ============================================================================
;; MAIN LEXER
;; ============================================================================

;; Get next token
(define next-token
  (fn (state)
    ;; Skip whitespace and comments
    (let ((state1 (skip-whitespace state)))
      (match (current-char state1)
        ;; EOF
        (case none (pair eof-token state1))
        
        ;; Character dispatch
        (case (some ch)
          (match ch
            ;; Comment
            (case #\;
              (next-token (skip-comment state1)))
            
            ;; Parentheses
            (case #\( (pair lparen-token (advance state1)))
            (case #\) (pair rparen-token (advance state1)))
            
            ;; Quote
            (case #\' (pair quote-token (advance state1)))
            
            ;; String
            (case #\"
              (match (read-string state1)
                (case (pair str new-state)
                  (pair (string-token str) new-state))))
            
            ;; Number or symbol
            (case _
              (if (is-digit? ch)
                  ;; Number
                  (match (read-number state1)
                    (case (pair n new-state)
                      (pair (number-token n) new-state)))
                  ;; Symbol
                  (match (read-symbol state1)
                    (case (pair sym new-state)
                      (pair (symbol-token sym) new-state)))))))))))

;; Tokenize entire input
(define tokenize
  (fn (input)
    (tokenize-helper (lexer-state input zero zero zero) nil)))

(define tokenize-helper
  (fn (state tokens)
    (match (next-token state)
      (case (pair eof-token _)
        (reverse tokens))
      (case (pair token new-state)
        (tokenize-helper new-state (cons token tokens))))))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export SExpToken)
(export tokenize)
(export lparen-token)
(export rparen-token)
(export symbol-token)
(export number-token)
(export string-token)
(export quote-token)
(export eof-token)