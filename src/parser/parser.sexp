;; ============================================================================
;; S-EXPRESSION PARSER FOR PATHFINDER
;; ============================================================================
;; Parse tokenized S-expressions into structured data

(import core foundations)
(import types list)
(import lexer lexer)

;; ============================================================================
;; S-EXPRESSION DATA TYPE
;; ============================================================================

(data SExpr U0
  (case atom (-> String SExpr))
  (case number (-> Nat SExpr))
  (case string (-> String SExpr))
  (case list (-> (List SExpr) SExpr))
  (case quoted (-> SExpr SExpr)))

;; ============================================================================
;; PARSER STATE
;; ============================================================================

(data ParseState U0
  (case parse-state
    (-> (tokens : List SExpToken)
        (position : Nat)
        ParseState)))

;; Parser result
(data ParseResult U0
  (case parse-success (-> SExpr ParseState ParseResult))
  (case parse-error (-> String ParseResult)))

;; ============================================================================
;; PARSER HELPERS
;; ============================================================================

;; Get current token
(define current-token
  (fn (state)
    (match state
      (case (parse-state tokens pos)
        (list-nth tokens pos)))))

;; Advance to next token
(define advance-token
  (fn (state)
    (match state
      (case (parse-state tokens pos)
        (parse-state tokens (succ pos))))))

;; Check if at end
(define at-end?
  (fn (state)
    (match (current-token state)
      (case none true)
      (case (some tok)
        (match tok
          (case eof-token true)
          (case _ false))))))

;; ============================================================================
;; MAIN PARSER
;; ============================================================================

;; Parse a single S-expression
(define parse-sexpr
  (fn (state)
    (match (current-token state)
      (case none 
        (parse-error "Unexpected end of input"))
      
      (case (some token)
        (match token
          ;; Left paren - parse list
          (case lparen-token
            (parse-list (advance-token state)))
          
          ;; Symbol
          (case (symbol-token sym)
            (parse-success (atom sym) (advance-token state)))
          
          ;; Number
          (case (number-token n)
            (parse-success (number n) (advance-token state)))
          
          ;; String
          (case (string-token s)
            (parse-success (string s) (advance-token state)))
          
          ;; Quote
          (case quote-token
            (match (parse-sexpr (advance-token state))
              (case (parse-success expr new-state)
                (parse-success (quoted expr) new-state))
              (case err err)))
          
          ;; Errors
          (case rparen-token
            (parse-error "Unexpected right parenthesis"))
          
          (case eof-token
            (parse-error "Unexpected end of file")))))))

;; Parse a list
(define parse-list
  (fn (state)
    (parse-list-elements state nil)))

(define parse-list-elements
  (fn (state elements)
    (match (current-token state)
      (case none
        (parse-error "Unterminated list"))
      
      (case (some token)
        (match token
          ;; Right paren - end of list
          (case rparen-token
            (parse-success (list (reverse elements)) (advance-token state)))
          
          ;; EOF - error
          (case eof-token
            (parse-error "Unterminated list at end of file"))
          
          ;; Otherwise parse element
          (case _
            (match (parse-sexpr state)
              (case (parse-success elem new-state)
                (parse-list-elements new-state (cons elem elements)))
              (case err err))))))))

;; ============================================================================
;; TOP-LEVEL PARSER
;; ============================================================================

;; Parse multiple S-expressions (a file)
(define parse-file
  (fn (tokens)
    (parse-file-helper (parse-state tokens zero) nil)))

(define parse-file-helper
  (fn (state exprs)
    (if (at-end? state)
        (ok (reverse exprs))
        (match (parse-sexpr state)
          (case (parse-success expr new-state)
            (parse-file-helper new-state (cons expr exprs)))
          (case (parse-error msg)
            (error msg))))))

;; Parse a single expression from string
(define parse-string
  (fn (input)
    (let ((tokens (tokenize input)))
      (match (parse-sexpr (parse-state tokens zero))
        (case (parse-success expr _) (ok expr))
        (case (parse-error msg) (error msg))))))

;; ============================================================================
;; S-EXPRESSION UTILITIES
;; ============================================================================

;; Check if S-expression is atom with given name
(define is-atom?
  (fn (sexpr name)
    (match sexpr
      (case (atom s) (string-equal? s name))
      (case _ false))))

;; Get atom name (if atom)
(define get-atom
  (fn (sexpr)
    (match sexpr
      (case (atom s) (some s))
      (case _ none))))

;; Get list elements (if list)
(define get-list
  (fn (sexpr)
    (match sexpr
      (case (list elems) (some elems))
      (case _ none))))

;; Pattern match on list structure
(define match-list
  (fn (sexpr)
    (match sexpr
      (case (list elems) elems)
      (case _ nil))))

;; ============================================================================
;; RESULT TYPE
;; ============================================================================

(data Result U0
  (case ok (-> SExpr Result))
  (case error (-> String Result)))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export SExpr)
(export atom)
(export number) 
(export string)
(export list)
(export quoted)
(export parse-string)
(export parse-file)
(export is-atom?)
(export get-atom)
(export get-list)
(export match-list)
(export Result)
(export ok)
(export error)