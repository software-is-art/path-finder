;; ============================================================================
;; PURE MATHEMATICAL HOTT-NATIVE STRING TYPE (S-EXPRESSION VERSION)
;; ============================================================================
;; Strings as inductive types: String = EmptyString | StringCons Char String
;; Characters as Unicode codepoints: Char = MkChar Nat

(import types types)
(import evaluator values)

;; ============================================================================
;; CHARACTER TYPE DEFINITION
;; ============================================================================

;; Character type: Unicode codepoint
(data Char U0
  (case char (-> Nat Char)))

;; Character constructor
(type make-char (-> Nat Char))
(define make-char
  (fn (codepoint) (char codepoint)))

;; Get codepoint from character
(type char-codepoint (-> Char Nat))
(define char-codepoint
  (fn (ch)
    (match ch
      (case (char cp) cp))))

;; ============================================================================
;; STRING TYPE DEFINITION
;; ============================================================================

;; String type: inductive definition
(data String U0
  (case empty-string String)
  (case string-cons (-> Char String String)))

;; ============================================================================
;; STRING CONSTRUCTORS
;; ============================================================================

;; Create a single-character string
(type make-singleton-string (-> Char String))
(define make-singleton-string
  (fn (ch) (string-cons ch empty-string)))

;; ============================================================================
;; STRING OPERATIONS
;; ============================================================================

;; String length
(type string-length (-> String Nat))
(define string-length
  (fn (str)
    (match str
      (case empty-string zero)
      (case (string-cons _ rest) (succ (string-length rest))))))

;; String append
(type string-append (-> String String String))
(define string-append
  (fn (s1 s2)
    (match s1
      (case empty-string s2)
      (case (string-cons ch rest) 
        (string-cons ch (string-append rest s2))))))

;; String equality
(type string-equal? (-> String String Bool))
(define string-equal?
  (fn (s1 s2)
    (match (pair s1 s2)
      (case (pair empty-string empty-string) true)
      (case (pair (string-cons c1 r1) (string-cons c2 r2))
        (and (char-equal? c1 c2) (string-equal? r1 r2)))
      (case _ false))))

;; Character equality
(type char-equal? (-> Char Char Bool))
(define char-equal?
  (fn (c1 c2)
    (nat-equal? (char-codepoint c1) (char-codepoint c2))))

;; ============================================================================
;; STRING CONVERSION
;; ============================================================================

;; Convert string to list of characters
(type string->list (-> String (List Char)))
(define string->list
  (fn (str)
    (match str
      (case empty-string nil)
      (case (string-cons ch rest)
        (cons ch (string->list rest))))))

;; Convert list of characters to string
(type list->string (-> (List Char) String))
(define list->string
  (fn (chars)
    (match chars
      (case nil empty-string)
      (case (cons ch rest)
        (string-cons ch (list->string rest))))))

;; ============================================================================
;; STRING UTILITIES
;; ============================================================================

;; String reverse
(type string-reverse (-> String String))
(define string-reverse
  (fn (str)
    (string-reverse-acc str empty-string)))

(type string-reverse-acc (-> String String String))
(define string-reverse-acc
  (fn (str acc)
    (match str
      (case empty-string acc)
      (case (string-cons ch rest)
        (string-reverse-acc rest (string-cons ch acc))))))

;; Substring (take first n characters)
(type string-take (-> Nat String String))
(define string-take
  (fn (n str)
    (match (pair n str)
      (case (pair zero _) empty-string)
      (case (pair _ empty-string) empty-string)
      (case (pair (succ m) (string-cons ch rest))
        (string-cons ch (string-take m rest))))))

;; Drop first n characters
(type string-drop (-> Nat String String))
(define string-drop
  (fn (n str)
    (match (pair n str)
      (case (pair zero s) s)
      (case (pair _ empty-string) empty-string)
      (case (pair (succ m) (string-cons _ rest))
        (string-drop m rest)))))

;; ============================================================================
;; CHARACTER UTILITIES
;; ============================================================================

;; Common character constants
(define char-space (make-char 32))
(define char-newline (make-char 10))
(define char-tab (make-char 9))
(define char-zero (make-char 48))
(define char-nine (make-char 57))
(define char-A (make-char 65))
(define char-Z (make-char 90))
(define char-a (make-char 97))
(define char-z (make-char 122))

;; Character predicates
(type is-digit? (-> Char Bool))
(define is-digit?
  (fn (ch)
    (let ((cp (char-codepoint ch)))
      (and (nat-le? (char-codepoint char-zero) cp)
           (nat-le? cp (char-codepoint char-nine))))))

(type is-alpha? (-> Char Bool))
(define is-alpha?
  (fn (ch)
    (let ((cp (char-codepoint ch)))
      (or (and (nat-le? (char-codepoint char-A) cp)
               (nat-le? cp (char-codepoint char-Z)))
          (and (nat-le? (char-codepoint char-a) cp)
               (nat-le? cp (char-codepoint char-z)))))))

(type is-whitespace? (-> Char Bool))
(define is-whitespace?
  (fn (ch)
    (or (char-equal? ch char-space)
        (char-equal? ch char-newline)
        (char-equal? ch char-tab))))

;; ============================================================================
;; STRING FOLDING
;; ============================================================================

;; Fold left over string
(type string-fold-left (-> (-> A Char A) A String A))
(define string-fold-left
  (fn (f init str)
    (match str
      (case empty-string init)
      (case (string-cons ch rest)
        (string-fold-left f (f init ch) rest)))))

;; Fold right over string
(type string-fold-right (-> (-> Char A A) A String A))
(define string-fold-right
  (fn (f init str)
    (match str
      (case empty-string init)
      (case (string-cons ch rest)
        (f ch (string-fold-right f init rest))))))

;; ============================================================================
;; STRING VALUES
;; ============================================================================

;; Convert HoTT string to runtime value
(type string-to-value (-> String Value))
(define string-to-value
  (fn (str)
    (string-value (string-to-host str))))

;; Convert string to host representation (placeholder)
(type string-to-host (-> String String))
(define string-to-host
  (fn (str) str)) ;; Identity for now

;; Create string value from literal
(type make-string-value (-> String Value))
(define make-string-value
  (fn (s) (string-value s)))