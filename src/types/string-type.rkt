#lang racket/base

(require racket/contract
         racket/match
         "types.rkt"
         "../evaluator/values.rkt")

(provide (all-defined-out))

;; ============================================================================
;; HOTT-NATIVE STRING TYPE DEFINITION
;; ============================================================================
;; Strings as inductive types: String = EmptyString | StringCons Char String

;; Character type: Char = MkChar Nat (where Nat is the Unicode codepoint)
(define Char-constructors
  (list (type-constructor "char" (list Nat) 'Char)))

(define Char (inductive-type "Char" Char-constructors))

;; String type: String = EmptyString | StringCons Char String  
(define String-constructors
  (list (type-constructor "empty-string" '() 'String)
        (type-constructor "string-cons" (list Char 'String) 'String)))

(define String (inductive-type "String" String-constructors))

;; ============================================================================
;; CHARACTER OPERATIONS
;; ============================================================================

;; Character constructor: Nat -> Char
(define/contract (make-char codepoint)
  (-> constructor-value? constructor-value?)
  (constructor-value "char" (list codepoint) Char))

;; Extract codepoint from character
(define/contract (char-codepoint ch)
  (-> constructor-value? constructor-value?)
  (match ch
    [(constructor-value "char" (list cp) _) cp]
    [_ (error "Not a character: " ch)]))

;; ============================================================================
;; STRING CONSTRUCTORS
;; ============================================================================

;; Empty string constructor
(define empty-string (constructor-value "empty-string" '() String))

;; String cons constructor: Char -> String -> String
(define/contract (string-cons ch str)
  (-> constructor-value? constructor-value? constructor-value?)
  (constructor-value "string-cons" (list ch str) String))

;; ============================================================================
;; STRING FROM RACKET STRING (Host Bridge)
;; ============================================================================

;; Convert Racket string to HoTT string
(define/contract (racket-string->hott-string str)
  (-> string? constructor-value?)
  (if (string=? str "")
      empty-string
      (let* ([first-char (string-ref str 0)]
             [rest-str (substring str 1)]
             [char-code (char->integer first-char)])
        (string-cons 
         (make-char (racket-nat->hott-nat char-code))
         (racket-string->hott-string rest-str)))))

;; Convert HoTT string to Racket string (for display)
(define/contract (hott-string->racket-string hott-str)
  (-> constructor-value? string?)
  (match hott-str
    [(constructor-value "empty-string" '() _) ""]
    [(constructor-value "string-cons" (list ch rest) _)
     (let* ([codepoint (char-codepoint ch)]
            [racket-cp (hott-nat->racket-nat codepoint)]
            [racket-char (integer->char racket-cp)]
            [rest-string (hott-string->racket-string rest)])
       (string-append (string racket-char) rest-string))]
    [_ (error "Not a HoTT string: " hott-str)]))

;; Helper: Convert Racket nat to HoTT nat
(define/contract (racket-nat->hott-nat n)
  (-> exact-nonnegative-integer? constructor-value?)
  (if (= n 0)
      zero-value
      (succ-value (racket-nat->hott-nat (- n 1)))))

;; Helper: Convert HoTT nat to Racket nat  
(define/contract (hott-nat->racket-nat hott-n)
  (-> constructor-value? exact-nonnegative-integer?)
  (match hott-n
    [(constructor-value "zero" '() _) 0]
    [(constructor-value "next" (list pred) _) 
     (+ 1 (hott-nat->racket-nat pred))]
    [_ (error "Not a HoTT nat: " hott-n)]))

;; ============================================================================
;; STRING OPERATIONS (HoTT-Native)
;; ============================================================================

;; String length: String -> Nat
(define/contract (hott-string-length str)
  (-> constructor-value? constructor-value?)
  (match str
    [(constructor-value "empty-string" '() _) zero-value]
    [(constructor-value "string-cons" (list _ rest) _)
     (succ-value (hott-string-length rest))]
    [_ (error "Not a string: " str)]))

;; String append: String -> String -> String
(define/contract (hott-string-append s1 s2)
  (-> constructor-value? constructor-value? constructor-value?)
  (match s1
    [(constructor-value "empty-string" '() _) s2]
    [(constructor-value "string-cons" (list ch rest) _)
     (string-cons ch (hott-string-append rest s2))]
    [_ (error "Not a string: " s1)]))

;; Note: String and character equality functions are implemented in 
;; hott-evaluator.rkt to avoid circular imports

;; ============================================================================
;; UTILITY FUNCTIONS  
;; ============================================================================

;; Check if value is a string
(define/contract (hott-string? val)
  (-> any/c boolean?)
  (and (constructor-value? val)
       (let ([type (constructor-value-type val)])
         (and (inductive-type? type)
              (string=? (inductive-type-name type) "String")))))

;; Check if value is a character
(define/contract (hott-char? val)
  (-> any/c boolean?)
  (and (constructor-value? val)
       (let ([type (constructor-value-type val)])
         (and (inductive-type? type)
              (string=? (inductive-type-name type) "Char")))))

;; Create a single-character string
(define/contract (make-singleton-string ch)
  (-> constructor-value? constructor-value?)
  (string-cons ch empty-string))

;; Extract first character (head) with proof
(define/contract (string-head str non-empty-proof)
  (-> constructor-value? any/c constructor-value?)
  (match str
    [(constructor-value "string-cons" (list ch _) _) ch]
    [(constructor-value "empty-string" '() _)
     (error "Empty string - proof should have prevented this!")]
    [_ (error "Not a string: " str)]))

;; Extract tail with proof
(define/contract (string-tail str non-empty-proof)
  (-> constructor-value? any/c constructor-value?)
  (match str
    [(constructor-value "string-cons" (list _ rest) _) rest]
    [(constructor-value "empty-string" '() _)
     (error "Empty string - proof should have prevented this!")]
    [_ (error "Not a string: " str)]))

;; Check if string is empty
(define/contract (string-empty? str)
  (-> constructor-value? constructor-value?)
  (match str
    [(constructor-value "empty-string" '() _) true-value]
    [(constructor-value "string-cons" _ _) false-value]
    [_ (error "Not a string: " str)]))

