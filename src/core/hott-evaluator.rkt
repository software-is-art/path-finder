#lang racket/base

(require racket/contract
         racket/match
         "../types/types.rkt"
         "../evaluator/values.rkt"
         "../effects/pure-hott-effects.rkt"
         (prefix-in literals: "hott-literals.rkt")
         "hott-literals-pure.rkt")

(provide (all-defined-out))

;; ============================================================================
;; HOTT-NATIVE EVALUATOR
;; ============================================================================
;; Evaluation using only HoTT constructors - no Racket arithmetic

;; HoTT-native arithmetic using computational proofs
(define/contract (hott-add n1 n2)
  (-> constructor-value? constructor-value? constructor-value?)
  ;; Addition via Peano arithmetic
  (match* (n1 n2)
    [((constructor-value "zero" '() _) n) n]
    [((constructor-value "next" (list pred) _) n)
     (constructor-value "next" 
                       (list (hott-add pred n))
                       Nat)]))

(define/contract (hott-mult n1 n2)
  (-> constructor-value? constructor-value? constructor-value?)
  ;; Multiplication via repeated addition
  (match* (n1 n2)
    [((constructor-value "zero" '() _) _) zero-value]
    [((constructor-value "next" (list pred) _) n)
     (hott-add n (hott-mult pred n))]))

(define/contract (hott-pred n)
  (-> constructor-value? constructor-value?)
  ;; Predecessor (safe, returns zero for zero)
  (match n
    [(constructor-value "zero" '() _) zero-value]
    [(constructor-value "next" (list pred) _) pred]))

(define/contract (hott-sub n1 n2)
  (-> constructor-value? constructor-value? constructor-value?)
  ;; Subtraction via structural recursion (truncated at zero)
  (match* (n1 n2)
    [(n (constructor-value "zero" '() _)) n]
    [((constructor-value "zero" '() _) _) zero-value]
    [((constructor-value "next" (list p1) _) (constructor-value "next" (list p2) _))
     (hott-sub p1 p2)]))

(define/contract (hott-equal? n1 n2)
  (-> constructor-value? constructor-value? constructor-value?)
  ;; Equality via structural recursion
  (match* (n1 n2)
    [((constructor-value "zero" '() _) (constructor-value "zero" '() _))
     true-value]
    [((constructor-value "next" (list p1) _) (constructor-value "next" (list p2) _))
     (hott-equal? p1 p2)]
    [(_ _) false-value]))

(define/contract (hott-less? n1 n2)
  (-> constructor-value? constructor-value? constructor-value?)
  ;; Less-than via structural recursion
  (match* (n1 n2)
    [((constructor-value "zero" '() _) (constructor-value "zero" '() _))
     false-value]
    [((constructor-value "zero" '() _) (constructor-value "next" _ _))
     true-value]
    [((constructor-value "next" _ _) (constructor-value "zero" '() _))
     false-value]
    [((constructor-value "next" (list p1) _) (constructor-value "next" (list p2) _))
     (hott-less? p1 p2)]))

;; ============================================================================
;; STRING OPERATIONS (HoTT-native)
;; ============================================================================

(define/contract (hott-string-append s1 s2)
  (-> constructor-value? constructor-value? constructor-value?)
  ;; Append two HoTT strings
  (match s1
    [(constructor-value "empty-string" '() _) s2]
    [(constructor-value "string-cons" (list ch rest) _)
     (constructor-value "string-cons" 
                       (list ch (hott-string-append rest s2))
                       String)]))

(define/contract (hott-string-equal? s1 s2)
  (-> constructor-value? constructor-value? constructor-value?)
  ;; String equality
  (match* (s1 s2)
    [((constructor-value "empty-string" '() _) 
      (constructor-value "empty-string" '() _))
     true-value]
    [((constructor-value "string-cons" (list c1 r1) _)
      (constructor-value "string-cons" (list c2 r2) _))
     (hott-and (hott-char-equal? c1 c2)
               (hott-string-equal? r1 r2))]
    [(_ _) false-value]))

(define/contract (hott-char-equal? c1 c2)
  (-> constructor-value? constructor-value? constructor-value?)
  ;; Character equality via codepoint comparison
  (match* (c1 c2)
    [((constructor-value "char" (list n1) _)
      (constructor-value "char" (list n2) _))
     (hott-equal? n1 n2)]))

;; ============================================================================
;; BOOLEAN OPERATIONS (Already HoTT-native)
;; ============================================================================

(define/contract (hott-and b1 b2)
  (-> constructor-value? constructor-value? constructor-value?)
  (match* (b1 b2)
    [((constructor-value "true" '() _) (constructor-value "true" '() _))
     true-value]
    [(_ _) false-value]))

(define/contract (hott-or b1 b2)
  (-> constructor-value? constructor-value? constructor-value?)
  (match* (b1 b2)
    [((constructor-value "false" '() _) (constructor-value "false" '() _))
     false-value]
    [(_ _) true-value]))

(define/contract (hott-not b)
  (-> constructor-value? constructor-value?)
  (match b
    [(constructor-value "true" '() _) false-value]
    [(constructor-value "false" '() _) true-value]))

;; ============================================================================
;; HOTT-NATIVE BUILTIN ENVIRONMENT
;; ============================================================================

(define hott-builtin-environment
  (let ([env (make-hash)])
    ;; Arithmetic - all HoTT-native
    (hash-set! env "+" hott-add)
    (hash-set! env "*" hott-mult)
    (hash-set! env "-" (lambda (a b) (hott-sub a b)))  ; Implement hott-sub
    (hash-set! env "=" hott-equal?)
    (hash-set! env "<" hott-less?)
    (hash-set! env "predecessor" hott-pred)
    
    ;; Booleans - all HoTT-native
    (hash-set! env "and" hott-and)
    (hash-set! env "or" hott-or)
    (hash-set! env "not" hott-not)
    
    ;; Strings - all HoTT-native
    (hash-set! env "string-append" hott-string-append)
    (hash-set! env "string-equal?" hott-string-equal?)
    
    ;; Constructors
    (hash-set! env "zero" (lambda () zero-value))
    (hash-set! env "next" (lambda (n) (succ-value n)))
    (hash-set! env "true" (lambda () true-value))
    (hash-set! env "false" (lambda () false-value))
    
    env))

;; ============================================================================
;; CONVERSION UTILITIES (Only for the bridge)
;; ============================================================================

;; Convert HoTT value to "display string" for output
(define/contract (hott-value->display-string val)
  (-> constructor-value? constructor-value?)
  ;; Returns a HoTT string representation of the value
  (match val
    [(constructor-value "zero" '() _) 
     (make-hott-string "0")]
    [(constructor-value "next" _ _)
     (make-hott-string (number->string (hott-nat->number val)))]
    [(constructor-value "true" '() _)
     (make-hott-string "true")]
    [(constructor-value "false" '() _)
     (make-hott-string "false")]
    [_ (make-hott-string "<??>")]))

;; Helper: Create HoTT string from Racket string (for bootstrapping only)
(define/contract (make-hott-string s)
  (-> string? constructor-value?)
  (if (= (string-length s) 0)
      (constructor-value "empty-string" '() String)
      (let ([chars (string->list s)])
        (foldr (lambda (ch acc)
                 (constructor-value "string-cons"
                                   (list (constructor-value "char" 
                                                          (list (pure-racket-number->hott-nat
                                                                 (char->integer ch)))
                                                          Char)
                                         acc)
                                   String))
               (constructor-value "empty-string" '() String)
               chars))))

;; Helper: Convert HoTT nat to number (for display only)
(define/contract (hott-nat->number n)
  (-> constructor-value? exact-nonnegative-integer?)
  (match n
    [(constructor-value "zero" '() _) 0]
    [(constructor-value "next" (list pred) _)
     (+ 1 (hott-nat->number pred))]))

;; ============================================================================
;; EFFECT-BASED I/O (The only external interface)
;; ============================================================================

;; All I/O goes through pure HoTT effects - no direct Racket calls
(define/contract (hott-print value)
  (-> constructor-value? constructor-value?)
  ;; Convert value to HoTT string, then use pure HoTT Console effect
  (let ([str (hott-value->display-string value)])
    (console-print-effect (string-value str))))

(define/contract (hott-read-file path)
  (-> constructor-value? constructor-value?)
  ;; Use pure HoTT FileIO effect with HoTT string path
  (file-read-effect path))

;; No direct Racket I/O - everything through effects!