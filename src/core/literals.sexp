;; ============================================================================
;; PURE MATHEMATICAL HOTT LITERAL CONSTRUCTION (S-EXPRESSION VERSION)
;; ============================================================================
;; Pure constructor building with no host language arithmetic operations.

(import types types)
(import evaluator values)

;; ============================================================================
;; NATURAL NUMBER LITERALS
;; ============================================================================

;; Build natural number from zero using successor
(type build-nat (-> Nat Value))
(define build-nat
  (fn (n)
    (nat-elim (fn (_) Value)
      zero-value                          ;; Base case: 0
      (fn (k rec) (succ-value rec))      ;; Recursive case: succ
      n)))

;; Common small natural numbers
(define one (succ-value zero-value))
(define two (succ-value one))
(define three (succ-value two))
(define four (succ-value three))
(define five (succ-value four))
(define six (succ-value five))
(define seven (succ-value six))
(define eight (succ-value seven))
(define nine (succ-value eight))
(define ten (succ-value nine))

;; Build larger numbers by addition
(define twenty (nat-add ten ten))
(define thirty (nat-add twenty ten))
(define forty (nat-add thirty ten))
(define fifty (nat-add forty ten))
(define hundred (nat-multiply ten ten))

;; Natural number addition (pure HoTT)
(type nat-add (-> Value Value Value))
(define nat-add
  (fn (m n)
    (nat-value-elim Value m
      n                                   ;; 0 + n = n
      (fn (pred rec) (succ-value rec))))) ;; (succ m) + n = succ (m + n)

;; Natural number multiplication (pure HoTT)
(type nat-multiply (-> Value Value Value))
(define nat-multiply
  (fn (m n)
    (nat-value-elim Value m
      zero-value                          ;; 0 * n = 0
      (fn (pred rec) (nat-add n rec))))) ;; (succ m) * n = n + (m * n)

;; Natural value eliminator helper
(type nat-value-elim (-> Type Value A (-> Value A A) A))
(define nat-value-elim
  (fn (A val base step)
    (match val
      (case (constructor-value name args _)
        (match name
          (case "zero" base)
          (case "succ" 
            (match args
              (case (cons pred nil)
                (step pred (nat-value-elim A pred base step)))
              (case _ (error "Invalid succ"))))
          (case _ (error "Not a natural"))))
      (case _ (error "Not a natural")))))

;; ============================================================================
;; BOOLEAN LITERALS
;; ============================================================================

;; Boolean values
(define true-literal true-value)
(define false-literal false-value)

;; ============================================================================
;; STRING LITERALS
;; ============================================================================

;; Build string from character list
(type build-string (-> (List Char) Value))
(define build-string
  (fn (chars)
    (list-elim (fn (_) Value)
      (string-value "")                   ;; Empty string
      (fn (ch rest rec)                   ;; Cons character
        (string-append-char rec ch))
      chars)))

;; String append character (simplified)
(type string-append-char (-> Value Char Value))
(define string-append-char
  (fn (str ch)
    (match str
      (case (string-value s)
        (string-value (string-cons ch s)))
      (case _ (error "Not a string")))))

;; ============================================================================
;; LIST LITERALS
;; ============================================================================

;; Build list from elements
(type build-list (-> Type (List A) Value))
(define build-list
  (fn (elem-type elems)
    (list-elim (fn (_) Value)
      (constructor-value "nil" nil (list-type elem-type))
      (fn (head tail rec)
        (constructor-value "cons" 
                          (list head rec)
                          (list-type elem-type)))
      elems)))

;; ============================================================================
;; NUMERIC LITERAL PARSING
;; ============================================================================

;; Parse decimal string to natural number
(type parse-decimal (-> String Value))
(define parse-decimal
  (fn (str)
    (string-fold-left 
      (fn (acc ch)
        (nat-add (nat-multiply acc ten)
                (digit-value ch)))
      zero-value
      str)))

;; Get numeric value of digit character
(type digit-value (-> Char Value))
(define digit-value
  (fn (ch)
    (match ch
      (case #\0 zero-value)
      (case #\1 one)
      (case #\2 two)
      (case #\3 three)
      (case #\4 four)
      (case #\5 five)
      (case #\6 six)
      (case #\7 seven)
      (case #\8 eight)
      (case #\9 nine)
      (case _ (error "Not a digit")))))

;; ============================================================================
;; TYPE LITERALS
;; ============================================================================

;; Common type constructors
(define nat-type (inductive-type "Nat" nat-constructors))
(define bool-type (inductive-type "Bool" bool-constructors))
(define unit-type-literal unit-type)
(define empty-type-literal empty-type)

;; List type constructor
(type list-type (-> Type Type))
(define list-type
  (fn (elem-type)
    (inductive-type "List" 
                   (list (make-constructor "nil" nil "List")
                         (make-constructor "cons" 
                                          (list elem-type (list-type elem-type))
                                          "List")))))

;; ============================================================================
;; LITERAL VALIDATION
;; ============================================================================

;; Check if value is a valid literal
(type is-literal? (-> Value Bool))
(define is-literal?
  (fn (val)
    (match val
      (case (constructor-value name args _)
        (or (and (string-equal? name "zero") (null? args))
            (and (string-equal? name "succ") 
                 (= (list-length args) 1)
                 (is-literal? (car args)))
            (and (member? name (list "true" "false" "nil"))
                 (null? args))
            (and (string-equal? name "cons")
                 (= (list-length args) 2))))
      (case (string-value _) true)
      (case unit-value true)
      (case _ false))))