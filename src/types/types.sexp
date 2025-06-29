;; ============================================================================
;; PURE MATHEMATICAL HOTT TYPE SYSTEM (S-EXPRESSION VERSION)
;; ============================================================================
;; Foundation layer - no imports needed

;; ============================================================================
;; UNIVERSE HIERARCHY
;; ============================================================================

;; Universe type
(data Universe Uω
  (case U (-> Nat Universe)))

;; Predefined universe levels
(define Type0 (U zero))
(define Type1 (U (succ zero)))
(define Type2 (U (succ (succ zero))))

;; Universe level accessor
(type universe-level (-> Universe Nat))
(define universe-level
  (fn (u)
    (match u
      (case (U n) n))))

;; ============================================================================
;; CORE TYPE FORMERS FROM HOTT
;; ============================================================================

;; The type of types at each universe level
(data Type Uω
  ;; Universe types
  (case universe (-> Nat Type))
  
  ;; Π-types (dependent function types)
  (case pi-type (-> String Type Type Type))
  
  ;; Σ-types (dependent pair types)
  (case sigma-type (-> String Type Type Type))
  
  ;; Sum types: A + B
  (case sum-type (-> Type Type Type))
  
  ;; Identity types: paths/equality
  (case identity-type (-> Type Value Value Type))
  
  ;; Unit type: has exactly one element
  (case unit-type Type)
  
  ;; Empty type: has no elements
  (case empty-type Type)
  
  ;; Inductive types with constructors
  (case inductive-type (-> String (List Constructor) Type)))

;; Non-dependent function type (arrow)
(type arrow-type (-> Type Type Type))
(define arrow-type
  (fn (A B)
    (pi-type "_" A B)))

;; Non-dependent pair type (product)
(type product-type (-> Type Type Type))
(define product-type
  (fn (A B)
    (sigma-type "_" A B)))

;; ============================================================================
;; TYPE CHECKING CONTEXT
;; ============================================================================

(data Context U0
  (case empty-context Context)
  (case extend-context (-> Context String Type Context)))

;; Context lookup
(define context-lookup
  (fn (ctx var)
    (match ctx
      (case empty-context none)
      (case (extend-context rest v t)
        (if (string-equal? v var)
            (some t)
            (context-lookup rest var))))))

;; ============================================================================
;; TYPE EQUALITY AND CONVERSION
;; ============================================================================

;; Type equality (decidable)
(define type-equal?
  (fn (t1 t2)
    (match (pair t1 t2)
      ;; Universe equality
      (case (pair (universe n1) (universe n2))
        (nat-equal? n1 n2))
      
      ;; Pi type equality
      (case (pair (pi-type x1 A1 B1) (pi-type x2 A2 B2))
        (and (type-equal? A1 A2)
             (type-equal? B1 B2)))
      
      ;; Sigma type equality
      (case (pair (sigma-type x1 A1 B1) (sigma-type x2 A2 B2))
        (and (type-equal? A1 A2)
             (type-equal? B1 B2)))
      
      ;; Other cases...
      (case _ false))))

;; ============================================================================
;; TYPE UTILITIES
;; ============================================================================

;; Get universe level of a type
(define type-universe-level
  (fn (t)
    (match t
      (case (universe n) n)
      (case (pi-type _ A B)
        (max (type-universe-level A)
             (type-universe-level B)))
      (case (sigma-type _ A B)
        (max (type-universe-level A)
             (type-universe-level B)))
      (case unit-type zero)
      (case empty-type zero)
      (case _ zero))))