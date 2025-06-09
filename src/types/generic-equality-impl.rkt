#lang racket/base

(require racket/contract
         racket/match
         racket/string
         "types.rkt"
         "list-type.rkt"
         "equality-family.rkt"
         "../evaluator/values.rkt"
         "../core/hott-evaluator.rkt")

(provide (all-defined-out))

;; ============================================================================
;; UNIVERSE POLYMORPHIC EQUALITY FUNCTIONS (HoTT-Native)
;; ============================================================================
;; Implements equality functions that return identity type proofs
;; Following HoTT principles: (A : Type₀) → A → A → (Id A x y) + ¬(Id A x y)

;; ============================================================================
;; GENERIC EQUALITY WITH IDENTITY PROOFS
;; ============================================================================

;; Universe polymorphic equality: takes type explicitly as parameter
;; Returns either a proof of equality or a proof of inequality
(define/contract (hott-generic-equal type x y)
  (-> extended-hott-type/c constructor-value? constructor-value? equality-proof?)
  (let ([eq-instance (get-decidable-equality type x y)])
    (dispatch-equality-with-proof eq-instance type x y)))

;; Dispatch to type-specific equality with proof construction
(define/contract (dispatch-equality-with-proof eq-instance type x y)
  (-> decidable-equality-type? extended-hott-type/c constructor-value? constructor-value? equality-proof?)
  (match type
    ;; Natural numbers  
    [(inductive-type "Nat" _)
     (nat-equal-with-identity-proof x y)]
    
    ;; Booleans
    [(inductive-type "Bool" _)
     (bool-equal-with-identity-proof x y)]
    
    ;; Unit type - always equal
    [(unit-type)
     (unit-equal-with-identity-proof x y)]
    
    ;; Empty type - vacuously equal (no inhabitants)
    [(empty-type)
     (empty-equal-with-identity-proof x y)]
    
    ;; Lists with element equality
    [(inductive-type name _) #:when (string-prefix? name "List-")
     (list-equal-with-identity-proof type x y)]
    
    ;; Strings (HoTT-native)
    [(inductive-type "String" _)
     (string-equal-with-identity-proof x y)]
    
    ;; Characters (HoTT-native)  
    [(inductive-type "Char" _)
     (char-equal-with-identity-proof x y)]
    
    ;; Default: structural equality for unknown inductive types
    [_
     (structural-equal-with-identity-proof type x y)]))

;; ============================================================================
;; TYPE-SPECIFIC EQUALITY WITH IDENTITY TYPE PROOFS
;; ============================================================================

;; Natural number equality with path construction
(define/contract (nat-equal-with-identity-proof n1 n2)
  (-> constructor-value? constructor-value? equality-proof?)
  (cond
    ;; Case: both zero - construct refl : Id Nat zero zero
    [(and (string=? (constructor-value-constructor-name n1) "zero")
          (string=? (constructor-value-constructor-name n2) "zero"))
     (equality-proof Nat #t (make-refl-path Nat n1))]
    
    ;; Case: both next - recurse and apply congruence
    [(and (string=? (constructor-value-constructor-name n1) "next")
          (string=? (constructor-value-constructor-name n2) "next"))
     (let* ([pred1 (car (constructor-value-args n1))]
            [pred2 (car (constructor-value-args n2))]
            [pred-eq (nat-equal-with-identity-proof pred1 pred2)])
       (if (equality-proof-equal? pred-eq)
           ;; Construct: cong next p : Id Nat (next m) (next n)
           (equality-proof Nat #t 
                          (make-cong-path 'next (equality-proof-path-or-negation pred-eq)))
           ;; Construct negation proof
           (equality-proof Nat #f 
                          `(nat-next-neq-proof ,n1 ,n2 ,(equality-proof-path-or-negation pred-eq)))))]
    
    ;; Case: different constructors - construct inequality proof
    [else
     (equality-proof Nat #f `(nat-constructor-neq-proof ,n1 ,n2))]))

;; Boolean equality with identity type proof
(define/contract (bool-equal-with-identity-proof b1 b2)
  (-> constructor-value? constructor-value? equality-proof?)
  (let ([name1 (constructor-value-constructor-name b1)]
        [name2 (constructor-value-constructor-name b2)])
    (cond
      ;; Both true - construct refl : Id Bool true true
      [(and (string=? name1 "true") (string=? name2 "true"))
       (equality-proof Bool #t (make-refl-path Bool b1))]
      
      ;; Both false - construct refl : Id Bool false false  
      [(and (string=? name1 "false") (string=? name2 "false"))
       (equality-proof Bool #t (make-refl-path Bool b1))]
      
      ;; Different - construct inequality proof
      [else
       (equality-proof Bool #f `(bool-neq-proof ,b1 ,b2))])))

;; Unit type equality - always equal (contractibility)
(define/contract (unit-equal-with-identity-proof u1 u2)
  (-> constructor-value? constructor-value? equality-proof?)
  ;; Unit type is contractible - all elements are equal
  (equality-proof Unit #t `(unit-contractible-proof ,u1 ,u2)))

;; Empty type equality - vacuously true (no inhabitants)
(define/contract (empty-equal-with-identity-proof e1 e2)
  (-> constructor-value? constructor-value? equality-proof?)
  ;; Empty type has no inhabitants, so this is vacuously true
  (equality-proof Empty #t `(empty-vacuous-proof ,e1 ,e2)))

;; List equality with recursive identity proofs
(define/contract (list-equal-with-identity-proof list-type lst1 lst2)
  (-> extended-hott-type/c constructor-value? constructor-value? equality-proof?)
  (let ([element-type (extract-list-element-type list-type)])
    (match* (lst1 lst2)
      ;; Both nil - construct refl : Id (List A) nil nil
      [((constructor-value "nil" '() _) (constructor-value "nil" '() _))
       (equality-proof list-type #t (make-refl-path list-type lst1))]
      
      ;; Both cons - recurse on heads and tails
      [((constructor-value "cons" (list h1 t1) _) 
        (constructor-value "cons" (list h2 t2) _))
       (let ([head-eq (hott-generic-equal element-type h1 h2)]
             [tail-eq (list-equal-with-identity-proof list-type t1 t2)])
         (if (and (equality-proof-equal? head-eq) (equality-proof-equal? tail-eq))
             ;; Construct: cong₂ cons head-proof tail-proof
             (equality-proof list-type #t 
                            `(list-cons-eq-proof 
                              ,(equality-proof-path-or-negation head-eq)
                              ,(equality-proof-path-or-negation tail-eq)))
             ;; Construct inequality proof
             (equality-proof list-type #f 
                            `(list-cons-neq-proof ,h1 ,h2 ,t1 ,t2))))]
      
      ;; Different constructors
      [(_ _)
       (equality-proof list-type #f `(list-constructor-neq-proof ,lst1 ,lst2))])))

;; String equality with identity type proof (HoTT-native)
(define/contract (string-equal-with-identity-proof s1 s2)
  (-> constructor-value? constructor-value? equality-proof?)
  (match* (s1 s2)
    ;; Both empty strings - reflexivity
    [((constructor-value "empty-string" '() _) 
      (constructor-value "empty-string" '() _))
     (equality-proof String #t (make-refl-path String s1))]
    
    ;; Both non-empty - recurse on head and tail
    [((constructor-value "string-cons" (list c1 r1) _)
      (constructor-value "string-cons" (list c2 r2) _))
     (let ([char-eq (char-equal-with-identity-proof c1 c2)]
           [rest-eq (string-equal-with-identity-proof r1 r2)])
       (if (and (equality-proof-equal? char-eq) (equality-proof-equal? rest-eq))
           (equality-proof String #t 
                          `(string-cons-eq-proof 
                            ,(equality-proof-path-or-negation char-eq)
                            ,(equality-proof-path-or-negation rest-eq)))
           (equality-proof String #f 
                          `(string-cons-neq-proof ,c1 ,c2 ,r1 ,r2))))]
    
    ;; Different constructors
    [(_ _)
     (equality-proof String #f `(string-constructor-neq-proof ,s1 ,s2))]))

;; Character equality with identity type proof (HoTT-native)
(define/contract (char-equal-with-identity-proof c1 c2)
  (-> constructor-value? constructor-value? equality-proof?)
  (match* (c1 c2)
    [((constructor-value "char" (list cp1) _)
      (constructor-value "char" (list cp2) _))
     ;; Use natural number equality on codepoints
     (let ([nat-eq (nat-equal-with-identity-proof cp1 cp2)])
       (if (equality-proof-equal? nat-eq)
           (equality-proof Char #t 
                          `(char-eq-proof ,(equality-proof-path-or-negation nat-eq)))
           (equality-proof Char #f 
                          `(char-neq-proof ,cp1 ,cp2))))]
    [(_ _)
     (equality-proof Char #f `(char-constructor-neq-proof ,c1 ,c2))]))

;; Structural equality for unknown inductive types
(define/contract (structural-equal-with-identity-proof type x y)
  (-> extended-hott-type/c constructor-value? constructor-value? equality-proof?)
  (let ([name1 (constructor-value-constructor-name x)]
        [name2 (constructor-value-constructor-name y)]
        [args1 (constructor-value-args x)]
        [args2 (constructor-value-args y)])
    (cond
      ;; Same constructor, check arguments recursively
      [(and (string=? name1 name2) (= (length args1) (length args2)))
       (let ([arg-proofs (map (lambda (a1 a2) 
                               ;; For structural equality, we assume all args have decidable equality
                               ;; In practice, would extract arg types from inductive type definition
                               (structural-equal-with-identity-proof type a1 a2))
                             args1 args2)])
         (if (andmap equality-proof-equal? arg-proofs)
             (equality-proof type #t 
                            `(structural-eq-proof ,name1 
                              ,(map equality-proof-path-or-negation arg-proofs)))
             (equality-proof type #f 
                            `(structural-neq-proof ,x ,y))))]
      
      ;; Different constructors
      [else
       (equality-proof type #f `(constructor-mismatch-proof ,x ,y))])))

;; ============================================================================
;; IDENTITY TYPE PATH CONSTRUCTION HELPERS
;; ============================================================================

;; Construct reflexivity path: refl : Id A x x
(define/contract (make-refl-path type term)
  (-> extended-hott-type/c any/c any/c)
  `(refl ,type ,term))

;; Construct congruence path: cong f p : Id B (f x) (f y)
(define/contract (make-cong-path function path)
  (-> any/c any/c any/c)
  `(cong ,function ,path))

;; Construct symmetry path: sym p : Id A y x  
(define/contract (make-sym-path path)
  (-> any/c any/c)
  `(sym ,path))

;; Construct transitivity path: trans p q : Id A x z
(define/contract (make-trans-path path1 path2)
  (-> any/c any/c any/c)
  `(trans ,path1 ,path2))

;; ============================================================================
;; DECIDABLE EQUALITY INTERFACE (Boolean-Returning)
;; ============================================================================

;; Simple interface that returns Bool for compatibility
(define/contract (decidable-equal? type x y)
  (-> extended-hott-type/c constructor-value? constructor-value? constructor-value?)
  (let ([proof (hott-generic-equal type x y)])
    (if (equality-proof-equal? proof)
        true-value
        false-value)))

;; Interface that returns the proof witness
(define/contract (equal-with-witness type x y)
  (-> extended-hott-type/c constructor-value? constructor-value? any/c)
  (let ([proof (hott-generic-equal type x y)])
    (if (equality-proof-equal? proof)
        `(inl ,(equality-proof-path-or-negation proof))  ; Left injection: proof of equality
        `(inr ,(equality-proof-path-or-negation proof))))) ; Right injection: proof of inequality

;; ============================================================================
;; TYPE CLASS-STYLE INTERFACE (for convenience)
;; ============================================================================

;; Eq "type class" record
(struct eq-instance (type equal-fn refl-proof sym-proof trans-proof) #:transparent)

;; Create Eq instance for a type
(define/contract (make-eq-instance type)
  (-> extended-hott-type/c eq-instance?)
  (eq-instance type
               (lambda (x y) (hott-generic-equal type x y))
               (lambda (x) (make-refl-path type x))
               (lambda (p) (make-sym-path p))
               (lambda (p q) (make-trans-path p q))))

;; Get default Eq instance for basic types
(define/contract (get-eq-instance type)
  (-> extended-hott-type/c (or/c eq-instance? #f))
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (make-eq-instance type)))

;; ============================================================================
;; EXPORT STANDARD EQUALITY FUNCTIONS
;; ============================================================================

;; Standard equality that most code will use
(define/contract (equal? x y)
  (-> constructor-value? constructor-value? constructor-value?)
  ;; Extract type from first argument and check compatibility
  (let ([type1 (constructor-value-type x)]
        [type2 (constructor-value-type y)])
    (if (hott-type-equal? type1 type2)
        (decidable-equal? type1 x y)
        false-value)))

;; Equality with proof extraction
(define/contract (equal-with-proof? x y)
  (-> constructor-value? constructor-value? equality-proof?)
  (let ([type1 (constructor-value-type x)]
        [type2 (constructor-value-type y)])
    (if (hott-type-equal? type1 type2)
        (hott-generic-equal type1 x y)
        (equality-proof type1 #f `(type-mismatch-proof ,x ,y)))))