#lang racket/base

(require racket/contract
         racket/match
         racket/string
         "types.rkt"
         "type-families.rkt"
         "equality-family.rkt"
         "generic-equality-impl.rkt"
         "../evaluator/values.rkt"
         "../core/hott-evaluator.rkt")

(provide (all-defined-out))

;; ============================================================================
;; TYPE-SPECIFIC EQUALITY INSTANCE REGISTRATION
;; ============================================================================
;; Registers decidable equality instances for PathFinder's basic types
;; with the tier-aware type family system

;; ============================================================================
;; INSTANCE REGISTRY
;; ============================================================================

;; Registry for type-specific equality instances
(define equality-instance-registry (make-hash))

;; Equality instance structure
(struct equality-instance (type-name equality-function proof-constructor tier-optimized?) #:transparent)

;; Register an equality instance for a type
(define/contract (register-equality-instance! type-name eq-function proof-constructor [tier-optimized? #t])
  (->* (string? procedure? procedure?) (boolean?) void?)
  (let ([instance (equality-instance type-name eq-function proof-constructor tier-optimized?)])
    (hash-set! equality-instance-registry type-name instance)
    (printf "✓ Registered equality instance for ~a~n" type-name)))

;; Get equality instance for a type
(define/contract (get-equality-instance type-name)
  (-> string? (or/c equality-instance? #f))
  (hash-ref equality-instance-registry type-name #f))

;; ============================================================================
;; BASIC TYPE EQUALITY INSTANCES
;; ============================================================================

;; Natural number equality instance
(define/contract (nat-equality-function n1 n2)
  (-> constructor-value? constructor-value? equality-proof?)
  (nat-equal-with-identity-proof n1 n2))

(define/contract (nat-proof-constructor n1 n2 equal?)
  (-> constructor-value? constructor-value? boolean? any/c)
  (if equal?
      (construct-nat-equality-path n1 n2)
      `(nat-inequality-proof ,n1 ,n2)))

;; Boolean equality instance  
(define/contract (bool-equality-function b1 b2)
  (-> constructor-value? constructor-value? equality-proof?)
  (bool-equal-with-identity-proof b1 b2))

(define/contract (bool-proof-constructor b1 b2 equal?)
  (-> constructor-value? constructor-value? boolean? any/c)
  (if equal?
      `(bool-refl-proof ,(constructor-value-constructor-name b1))
      `(bool-inequality-proof ,b1 ,b2)))

;; Unit type equality instance (always equal - contractibility)
(define/contract (unit-equality-function u1 u2)
  (-> constructor-value? constructor-value? equality-proof?)
  (equality-proof Unit #t `(unit-contractible-proof ,u1 ,u2)))

(define/contract (unit-proof-constructor u1 u2 equal?)
  (-> constructor-value? constructor-value? boolean? any/c)
  ;; Unit is contractible - all inhabitants are equal
  `(unit-contractible-proof ,u1 ,u2))

;; List equality instance (parametric)
(define/contract (list-equality-function element-type)
  (-> extended-hott-type/c procedure?)
  (lambda (lst1 lst2)
    (list-equal-with-identity-proof (make-list-type element-type) lst1 lst2)))

(define/contract (list-proof-constructor element-type)
  (-> extended-hott-type/c procedure?)
  (lambda (lst1 lst2 equal?)
    (if equal?
        `(list-structural-eq-proof ,element-type ,lst1 ,lst2)
        `(list-inequality-proof ,element-type ,lst1 ,lst2))))

;; String equality instance
(define/contract (string-equality-function s1 s2)
  (-> constructor-value? constructor-value? equality-proof?)
  (string-equal-with-identity-proof s1 s2))

(define/contract (string-proof-constructor s1 s2 equal?)
  (-> constructor-value? constructor-value? boolean? any/c)
  (if equal?
      `(string-char-eq-proof ,s1 ,s2)
      `(string-inequality-proof ,s1 ,s2)))

;; ============================================================================
;; PATH CONSTRUCTION FOR SPECIFIC TYPES
;; ============================================================================

;; Construct equality path for natural numbers by induction
(define/contract (construct-nat-equality-path n1 n2)
  (-> constructor-value? constructor-value? any/c)
  (match* (n1 n2)
    ;; Base case: zero = zero by reflexivity
    [((constructor-value "zero" '() _) (constructor-value "zero" '() _))
     `(refl Nat zero)]
    
    ;; Inductive case: next m = next n by congruence if m = n
    [((constructor-value "next" (list pred1) _) (constructor-value "next" (list pred2) _))
     (let ([pred-path (construct-nat-equality-path pred1 pred2)])
       `(cong next ,pred-path))]
    
    ;; Different constructors - should not happen if called with equal numbers
    [(_ _)
     (error "Cannot construct equality path for different natural numbers")]))

;; ============================================================================
;; TIER-AWARE OPTIMIZATION INSTANCES
;; ============================================================================

;; Tier 1 optimization: compile-time natural number equality
(define/contract (tier1-nat-equality n1-static n2-static)
  (-> any/c any/c any/c)
  ;; When both numbers are known at compile time, we can precompute
  (cond
    [(and (constructor-value? n1-static) (constructor-value? n2-static))
     (let ([result (hott-equal? n1-static n2-static)])
       `(compile-time-nat-eq ,(constructor-value-constructor-name result)))]
    [else 'runtime-dispatch]))

;; Tier 1 optimization: compile-time boolean equality
(define/contract (tier1-bool-equality b1-static b2-static)
  (-> any/c any/c any/c)
  (cond
    [(and (constructor-value? b1-static) (constructor-value? b2-static))
     (let ([name1 (constructor-value-constructor-name b1-static)]
           [name2 (constructor-value-constructor-name b2-static)])
       `(compile-time-bool-eq ,(string=? name1 name2)))]
    [else 'runtime-dispatch]))

;; ============================================================================
;; TYPE FAMILY INTEGRATION
;; ============================================================================

;; Enhanced type family instantiation that uses registered instances
(define/contract (instantiate-equality-with-instances type)
  (-> extended-hott-type/c decidable-equality-type?)
  (match type
    [(inductive-type type-name _)
     (let ([instance (get-equality-instance type-name)])
       (if instance
           ;; Use registered instance
           (decidable-equality-type type 
                                   (equality-instance-equality-function instance)
                                   (equality-instance-proof-constructor instance))
           ;; Fall back to default instantiation
           (equality-type-instantiation type)))]
    
    ;; For non-inductive types, use default instantiation
    [_ (equality-type-instantiation type)]))

;; ============================================================================
;; PARAMETRIC TYPE INSTANCES
;; ============================================================================

;; Register list equality for specific element types
(define/contract (register-list-equality-instance! element-type-name element-type)
  (-> string? extended-hott-type/c void?)
  (let ([list-type-name (string-append "List-" element-type-name)])
    (register-equality-instance! 
     list-type-name
     (list-equality-function element-type)
     (list-proof-constructor element-type)
     #t)))

;; Register option equality (when Option type is implemented)
(define/contract (register-option-equality-instance! element-type-name element-type)
  (-> string? extended-hott-type/c void?)
  (let ([option-type-name (string-append "Option-" element-type-name)])
    ;; Placeholder for future Option type implementation
    (printf "TODO: Register Option equality for ~a~n" element-type-name)))

;; ============================================================================
;; INITIALIZATION AND REGISTRATION
;; ============================================================================

;; Register all basic type equality instances
(define/contract (register-basic-equality-instances!)
  (-> void?)
  ;; Natural numbers
  (register-equality-instance! "Nat" nat-equality-function nat-proof-constructor #t)
  
  ;; Booleans  
  (register-equality-instance! "Bool" bool-equality-function bool-proof-constructor #t)
  
  ;; Unit type
  (register-equality-instance! "Unit" unit-equality-function unit-proof-constructor #t)
  
  ;; Strings (if implemented)
  (register-equality-instance! "String" string-equality-function string-proof-constructor #t)
  
  ;; Lists for basic element types
  (register-list-equality-instance! "Nat" Nat)
  (register-list-equality-instance! "Bool" Bool)
  (register-list-equality-instance! "Unit" Unit)
  
  (printf "✓ All basic equality instances registered~n"))

;; Register tier-specific optimizations
(define/contract (register-tier-optimizations!)
  (-> void?)
  ;; Register compile-time optimizations for Tier 1
  (hash-set! equality-specialization-cache 'nat-tier1 tier1-nat-equality)
  (hash-set! equality-specialization-cache 'bool-tier1 tier1-bool-equality)
  
  (printf "✓ Tier-aware optimizations registered~n"))

;; ============================================================================
;; TYPE CHECKING INTEGRATION
;; ============================================================================

;; Check if two types have compatible equality
(define/contract (types-have-compatible-equality? type1 type2)
  (-> extended-hott-type/c extended-hott-type/c boolean?)
  (and (hott-type-equal? type1 type2)
       (has-decidable-equality? type1)))

;; Get the most efficient equality function for two values
(define/contract (get-optimal-equality-function val1 val2)
  (-> constructor-value? constructor-value? procedure?)
  (let* ([type1 (constructor-value-type val1)]
         [type2 (constructor-value-type val2)])
    (if (hott-type-equal? type1 type2)
        (let ([tier (determine-tier (list type1) (list val1 val2))])
          (match tier
            ['tier1 (get-tier1-equality-function type1)]
            ['tier2 (get-tier2-equality-function type1)]
            ['tier3 (get-tier3-equality-function type1)]))
        (lambda (x y) (equality-proof type1 #f `(type-mismatch ,x ,y))))))

;; Get tier-specific equality functions
(define/contract (get-tier1-equality-function type)
  (-> extended-hott-type/c procedure?)
  ;; Compile-time optimized version
  (lambda (x y) (hott-generic-equal type x y)))

(define/contract (get-tier2-equality-function type)
  (-> extended-hott-type/c procedure?)
  ;; Type-resolution optimized version
  (lambda (x y) (hott-generic-equal type x y)))

(define/contract (get-tier3-equality-function type)
  (-> extended-hott-type/c procedure?)
  ;; Runtime dispatch version
  (lambda (x y) (hott-generic-equal type x y)))

;; ============================================================================
;; INITIALIZATION
;; ============================================================================

;; Initialize all equality instances
(define/contract (initialize-equality-instances!)
  (-> void?)
  (register-basic-equality-instances!)
  (register-tier-optimizations!)
  (printf "✓ Generic equality system fully initialized~n"))

;; Auto-initialize on module load
(initialize-equality-instances!)