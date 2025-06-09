#lang racket/base

(require racket/contract
         racket/match
         racket/string
         "types.rkt"
         "type-families.rkt"
         "list-type.rkt"
         "../evaluator/values.rkt"
         "../core/hott-evaluator.rkt")

(provide (all-defined-out))

;; ============================================================================
;; GENERIC EQUALITY TYPE FAMILY (HoTT-Native)
;; ============================================================================
;; Implements generic decidable equality using PathFinder's tier-aware type 
;; family system with universe polymorphism and identity type proofs

;; ============================================================================
;; DECIDABLE EQUALITY TYPE FAMILY
;; ============================================================================

;; DecEq type family: Type₀ → Type₁
;; For any type A : Type₀, DecEq A provides decidable equality for A
(struct decidable-equality-type hott-type (base-type equality-function proof-constructor) #:transparent)

;; Extended type contract to include decidable equality  
;; (Note: extended-hott-type/c is defined in types.rkt)

;; Type family instantiation function
(define/contract (equality-type-instantiation base-type)
  (-> extended-hott-type/c decidable-equality-type?)
  (match base-type
    ;; Natural numbers have decidable equality
    [(inductive-type "Nat" _)
     (decidable-equality-type base-type 'nat-equal 'nat-eq-proof)]
    
    ;; Booleans have decidable equality  
    [(inductive-type "Bool" _)
     (decidable-equality-type base-type 'bool-equal 'bool-eq-proof)]
    
    ;; Unit type has trivial equality
    [(unit-type)
     (decidable-equality-type base-type 'unit-equal 'unit-eq-proof)]
    
    ;; Empty type has vacuous equality
    [(empty-type)
     (decidable-equality-type base-type 'empty-equal 'empty-eq-proof)]
    
    ;; Lists have decidable equality if their elements do
    [(inductive-type name _) #:when (string-prefix? name "List-")
     (let ([element-type (extract-list-element-type base-type)])
       (decidable-equality-type base-type 'list-equal 
                               `(list-eq-proof ,(equality-type-instantiation element-type))))]
    
    ;; String equality (HoTT-native)
    [(inductive-type "String" _)
     (decidable-equality-type base-type 'string-equal 'string-eq-proof)]
    
    ;; Character equality (HoTT-native)  
    [(inductive-type "Char" _)
     (decidable-equality-type base-type 'char-equal 'char-eq-proof)]
    
    ;; Default: unknown types don't have decidable equality
    [_
     (error "Type does not have decidable equality: " base-type)]))

;; Create the DecEq type family
(define DecEq-family
  (make-type-family 'DecEq 1 equality-type-instantiation))

;; ============================================================================
;; UNIVERSE POLYMORPHIC EQUALITY INTERFACE
;; ============================================================================

;; Universe polymorphic equality type: (A : Type₀) → A → A → Type₀
;; This is the HoTT-native way to express generic equality
(define/contract (make-equality-type base-type left-term right-term)
  (-> extended-hott-type/c any/c any/c identity-type?)
  (identity-type base-type left-term right-term))

;; Generic equality proof type: decidable equality for A
;; Either constructs Id A x y or ¬(Id A x y)
(struct equality-proof (type equal? path-or-negation) #:transparent)

;; Decidable equality result - either equal with path or not equal with proof
(struct dec-eq-result (decidable path-proof negation-proof) #:transparent)

;; ============================================================================
;; TIER-AWARE EQUALITY INSTANTIATION
;; ============================================================================

;; Get decidable equality instance for a type (tier-aware)
(define/contract (get-decidable-equality type . values)
  (->* (extended-hott-type/c) () #:rest (listof any/c) decidable-equality-type?)
  (let ([tier (determine-tier (list type) values)])
    (match tier
      ;; Tier 1: Compile-time full specialization
      ['tier1 (tier1-equality-instance type values)]
      
      ;; Tier 2: Type-resolution with compile-time effects  
      ['tier2 (tier2-equality-instance type)]
      
      ;; Tier 3: Runtime dispatch
      ['tier3 (tier3-equality-instance type)])))

;; Tier 1: Compile-time equality specialization
(define/contract (tier1-equality-instance type values)
  (-> extended-hott-type/c (listof any/c) decidable-equality-type?)
  ;; Full compile-time optimization - can inline specific equality checks
  (let ([eq-type (instantiate-type-family 'DecEq type)])
    ;; Cache specialized equality functions for these specific values
    (when (= (length values) 2)
      (cache-specialized-equality type (car values) (cadr values)))
    eq-type))

;; Tier 2: Type-resolution equality (uses compile-time effects)
(define/contract (tier2-equality-instance type)
  (-> extended-hott-type/c decidable-equality-type?)
  ;; Type known at compile-time, can specialize the equality function
  (instantiate-type-family 'DecEq type))

;; Tier 3: Runtime equality dispatch  
(define/contract (tier3-equality-instance type)
  (-> extended-hott-type/c decidable-equality-type?)
  ;; Runtime type dispatch - create generic equality wrapper
  (decidable-equality-type type 'runtime-equal 'runtime-eq-proof))

;; ============================================================================
;; SPECIALIZED EQUALITY CACHING (Performance Optimization)
;; ============================================================================

;; Cache for specialized equality functions
(define equality-specialization-cache (make-hash))

;; Cache a specialized equality computation
(define/contract (cache-specialized-equality type val1 val2)
  (-> extended-hott-type/c any/c any/c void?)
  (let ([cache-key (list type val1 val2)])
    ;; For Tier 1, we can precompute the equality result
    (hash-set! equality-specialization-cache cache-key 
               (compute-equality-at-compile-time type val1 val2))))

;; Compute equality at compile time (when values are constructor-values)
(define/contract (compute-equality-at-compile-time type val1 val2)
  (-> extended-hott-type/c any/c any/c any/c)
  (match type
    [(inductive-type "Nat" _)
     (if (and (constructor-value? val1) (constructor-value? val2))
         `(compile-time-nat-eq ,(hott-equal? val1 val2))
         'runtime-dispatch)]
    [(inductive-type "Bool" _)
     (if (and (constructor-value? val1) (constructor-value? val2))
         `(compile-time-bool-eq ,(equal? val1 val2))
         'runtime-dispatch)]
    [_ 'runtime-dispatch]))

;; ============================================================================
;; HELPER FUNCTIONS
;; ============================================================================

;; Note: extract-list-element-type is provided by list-type.rkt

;; Note: string-contains? is provided by racket/string

;; Check if type has decidable equality
(define/contract (has-decidable-equality? type)
  (-> extended-hott-type/c boolean?)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (decidable-equality-type? (instantiate-type-family 'DecEq type))))

;; ============================================================================
;; REGISTRATION AND INITIALIZATION
;; ============================================================================

;; Register the DecEq type family with the type family system
(define/contract (initialize-equality-family!)
  (-> void?)
  (register-type-family! DecEq-family)
  (printf "✓ Generic equality type family registered~n"))

;; Initialize on load
(initialize-equality-family!)