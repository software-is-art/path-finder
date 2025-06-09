#lang racket/base

(require racket/contract
         racket/match
         "types.rkt"
         "equality-family.rkt"
         "generic-equality-impl.rkt"
         "equality-instances.rkt"
         "../evaluator/values.rkt"
         "../evaluator/evaluator.rkt")

(provide (all-defined-out))

;; ============================================================================
;; BUILTIN INTEGRATION FOR GENERIC EQUALITY (HoTT-Native)
;; ============================================================================
;; Integrates the generic equality system with PathFinder's builtin environment
;; Provides both simple Boolean equality and proof-returning equality functions

;; ============================================================================
;; BUILTIN WRAPPER FUNCTIONS
;; ============================================================================

;; Generic equality that returns Bool (for compatibility with existing code)
(define/contract (generic-equal-builtin x y)
  (-> value/c value/c value/c)
  (cond
    [(and (constructor-value? x) (constructor-value? y))
     ;; Use the HoTT-native generic equality 
     (let ([type1 (constructor-value-type x)]
           [type2 (constructor-value-type y)])
       (if (hott-type-equal? type1 type2)
           (decidable-equal? type1 x y)
           false-value))]
    [else false-value]))

;; Generic equality that returns proof witness  
(define/contract (equal-with-proof-builtin x y)
  (-> value/c value/c value/c)
  (cond
    [(and (constructor-value? x) (constructor-value? y))
     (let ([proof (equal-with-proof? x y)])
       ;; Convert equality-proof to a sum type value: (Equal p) | (NotEqual p)
       (if (equality-proof-equal? proof)
           (constructor-value "Equal" 
                             (list (equality-proof-path-or-negation proof))
                             (inductive-type "EqualityResult" '()))
           (constructor-value "NotEqual"
                             (list (equality-proof-path-or-negation proof))
                             (inductive-type "EqualityResult" '()))))]
    [else (constructor-value "NotEqual" 
                            (list `(type-error ,x ,y))
                            (inductive-type "EqualityResult" '()))]))

;; Type-specific equality builtins (for backward compatibility)
(define/contract (nat-equal-builtin n1 n2)
  (-> value/c value/c value/c)
  (if (and (constructor-value? n1) (constructor-value? n2))
      (let ([proof (nat-equal-with-identity-proof n1 n2)])
        (if (equality-proof-equal? proof) true-value false-value))
      false-value))

(define/contract (bool-equal-builtin b1 b2)
  (-> value/c value/c value/c)
  (if (and (constructor-value? b1) (constructor-value? b2))
      (let ([proof (bool-equal-with-identity-proof b1 b2)])
        (if (equality-proof-equal? proof) true-value false-value))
      false-value))

(define/contract (list-equal-builtin element-type lst1 lst2)
  (-> value/c value/c value/c value/c)
  (if (and (constructor-value? lst1) (constructor-value? lst2))
      (let* ([list-type (make-list-type element-type)]
             [proof (list-equal-with-identity-proof list-type lst1 lst2)])
        (if (equality-proof-equal? proof) true-value false-value))
      false-value))

;; ============================================================================
;; UNIVERSE POLYMORPHIC EQUALITY BUILTINS
;; ============================================================================

;; Universe polymorphic equality: takes type as explicit parameter
;; equal : (A : Type₀) → A → A → Bool
(define/contract (universe-equal-builtin type-arg x y)
  (-> value/c value/c value/c value/c)
  (cond
    ;; Type argument should be a type (for now, we'll extract from values)
    [(and (constructor-value? x) (constructor-value? y))
     (generic-equal-builtin x y)]
    [else false-value]))

;; Equal with explicit type and proof: (A : Type₀) → A → A → (Id A x y) + ¬(Id A x y)  
(define/contract (universe-equal-with-proof-builtin type-arg x y)
  (-> value/c value/c value/c value/c)
  (cond
    [(and (constructor-value? x) (constructor-value? y))
     (equal-with-proof-builtin x y)]
    [else (constructor-value "NotEqual" 
                            (list `(invalid-args ,type-arg ,x ,y))
                            (inductive-type "EqualityResult" '()))]))

;; ============================================================================
;; DECIDABLE EQUALITY TYPE CLASS OPERATIONS  
;; ============================================================================

;; Check if a type has decidable equality
(define/contract (has-decidable-equality-builtin type-val)
  (-> value/c value/c)
  ;; For now, simplified check - in full implementation would inspect the type
  (match type-val
    [(constructor-value name _ _)
     (cond
       [(member name '("Nat" "Bool" "Unit" "String")) true-value]
       [(string-prefix? name "List-") true-value]
       [else false-value])]
    [_ false-value]))

;; Get equality instance for a type (returns function)
(define/contract (get-equality-instance-builtin type-val)
  (-> value/c value/c)
  ;; This would return a function value in the full implementation
  ;; For now, return a placeholder
  (constructor-value "EqualityInstance" 
                    (list type-val)
                    (inductive-type "TypeClass" '())))

;; ============================================================================
;; PATH AND IDENTITY TYPE OPERATIONS
;; ============================================================================

;; Reflexivity: refl : (A : Type₀) → (x : A) → Id A x x
(define/contract (reflexivity-builtin type-arg x)
  (-> value/c value/c value/c)
  (constructor-value "refl" 
                    (list type-arg x)
                    (identity-type type-arg x x)))

;; Symmetry: sym : (A : Type₀) → (x y : A) → Id A x y → Id A y x
(define/contract (symmetry-builtin type-arg x y path)
  (-> value/c value/c value/c value/c value/c)
  (constructor-value "sym"
                    (list path)
                    (identity-type type-arg y x)))

;; Transitivity: trans : (A : Type₀) → (x y z : A) → Id A x y → Id A y z → Id A x z
(define/contract (transitivity-builtin type-arg x y z path1 path2)
  (-> value/c value/c value/c value/c value/c value/c value/c)
  (constructor-value "trans"
                    (list path1 path2)
                    (identity-type type-arg x z)))

;; ============================================================================
;; ENVIRONMENT INTEGRATION
;; ============================================================================

;; Add generic equality functions to an environment
(define/contract (extend-environment-with-generic-equality! env)
  (-> environment? void?)
  ;; Replace the old "=" function with generic equality
  (let ([generic-eq-type (make-function-type 
                         (make-product-type Type0 Type0) Bool)])
    (env-define! env "=" (builtin-value "=" generic-equal-builtin generic-eq-type)))
  
  ;; Add proof-returning equality functions
  (let ([eq-with-proof-type (make-function-type 
                            (make-product-type Type0 Type0) 
                            (inductive-type "EqualityResult" '()))])
    (env-define! env "equal-with-proof" 
                (builtin-value "equal-with-proof" equal-with-proof-builtin eq-with-proof-type)))
  
  ;; Add universe polymorphic equality
  (let ([univ-eq-type (pi-type "A" Type0 
                              (make-function-type 
                               (make-product-type (pi-type "x" Type0 Type0) (pi-type "y" Type0 Type0))
                               Bool))])
    (env-define! env "equal" 
                (builtin-value "equal" universe-equal-builtin univ-eq-type))
    (env-define! env "equal-proof" 
                (builtin-value "equal-proof" universe-equal-with-proof-builtin univ-eq-type)))
  
  ;; Add type-specific equality for compatibility
  (let ([nat-eq-type (make-function-type (make-product-type Nat Nat) Bool)]
        [bool-eq-type (make-function-type (make-product-type Bool Bool) Bool)])
    (env-define! env "nat-equal?" (builtin-value "nat-equal?" nat-equal-builtin nat-eq-type))
    (env-define! env "bool-equal?" (builtin-value "bool-equal?" bool-equal-builtin bool-eq-type)))
  
  ;; Add identity type operations
  (let ([refl-type (pi-type "A" Type0 
                           (pi-type "x" Type0 
                                   (identity-type Type0 Type0 Type0)))]
        [sym-type (pi-type "A" Type0 
                          (pi-type "x" Type0
                                  (pi-type "y" Type0
                                          (make-function-type (identity-type Type0 Type0 Type0)
                                                             (identity-type Type0 Type0 Type0)))))])
    (env-define! env "refl" (builtin-value "refl" reflexivity-builtin refl-type))
    (env-define! env "sym" (builtin-value "sym" symmetry-builtin sym-type)))
  
  ;; Add decidable equality type class operations
  (let ([has-eq-type (make-function-type Type0 Bool)]
        [get-eq-type (make-function-type Type0 (inductive-type "TypeClass" '()))])
    (env-define! env "has-decidable-equality?" 
                (builtin-value "has-decidable-equality?" has-decidable-equality-builtin has-eq-type))
    (env-define! env "get-equality-instance" 
                (builtin-value "get-equality-instance" get-equality-instance-builtin get-eq-type)))
  
  (printf "✓ Generic equality functions added to builtin environment~n"))

;; Create new builtin environment with generic equality
(define/contract (make-builtin-environment-with-generic-equality)
  (-> environment?)
  (let ([env (builtin-environment)])  ; Get the original builtin environment
    (extend-environment-with-generic-equality! env)
    env))

;; ============================================================================
;; COMPATIBILITY LAYER
;; ============================================================================

;; Wrapper to maintain compatibility with existing code
(define/contract (upgrade-builtin-environment! env)
  (-> environment? environment?)
  ;; Extend existing environment with generic equality
  (extend-environment-with-generic-equality! env)
  env)

;; Helper to check if environment has generic equality
(define/contract (environment-has-generic-equality? env)
  (-> environment? boolean?)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (let ([equal-fn (env-lookup env "equal-with-proof")])
      (builtin-value? equal-fn))))

;; Auto-upgrade function for backwards compatibility
(define/contract (ensure-generic-equality-environment env)
  (-> environment? environment?)
  (if (environment-has-generic-equality? env)
      env
      (upgrade-builtin-environment! env)))

;; ============================================================================
;; INITIALIZATION
;; ============================================================================

;; Initialize generic equality in global environment
(define/contract (initialize-global-generic-equality!)
  (-> void?)
  ;; This will be called by the main evaluator initialization
  (printf "✓ Generic equality integration initialized~n"))