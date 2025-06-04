#lang racket/base

(require "../src/types/types.rkt"
         rackunit)

;; Tests for PathFinder LISP HoTT-based Type System

;; Universe hierarchy tests
(test-case "universe creation and equality"
  (check-true (universe? Type0))
  (check-true (universe? Type1))  
  (check-equal? (universe-level Type0) 0)
  (check-equal? (universe-level Type1) 1)
  (check-true (hott-type-equal? Type0 Type0))
  (check-false (hott-type-equal? Type0 Type1)))

;; Basic type former tests
(test-case "unit and empty types"
  (check-true (unit-type? Unit))
  (check-true (empty-type? Empty))
  (check-true (hott-type-equal? Unit Unit))
  (check-true (hott-type-equal? Empty Empty))
  (check-false (hott-type-equal? Unit Empty)))

;; Π-type (function type) tests
(test-case "simple function types"
  (let ([nat-to-bool (make-function-type Nat Bool)])
    (check-true (pi-type? nat-to-bool))
    (check-equal? (pi-type-var-name nat-to-bool) "_")
    (check-true (hott-type-equal? (pi-type-domain nat-to-bool) Nat))
    (check-true (hott-type-equal? (pi-type-codomain nat-to-bool) Bool))))

(test-case "dependent function types"
  (let ([dep-func (make-pi-type "n" Nat Bool)])
    (check-true (pi-type? dep-func))
    (check-equal? (pi-type-var-name dep-func) "n")
    (check-true (hott-type-equal? (pi-type-domain dep-func) Nat))
    (check-true (hott-type-equal? (pi-type-codomain dep-func) Bool))))

(test-case "function type equality"
  (let ([f1 (make-function-type Nat Bool)]
        [f2 (make-function-type Nat Bool)]
        [f3 (make-function-type Bool Nat)])
    (check-true (hott-type-equal? f1 f2))
    (check-false (hott-type-equal? f1 f3))))

;; Σ-type (product/dependent pair) tests
(test-case "simple product types"
  (let ([nat-times-bool (make-product-type Nat Bool)])
    (check-true (sigma-type? nat-times-bool))
    (check-equal? (sigma-type-var-name nat-times-bool) "_")
    (check-true (hott-type-equal? (sigma-type-first-type nat-times-bool) Nat))
    (check-true (hott-type-equal? (sigma-type-second-type nat-times-bool) Bool))))

(test-case "dependent pair types"
  (let ([dep-pair (make-sigma-type "x" Nat Bool)])
    (check-true (sigma-type? dep-pair))
    (check-equal? (sigma-type-var-name dep-pair) "x")
    (check-true (hott-type-equal? (sigma-type-first-type dep-pair) Nat))
    (check-true (hott-type-equal? (sigma-type-second-type dep-pair) Bool))))

;; Sum type tests
(test-case "sum types"
  (let ([nat-or-bool (make-sum-type Nat Bool)])
    (check-true (sum-type? nat-or-bool))
    (check-true (hott-type-equal? (sum-type-left-type nat-or-bool) Nat))
    (check-true (hott-type-equal? (sum-type-right-type nat-or-bool) Bool))))

(test-case "sum type equality"
  (let ([s1 (make-sum-type Nat Bool)]
        [s2 (make-sum-type Nat Bool)]
        [s3 (make-sum-type Bool Nat)])
    (check-true (hott-type-equal? s1 s2))
    (check-false (hott-type-equal? s1 s3))))

;; Identity type tests
(test-case "identity types"
  (let ([id-type (make-identity-type Nat 'zero 'zero)])
    (check-true (identity-type? id-type))
    (check-true (hott-type-equal? (identity-type-type-expr id-type) Nat))
    (check-equal? (identity-type-left-term id-type) 'zero)
    (check-equal? (identity-type-right-term id-type) 'zero)))

;; Inductive type tests
(test-case "natural numbers inductive type"
  (check-true (inductive-type? Nat))
  (check-equal? (inductive-type-name Nat) "Nat")
  (check-equal? (length (inductive-type-constructors Nat)) 2))

(test-case "boolean inductive type"
  (check-true (inductive-type? Bool))
  (check-equal? (inductive-type-name Bool) "Bool")
  (check-equal? (length (inductive-type-constructors Bool)) 2))

(test-case "list type construction"
  (let ([nat-list (make-list-type Nat)])
    (check-true (inductive-type? nat-list))
    (check-equal? (inductive-type-name nat-list) "List-Nat")
    (check-equal? (length (inductive-type-constructors nat-list)) 2)))

;; Universe level tests
(test-case "basic types in Type₀"
  (check-true (type-in-universe? Unit 0))
  (check-true (type-in-universe? Empty 0))
  (check-true (type-in-universe? Nat 0))
  (check-true (type-in-universe? Bool 0)))

(test-case "type universe levels"
  (check-equal? (type-universe-level Unit) 0)
  (check-equal? (type-universe-level Nat) 0)
  (check-equal? (type-universe-level Type0) 1)
  (check-equal? (type-universe-level Type1) 2))

(test-case "function type universe levels"
  (let ([nat-to-bool (make-function-type Nat Bool)])
    (check-equal? (type-universe-level nat-to-bool) 0)
    (check-true (type-in-universe? nat-to-bool 0))))

;; Pretty printing tests
(test-case "type->string for basic types"
  (check-equal? (type->string Unit) "𝟙")
  (check-equal? (type->string Empty) "𝟘") 
  (check-equal? (type->string Nat) "Nat")
  (check-equal? (type->string Bool) "Bool"))

(test-case "type->string for universes"
  (check-equal? (type->string Type0) "Type0")
  (check-equal? (type->string Type1) "Type1")
  (check-equal? (type->string Type2) "Type2"))

(test-case "type->string for function types"
  (let ([nat-to-bool (make-function-type Nat Bool)])
    (check-equal? (type->string nat-to-bool) "(Nat → Bool)")))

(test-case "type->string for dependent functions"
  (let ([dep-func (make-pi-type "n" Nat Bool)])
    (check-equal? (type->string dep-func) "(Π (n : Nat) Bool)")))

(test-case "type->string for product types"
  (let ([prod (make-product-type Nat Bool)])
    (check-equal? (type->string prod) "(Nat × Bool)")))

(test-case "type->string for dependent pairs"
  (let ([dep-pair (make-sigma-type "x" Nat Bool)])
    (check-equal? (type->string dep-pair) "(Σ (x : Nat) Bool)")))

(test-case "type->string for sum types"
  (let ([sum (make-sum-type Nat Bool)])
    (check-equal? (type->string sum) "(Nat + Bool)")))

(test-case "type->string for identity types"
  (let ([id (make-identity-type Nat 'zero 'zero)])
    (check-equal? (type->string id) "(Id Nat zero zero)")))

;; Complex type construction tests
(test-case "nested function types"
  (let* ([nat-to-bool (make-function-type Nat Bool)]
         [higher-order (make-function-type nat-to-bool Bool)])
    (check-equal? (type->string higher-order) "((Nat → Bool) → Bool)")))

(test-case "function returning product"
  (let* ([prod (make-product-type Nat Bool)]
         [func (make-function-type Unit prod)])
    (check-equal? (type->string func) "(𝟙 → (Nat × Bool))")))

(test-case "complex nested types"
  (let* ([list-nat (make-list-type Nat)]
         [func-type (make-function-type Nat list-nat)]
         [prod-type (make-product-type func-type Bool)])
    (check-true (hott-type? prod-type))
    (check-equal? (type->string prod-type) "((Nat → List-Nat) × Bool)")))

;; Type validation tests
(test-case "valid-hott-type? validation"
  (check-true (valid-hott-type? Nat))
  (check-true (valid-hott-type? (make-function-type Nat Bool)))
  (check-true (valid-hott-type? Type0))
  (check-false (valid-hott-type? "not a type"))
  (check-false (valid-hott-type? 42)))

;; Debug the failing test
(test-case "debug hott-type predicate"
  (check-true (hott-type? Nat))
  (check-true (hott-type? Type0)))