#lang racket/base

(require "../src/types/types.rkt"
         rackunit)

;; Tests for PathFinder LISP Type System

;; Base type structure tests
(test-case "type-atom creation"
  (let ([t (make-type-atom "MyType")])
    (check-true (type? t))
    (check-true (type-atom? t))
    (check-equal? (type-atom-name t) "MyType")))

(test-case "function-type creation"
  (let ([ft (make-function-type (list Nat Bool) String)])
    (check-true (type? ft))
    (check-true (function-type? ft))
    (check-equal? (length (function-type-param-types ft)) 2)
    (check-equal? (function-type-return-type ft) String)))

;; Predefined primitive types tests
(test-case "predefined Nat type"
  (check-true (type-atom? Nat))
  (check-equal? (type-atom-name Nat) "Nat")
  (check-true (primitive-type? Nat)))

(test-case "predefined Bool type"
  (check-true (type-atom? Bool))
  (check-equal? (type-atom-name Bool) "Bool")
  (check-true (primitive-type? Bool)))

(test-case "predefined String type"
  (check-true (type-atom? String))
  (check-equal? (type-atom-name String) "String")
  (check-true (primitive-type? String)))

;; Type equality tests
(test-case "atomic type equality - same types"
  (check-true (type-equal? Nat Nat))
  (check-true (type-equal? Bool Bool))
  (check-true (type-equal? String String)))

(test-case "atomic type equality - different types"
  (check-false (type-equal? Nat Bool))
  (check-false (type-equal? Bool String))
  (check-false (type-equal? String Nat)))

(test-case "atomic type equality - custom types"
  (let ([t1 (make-type-atom "Custom")]
        [t2 (make-type-atom "Custom")]
        [t3 (make-type-atom "Different")])
    (check-true (type-equal? t1 t2))
    (check-false (type-equal? t1 t3))))

(test-case "function type equality - simple"
  (let ([ft1 (make-function-type (list Nat) Bool)]
        [ft2 (make-function-type (list Nat) Bool)]
        [ft3 (make-function-type (list Bool) Nat)])
    (check-true (type-equal? ft1 ft2))
    (check-false (type-equal? ft1 ft3))))

(test-case "function type equality - multiple parameters"
  (let ([ft1 (make-function-type (list Nat Bool) String)]
        [ft2 (make-function-type (list Nat Bool) String)]
        [ft3 (make-function-type (list Bool Nat) String)]
        [ft4 (make-function-type (list Nat Bool) Nat)])
    (check-true (type-equal? ft1 ft2))
    (check-false (type-equal? ft1 ft3))
    (check-false (type-equal? ft1 ft4))))

(test-case "function type equality - different arities"
  (let ([ft1 (make-function-type (list Nat) Bool)]
        [ft2 (make-function-type (list Nat Bool) String)])
    (check-false (type-equal? ft1 ft2))))

(test-case "function type equality - nested functions"
  (let* ([inner1 (make-function-type (list Nat) Bool)]
         [inner2 (make-function-type (list Nat) Bool)]
         [ft1 (make-function-type (list inner1) String)]
         [ft2 (make-function-type (list inner2) String)])
    (check-true (type-equal? ft1 ft2))))

(test-case "mixed type equality"
  (check-false (type-equal? Nat (make-function-type (list Bool) String))))

;; Helper function tests
(test-case "unary function type helper"
  (let ([ft1 (make-unary-function-type Nat Bool)]
        [ft2 (make-function-type (list Nat) Bool)])
    (check-true (type-equal? ft1 ft2))))

(test-case "binary function type helper"
  (let ([ft1 (make-binary-function-type Nat Bool String)]
        [ft2 (make-function-type (list Nat Bool) String)])
    (check-true (type-equal? ft1 ft2))))

;; Pretty printing tests
(test-case "type->string for atomic types"
  (check-equal? (type->string Nat) "Nat")
  (check-equal? (type->string Bool) "Bool")
  (check-equal? (type->string String) "String"))

(test-case "type->string for unary functions"
  (let ([ft (make-unary-function-type Nat Bool)])
    (check-equal? (type->string ft) "(Nat → Bool)")))

(test-case "type->string for binary functions"
  (let ([ft (make-binary-function-type Nat Bool String)])
    (check-equal? (type->string ft) "(Nat × Bool → String)")))

(test-case "type->string for nullary functions"
  (let ([ft (make-function-type '() Bool)])
    (check-equal? (type->string ft) "(→ Bool)")))

(test-case "type->string for nested functions"
  (let* ([inner (make-unary-function-type Nat Bool)]
         [outer (make-unary-function-type inner String)])
    (check-equal? (type->string outer) "((Nat → Bool) → String)")))

;; Type validation tests
(test-case "valid-type? for atomic types"
  (check-true (valid-type? Nat))
  (check-true (valid-type? Bool))
  (check-true (valid-type? String))
  (check-true (valid-type? (make-type-atom "Custom"))))

(test-case "valid-type? for function types"
  (check-true (valid-type? (make-function-type (list Nat) Bool)))
  (check-true (valid-type? (make-function-type '() String))))

(test-case "valid-type? for invalid values"
  (check-false (valid-type? "not a type"))
  (check-false (valid-type? 42))
  (check-false (valid-type? #f)))

;; Primitive type checking
(test-case "primitive-type? positive cases"
  (check-true (primitive-type? Nat))
  (check-true (primitive-type? Bool))
  (check-true (primitive-type? String)))

(test-case "primitive-type? negative cases"
  (check-false (primitive-type? (make-type-atom "Custom")))
  (check-false (primitive-type? (make-function-type (list Nat) Bool))))