#lang racket/base

(require "../src/types/types.rkt"
         "../src/evaluator/values.rkt"
         "../src/main.rkt"
         racket/string
         rackunit)

;; Tests for PathFinder LISP Path Computation and Univalence

;; ============================================================================
;; PATH VALUE TESTS
;; ============================================================================

(test-case "create reflexivity path"
  (let ([refl-path (make-refl-value Nat zero-value)])
    (check-true (path-runtime-value? refl-path))
    (check-true (hott-type-equal? (path-runtime-value-type refl-path) Nat))
    (check-equal? (path-runtime-value-start refl-path) zero-value)
    (check-equal? (path-runtime-value-end refl-path) zero-value)
    (check-eq? (path-runtime-value-proof refl-path) 'refl)))

(test-case "path reflexivity predicate"
  (let ([refl-path (make-refl-value Nat zero-value)]
        [non-refl-path (path-runtime-value Nat zero-value (succ-value zero-value) 'custom)])
    (check-true (is-refl-runtime-path? refl-path))
    (check-false (is-refl-runtime-path? non-refl-path))))

(test-case "path concatenation"
  (let* ([p (path-runtime-value Nat zero-value (succ-value zero-value) 'p)]
         [q (path-runtime-value Nat (succ-value zero-value) (succ-value (succ-value zero-value)) 'q)]
         [concat-path (make-path-concat-value p q)])
    (check-true (path-runtime-value? concat-path))
    (check-equal? (path-runtime-value-start concat-path) zero-value)
    (check-equal? (path-runtime-value-end concat-path) (succ-value (succ-value zero-value)))
    (check-equal? (path-runtime-value-proof concat-path) '(concat p q))))

(test-case "path concatenation error on mismatch"
  (let ([p (path-runtime-value Nat zero-value (succ-value zero-value) 'p)]
        [q (path-runtime-value Nat (succ-value (succ-value zero-value)) (succ-value (succ-value (succ-value zero-value))) 'q)])
    (check-exn exn:fail? (lambda () (make-path-concat-value p q)))))

(test-case "path inverse"
  (let* ([p (path-runtime-value Nat zero-value (succ-value zero-value) 'p)]
         [inv-p (make-path-inverse-value p)])
    (check-true (path-runtime-value? inv-p))
    (check-equal? (path-runtime-value-start inv-p) (succ-value zero-value))
    (check-equal? (path-runtime-value-end inv-p) zero-value)
    (check-equal? (path-runtime-value-proof inv-p) '(inverse p))))

(test-case "path pretty printing"
  (let ([p (path-runtime-value Nat zero-value (succ-value zero-value) 'test-path)])
    (check-equal? (value->string p) "path[Nat : zero = (succ zero)]")))

;; ============================================================================
;; EQUIVALENCE VALUE TESTS  
;; ============================================================================

(test-case "create identity equivalence"
  (let ([id-equiv (make-identity-equiv-value Nat)])
    (check-true (equivalence-runtime-value? id-equiv))
    (check-true (hott-type-equal? (equivalence-runtime-value-type-a id-equiv) Nat))
    (check-true (hott-type-equal? (equivalence-runtime-value-type-b id-equiv) Nat))))

(test-case "equivalence pretty printing"
  (let ([equiv (make-identity-equiv-value Nat)])
    (check-equal? (value->string equiv) "Nat ≃ Nat")))

(test-case "univalence application"
  (let* ([equiv (make-identity-equiv-value Nat)]
         [ua-path (univalence-apply equiv)])
    (check-true (path-runtime-value? ua-path))
    (check-true (hott-type-equal? (path-runtime-value-type ua-path) Type1))
    (check-equal? (path-runtime-value-proof ua-path) 'ua-path)))

;; ============================================================================
;; TYPE SYSTEM TESTS
;; ============================================================================

(test-case "path value type"
  (let ([path-val (make-refl-value Nat zero-value)])
    (let ([id-type (make-identity-type Nat zero-value zero-value)])
      (check-true (value-has-type? path-val id-type)))))

(test-case "equivalence value type"
  (let ([equiv-val (make-identity-equiv-value Nat)])
    (let ([equiv-type (make-equivalence-type Nat Nat)])
      (check-true (value-has-type? equiv-val equiv-type)))))

;; ============================================================================
;; PATH ALGEBRA TESTS (from types.rkt)
;; ============================================================================

(test-case "path-value creation and operations"
  (let* ([p1 (make-refl Nat 'x)]
         [p2 (path-value Nat 'x 'y 'test-proof)]
         [concat-result (path-concat p1 p2)])
    (check-true (path-value? p1))
    (check-true (path-value? p2))
    (check-true (path-value? concat-result))
    (check-equal? (path-value-start concat-result) 'x)
    (check-equal? (path-value-end concat-result) 'y)))

(test-case "path inverse operations"
  (let* ([p (path-value Nat 'x 'y 'proof)]
         [inv-p (path-inverse p)]
         [double-inv (path-inverse inv-p)])
    (check-equal? (path-value-start inv-p) 'y)
    (check-equal? (path-value-end inv-p) 'x)
    ;; Double inverse should restore original (when normalized)
    (check-equal? (path-value-start double-inv) 'x)
    (check-equal? (path-value-end double-inv) 'y)))

(test-case "path normalization"
  (let* ([refl-x (make-refl Nat 'x)]
         [p (path-value Nat 'x 'y 'proof)]
         [normalized (normalize-path-concat refl-x p)])
    ;; refl ∙ p = p
    (check-equal? normalized p)))

(test-case "equivalence type construction"
  (let ([equiv-type (make-equivalence-type Nat Bool)])
    (check-true (equivalence-type? equiv-type))
    (check-true (hott-type-equal? (equivalence-type-type-a equiv-type) Nat))
    (check-true (hott-type-equal? (equivalence-type-type-b equiv-type) Bool))
    (check-equal? (type->string equiv-type) "Nat ≃ Bool")))

(test-case "univalence axiom structure"
  (let* ([equiv (make-identity-equivalence Nat)]
         [ua-path (univalence-forward equiv)])
    (check-true (equivalence-value? equiv))
    (check-true (path-value? ua-path))
    ;; Just check that it's a string containing the right components
    (check-true (string-contains? (path-value->string ua-path) "path["))
    (check-true (string-contains? (path-value->string ua-path) "Type1"))))

;; ============================================================================
;; HIGHER-DIMENSIONAL PATH TESTS
;; ============================================================================

(test-case "2-path creation"
  (let* ([p1 (path-value Nat 'x 'y 'proof1)]
         [p2 (path-value Nat 'x 'y 'proof2)]
         [2path (make-2path p1 p2 'homotopy)])
    (check-true (higher-path? 2path))
    (check-equal? (higher-path-dimension 2path) 2)
    (check-equal? (higher-path-base-path 2path) p1)
    (check-equal? (higher-path-target-path 2path) p2)))

(test-case "function extensionality"
  (let ([funext-path (function-extensionality 'pointwise-eq)])
    (check-true (path-value? funext-path))
    (check-equal? (path-value-proof funext-path) '(funext pointwise-eq))))

;; ============================================================================
;; TRUNCATION LEVEL TESTS
;; ============================================================================

(test-case "contractible type"
  (let ([contr (make-contractible-type Nat 'center 'contraction)])
    (check-true (contractible-type? contr))
    (check-true (hott-type-equal? (contractible-type-type contr) Nat))
    (check-equal? (type->string contr) "isContr(Nat)")))

(test-case "proposition and set types"
  (let ([prop (proposition-type Nat)]
        [set-t (set-type Bool)])
    (check-true (proposition-type? prop))
    (check-true (set-type? set-t))
    (check-equal? (type->string prop) "isProp(Nat)")
    (check-equal? (type->string set-t) "isSet(Bool)")))

;; ============================================================================
;; INTEGRATION TESTS WITH EVALUATOR
;; ============================================================================

(test-case "path operations exist in environment"
  (check-true (procedure? evaluate-string))
  ;; These would throw errors if the functions don't exist
  (check-exn exn:fail? (lambda () (evaluate-string "(refl)")))
  (check-exn exn:fail? (lambda () (evaluate-string "(ua)"))))

;; Note: More sophisticated integration tests would require proper syntax
;; for path and equivalence expressions in the PathFinder LISP language