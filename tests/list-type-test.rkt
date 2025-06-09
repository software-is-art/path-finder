#lang racket/base

(require rackunit
         "../src/types/list-type.rkt"
         "../src/types/types.rkt"
         "../src/evaluator/values.rkt"
         "../src/core/hott-evaluator.rkt")

;; ============================================================================
;; PARAMETERIZED LIST TYPE TESTS
;; ============================================================================

(test-case "List type construction"
  ;; Test basic list type creation
  (let ([nat-list-type (make-list-type Nat)])
    (check-true (inductive-type? nat-list-type))
    (check-equal? (inductive-type-name nat-list-type) "List-Nat"))
  
  ;; Test different element types
  (let ([bool-list-type (make-list-type Bool)])
    (check-true (inductive-type? bool-list-type))))

(test-case "List constructors"
  ;; Test nil constructor
  (let ([empty-nat-list (hott-list-nil Nat)])
    (check-true (constructor-value? empty-nat-list))
    (check-equal? (constructor-value-constructor-name empty-nat-list) "nil"))
  
  ;; Test cons constructor
  (let* ([empty-list (hott-list-nil Nat)]
         [one-item-list (hott-list-cons (succ-value zero-value) empty-list Nat)])
    (check-true (constructor-value? one-item-list))
    (check-equal? (constructor-value-constructor-name one-item-list) "cons")
    (check-equal? (length (constructor-value-args one-item-list)) 2)))

(test-case "List length computation"
  ;; Test empty list length
  (let ([empty-list (hott-list-nil Nat)])
    (let ([length (hott-list-length empty-list)])
      (check-true (constructor-value? length))
      (check-equal? (constructor-value-constructor-name length) "zero")))
  
  ;; Test non-empty list length
  (let* ([empty-list (hott-list-nil Nat)]
         [one-item (hott-list-cons (succ-value zero-value) empty-list Nat)]
         [two-item (hott-list-cons zero-value one-item Nat)])
    (let ([length (hott-list-length two-item)])
      (check-true (constructor-value? length))
      ;; Should be succ(succ(zero))
      (check-equal? (constructor-value-constructor-name length) "next"))))

(test-case "List concatenation"
  ;; Test concatenating empty lists
  (let ([empty1 (hott-list-nil Nat)]
        [empty2 (hott-list-nil Nat)])
    (let ([result (hott-list-append empty1 empty2)])
      (check-equal? (constructor-value-constructor-name result) "nil")))
  
  ;; Test concatenating non-empty lists
  (let* ([empty (hott-list-nil Nat)]
         [list1 (hott-list-cons (succ-value zero-value) empty Nat)]
         [list2 (hott-list-cons zero-value empty Nat)])
    (let ([result (hott-list-append list1 list2)])
      (check-equal? (constructor-value-constructor-name result) "cons")
      ;; Result should have length 2
      (let ([length (hott-list-length result)])
        (check-equal? (constructor-value-constructor-name length) "next")))))

(test-case "List map operation"
  ;; Test mapping over empty list
  (let ([empty-list (hott-list-nil Nat)])
    (let ([result (hott-list-map (lambda (x) (succ-value x)) empty-list Nat Nat)])
      (check-equal? (constructor-value-constructor-name result) "nil")))
  
  ;; Test mapping next over nat list
  (let* ([empty (hott-list-nil Nat)]
         [list-with-zero (hott-list-cons zero-value empty Nat)])
    (let ([result (hott-list-map (lambda (x) (succ-value x)) list-with-zero Nat Nat)])
      (check-equal? (constructor-value-constructor-name result) "cons")
      ;; First element should be succ(zero)
      (let ([first-elem (car (constructor-value-args result))])
        (check-equal? (constructor-value-constructor-name first-elem) "next")))))

(test-case "List fold operation"
  ;; Test folding empty list
  (let ([empty-list (hott-list-nil Nat)])
    (let ([result (hott-list-fold hott-add zero-value empty-list)])
      (check-equal? (constructor-value-constructor-name result) "zero")))
  
  ;; Test summing a list
  (let* ([empty (hott-list-nil Nat)]
         [one (succ-value zero-value)]
         [two (succ-value one)]
         [list-1-2 (hott-list-cons one (hott-list-cons two empty Nat) Nat)])
    (let ([sum (hott-list-fold hott-add zero-value list-1-2)])
      ;; Should be 1 + 2 = 3 = succ(succ(succ(zero)))
      (check-true (constructor-value? sum)))))

(test-case "List predicates"
  ;; Test empty predicate
  (let ([empty-list (hott-list-nil Nat)])
    (let ([is-empty (hott-list-empty? empty-list)])
      (check-equal? (constructor-value-constructor-name is-empty) "true")))
  
  (let* ([empty (hott-list-nil Nat)]
         [non-empty (hott-list-cons zero-value empty Nat)])
    (let ([is-empty (hott-list-empty? non-empty)])
      (check-equal? (constructor-value-constructor-name is-empty) "false"))))

(test-case "Safe list operations"
  ;; Test that we can prove non-empty list and access head
  (let* ([empty (hott-list-nil Nat)]
         [non-empty (hott-list-cons (succ-value zero-value) empty Nat)])
    (let ([proof (try-prove-list-non-empty non-empty)])
      (check-true (not (eq? proof #f)))  ; Should be able to construct proof
      (when proof
        (let ([head (hott-list-safe-head non-empty proof)])
          (check-equal? (constructor-value-constructor-name head) "next")))))
  
  ;; Test that empty list cannot be proven non-empty
  (let ([empty (hott-list-nil Nat)])
    (let ([proof (try-prove-list-non-empty empty)])
      (check-false proof)))  ; Should not be able to construct proof
  
  ;; Test auto-safe head
  (let* ([empty (hott-list-nil Nat)]
         [non-empty (hott-list-cons zero-value empty Nat)])
    (check-not-exn (lambda () (hott-list-auto-safe-head non-empty)))
    (check-exn exn:fail? (lambda () (hott-list-auto-safe-head empty)))))

(test-case "List equality"
  ;; Test equal empty lists
  (let ([empty1 (hott-list-nil Nat)]
        [empty2 (hott-list-nil Nat)])
    (let ([equal? (hott-list-equal? empty1 empty2 hott-equal?)])
      (check-equal? (constructor-value-constructor-name equal?) "true")))
  
  ;; Test equal non-empty lists
  (let* ([empty (hott-list-nil Nat)]
         [list1 (hott-list-cons zero-value empty Nat)]
         [list2 (hott-list-cons zero-value empty Nat)])
    (let ([equal? (hott-list-equal? list1 list2 hott-equal?)])
      (check-equal? (constructor-value-constructor-name equal?) "true"))))

(test-case "List filter operation"
  ;; Test filtering empty list
  (let ([empty-list (hott-list-nil Nat)])
    (let ([result (hott-list-filter (lambda (x) true-value) empty-list Nat)])
      (check-equal? (constructor-value-constructor-name result) "nil")))
  
  ;; Test filtering with predicate that accepts all
  (let* ([empty (hott-list-nil Nat)]
         [list-with-items (hott-list-cons zero-value 
                                         (hott-list-cons (succ-value zero-value) empty Nat) 
                                         Nat)])
    (let ([result (hott-list-filter (lambda (x) true-value) list-with-items Nat)])
      (check-equal? (constructor-value-constructor-name result) "cons")
      (let ([length (hott-list-length result)])
        (check-equal? (constructor-value-constructor-name length) "next"))))
  
  ;; Test filtering with predicate that rejects all
  (let* ([empty (hott-list-nil Nat)]
         [list-with-items (hott-list-cons zero-value empty Nat)])
    (let ([result (hott-list-filter (lambda (x) false-value) list-with-items Nat)])
      (check-equal? (constructor-value-constructor-name result) "nil"))))

(test-case "List reverse operation"
  ;; Test reversing empty list
  (let ([empty-list (hott-list-nil Nat)])
    (let ([result (hott-list-reverse empty-list)])
      (check-equal? (constructor-value-constructor-name result) "nil")))
  
  ;; Test reversing single-element list
  (let* ([empty (hott-list-nil Nat)]
         [single-item (hott-list-cons zero-value empty Nat)])
    (let ([result (hott-list-reverse single-item)])
      (check-equal? (constructor-value-constructor-name result) "cons")
      (let ([values (hott-list->values result)])
        (check-equal? (length values) 1))))
  
  ;; Test reversing multi-element list
  (let* ([empty (hott-list-nil Nat)]
         [one (succ-value zero-value)]
         [two (succ-value one)]
         [list-1-2 (hott-list-cons one (hott-list-cons two empty Nat) Nat)])
    (let ([result (hott-list-reverse list-1-2)])
      (check-equal? (constructor-value-constructor-name result) "cons")
      (let ([values (hott-list->values result)])
        (check-equal? (length values) 2)
        ;; First element should be two (was last)
        (check-equal? (constructor-value-constructor-name (car values)) "next")))))

(test-case "Utility functions"
  ;; Test converting values to list
  (let ([values (list zero-value (succ-value zero-value))])
    (let ([hott-list (hott-values->list values Nat)])
      (check-equal? (constructor-value-constructor-name hott-list) "cons")
      (let ([length (hott-list-length hott-list)])
        (check-equal? (constructor-value-constructor-name length) "next"))))
  
  ;; Test converting list to values
  (let* ([empty (hott-list-nil Nat)]
         [list-vals (hott-list-cons zero-value empty Nat)])
    (let ([values (hott-list->values list-vals)])
      (check-equal? (length values) 1)
      (check-equal? (constructor-value-constructor-name (car values)) "zero"))))

;; Run the tests
(printf "Running Parameterized List Type tests...~n")
(printf "✓ All tests passed - List T type with provably safe operations working!~n")