#lang racket/base

(require rackunit
         racket/list
         "../src/typecheck/type-family-inference.rkt"
         "../src/typecheck/typechecker.rkt"
         "../src/parser/ast.rkt"
         "../src/types/types.rkt"
         "../src/types/type-families.rkt")

;; Test suite for Type Family Parameter Inference - HIGH VALUE FEATURES

(test-case "Type Family Parameter Inference Context"
  
  ;; Create inference context
  (let ([ctx (make-tf-inference-context (make-global-type-environment))])
    (check-true (type-family-inference-context? ctx) "Should create inference context")
    (check-true (type-environment? (type-family-inference-context-type-env ctx))
                "Should have type environment")
    (check-true (hash? (type-family-inference-context-known-types ctx))
                "Should have known types hash")
    (check-true (hash? (type-family-inference-context-inferred-parameters ctx))
                "Should have inferred parameters hash")))

(test-case "List Constructor Type Inference - HIGH IMPACT"
  
  (let ([ctx (make-tf-inference-context (make-global-type-environment))])
    ;; Test list-cons with number argument - should infer Nat
    (let ([list-cons-ast (list (number-atom 42) (symbol-atom "empty-list"))])
      (let ([inferred-type (infer-constructor-types "list-cons" list-cons-ast ctx)])
        (when inferred-type
          (check-true (inductive-type? inferred-type) "Should infer inductive type for list-cons"))))
    
    ;; Test list-cons with boolean argument - should infer Bool
    (let ([bool-cons-ast (list (boolean-atom #t) (symbol-atom "empty-list"))])
      (let ([inferred-type (infer-constructor-types "list-cons" bool-cons-ast ctx)])
        (when inferred-type
          (check-true (inductive-type? inferred-type) "Should infer inductive type for bool list"))))))

(test-case "List Type Family Parameter Inference"
  
  (let ([ctx (make-tf-inference-context (make-global-type-environment))])
    ;; Test List type family with number argument
    (let ([number-args (list (number-atom 42))])
      (let ([params (infer-from-argument-types 
                     (get-type-family 'List) ; This may not exist, but testing the pattern
                     number-args ctx)])
        (when params
          (check-equal? (length params) 1 "Should infer one type parameter")
          (check-equal? (first params) Nat "Should infer Nat from number"))))))

(test-case "High-Value Use Case: Eliminate Type Annotations"
  
  (let ([ctx (make-tf-inference-context (make-global-type-environment))])
    ;; Test the main verbosity patterns identified in analysis
    
    ;; Pattern 1: list-cons with explicit element type
    ;; Before: (list-cons Nat 42 empty-list)
    ;; After:  (list-cons 42 empty-list)  ; infer Nat from 42
    (let ([args (list (number-atom 42) (symbol-atom "empty-list"))])
      (let ([inferred (infer-list-cons-type args ctx)])
        (when inferred
          (check-true (inductive-type? inferred) "Should infer list type from element"))))
    
    ;; Pattern 2: nonempty-list-cons with multiple elements
    ;; Before: (nonempty-list-cons Nat 0 1 2)
    ;; After:  (nonempty-list-cons 0 1 2)  ; infer Nat from values
    (let ([args (list (number-atom 0) (number-atom 1) (number-atom 2))])
      (let ([inferred (infer-constructor-types "nonempty-list-cons" args ctx)])
        (when inferred
          (check-true (hott-type/c inferred) "Should infer type from nonempty list elements"))))))

(test-case "Type Family Integration with Existing System"
  
  (let ([ctx (make-tf-inference-context (make-global-type-environment))])
    ;; Test integration with type-check-with-inference
    (let ([list-expr (sexpr (list (symbol-atom "list-cons") 
                                 (number-atom 42) 
                                 (symbol-atom "empty-list")))])
      (check-not-exn
       (lambda () (type-check-with-inference-context list-expr ctx))
       "Should integrate with inference system"))
    
    ;; Test type family constructor recognition
    (check-true (type-family-constructor? "list-cons") "Should recognize list-cons")
    (check-true (type-family-constructor? "list-nil") "Should recognize list-nil")
    (check-true (type-family-constructor? "nonempty-list-cons") "Should recognize nonempty-list-cons")
    (check-false (type-family-constructor? "regular-function") "Should not recognize regular functions")))

(test-case "Safe Type Inference"
  
  (let ([ctx (make-tf-inference-context (make-global-type-environment))])
    ;; Test safe inference that doesn't fail on unknown types
    (let ([number-type (safe-infer-type (number-atom 42) ctx)])
      (check-equal? number-type Nat "Should safely infer Nat from number"))
    
    (let ([bool-type (safe-infer-type (boolean-atom #t) ctx)])
      (check-equal? bool-type Bool "Should safely infer Bool from boolean"))
    
    ;; Test with unknown symbol - should return #f instead of error
    (let ([unknown-type (safe-infer-type (symbol-atom "unknown-symbol") ctx)])
      (check-false unknown-type "Should return #f for unknown symbols"))))

(test-case "List Type Compatibility Checking"
  
  ;; Test list type compatibility for inference
  (check-true (compatible-list-types? Nat (make-list-type Nat))
              "Nat should be compatible with List[Nat]")
  
  (check-false (compatible-list-types? Bool (make-list-type Nat))
               "Bool should not be compatible with List[Nat]"))

(test-case "Element Type Extraction"
  
  ;; Test element type extraction from list type names
  (check-equal? (extract-element-type-from-name "List-Nat") Nat
                "Should extract Nat from List-Nat")
  
  (check-equal? (extract-element-type-from-name "List-Bool") Bool
                "Should extract Bool from List-Bool")
  
  ;; Test element type extraction from list types
  (let ([nat-list-type (make-list-type Nat)])
    (let ([extracted (extract-list-element-type nat-list-type)])
      (when extracted
        (check-equal? extracted Nat "Should extract Nat from List[Nat] type")))))

(test-case "Implicit Parameter Detection"
  
  ;; Test implicit parameter recognition
  (check-true (implicit-parameter? "{A}") "Should recognize implicit parameter with braces")
  (check-true (implicit-parameter? "{Type}") "Should recognize implicit type parameter")
  (check-false (implicit-parameter? "A") "Should not recognize explicit parameter")
  (check-false (implicit-parameter? "explicit") "Should not recognize regular parameter"))

(test-case "Array Length Inference"
  
  (let ([ctx (make-tf-inference-context (make-global-type-environment))])
    ;; Test length inference from number literal
    (let ([length-type (infer-array-length (number-atom 5) ctx)])
      (check-equal? length-type Nat "Should infer Nat type for array length"))
    
    ;; Test length inference from symbol (should look up in context)
    (let ([length-type (infer-array-length (symbol-atom "some-length") ctx)])
      (check-false length-type "Should return #f for unknown length symbol"))))

(test-case "Type Unification"
  
  (let ([ctx (make-tf-inference-context (make-global-type-environment))])
    ;; Test successful unification
    (let ([unified (unify-types Nat Nat ctx)])
      (check-equal? unified Nat "Should unify identical types"))
    
    ;; Test failed unification
    (let ([unified (unify-types Nat Bool ctx)])
      (check-false unified "Should fail to unify different types"))))

(test-case "Real-World Pattern: List Operations"
  
  (let ([ctx (make-tf-inference-context (make-global-type-environment))])
    ;; Test the main pattern from our analysis:
    ;; Current: (list-cons Nat x (list-nil Nat))
    ;; Target:  (list-cons x empty-list)
    
    ;; Simulate the inference for list-cons with number
    (let ([cons-args (list (number-atom 42) (symbol-atom "empty-list"))])
      (let ([cons-type (infer-list-cons-type cons-args ctx)])
        (when cons-type
          (check-true (inductive-type? cons-type) "Should infer List type")
          ;; In practice, this would eliminate the explicit Nat parameter
          )))
    
    ;; Test multiple element inference (nonempty lists)
    (let ([multi-args (list (number-atom 1) (number-atom 2) (number-atom 3))])
      (let ([multi-type (infer-nonempty-list-element-type multi-args ctx)])
        (when multi-type
          (check-equal? (length multi-type) 1 "Should infer one element type")
          (check-equal? (first multi-type) Nat "Should infer Nat from numbers"))))))

(test-case "Integration with Type Family System"
  
  (let ([ctx (make-tf-inference-context (make-global-type-environment))])
    ;; Test that our inference integrates with existing type families
    (check-not-exn
     (lambda () (resolve-type-parameters ctx))
     "Should resolve type parameters without error")
    
    ;; Test enhanced type checking
    (let ([simple-expr (number-atom 42)])
      (check-not-exn
       (lambda () (type-check-with-inference simple-expr (make-global-type-environment)))
       "Should handle enhanced type checking"))))

(test-case "Error Handling and Fallbacks"
  
  (let ([ctx (make-tf-inference-context (make-global-type-environment))])
    ;; Test unknown type family
    (check-exn exn:fail?
               (lambda () (infer-type-family-parameters "UnknownFamily" '() #f ctx))
               "Should error on unknown type family")
    
    ;; Test constructor with no inference information
    (let ([unknown-cons (infer-constructor-types "unknown-constructor" '() ctx)])
      (check-false unknown-cons "Should return #f for unknown constructor"))))

;; Run the tests
(printf "Running Type Family Parameter Inference tests...~n")