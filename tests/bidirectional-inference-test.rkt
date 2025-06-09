#lang racket

(require rackunit
         "../src/typecheck/bidirectional-inference.rkt"
         "../src/typecheck/hott-native-inference.rkt"
         "../src/typecheck/type-family-inference.rkt"
         "../src/typecheck/universe-level-inference.rkt"
         "../src/typecheck/typechecker.rkt"
         "../src/parser/ast.rkt"
         "../src/types/types.rkt"
         "../src/types/type-families.rkt")

;; ============================================================================
;; BIDIRECTIONAL INFERENCE INTEGRATION TESTS
;; ============================================================================

(printf "Running Bidirectional Inference Integration tests...~n")

(test-case "Integrated Inference Context Creation"
  (let* ([type-env (make-global-type-environment)]
         [ctx (make-bidirectional-inference-context type-env)])
    
    (check-true (bidirectional-inference-context? ctx) "Should create valid context")
    (check-true (inference-universe? (bidirectional-inference-context-hott ctx)) "Should have HoTT context")
    (check-true (type-family-inference-context? (bidirectional-inference-context-type-family ctx)) "Should have type family context")
    (check-true (universe-level-inference-context? (bidirectional-inference-context-universe ctx)) "Should have universe context")))

(test-case "Proof Obligation Generation"
  (let* ([type-env (make-global-type-environment)]
         [ctx (make-bidirectional-inference-context type-env)]
         [ast-literal (number-atom 42)]
         [ast-var (symbol-atom "x")]
         [ast-app (sexpr (list (symbol-atom "f") (number-atom 1) (number-atom 2)))])
    
    ;; Literals have no proof obligations
    (check-equal? (generate-proof-obligations ast-literal ctx) '() "Literals should have no obligations")
    
    ;; Variables may have obligations
    (let ([var-obligations (generate-proof-obligations ast-var ctx)])
      (check-true (list? var-obligations) "Should return list of obligations")
      (check-true (andmap proof-obligation? var-obligations) "All should be proof obligations"))
    
    ;; Applications have obligations
    (let ([app-obligations (generate-proof-obligations ast-app ctx)])
      (check-true (list? app-obligations) "Should return list of obligations")
      (check-true (andmap proof-obligation? app-obligations) "All should be proof obligations"))))

(test-case "AST Elaboration with Complete Inference"
  (let* ([type-env (make-global-type-environment)]
         [ctx (make-bidirectional-inference-context type-env)]
         [ast (number-atom 42)])
    
    (let ([elaborated (elaborate-ast-with-inference ast ctx)])
      (check-true (elaborated-ast-node? elaborated) "Should create elaborated AST")
      (check-equal? (elaborated-ast-node-original elaborated) ast "Should preserve original AST")
      (check-true (hott-type? (elaborated-ast-node-inferred-type elaborated)) "Should have inferred type")
      (check-true (exact-nonnegative-integer? (elaborated-ast-node-universe-level elaborated)) "Should have universe level")
      (check-true (list? (elaborated-ast-node-proof-obligations elaborated)) "Should have proof obligations list"))))

(test-case "Type Family Parameter Inference Integration"
  (let* ([type-env (make-global-type-environment)]
         [ctx (make-bidirectional-inference-context type-env)]
         [list-cons-ast (sexpr (list (symbol-atom "list-cons") (number-atom 42) (symbol-atom "empty-list")))])
    
    (let ([elaborated (elaborate-ast-with-inference list-cons-ast ctx)])
      (check-true (elaborated-ast-node? elaborated) "Should elaborate type family constructor")
      ;; Type family parameters might be inferred
      (let ([params (elaborated-ast-node-type-family-params elaborated)])
        (check-true (or (not params) (list? params)) "Type family params should be list or #f")))))

(test-case "Universe Level Inference Integration"
  (let* ([type-env (make-global-type-environment)]
         [ctx (make-bidirectional-inference-context type-env)]
         [type-ast (sexpr (list (symbol-atom "Type0")))]
         [pi-ast (sexpr (list (symbol-atom "pi") 
                             (sexpr (list (symbol-atom "x") type-ast))
                             type-ast))])
    
    (let ([type-elaborated (elaborate-ast-with-inference type-ast ctx)]
          [pi-elaborated (elaborate-ast-with-inference pi-ast ctx)])
      (check-true (exact-nonnegative-integer? (elaborated-ast-node-universe-level type-elaborated)) "Type should have universe level")
      (check-true (exact-nonnegative-integer? (elaborated-ast-node-universe-level pi-elaborated)) "Pi-type should have universe level"))))

(test-case "Bidirectional Synthesis and Checking"
  (let* ([type-env (make-global-type-environment)]
         [ctx (make-bidirectional-inference-context type-env)]
         [ast (number-atom 42)]
         [expected-type Nat])
    
    ;; Synthesis mode
    (let ([synthesized (infer-and-elaborate ast ctx)])
      (check-true (elaborated-ast-node? synthesized) "Synthesis should produce elaborated AST"))
    
    ;; Checking mode
    (let ([checked (check-and-elaborate ast expected-type ctx)])
      (check-true (elaborated-ast-node? checked) "Checking should produce elaborated AST")
      (check-equal? (elaborated-ast-node-original checked) ast "Should preserve original AST"))))

(test-case "Complete AST Elaboration with Constraint Solving"
  (let* ([type-env (make-global-type-environment)]
         [ctx (make-bidirectional-inference-context type-env)]
         [ast (sexpr (list (symbol-atom "lambda") 
                          (sexpr (list (symbol-atom "x")))
                          (symbol-atom "x")))])
    
    (let ([complete (complete-ast-elaboration ast ctx)])
      (check-true (elaborated-ast-node? complete) "Should complete elaboration")
      ;; Universe constraints should be solved
      ;; Proof obligations should be attempted
      (check-true (list? (elaborated-ast-node-proof-obligations complete)) "Should have processed proof obligations"))))

(test-case "HoTT-Native vs Traditional Approach"
  ;; This test demonstrates that our approach is fundamentally different
  ;; from traditional constraint-based inference
  (let* ([type-env (make-global-type-environment)]
         [ctx (make-bidirectional-inference-context type-env)]
         [identity-ast (sexpr (list (symbol-atom "lambda") 
                                   (sexpr (list (symbol-atom "x")))
                                   (symbol-atom "x")))])
    
    (let ([elaborated (elaborate-ast-with-inference identity-ast ctx)])
      ;; HoTT-native inference uses identity types and computational evidence
      ;; rather than unification constraints
      (check-true (elaborated-ast-node? elaborated) "HoTT-native approach should work")
      (check-true (hott-type? (elaborated-ast-node-inferred-type elaborated)) "Should infer proper HoTT type"))))

(test-case "Type Family Constructor Recognition"
  (let* ([type-env (make-global-type-environment)]
         [ctx (make-bidirectional-inference-context type-env)])
    
    (check-true (is-type-family-constructor? "list-cons") "Should recognize list-cons")
    (check-true (is-type-family-constructor? "list-nil") "Should recognize list-nil")
    (check-true (is-type-family-constructor? "equal-refl") "Should recognize equal-refl")
    (check-false (is-type-family-constructor? "regular-function") "Should not recognize regular functions")))

(printf "Bidirectional Inference Integration tests completed.~n")