#lang racket/base

(require rackunit
         "../src/typecheck/hott-native-inference.rkt"
         "../src/typecheck/typechecker.rkt"
         "../src/parser/ast.rkt"
         "../src/types/types.rkt"
         "../src/types/type-families.rkt")

;; Test suite for HoTT-Native Type Inference Engine

(test-case "HoTT Universe and Computational Evidence"
  
  ;; Create inference universe at level 0
  (let ([universe (make-inference-universe 0 (make-global-type-environment))])
    (check-equal? (inference-universe-level universe) 0 "Universe should be at level 0")
    (check-equal? (inference-universe-tier universe) 'compile-time "Default tier should be compile-time"))
  
  ;; Type variables in universe
  (let ([universe (make-inference-universe 1 (make-global-type-environment))])
    (let ([var1 (fresh-type-variable universe)]
          [var2 (fresh-type-variable universe)])
      (check-true (type-variable? var1) "Should create type variable")
      (check-true (type-variable? var2) "Should create second type variable")
      (check-equal? (type-variable-universe-level var1) 1 "Variable should inherit universe level")
      (check-not-equal? (type-variable-name var1) (type-variable-name var2) 
                        "Variables should have different names"))))

(test-case "HoTT Type Synthesis with Computational Evidence"
  
  (let ([universe (make-inference-universe 0 (make-global-type-environment))])
    ;; Literal synthesis with evidence
    (check-equal? (hott-infer (number-atom 42) universe) Nat
                  "Number literal should synthesize Nat")
    
    (check-equal? (hott-infer (boolean-atom #t) universe) Bool
                  "Boolean literal should synthesize Bool")
    
    ;; Evidence should be recorded
    (let ([evidence-ctx (inference-universe-evidence-context universe)])
      (check-true (> (length (evidence-context-proofs evidence-ctx)) 0)
                  "Should record computational evidence for literals"))))

(test-case "Universe Polymorphic Variable Lookup"
  
  (let ([universe (make-inference-universe 0 (make-global-type-environment))])
    ;; Known variable lookup
    (let ([plus-type (universe-lookup universe "+")])
      (check-true (pi-type? plus-type) "Plus should have Pi-type"))
    
    ;; Unknown variable creates type variable
    (let ([unknown-type (universe-lookup universe "unknown-var")])
      (check-true (type-variable? unknown-type) "Unknown variable should create type variable")
      
      ;; Second lookup returns same type variable
      (let ([same-type (universe-lookup universe "unknown-var")])
        (check-equal? unknown-type same-type "Should return same type variable for same name")))))

(test-case "Π-Type Formation with Computational Evidence"
  
  (let ([universe (make-inference-universe 0 (make-global-type-environment))])
    ;; Pi-type formation: (pi (x Nat) Bool)
    (let ([pi-ast (sexpr (list (symbol-atom "pi")
                              (sexpr (list (symbol-atom "x") (symbol-atom "Nat")))
                              (symbol-atom "Bool")))])
      (let ([pi-type (hott-infer pi-ast universe)])
        (check-true (pi-type? pi-type) "Should infer Pi-type")
        (check-equal? (pi-type-var-name pi-type) "x" "Should preserve variable name")
        (check-equal? (pi-type-domain pi-type) Nat "Domain should be Nat")
        (check-equal? (pi-type-codomain pi-type) Bool "Codomain should be Bool")))
    
    ;; Check computational evidence was recorded
    (let ([evidence-ctx (inference-universe-evidence-context universe)])
      (check-true (> (length (evidence-context-witnesses evidence-ctx)) 0)
                  "Should record evidence for Pi-type formation"))))

(test-case "Identity Type Formation and Proof Obligations"
  
  (let ([universe (make-inference-universe 0 (make-global-type-environment))])
    ;; Identity type: (identity Nat 0 0)
    (let ([id-ast (sexpr (list (symbol-atom "identity")
                              (symbol-atom "Nat")
                              (number-atom 0)
                              (number-atom 0)))])
      (let ([id-type (hott-infer id-ast universe)])
        (check-true (identity-type? id-type) "Should infer identity type")
        (check-equal? (identity-type-type-expr id-type) Nat "Should have correct type expression")))
    
    ;; Check proof obligations for mismatched types
    (let ([bad-id-ast (sexpr (list (symbol-atom "identity")
                                  (symbol-atom "Nat")
                                  (number-atom 0)
                                  (boolean-atom #t)))])
      (let ([_ (hott-infer bad-id-ast universe)]
            [evidence-ctx (inference-universe-evidence-context universe)])
        (check-true (> (length (evidence-context-obligations evidence-ctx)) 0)
                    "Should generate proof obligations for type mismatches")))))

(test-case "Lambda Inference with Π-Introduction Evidence"
  
  (let ([universe (make-inference-universe 0 (make-global-type-environment))])
    ;; Simple lambda
    (let ([lambda-ast (sexpr (list (symbol-atom "lambda")
                                  (sexpr (list (symbol-atom "x")))
                                  (symbol-atom "x")))])
      (let ([lambda-type (hott-infer lambda-ast universe)])
        (check-true (pi-type? lambda-type) "Lambda should have Pi-type")
        
        ;; Check that computational evidence includes Pi-introduction
        (let ([evidence-ctx (inference-universe-evidence-context universe)])
          (let ([witnesses (evidence-context-witnesses evidence-ctx)])
            (check-true (> (length witnesses) 0) "Should record Pi-introduction evidence")))))))

(test-case "Function Application with Π-Elimination"
  
  (let ([universe (make-inference-universe 0 (make-global-type-environment))])
    ;; Application of known function
    (let ([app-ast (sexpr (list (symbol-atom "+")
                               (number-atom 1)
                               (number-atom 2)))])
      (let ([result-type (hott-infer app-ast universe)])
        (check-equal? result-type Nat "Addition should return Nat")))
    
    ;; Check computational evidence for elimination
    (let ([evidence-ctx (inference-universe-evidence-context universe)])
      (check-true (> (length (evidence-context-witnesses evidence-ctx)) 0)
                  "Should record Pi-elimination evidence"))))

(test-case "Type Family Integration (Adaptive Dispatch)"
  
  (let ([universe (make-inference-universe 0 (make-global-type-environment) 'compile-time)])
    ;; This would test type family application if we had registered families
    ;; For now, just test the tier system
    (check-equal? (inference-universe-tier universe) 'compile-time 
                  "Should support tier-aware inference")
    
    ;; Test tier switching
    (let ([runtime-universe (make-inference-universe 0 (make-global-type-environment) 'runtime)])
      (check-equal? (inference-universe-tier runtime-universe) 'runtime
                    "Should support runtime tier inference"))))

(test-case "HoTT Type Checking with Identity Constraints"
  
  (let ([universe (make-inference-universe 0 (make-global-type-environment))])
    ;; Check number against Nat
    (check-not-exn
     (lambda () (hott-check (number-atom 42) Nat universe))
     "Number should check against Nat")
    
    ;; Check type mismatch generates constraint
    (let ([_ (hott-check (boolean-atom #t) Nat universe)]
          [evidence-ctx (inference-universe-evidence-context universe)])
      (check-true (> (length (evidence-context-obligations evidence-ctx)) 0)
                  "Type mismatch should generate proof obligation"))))

(test-case "Universe Level Computation and Evidence"
  
  (let ([universe (make-inference-universe 1 (make-global-type-environment))])
    ;; Type variable at universe level
    (let ([var (fresh-type-variable universe)])
      (check-equal? (type-variable-universe-level var) 1
                    "Type variable should inherit universe level"))
    
    ;; Universe level calculation for complex types
    (let ([pi-ast (sexpr (list (symbol-atom "pi")
                              (sexpr (list (symbol-atom "x") (symbol-atom "Nat")))
                              (symbol-atom "Bool")))])
      (let ([pi-type (hott-infer pi-ast universe)])
        ;; Universe level is max of domain and codomain levels
        (check-equal? (type-universe-level pi-type) 0
                      "Pi-type should have correct universe level")))))

(test-case "HoTT-Native AST Elaboration"
  
  (let ([universe (make-inference-universe 0 (make-global-type-environment))])
    ;; Simple elaboration
    (let ([simple-ast (number-atom 42)])
      (let ([_ (hott-infer simple-ast universe)]  ; Generate evidence
            [elaborated (hott-elaborate simple-ast universe)])
        (check-equal? elaborated simple-ast "Simple AST should remain unchanged")))
    
    ;; Lambda elaboration with evidence
    (let ([lambda-ast (sexpr (list (symbol-atom "lambda")
                                  (sexpr (list (symbol-atom "x")))
                                  (symbol-atom "x")))])
      (let ([_ (hott-infer lambda-ast universe)]  ; Generate evidence
            [elaborated (hott-elaborate lambda-ast universe)])
        (check-true (sexpr? elaborated) "Elaborated lambda should be S-expression")))))

(test-case "Computational Evidence and Proof Synthesis"
  
  (let ([universe (make-inference-universe 0 (make-global-type-environment))])
    ;; Generate computational evidence
    (let ([ast (number-atom 42)])
      (let ([type-with-evidence (with-computational-evidence universe Nat '(nat-literal 42) ast)])
        (check-equal? type-with-evidence Nat "Should return type")
        
        ;; Evidence should be recorded
        (let ([proofs (evidence-context-proofs (inference-universe-evidence-context universe))])
          (check-true (> (length proofs) 0) "Should record computational evidence"))))
    
    ;; Identity constraints should generate obligations
    (generate-identity-constraint universe Bool Nat (boolean-atom #t))
    (let ([obligations (evidence-context-obligations (inference-universe-evidence-context universe))])
      (check-true (> (length obligations) 0) "Should generate proof obligations"))))

(test-case "HoTT vs Traditional Type Inference Comparison"
  
  ;; Traditional approach: separate constraint solving
  ;; HoTT approach: computational evidence + identity types
  
  (let ([universe (make-inference-universe 0 (make-global-type-environment))])
    ;; In HoTT, type equality is computational evidence
    (let ([constraint-ast (sexpr (list (symbol-atom "identity")
                                      (symbol-atom "Nat")
                                      (symbol-atom "+")
                                      (symbol-atom "add")))])
      (let ([id-type (hott-infer constraint-ast universe)])
        (check-true (identity-type? id-type) 
                    "HoTT uses identity types instead of unification constraints")))
    
    ;; Proof obligations replace constraint solving
    (let ([evidence-ctx (inference-universe-evidence-context universe)])
      (check-true (list? (evidence-context-obligations evidence-ctx))
                  "HoTT uses proof obligations instead of constraint solving"))))

(test-case "Integration with Existing HoTT Features"
  
  (let ([universe (make-inference-universe 0 (make-global-type-environment))])
    ;; Should work with existing pattern matching
    (let ([match-ast (match-expr (number-atom 42) (list (number-atom 42)))])
      (check-not-exn 
       (lambda () (hott-infer match-ast universe))
       "Should integrate with pattern matching"))
    
    ;; Should work with existing effect system
    (let ([effect-ast (symbol-atom "perform-effect")])
      (check-not-exn
       (lambda () (hott-infer effect-ast universe))
       "Should integrate with effect system"))))

;; Run the tests
(printf "Running HoTT-Native Type Inference tests...~n")