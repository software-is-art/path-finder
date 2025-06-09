#lang racket/base

(require rackunit
         racket/list
         "../src/typecheck/universe-level-inference.rkt"
         "../src/typecheck/typechecker.rkt"
         "../src/parser/ast.rkt"
         "../src/types/types.rkt")

;; Test suite for HoTT Universe Level Inference

(test-case "Universe Level Variables and Constraints"
  
  ;; Create universe inference context
  (let ([ctx (make-universe-inference-context (make-global-type-environment))])
    (check-true (universe-level-inference-context? ctx) "Should create universe context")
    
    ;; Create level variables
    (let ([var1 (fresh-level-variable ctx 'u1)]
          [var2 (fresh-level-variable ctx 'u2)])
      (check-true (level-variable? var1) "Should create level variable")
      (check-true (level-variable? var2) "Should create second level variable")
      (check-not-equal? (level-variable-id var1) (level-variable-id var2)
                        "Level variables should have different IDs"))
    
    ;; Add universe constraints
    (let ([var1 (fresh-level-variable ctx 'u1)]
          [var2 (fresh-level-variable ctx 'u2)])
      (add-universe-constraint! ctx universe-less-than var1 var2)
      (add-universe-constraint! ctx universe-equal var1 0)
      (check-true (> (length (universe-level-inference-context-constraints ctx)) 0)
                  "Should record universe constraints"))))

(test-case "Basic Universe Level Inference"
  
  (let ([ctx (make-universe-inference-context (make-global-type-environment))])
    ;; Literals are in universe 0
    (check-equal? (infer-universe-levels (number-atom 42) ctx) 0
                  "Numbers should be in universe 0")
    
    (check-equal? (infer-universe-levels (boolean-atom #t) ctx) 0
                  "Booleans should be in universe 0")
    
    (check-equal? (infer-universe-levels (string-atom "hello") ctx) 0
                  "Strings should be in universe 0")
    
    ;; Unit type in universe 0
    (check-equal? (infer-universe-levels (sexpr '()) ctx) 0
                  "Unit type should be in universe 0")))

(test-case "Type Literal Universe Level Rules"
  
  (let ([ctx (make-universe-inference-context (make-global-type-environment))])
    ;; Type₀ : Type₁
    (let ([type0-ast (sexpr (list (symbol-atom "Type")))])
      (check-equal? (infer-universe-levels type0-ast ctx) 1
                    "Type₀ should be in universe 1"))
    
    ;; Type i : Type (i+1)
    (let ([type2-ast (sexpr (list (symbol-atom "Type") (number-atom 2)))])
      (check-equal? (infer-universe-levels type2-ast ctx) 3
                    "Type₂ should be in universe 3"))))

(test-case "Π-Type Universe Level Rules"
  
  (let ([ctx (make-universe-inference-context (make-global-type-environment))])
    ;; (Π (x : Nat) Bool) : Type (max(level(Nat), level(Bool))) = Type 0
    (let ([pi-ast (sexpr (list (symbol-atom "pi")
                              (sexpr (list (symbol-atom "x") (symbol-atom "Nat")))
                              (symbol-atom "Bool")))])
      (check-equal? (infer-universe-levels pi-ast ctx) 0
                    "Simple Pi-type should be in universe 0"))
    
    ;; (Π (A : Type) (x : A) -> A) should be in universe 1
    (let ([poly-pi-ast (sexpr (list (symbol-atom "pi")
                                   (sexpr (list (symbol-atom "A") 
                                               (sexpr (list (symbol-atom "Type")))))
                                   (sexpr (list (symbol-atom "pi")
                                               (sexpr (list (symbol-atom "x") (symbol-atom "A")))
                                               (symbol-atom "A")))))])
      (check-equal? (infer-universe-levels poly-pi-ast ctx) 1
                    "Polymorphic Pi-type should be in universe 1"))))

(test-case "Σ-Type Universe Level Rules"
  
  (let ([ctx (make-universe-inference-context (make-global-type-environment))])
    ;; (Σ (x : Nat) Bool) : Type (max(level(Nat), level(Bool))) = Type 0
    (let ([sigma-ast (sexpr (list (symbol-atom "sigma")
                                 (sexpr (list (symbol-atom "x") (symbol-atom "Nat")))
                                 (symbol-atom "Bool")))])
      (check-equal? (infer-universe-levels sigma-ast ctx) 0
                    "Simple Sigma-type should be in universe 0"))))

(test-case "Identity Type Universe Level Rules"
  
  (let ([ctx (make-universe-inference-context (make-global-type-environment))])
    ;; Id Nat x y : Type (level(Nat)) = Type 0
    (let ([id-ast (sexpr (list (symbol-atom "identity")
                              (symbol-atom "Nat")
                              (number-atom 0)
                              (number-atom 0)))])
      (check-equal? (infer-universe-levels id-ast ctx) 0
                    "Identity type should inherit type's universe level"))))

(test-case "Sum Type Universe Level Rules"
  
  (let ([ctx (make-universe-inference-context (make-global-type-environment))])
    ;; Nat + Bool : Type (max(level(Nat), level(Bool))) = Type 0
    (let ([sum-ast (sexpr (list (symbol-atom "+")
                               (symbol-atom "Nat")
                               (symbol-atom "Bool")))])
      (check-equal? (infer-universe-levels sum-ast ctx) 0
                    "Sum type should be max of component levels"))))

(test-case "Universe Constraint Generation"
  
  (let ([ctx (make-universe-inference-context (make-global-type-environment))])
    ;; Generate constraints from type expressions
    (let ([var1 (fresh-level-variable ctx 'u1)]
          [var2 (fresh-level-variable ctx 'u2)])
      
      ;; Add various constraint types
      (add-universe-constraint! ctx 'less-than var1 var2)
      (add-universe-constraint! ctx 'less-equal var1 1)
      (add-universe-constraint! ctx 'equal var2 2)
      
      (let ([constraints (universe-level-inference-context-constraints ctx)])
        (check-equal? (length constraints) 3 "Should have three constraints")
        
        ;; Check constraint types
        (let ([types (map universe-constraint-type constraints)])
          (check-true (and (member 'less-than types) #t) "Should have less-than constraint")
          (check-true (and (member 'less-equal types) #t) "Should have less-equal constraint") 
          (check-true (and (member 'equal types) #t) "Should have equal constraint"))))))

(test-case "Universe Constraint Solving"
  
  (let ([ctx (make-universe-inference-context (make-global-type-environment))])
    (let ([var1 (fresh-level-variable ctx 'u1)]
          [var2 (fresh-level-variable ctx 'u2)]
          [var3 (fresh-level-variable ctx 'u3)])
      
      ;; Set up constraint system: u1 < u2, u2 ≤ u3, u3 = 2
      (add-universe-constraint! ctx 'less-than var1 var2)
      (add-universe-constraint! ctx 'less-equal var2 var3)
      (add-universe-constraint! ctx 'equal var3 2)
      
      ;; Solve constraints
      (solve-universe-constraints ctx)
      
      ;; Check that assignments exist
      (let ([assignments (universe-level-inference-context-level-assignments ctx)])
        (check-true (hash? assignments) "Should have level assignments")))))

(test-case "Universe Consistency Checking"
  
  (let ([assignments (make-hash)])
    ;; Set up assignment: u1 = 0, u2 = 1, u3 = 2
    (let ([var1 (level-variable 1 'u1)]
          [var2 (level-variable 2 'u2)]
          [var3 (level-variable 3 'u3)])
      
      (hash-set! assignments var1 0)
      (hash-set! assignments var2 1)
      (hash-set! assignments var3 2)
      
      ;; Create consistent constraints
      (let ([consistent-constraints
             (list (universe-constraint 'less-than var1 var2 #f)
                   (universe-constraint 'less-equal var2 var3 #f)
                   (universe-constraint 'equal var3 2 #f))])
        
        (check-true (consistent-universe-assignment? assignments consistent-constraints)
                    "Should accept consistent assignment"))
      
      ;; Create inconsistent constraints
      (let ([inconsistent-constraints
             (list (universe-constraint 'less-than var2 var1 #f))])  ; 1 < 0 is false
        
        (check-false (consistent-universe-assignment? assignments inconsistent-constraints)
                     "Should reject inconsistent assignment")))))

(test-case "Level Resolution"
  
  (let ([assignments (make-hash)])
    (let ([var1 (level-variable 1 'u1)])
      (hash-set! assignments var1 5)
      
      ;; Test level variable resolution
      (check-equal? (resolve-level var1 assignments) 5
                    "Should resolve level variable to assigned value")
      
      ;; Test concrete level resolution
      (check-equal? (resolve-level 3 assignments) 3
                    "Should resolve concrete level to itself")
      
      ;; Test unknown variable resolution
      (let ([var2 (level-variable 2 'u2)])
        (check-equal? (resolve-level var2 assignments) 0
                      "Should resolve unknown variable to 0")))))

(test-case "Integration with Type Checking"
  
  ;; Test enhanced type checking with universe inference
  (let ([simple-expr (number-atom 42)])
    (let-values ([(type level) (type-check-with-universe-inference simple-expr (make-global-type-environment))])
      (check-equal? type Nat "Should infer Nat type")
      (check-equal? level 0 "Should infer universe level 0")))
  
  ;; Test with boolean expression  
  (let ([bool-expr (boolean-atom #f)])
    (let-values ([(type level) (type-check-with-universe-inference bool-expr (make-global-type-environment))])
      (check-equal? type Bool "Should infer Bool type")
      (check-equal? level 0 "Bool should be in universe 0"))))

(test-case "AST Elaboration with Universe Levels"
  
  (let ([ctx (make-universe-inference-context (make-global-type-environment))])
    ;; Test Type literal elaboration
    (let ([type-ast (sexpr (list (symbol-atom "Type")))]
          [elaborated (elaborate-with-universe-levels (sexpr (list (symbol-atom "Type"))) ctx)])
      (check-true (sexpr? elaborated) "Should elaborate to S-expression")
      ;; The elaborated form should include explicit level
      (let ([elements (sexpr-elements elaborated)])
        (check-equal? (length elements) 2 "Should have symbol and level")
        (check-true (number-atom? (second elements)) "Should have numeric level")))
    
    ;; Test complex expression elaboration
    (let ([complex-ast (sexpr (list (symbol-atom "pi")
                                   (sexpr (list (symbol-atom "x") (symbol-atom "Nat")))
                                   (symbol-atom "Bool")))])
      (let ([elaborated (elaborate-with-universe-levels complex-ast ctx)])
        (check-true (sexpr? elaborated) "Should elaborate complex expressions")))))

(test-case "HoTT Universe Hierarchy Properties"
  
  (let ([ctx (make-universe-inference-context (make-global-type-environment))])
    ;; Test universe hierarchy: Type₀ : Type₁ : Type₂ : ...
    (check-equal? (infer-universe-levels (sexpr (list (symbol-atom "Type") (number-atom 0))) ctx) 1
                  "Type₀ ∈ Type₁")
    
    (check-equal? (infer-universe-levels (sexpr (list (symbol-atom "Type") (number-atom 1))) ctx) 2
                  "Type₁ ∈ Type₂")
    
    (check-equal? (infer-universe-levels (sexpr (list (symbol-atom "Type") (number-atom 5))) ctx) 6
                  "Type₅ ∈ Type₆")
    
    ;; Test max operation in universe levels
    ;; For Π and Σ types, universe level is max of components
    (let ([pi-with-type (sexpr (list (symbol-atom "pi")
                                    (sexpr (list (symbol-atom "A") 
                                                (sexpr (list (symbol-atom "Type") (number-atom 1)))))
                                    (symbol-atom "A")))])
      (check-equal? (infer-universe-levels pi-with-type ctx) 2
                    "Π type with Type₁ should be in universe 2"))))

(test-case "Complex Universe Level Scenarios"
  
  (let ([ctx (make-universe-inference-context (make-global-type-environment))])
    ;; Test polymorphic identity function type
    ;; ∀(A : Type) → A → A : Type₁
    (let ([poly-id-type (sexpr (list (symbol-atom "pi")
                                    (sexpr (list (symbol-atom "A") 
                                                (sexpr (list (symbol-atom "Type")))))
                                    (sexpr (list (symbol-atom "pi")
                                                (sexpr (list (symbol-atom "x") (symbol-atom "A")))
                                                (symbol-atom "A")))))])
      (check-equal? (infer-universe-levels poly-id-type ctx) 1
                    "Polymorphic function should be in universe 1"))
    
    ;; Test dependent pair with type component
    ;; Σ(A : Type) A : Type₁
    (let ([dependent-pair (sexpr (list (symbol-atom "sigma")
                                      (sexpr (list (symbol-atom "A") 
                                                  (sexpr (list (symbol-atom "Type")))))
                                      (symbol-atom "A")))])
      (check-equal? (infer-universe-levels dependent-pair ctx) 1
                    "Dependent pair with type should be in universe 1"))))

(test-case "Error Cases and Edge Conditions"
  
  (let ([ctx (make-universe-inference-context (make-global-type-environment))])
    ;; Test malformed pi type
    (check-exn exn:fail?
               (lambda () (infer-universe-levels 
                          (sexpr (list (symbol-atom "pi") (symbol-atom "bad"))) ctx))
               "Should error on malformed pi type")
    
    ;; Test malformed sigma type  
    (check-exn exn:fail?
               (lambda () (infer-universe-levels
                          (sexpr (list (symbol-atom "sigma") (symbol-atom "bad"))) ctx))
               "Should error on malformed sigma type")
    
    ;; Test malformed identity type
    (check-exn exn:fail?
               (lambda () (infer-universe-levels
                          (sexpr (list (symbol-atom "identity") (symbol-atom "incomplete"))) ctx))
               "Should error on malformed identity type")))

;; Run the tests
(printf "Running Universe Level Inference tests...~n")