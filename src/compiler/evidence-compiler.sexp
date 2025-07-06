;; ============================================================================
;; EVIDENCE RULE COMPILER
;; ============================================================================
;; Compiles PathFinder evidence rules to efficient Rust code
;; This enables metacircular evidence-aware compilation

(import types types)
(import compiler rust-ast)
(import compiler code-generator)
(import core foundations)
(import effects computation-as-effect)

;; ============================================================================
;; EVIDENCE RULE REPRESENTATION
;; ============================================================================

(data EvidenceRule U0
  ;; Termination rule
  (case termination-rule (-> (List Pattern) TerminationEvidence EvidenceRule))
  
  ;; Complexity rule
  (case complexity-rule (-> (List Pattern) ComplexityEvidence EvidenceRule))
  
  ;; Space rule
  (case space-rule (-> (List Pattern) SpaceEvidence EvidenceRule))
  
  ;; Composite rule
  (case composite-rule (-> String (List EvidenceRule) EvidenceRule)))

(data Pattern U0
  ;; Variable pattern
  (case var-pattern (-> String Pattern))
  
  ;; Constructor pattern
  (case cons-pattern (-> String (List Pattern) Pattern))
  
  ;; Literal pattern
  (case lit-pattern (-> Value Pattern))
  
  ;; Wildcard
  (case wildcard-pattern Pattern))

;; ============================================================================
;; COMPILE EVIDENCE RULES TO RUST
;; ============================================================================

;; Compile a single evidence rule to Rust function
(type compile-evidence-rule (-> EvidenceRule RustItem))
(define compile-evidence-rule
  (fn (rule)
    (match rule
      ;; Termination rule compilation
      (case (termination-rule patterns evidence)
        (make-rust-fn 
          "check_termination"
          (list (pair "expr" (rust-named "HottValue")))
          (rust-named "TerminationEvidence")
          (compile-termination-check patterns evidence)))
      
      ;; Complexity rule compilation
      (case (complexity-rule patterns evidence)
        (make-rust-fn
          "check_complexity"
          (list (pair "expr" (rust-named "HottValue")))
          (rust-named "ComplexityEvidence")
          (compile-complexity-check patterns evidence)))
      
      ;; Space rule compilation
      (case (space-rule patterns evidence)
        (make-rust-fn
          "check_space"
          (list (pair "expr" (rust-named "HottValue")))
          (rust-named "SpaceEvidence")
          (compile-space-check patterns evidence)))
      
      ;; Composite rule
      (case (composite-rule name rules)
        (rust-impl name
                  (list-map compile-evidence-rule rules))))))

;; ============================================================================
;; TERMINATION EVIDENCE COMPILATION
;; ============================================================================

(type compile-termination-check (-> (List Pattern) TerminationEvidence RustExpr))
(define compile-termination-check
  (fn (patterns evidence)
    (match evidence
      ;; Immediate termination
      (case (immediate-termination steps)
        (rust-enum "TerminationEvidence" "AlwaysTerminates"
                  (list (rust-struct "TerminationInfo"
                                   (list (pair "steps" (rust-usize-lit steps)))))))
      
      ;; Structural termination
      (case (structural-termination measure decrease)
        (rust-enum "TerminationEvidence" "StructurallyDecreasing"
                  nil))
      
      ;; Well-founded termination
      (case (well-founded-termination rel proof)
        (rust-enum "TerminationEvidence" "WellFounded"
                  nil)))))

;; ============================================================================
;; COMPLEXITY EVIDENCE COMPILATION
;; ============================================================================

(type compile-complexity-check (-> (List Pattern) ComplexityEvidence RustExpr))
(define compile-complexity-check
  (fn (patterns evidence)
    (match evidence
      ;; Constant complexity
      (case (constant-complexity ops)
        (rust-enum "ComplexityEvidence" "Constant" nil))
      
      ;; Linear complexity
      (case (linear-complexity factor size)
        (rust-enum "ComplexityEvidence" "Linear" nil))
      
      ;; Logarithmic complexity
      (case (log-complexity base size)
        (rust-enum "ComplexityEvidence" "Logarithmic" nil))
      
      ;; Polynomial complexity
      (case (poly-complexity degree size)
        (rust-enum "ComplexityEvidence" "Polynomial"
                  (list (rust-struct "PolynomialInfo"
                                   (list (pair "degree" (rust-usize-lit degree)))))))
      
      ;; Composed complexity
      (case (composed-complexity c1 c2)
        (rust-call (rust-var "combine_complexity")
                  (list (compile-complexity-check nil c1)
                        (compile-complexity-check nil c2)))))))

;; ============================================================================
;; ARITHMETIC OPTIMIZATION COMPILER
;; ============================================================================

;; Compile PathFinder arithmetic to optimized Rust
(type compile-arithmetic-op (-> String RustItem))
(define compile-arithmetic-op
  (fn (op)
    (match op
      ;; Addition
      (case "add"
        (make-rust-fn
          "optimized_add"
          (list (pair "x" (rust-ref (rust-named "HottValue")))
                (pair "y" (rust-ref (rust-named "HottValue"))))
          (rust-named "HottValue")
          (rust-match (rust-tuple-pat (list (rust-var "x") (rust-var "y")))
                     (list
                       ;; Zero cases
                       (rust-match-arm
                         (rust-tuple-pat (list (rust-enum-pat "HottValue::Zero" nil)
                                             (rust-var-pat "y")))
                         (rust-clone (rust-var "y")))
                       (rust-match-arm
                         (rust-tuple-pat (list (rust-var-pat "x")
                                             (rust-enum-pat "HottValue::Zero" nil)))
                         (rust-clone (rust-var "x")))
                       ;; General case - fallback to Peano
                       (rust-match-arm
                         rust-wildcard
                         (rust-call (rust-var "peano_add")
                                   (list (rust-var "x") (rust-var "y"))))))))
      
      ;; Multiplication
      (case "mult"
        (make-rust-fn
          "optimized_mult"
          (list (pair "x" (rust-ref (rust-named "HottValue")))
                (pair "y" (rust-ref (rust-named "HottValue"))))
          (rust-named "HottValue")
          (rust-match (rust-tuple-pat (list (rust-var "x") (rust-var "y")))
                     (list
                       ;; Zero cases
                       (rust-match-arm
                         (rust-tuple-pat (list (rust-enum-pat "HottValue::Zero" nil)
                                             rust-wildcard))
                         (rust-enum "HottValue" "Zero" nil))
                       (rust-match-arm
                         (rust-tuple-pat (list rust-wildcard
                                             (rust-enum-pat "HottValue::Zero" nil)))
                         (rust-enum "HottValue" "Zero" nil))
                       ;; One cases
                       (rust-match-arm
                         (rust-tuple-pat (list (rust-enum-pat "HottValue::Succ"
                                                            (list (rust-enum-pat "HottValue::Zero" nil)))
                                             (rust-var-pat "y")))
                         (rust-clone (rust-var "y")))
                       ;; General case
                       (rust-match-arm
                         rust-wildcard
                         (rust-call (rust-var "peano_mult")
                                   (list (rust-var "x") (rust-var "y"))))))))
      
      ;; Default
      (case _ (rust-fn-def (string-append "optimized_" op) nil rust-unit nil)))))

;; ============================================================================
;; MODULE GENERATION
;; ============================================================================

;; Generate complete evidence module
(type generate-evidence-module (-> RustModule))
(define generate-evidence-module
  (fn ()
    (rust-module "evidence"
                (list (rust-use-item "crate::hott_values::*"))
                (list
                  ;; Termination checking
                  (compile-evidence-rule
                    (termination-rule nil (immediate-termination (succ zero))))
                  
                  ;; Complexity checking
                  (compile-evidence-rule
                    (complexity-rule nil (constant-complexity (succ zero))))
                  
                  ;; Optimized arithmetic
                  (compile-arithmetic-op "add")
                  (compile-arithmetic-op "mult")))))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export EvidenceRule)
(export Pattern)
(export compile-evidence-rule)
(export compile-termination-check)
(export compile-complexity-check)
(export compile-arithmetic-op)
(export generate-evidence-module)