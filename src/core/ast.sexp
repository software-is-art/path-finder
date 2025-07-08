;; ============================================================================
;; PURE MATHEMATICAL HOTT-NATIVE AST (S-EXPRESSION VERSION)
;; ============================================================================
;; The AST itself is mathematical data that can be manipulated by eliminators
;; Depends on: core/foundations

(import core foundations)

;; ============================================================================
;; AST AS HOTT INDUCTIVE TYPE
;; ============================================================================

;; HoTT-AST inductive type definition
(data HoTT-AST U0
  (case var (-> String HoTT-AST))
  (case app (-> HoTT-AST HoTT-AST HoTT-AST))
  (case lambda (-> String HoTT-AST HoTT-AST))
  (case pi-type (-> String HoTT-AST HoTT-AST HoTT-AST))
  (case sigma-type (-> String HoTT-AST HoTT-AST HoTT-AST))
  (case id-type (-> HoTT-AST HoTT-AST HoTT-AST HoTT-AST))
  (case eliminator (-> HoTT-AST (List HoTT-AST) HoTT-AST))
  (case type-app (-> String (List HoTT-AST) HoTT-AST))
  (case constructor (-> String (List HoTT-AST) HoTT-AST))
  (case literal (-> Value HoTT-AST))
  (case effect (-> Effect HoTT-AST))
  (case computation (-> ComputationalAction HoTT-AST))
  
  ;; Computation as effect nodes
  (case computation-expr (-> HoTT-AST Evidence HoTT-AST))
  (case evidence-annotation (-> EvidenceType EvidenceValue HoTT-AST))
  (case optimization-directive (-> OptimizationHint HoTT-AST HoTT-AST))
  
  (case let-expr (-> String HoTT-AST HoTT-AST HoTT-AST))
  (case match-expr (-> HoTT-AST (List MatchCase) HoTT-AST)))

;; Match case for pattern matching
(data MatchCase U0
  (case match-case (-> Pattern HoTT-AST MatchCase)))

;; ============================================================================
;; EVIDENCE TYPES FOR AST
;; ============================================================================

;; Evidence that can be attached to AST nodes
(data Evidence U0
  (case termination-evidence-ast (-> TerminationBound Evidence))
  (case complexity-evidence-ast (-> ComplexityBound Evidence))
  (case space-evidence-ast (-> SpaceBound Evidence))
  (case determinism-evidence-ast (-> DeterminismLevel Evidence))
  (case combined-evidence-ast (-> (List Evidence) Evidence)))

;; Evidence type categories
(data EvidenceType U0
  (case termination-type EvidenceType)
  (case complexity-type EvidenceType)
  (case space-type EvidenceType)
  (case correctness-type EvidenceType))

;; Evidence values (simplified for AST)
(data EvidenceValue U0
  (case constant-evidence (-> Nat EvidenceValue))
  (case linear-evidence (-> Nat EvidenceValue))
  (case polynomial-evidence (-> Nat Nat EvidenceValue))
  (case proven-evidence EvidenceValue)
  (case assumed-evidence EvidenceValue))

;; Optimization hints based on evidence
(data OptimizationHint U0
  (case optimize-for-speed OptimizationHint)
  (case optimize-for-space OptimizationHint)
  (case optimize-for-proof-size OptimizationHint)
  (case parallelize-when-safe OptimizationHint)
  (case cache-when-deterministic OptimizationHint))

;; Bounds for evidence
(data TerminationBound U0
  (case always-terminates (-> Nat TerminationBound))
  (case structurally-decreasing TerminationBound)
  (case well-founded (-> String TerminationBound)))

(data ComplexityBound U0
  (case constant-time ComplexityBound)
  (case linear-time ComplexityBound)
  (case logarithmic-time ComplexityBound)
  (case polynomial-time (-> Nat ComplexityBound)))

(data SpaceBound U0
  (case constant-space (-> Nat SpaceBound))
  (case linear-space SpaceBound)
  (case logarithmic-space SpaceBound))

(data DeterminismLevel U0
  (case fully-deterministic DeterminismLevel)
  (case context-deterministic DeterminismLevel)
  (case non-deterministic DeterminismLevel))

;; Pattern for matching
(data Pattern U0
  (case var-pattern (-> String Pattern))
  (case constructor-pattern (-> String (List Pattern) Pattern))
  (case literal-pattern (-> Value Pattern))
  (case wildcard-pattern Pattern))

;; ============================================================================
;; AST CONSTRUCTOR FUNCTIONS
;; ============================================================================

;; Smart constructors for creating AST nodes
(define parse-variable
  (fn (name) (var name)))

(define parse-application
  (fn (func arg) (app func arg)))

(define parse-lambda
  (fn (param body) (lambda param body)))

(define parse-pi-type
  (fn (var domain codomain)
    (pi-type var domain codomain)))

(define parse-sigma-type
  (fn (var first second)
    (sigma-type var first second)))

(define parse-id-type
  (fn (type left right)
    (id-type type left right)))

(define parse-let
  (fn (var value body)
    (let-expr var value body)))

(define parse-match
  (fn (scrutinee cases)
    (match-expr scrutinee cases)))

;; ============================================================================
;; AST ELIMINATOR
;; ============================================================================

;; General AST eliminator for structural recursion
(define ast-elim
  (fn (P p-var p-app p-lambda p-pi p-sigma p-id p-elim 
         p-type-app p-constructor p-literal p-effect p-let p-match ast)
    (match ast
      (case (var name) 
        (p-var name))
      (case (app func arg)
        (p-app func arg 
               (ast-elim P p-var p-app p-lambda p-pi p-sigma p-id p-elim
                        p-type-app p-constructor p-literal p-effect p-let p-match func)
               (ast-elim P p-var p-app p-lambda p-pi p-sigma p-id p-elim
                        p-type-app p-constructor p-literal p-effect p-let p-match arg)))
      (case (lambda param body)
        (p-lambda param body
                  (ast-elim P p-var p-app p-lambda p-pi p-sigma p-id p-elim
                           p-type-app p-constructor p-literal p-effect p-let p-match body)))
      ;; ... other cases follow similar pattern
      )))

;; ============================================================================
;; AST UTILITIES
;; ============================================================================

;; Get free variables in an AST
(define free-vars
  (fn (ast)
    (ast-elim (-> (List String))
      ;; var case
      (fn (name) (list name))
      ;; app case  
      (fn (func arg fv-func fv-arg)
        (union fv-func fv-arg))
      ;; lambda case
      (fn (param body fv-body)
        (remove param fv-body))
      ;; ... other cases
      ast)))

;; Substitute variable in AST
(define subst
  (fn (var replacement ast)
    (ast-elim (-> HoTT-AST)
      ;; var case
      (fn (name)
        (if (string-equal? name var)
            replacement
            (var name)))
      ;; app case
      (fn (func arg new-func new-arg)
        (app new-func new-arg))
      ;; lambda case
      (fn (param body new-body)
        (if (string-equal? param var)
            (lambda param body)  ;; no substitution under binder
            (lambda param new-body)))
      ;; ... other cases
      ast)))