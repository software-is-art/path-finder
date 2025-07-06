;; ============================================================================
;; COMPUTATION-FIRST VALUE SYSTEM
;; ============================================================================
;; All values are computations carrying evidence
;; This replaces the traditional value representation

(import types types)
(import effects computation-as-effect)

;; ============================================================================
;; PRIMARY VALUE TYPE: EVERYTHING IS A COMPUTATION
;; ============================================================================

;; The fundamental value type - all values come from computations
(data Value U0
  ;; Every value is a computation with evidence
  (case computation-value (-> (Computation A) Evidence Value)))

;; ============================================================================
;; LIFTING TRADITIONAL VALUES TO COMPUTATIONS
;; ============================================================================

;; Lift a constructor to a computation value
(type make-constructor-value (-> String (List Value) Type Value))
(define make-constructor-value
  (fn (name args type)
    (computation-value
      (value-computation 
        (raw-constructor name args type)
        (refl _ _))
      (immediate-evidence 0))))

;; Lift a closure to a computation value
(type make-closure-value (-> (List String) AST Environment Value))
(define make-closure-value
  (fn (params body env)
    (computation-value
      (value-computation
        (raw-closure params body env)
        (refl _ _))
      (closure-evidence params))))

;; Lift a builtin to a computation value  
(type make-builtin-value (-> String Nat Type Value))
(define make-builtin-value
  (fn (name arity type)
    (computation-value
      (value-computation
        (raw-builtin name arity type)
        (refl _ _))
      (builtin-evidence name arity))))

;; ============================================================================
;; RAW VALUE REPRESENTATIONS (INTERNAL)
;; ============================================================================

;; Raw values before lifting to computations
(data RawValue U0
  (case raw-constructor (-> String (List Value) Type RawValue))
  (case raw-closure (-> (List String) AST Environment RawValue))
  (case raw-builtin (-> String Nat Type RawValue))
  (case raw-unit RawValue)
  (case raw-string (-> String RawValue))
  (case raw-path (-> Type Value Value Proof RawValue))
  (case raw-equivalence (-> Type Type Value Value RawValue)))

;; ============================================================================
;; EVIDENCE FOR VALUES
;; ============================================================================

;; Evidence carried by values
(data ValueEvidence U0
  ;; Immediate value (no computation needed)
  (case immediate-evidence (-> (steps : Nat) ValueEvidence))
  
  ;; Closure evidence (potential computation)
  (case closure-evidence (-> (params : List String) ValueEvidence))
  
  ;; Builtin evidence (known complexity)
  (case builtin-evidence (-> (name : String) (arity : Nat) ValueEvidence))
  
  ;; Computed evidence (from evaluation)
  (case computed-evidence (-> (term : TerminationEvidence)
                             (complex : ComplexityEvidence)
                             (space : SpaceEvidence)
                             ValueEvidence)))

;; ============================================================================
;; VALUE OPERATIONS WITH EVIDENCE
;; ============================================================================

;; Apply a function value to an argument (builds evidence)
(type apply-value-with-evidence (-> Value Value (Computation Value)))
(define apply-value-with-evidence
  (fn (func-val arg-val)
    (match (extract-computation func-val)
      (case (value-computation (raw-closure params body env) _)
        (apply-computation
          (extract-computation func-val)
          (extract-computation arg-val)
          (make-app-evidence params body)))
      
      (case (value-computation (raw-builtin name arity type) _)
        (apply-builtin-computation name arity type arg-val))
      
      (case _ (error-computation "Cannot apply non-function")))))

;; Extract raw value from computation value
(type extract-raw (-> Value RawValue))
(define extract-raw
  (fn (v)
    (match v
      (case (computation-value comp _)
        (match comp
          (case (value-computation raw _) raw)
          (case _ (error "Not a value computation")))))))

;; Extract computation from value
(type extract-computation (-> Value (Computation A)))
(define extract-computation
  (fn (v)
    (match v
      (case (computation-value comp _) comp))))

;; Extract evidence from value
(type extract-evidence (-> Value Evidence))
(define extract-evidence
  (fn (v)
    (match v
      (case (computation-value _ ev) ev))))

;; ============================================================================
;; VALUE PREDICATES WITH EVIDENCE
;; ============================================================================

;; Check if value is a constructor (examines evidence)
(type is-constructor? (-> Value Bool))
(define is-constructor?
  (fn (v)
    (match (extract-raw v)
      (case (raw-constructor _ _ _) true)
      (case _ false))))

;; Check if value is a function
(type is-function? (-> Value Bool))
(define is-function?
  (fn (v)
    (match (extract-raw v)
      (case (raw-closure _ _ _) true)
      (case (raw-builtin _ _ _) true)
      (case _ false))))

;; Check if value is deterministic (uses evidence)
(type is-deterministic-value? (-> Value Bool))
(define is-deterministic-value?
  (fn (v)
    (match (extract-evidence v)
      (case (immediate-evidence _) true)
      (case (computed-evidence _ _ _) 
        (check-determinism-evidence (extract-evidence v)))
      (case _ false))))

;; ============================================================================
;; STANDARD VALUES AS COMPUTATIONS
;; ============================================================================

;; Zero as a computation value
(define zero-value
  (make-constructor-value "zero" nil (nat-type)))

;; Successor as a computation
(define succ-value
  (fn (n)
    (computation-value
      (arithmetic-computation
        succ-op
        (list (extract-computation n))
        (succ-evidence n))
      (computed-evidence
        (immediate-termination 1)
        (constant-complexity 1)
        (constant-space 0)))))

;; Boolean values as computations
(define true-value
  (make-constructor-value "true" nil (bool-type)))

(define false-value
  (make-constructor-value "false" nil (bool-type)))

;; Unit value as computation
(define unit-value
  (computation-value
    (value-computation raw-unit (refl _ _))
    (immediate-evidence 0)))

;; ============================================================================
;; EVIDENCE BUILDERS
;; ============================================================================

;; Build evidence for arithmetic operations
(type arithmetic-evidence (-> ArithmeticOp (List Value) Evidence))
(define arithmetic-evidence
  (fn (op args)
    (computed-evidence
      (immediate-termination 1)
      (arithmetic-complexity op (length args))
      (constant-space 1))))

;; Build evidence for function application
(type application-evidence (-> Value Value Evidence))
(define application-evidence
  (fn (func arg)
    (computed-evidence
      (infer-termination func arg)
      (compose-complexity 
        (extract-evidence func)
        (extract-evidence arg))
      (stack-space 1))))

;; ============================================================================
;; VALUE OPTIMIZATION BASED ON EVIDENCE
;; ============================================================================

;; Optimize a value based on its evidence
(type optimize-value (-> Value Value))
(define optimize-value
  (fn (v)
    (match (extract-evidence v)
      ;; Constant values can be cached
      (case (immediate-evidence 0)
        (cache-value v))
      
      ;; Deterministic computations can be memoized
      (case (computed-evidence term complex space)
        (if (is-deterministic? term)
            (memoize-value v)
            v))
      
      ;; Default: no optimization
      (case _ v))))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export Value)
(export computation-value)
(export make-constructor-value)
(export make-closure-value)
(export make-builtin-value)
(export apply-value-with-evidence)
(export extract-computation)
(export extract-evidence)
(export is-constructor?)
(export is-function?)
(export zero-value)
(export succ-value)
(export true-value)
(export false-value)
(export unit-value)
(export optimize-value)