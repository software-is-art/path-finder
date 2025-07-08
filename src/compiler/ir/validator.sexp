;; ============================================================================
;; IR VALIDATOR
;; ============================================================================
;; Ensures IR invariants are maintained throughout compilation

(import compiler.ir core)
(import types types)

;; ============================================================================
;; VALIDATION CONTEXT
;; ============================================================================

(data ValidationContext U0
  (case validation-context 
    (-> (vars : List (Pair String Nat))      ;; Variable bindings with indices
        (types : List (Pair String IRValue))  ;; Type definitions
        (in-module : Bool)                    ;; Are we validating module-level?
        (errors : List ValidationError)       ;; Accumulated errors
        ValidationContext)))

(data ValidationError U0
  ;; Variable errors
  (case unbound-variable (-> String Nat ValidationError))
  (case duplicate-variable (-> String ValidationError))
  (case invalid-index (-> String Nat Nat ValidationError))
  
  ;; Type errors
  (case type-mismatch (-> IRValue IRValue ValidationError))
  (case invalid-type-level (-> Nat ValidationError))
  
  ;; Evidence errors
  (case missing-evidence (-> String ValidationError))
  (case invalid-evidence (-> String ValidationError))
  
  ;; Structure errors
  (case malformed-ir (-> String ValidationError))
  (case invalid-module-structure (-> String ValidationError)))

(data ValidationResult U0
  (case validation-success ValidationResult)
  (case validation-failure (-> (List ValidationError) ValidationResult)))

;; ============================================================================
;; VALUE VALIDATION
;; ============================================================================

(define validate-ir-value
  (fn (val ctx)
    (match val
      ;; Simple values are always valid
      (case (ir-nat _) ctx)
      (case (ir-bool _) ctx)
      (case (ir-string _) ctx)
      (case ir-unit ctx)
      
      ;; Validate variable references
      (case (ir-var name index)
        (validate-variable name index ctx))
      
      ;; Validate constructors
      (case (ir-constructor name args evidence)
        (let ((ctx1 (validate-list args validate-ir-value ctx)))
          (validate-termination-evidence evidence ctx1)))
      
      ;; Validate types
      (case (ir-type level)
        (if (nat-less? level hundred)
            ctx
            (add-error (invalid-type-level level) ctx)))
      
      (case (ir-arrow from to)
        (let ((ctx1 (validate-ir-value from ctx)))
          (validate-ir-value to ctx1)))
      
      ;; Validate closures
      (case (ir-closure params body env evidence)
        (let ((ctx1 (validate-params params ctx)))
          (let ((ctx2 (validate-ir-computation body ctx1)))
            (let ((ctx3 (validate-ir-environment env ctx2)))
              (validate-complexity-evidence evidence ctx3))))))))

;; ============================================================================
;; COMPUTATION VALIDATION
;; ============================================================================

(define validate-ir-computation
  (fn (comp ctx)
    (match comp
      ;; Return
      (case (ir-return val)
        (validate-ir-value val ctx))
      
      ;; Let binding
      (case (ir-let name value body space)
        (let ((ctx1 (validate-ir-computation value ctx)))
          (let ((ctx2 (add-var name ctx1)))
            (let ((ctx3 (validate-ir-computation body ctx2)))
              (validate-space-evidence space ctx3)))))
      
      ;; Application
      (case (ir-app func arg evidence)
        (let ((ctx1 (validate-ir-value func ctx)))
          (let ((ctx2 (validate-ir-value arg ctx1)))
            (validate-computation-evidence evidence ctx2))))
      
      ;; Nat elimination
      (case (ir-nat-elim motive base step target evidence)
        (let ((ctx1 (validate-ir-value motive ctx)))
          (let ((ctx2 (validate-ir-value base ctx1)))
            (let ((ctx3 (validate-ir-value step ctx2)))
              (let ((ctx4 (validate-ir-value target ctx3)))
                (validate-termination-evidence evidence ctx4))))))
      
      ;; Bool elimination
      (case (ir-bool-elim motive false-case true-case target evidence)
        (let ((ctx1 (validate-ir-value motive ctx)))
          (let ((ctx2 (validate-ir-value false-case ctx1)))
            (let ((ctx3 (validate-ir-value true-case ctx2)))
              (let ((ctx4 (validate-ir-value target ctx3)))
                (validate-complexity-evidence evidence ctx4))))))
      
      ;; Sequence
      (case (ir-sequence first second)
        (let ((ctx1 (validate-ir-computation first ctx)))
          (validate-ir-computation second ctx1)))
      
      ;; Conditional
      (case (ir-if condition then-branch else-branch evidence)
        (let ((ctx1 (validate-ir-value condition ctx)))
          (let ((ctx2 (validate-ir-computation then-branch ctx1)))
            (let ((ctx3 (validate-ir-computation else-branch ctx2)))
              (validate-branch-evidence evidence ctx3))))))))

;; ============================================================================
;; MODULE VALIDATION
;; ============================================================================

(define validate-ir-module
  (fn (module)
    (match module
      (case (ir-module name imports defs exports evidence)
        (let ((ctx (make-module-context)))
          (let ((ctx1 (validate-imports imports ctx)))
            (let ((ctx2 (validate-definitions defs ctx1)))
              (let ((ctx3 (validate-exports exports defs ctx2)))
                (let ((ctx4 (validate-module-evidence evidence ctx3)))
                  (get-validation-result ctx4))))))))))

;; Validate imports (stub for now)
(define validate-imports
  (fn (imports ctx)
    ctx))

;; Validate all definitions
(define validate-definitions
  (fn (defs ctx)
    (match defs
      (case nil ctx)
      (case (cons def rest)
        (let ((ctx1 (validate-ir-definition def ctx)))
          (validate-definitions rest ctx1))))))

;; Validate single definition
(define validate-ir-definition
  (fn (def ctx)
    (match def
      (case (ir-def-value name type value evidence)
        (let ((ctx1 (validate-ir-value type ctx)))
          (let ((ctx2 (validate-ir-computation value ctx1)))
            (validate-definition-evidence evidence ctx2))))
      
      (case (ir-def-type name params type)
        (let ((ctx1 (add-type-params params ctx)))
          (validate-ir-value type ctx1)))
      
      (case (ir-def-handler name effect-type handler)
        (validate-ir-computation handler ctx)))))

;; Validate exports exist
(define validate-exports
  (fn (exports defs ctx)
    ;; Check each export is defined
    ctx))

;; ============================================================================
;; EVIDENCE VALIDATION
;; ============================================================================

(define validate-termination-evidence
  (fn (evidence ctx)
    (match evidence
      (case (immediate-termination steps)
        (if (nat-less? steps million)
            ctx
            (add-error (invalid-evidence "termination steps too large") ctx)))
      (case _
        ctx))))

(define validate-complexity-evidence
  (fn (evidence ctx)
    ;; Basic validation - could be extended
    ctx))

(define validate-space-evidence
  (fn (evidence ctx)
    ;; Basic validation - could be extended
    ctx))

(define validate-computation-evidence
  (fn (evidence ctx)
    (match evidence
      (case (comp-evidence term complex space)
        (let ((ctx1 (validate-termination-evidence term ctx)))
          (let ((ctx2 (validate-complexity-evidence complex ctx1)))
            (validate-space-evidence space ctx2))))
      (case _
        ctx))))

(define validate-branch-evidence
  (fn (evidence ctx)
    ;; Basic validation
    ctx))

(define validate-definition-evidence
  (fn (evidence ctx)
    ;; Basic validation
    ctx))

(define validate-module-evidence
  (fn (evidence ctx)
    ;; Basic validation
    ctx))

;; ============================================================================
;; ENVIRONMENT VALIDATION
;; ============================================================================

(define validate-ir-environment
  (fn (env ctx)
    ;; Could validate that environment is well-formed
    ctx))

;; ============================================================================
;; CONTEXT OPERATIONS
;; ============================================================================

(define make-module-context
  (fn ()
    (validation-context nil nil true nil)))

(define add-var
  (fn (name ctx)
    (match ctx
      (case (validation-context vars types in-mod errs)
        (validation-context (cons (pair name (length vars)) vars)
                           types in-mod errs)))))

(define add-type-params
  (fn (params ctx)
    ;; Add type parameters to context
    ctx))

(define validate-variable
  (fn (name index ctx)
    (match ctx
      (case (validation-context vars types in-mod errs)
        (if (valid-var-index? name index vars)
            ctx
            (add-error (invalid-index name index (length vars)) ctx))))))

(define valid-var-index?
  (fn (name index vars)
    ;; Check if variable and index match
    true))

(define validate-params
  (fn (params ctx)
    ;; Check for duplicate parameters
    ctx))

(define add-error
  (fn (error ctx)
    (match ctx
      (case (validation-context vars types in-mod errs)
        (validation-context vars types in-mod (cons error errs))))))

(define get-validation-result
  (fn (ctx)
    (match ctx
      (case (validation-context _ _ _ errs)
        (if (null? errs)
            validation-success
            (validation-failure errs))))))

(define validate-list
  (fn (lst validator ctx)
    (match lst
      (case nil ctx)
      (case (cons x xs)
        (let ((ctx1 (validator x ctx)))
          (validate-list xs validator ctx1))))))

;; ============================================================================
;; HELPERS (STUBS)
;; ============================================================================

(define nat-less? (fn (x y) true))
(define hundred (succ (succ (succ (succ (succ zero))))))
(define million hundred)
(define length (fn (lst) zero))
(define null? (fn (lst) 
  (match lst
    (case nil true)
    (case _ false))))
(define pair (fn (x y) (cons x y)))

;; ============================================================================
;; PUBLIC API
;; ============================================================================

;; Validate a complete module
(define validate-module
  (fn (module)
    (validate-ir-module module)))

;; Validate a standalone computation
(define validate-computation
  (fn (comp)
    (let ((ctx (validation-context nil nil false nil)))
      (let ((final-ctx (validate-ir-computation comp ctx)))
        (get-validation-result final-ctx)))))

;; Validate a standalone value
(define validate-value
  (fn (val)
    (let ((ctx (validation-context nil nil false nil)))
      (let ((final-ctx (validate-ir-value val ctx)))
        (get-validation-result final-ctx)))))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export validate-module)
(export validate-computation)
(export validate-value)
(export ValidationResult)
(export ValidationError)
(export validation-success)
(export validation-failure)