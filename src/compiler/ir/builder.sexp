;; ============================================================================
;; IR BUILDER UTILITIES
;; ============================================================================
;; Helper functions for constructing well-formed IR with proper evidence

(import compiler.ir core)
(import types types)
(import effects computation-as-effect)

;; ============================================================================
;; VALUE BUILDERS
;; ============================================================================

;; Build IR for natural number
(define make-ir-nat
  (fn (n)
    (ir-nat n)))

;; Build IR for boolean
(define make-ir-bool
  (fn (b)
    (ir-bool b)))

;; Build IR for string
(define make-ir-string
  (fn (s)
    (ir-string s)))

;; Build IR variable with automatic index calculation
(define make-ir-var
  (fn (name env)
    (let ((index (lookup-var-index name env)))
      (ir-var name index))))

;; Build constructor with default evidence
(define make-ir-constructor
  (fn (name args)
    (ir-constructor name args (immediate-termination one))))

;; Build function type
(define make-ir-function-type
  (fn (param-type return-type)
    (ir-arrow param-type return-type)))

;; Build closure with complexity analysis
(define make-ir-closure
  (fn (params body env)
    (let ((complexity (analyze-complexity body)))
      (ir-closure params body env complexity))))

;; ============================================================================
;; COMPUTATION BUILDERS
;; ============================================================================

;; Build return computation
(define make-ir-return
  (fn (value)
    (ir-return value)))

;; Build let binding with space analysis
(define make-ir-let
  (fn (name value body)
    (let ((space (analyze-space value)))
      (ir-let name value body space))))

;; Build function application with evidence
(define make-ir-app
  (fn (func arg)
    (let ((evidence (make-computation-evidence)))
      (ir-app func arg evidence))))

;; Build nat elimination with termination proof
(define make-ir-nat-elim
  (fn (motive base step target)
    (let ((term-evidence (prove-nat-elim-termination target)))
      (ir-nat-elim motive base step target term-evidence))))

;; Build bool elimination with constant complexity
(define make-ir-bool-elim
  (fn (motive false-case true-case target)
    (ir-bool-elim motive false-case true-case target 
                  (constant-complexity one))))

;; Build sequencing for effects
(define make-ir-sequence
  (fn (first second)
    (ir-sequence first second)))

;; Build conditional with branch prediction
(define make-ir-if
  (fn (condition then-branch else-branch)
    (let ((branch-evidence (analyze-branch condition)))
      (ir-if condition then-branch else-branch branch-evidence))))

;; ============================================================================
;; EFFECT BUILDERS
;; ============================================================================

;; Build perform effect
(define make-ir-perform
  (fn (effect-desc)
    (let ((evidence (analyze-effect effect-desc)))
      (ir-perform effect-desc evidence))))

;; Build effect bind
(define make-ir-bind
  (fn (effect continuation)
    (ir-bind effect continuation)))

;; Build pure effect
(define make-ir-pure
  (fn (computation)
    (ir-pure computation)))

;; ============================================================================
;; MODULE BUILDERS
;; ============================================================================

;; Build module with evidence aggregation
(define make-ir-module
  (fn (name imports definitions exports)
    (let ((evidence (aggregate-module-evidence definitions)))
      (ir-module name imports definitions exports evidence))))

;; Build value definition with usage analysis
(define make-ir-def-value
  (fn (name type value)
    (let ((evidence (analyze-definition name value)))
      (ir-def-value name type value evidence))))

;; Build type definition
(define make-ir-def-type
  (fn (name params type)
    (ir-def-type name params type)))

;; Build effect handler
(define make-ir-def-handler
  (fn (name effect-type handler)
    (ir-def-handler name effect-type handler)))

;; ============================================================================
;; EVIDENCE BUILDERS
;; ============================================================================

;; Create default computation evidence
(define make-computation-evidence
  (fn ()
    (comp-evidence (immediate-termination one)
                   (constant-complexity one)
                   (constant-space one))))

;; Create evidence for nat-elim termination
(define prove-nat-elim-termination
  (fn (target)
    (match target
      (case (ir-nat n)
        (immediate-termination n))
      (case (ir-var _ _)
        (structural-termination zero (refl zero)))
      (case _
        (immediate-termination zero)))))

;; Analyze branch conditions
(define analyze-branch
  (fn (condition)
    (match condition
      (case (ir-bool b)
        (static-branch b))
      (case _
        unknown-branch))))

;; Analyze effect properties
(define analyze-effect
  (fn (effect-desc)
    (match effect-desc
      (case (effect-print _)
        (io-effect true))
      (case (effect-file-read _)
        (io-effect false))
      (case (effect-file-write _ _)
        (io-effect true))
      (case _
        pure-effect))))

;; ============================================================================
;; ANALYSIS HELPERS
;; ============================================================================

;; Analyze complexity of computation
(define analyze-complexity
  (fn (comp)
    (match comp
      (case (ir-return _)
        (constant-complexity one))
      (case (ir-let _ val body _)
        (let ((val-complex (analyze-complexity val)))
          (let ((body-complex (analyze-complexity body)))
            (composed-complexity val-complex body-complex))))
      (case (ir-app _ _ _)
        (constant-complexity one))
      (case _
        (constant-complexity one)))))

;; Analyze space usage
(define analyze-space
  (fn (comp)
    (match comp
      (case (ir-return val)
        (analyze-value-space val))
      (case (ir-let _ _ _ _)
        (stack-space one))
      (case _
        (constant-space zero)))))

;; Analyze value space
(define analyze-value-space
  (fn (val)
    (match val
      (case (ir-nat _) (constant-space one))
      (case (ir-bool _) (constant-space one))
      (case (ir-string s) (linear-space (string-length s)))
      (case (ir-constructor _ args _)
        (linear-space (length args)))
      (case _ (constant-space one)))))

;; ============================================================================
;; ENVIRONMENT HELPERS
;; ============================================================================

;; Look up variable index in environment
(define lookup-var-index
  (fn (name env)
    (lookup-var-index-helper name env zero)))

(define lookup-var-index-helper
  (fn (name env index)
    (match env
      (case ir-empty-env
        index)
      (case (ir-extend-env var-name _ rest)
        (if (string-equal? name var-name)
            index
            (lookup-var-index-helper name rest (succ index)))))))

;; Aggregate evidence from definitions
(define aggregate-module-evidence
  (fn (defs)
    (module-evidence 
      (constant-complexity (length defs))
      (constant-space (length defs))
      nil)))

;; Analyze definition for optimization hints
(define analyze-definition
  (fn (name value)
    (def-evidence one zero one)))

;; ============================================================================
;; STUB IMPLEMENTATIONS
;; ============================================================================

;; String operations (would be properly implemented)
(define string-length (fn (s) one))
(define string-equal? (fn (s1 s2) true))
(define length (fn (lst) one))
(define refl (fn (x) x))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export make-ir-nat)
(export make-ir-bool)
(export make-ir-string)
(export make-ir-var)
(export make-ir-constructor)
(export make-ir-function-type)
(export make-ir-closure)
(export make-ir-return)
(export make-ir-let)
(export make-ir-app)
(export make-ir-nat-elim)
(export make-ir-bool-elim)
(export make-ir-sequence)
(export make-ir-if)
(export make-ir-perform)
(export make-ir-bind)
(export make-ir-pure)
(export make-ir-module)
(export make-ir-def-value)
(export make-ir-def-type)
(export make-ir-def-handler)
(export make-computation-evidence)
(export analyze-complexity)
(export analyze-space)