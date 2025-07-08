;; ============================================================================
;; COMPUTATION-AWARE EVALUATOR
;; ============================================================================
;; Every evaluation produces a computation with evidence
;; This replaces the traditional direct evaluation model

(import types types)
(import evaluator values-computation)
(import effects computation-as-effect)
(import core ast)

;; ============================================================================
;; EVALUATION CONTEXT WITH EVIDENCE TRACKING
;; ============================================================================

(data ComputationContext U0
  (case comp-context (-> Environment 
                        EffectContext 
                        EvidenceAccumulator
                        ComputationContext)))

;; Accumulate evidence during evaluation
(data EvidenceAccumulator U0
  (case empty-accumulator EvidenceAccumulator)
  (case add-evidence (-> Evidence EvidenceAccumulator EvidenceAccumulator)))

;; ============================================================================
;; MAIN EVALUATION FUNCTION - RETURNS COMPUTATIONS
;; ============================================================================

;; Evaluate expression to a computation (not a direct value!)
(type evaluate-to-computation (-> HoTT-AST ComputationContext (Computation Value)))
(define evaluate-to-computation
  (fn (expr ctx)
    (match expr
      ;; Variable lookup is a computation
      (case (var name)
        (lookup-computation name ctx))
      
      ;; Literal is immediate computation
      (case (literal value)
        (value-computation value (refl _ _)))
      
      ;; Application builds computational evidence
      (case (app func arg)
        (let ((func-comp (evaluate-to-computation func ctx))
              (arg-comp (evaluate-to-computation arg ctx)))
          (apply-computation
            func-comp
            arg-comp
            (infer-application-evidence func arg ctx))))
      
      ;; Lambda creates closure computation
      (case (lambda param body)
        (value-computation
          (make-closure-value (list param) body (context-env ctx))
          (refl _ _)))
      
      ;; Let binding with evidence propagation
      (case (let-expr var value body)
        (bind-computation
          (evaluate-to-computation value ctx)
          (fn (val)
            (evaluate-to-computation body 
              (extend-context ctx var val)))
          (let-binding-evidence var)))
      
      ;; Constructor application
      (case (constructor name args)
        (constructor-computation name
          (map (fn (arg) (evaluate-to-computation arg ctx)) args)
          (constructor-evidence name (length args))))
      
      ;; Pattern matching with exhaustiveness evidence
      (case (match-expr scrutinee cases)
        (match-computation
          (evaluate-to-computation scrutinee ctx)
          (evaluate-cases cases ctx)
          (exhaustiveness-evidence cases)
          (match-complexity-evidence cases)))
      
      ;; Computation expression (already has evidence)
      (case (computation-expr expr evidence)
        (attach-evidence
          (evaluate-to-computation expr ctx)
          evidence))
      
      ;; Evidence annotation
      (case (evidence-annotation ev-type ev-value)
        (value-computation
          (evidence-value ev-type ev-value)
          (annotation-evidence ev-type)))
      
      ;; Optimization directive
      (case (optimization-directive hint expr)
        (optimize-with-hint
          (evaluate-to-computation expr ctx)
          hint))
      
      ;; Pi type
      (case (pi-type param domain codomain)
        (pi-computation param
          (evaluate-to-computation domain ctx)
          (evaluate-to-computation codomain (extend-context-type ctx param))))
      
      ;; Sigma type
      (case (sigma-type param domain codomain)
        (sigma-computation param
          (evaluate-to-computation domain ctx)
          (evaluate-to-computation codomain (extend-context-type ctx param))))
      
      ;; Identity type
      (case (id-type type left right)
        (id-computation
          (evaluate-to-computation type ctx)
          (evaluate-to-computation left ctx)
          (evaluate-to-computation right ctx)))
      
      ;; Type application
      (case (type-app name args)
        (type-app-computation name
          (map (fn (arg) (evaluate-to-computation arg ctx)) args)))
      
      ;; Eliminator
      (case (eliminator target motives)
        (eliminator-computation
          (evaluate-to-computation target ctx)
          (map (fn (m) (evaluate-to-computation m ctx)) motives)
          (eliminator-evidence target))))))

;; ============================================================================
;; COMPUTATIONAL OPERATIONS WITH EVIDENCE
;; ============================================================================

;; Variable lookup as computation
(type lookup-computation (-> String ComputationContext (Computation Value)))
(define lookup-computation
  (fn (name ctx)
    (match (lookup-env (context-env ctx) name)
      (case (some value)
        (value-computation value
          (lookup-evidence name)))
      (case none
        (error-computation (string-append "Unbound variable: " name))))))

;; Constructor as computation
(type constructor-computation (-> String (List (Computation Value)) Evidence (Computation Value)))
(define constructor-computation
  (fn (name args evidence)
    (parallel-computation-list args
      (fn (arg-values)
        (value-computation
          (make-constructor-value name arg-values (infer-constructor-type name))
          evidence)))))

;; Application with evidence building
(type apply-computation (-> (Computation Value) (Computation Value) Evidence (Computation Value)))
(define apply-computation
  (fn (func-comp arg-comp app-evidence)
    (bind-computation func-comp
      (fn (func-val)
        (bind-computation arg-comp
          (fn (arg-val)
            (apply-value-with-evidence func-val arg-val))))
      app-evidence)))

;; ============================================================================
;; EVIDENCE INFERENCE AND PROPAGATION
;; ============================================================================

;; Infer evidence for application
(type infer-application-evidence (-> HoTT-AST HoTT-AST ComputationContext Evidence))
(define infer-application-evidence
  (fn (func arg ctx)
    (combined-evidence-ast
      (list
        (termination-evidence-ast (infer-termination func arg))
        (complexity-evidence-ast (infer-complexity func arg))
        (space-evidence-ast (infer-space func arg))))))

;; Infer termination bound
(type infer-termination (-> HoTT-AST HoTT-AST TerminationBound))
(define infer-termination
  (fn (func arg)
    (match func
      ;; Known builtins have immediate termination
      (case (var name)
        (if (is-builtin? name)
            (always-terminates 1)
            structurally-decreasing))
      ;; Lambda applications terminate if body terminates
      (case (lambda _ body)
        (infer-termination body arg))
      ;; Default: assume structural termination
      (case _ structurally-decreasing))))

;; Infer complexity bound
(type infer-complexity (-> HoTT-AST HoTT-AST ComplexityBound))
(define infer-complexity
  (fn (func arg)
    (match func
      ;; Arithmetic is constant time
      (case (var name)
        (if (is-arithmetic? name)
            constant-time
            linear-time))
      ;; Pattern matching can be linear
      (case (match-expr _ cases)
        (if (> (length cases) 5)
            linear-time
            constant-time))
      ;; Default
      (case _ linear-time))))

;; ============================================================================
;; CONTEXT OPERATIONS
;; ============================================================================

(type context-env (-> ComputationContext Environment))
(define context-env
  (fn (ctx)
    (match ctx
      (case (comp-context env _ _) env))))

(type extend-context (-> ComputationContext String Value ComputationContext))
(define extend-context
  (fn (ctx var val)
    (match ctx
      (case (comp-context env effects evidence)
        (comp-context
          (extend-env env var val)
          effects
          (add-evidence (binding-evidence var val) evidence))))))

;; ============================================================================
;; OPTIMIZATION WITH EVIDENCE
;; ============================================================================

;; Optimize computation based on hint and evidence
(type optimize-with-hint (-> (Computation Value) OptimizationHint (Computation Value)))
(define optimize-with-hint
  (fn (comp hint)
    (match hint
      (case optimize-for-speed
        (speed-optimize comp))
      (case optimize-for-space
        (space-optimize comp))
      (case parallelize-when-safe
        (try-parallelize comp))
      (case cache-when-deterministic
        (if (is-deterministic-computation? comp)
            (cache-computation comp)
            comp))
      (case _ comp))))

;; ============================================================================
;; EVIDENCE COMBINATORS
;; ============================================================================

;; Combine evidence from multiple sources
(type combine-evidence (-> (List Evidence) Evidence))
(define combine-evidence
  (fn (evs)
    (combined-evidence-ast evs)))

;; Extract total complexity from evidence
(type total-complexity (-> Evidence ComplexityBound))
(define total-complexity
  (fn (ev)
    (match ev
      (case (complexity-evidence-ast c) c)
      (case (combined-evidence-ast evs)
        (fold-complexity (map total-complexity evs)))
      (case _ constant-time))))

;; ============================================================================
;; RUNNING COMPUTATIONS
;; ============================================================================

;; Execute computation to get final value (loses evidence)
(type run-computation (-> (Computation Value) Value))
(define run-computation
  (fn (comp)
    (match comp
      (case (value-computation v _) v)
      (case (bind-computation c cont _)
        (run-computation (cont (run-computation c))))
      (case _ (error "Cannot run complex computation")))))

;; Execute computation preserving evidence
(type run-with-evidence (-> (Computation Value) (Pair Value Evidence)))
(define run-with-evidence
  (fn (comp)
    (match comp
      (case (value-computation v ev)
        (pair v ev))
      (case (bind-computation c cont ev)
        (let ((c-result (run-with-evidence c)))
          (let ((cont-result (run-with-evidence (cont (fst c-result)))))
            (pair (fst cont-result)
                  (combine-evidence (list ev (snd c-result) (snd cont-result)))))))
      (case _ (error "Cannot run complex computation")))))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export evaluate-to-computation)
(export ComputationContext)
(export run-computation)
(export run-with-evidence)
(export infer-application-evidence)
(export optimize-with-hint)