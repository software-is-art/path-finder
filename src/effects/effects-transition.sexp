;; ============================================================================
;; TRANSITION WRAPPER - BRIDGES OLD AND NEW EFFECT SYSTEMS
;; ============================================================================
;; Provides compatibility layer between monadic and HoTT-native effects

(import effects effects-hott)
(import effects effects)

;; ============================================================================
;; CONVERSION FUNCTIONS
;; ============================================================================

;; Convert old Effect to new ComputationalAction
(type effect-to-action (-> (Effect A) (ComputationalAction A)))
(define effect-to-action
  (fn (eff)
    (match eff
      ;; Pure effect becomes pure computation
      (case (pure-effect v)
        (pure-computation v (refl _ v)))
      
      ;; IO effect becomes io-action with default evidence
      (case (io-effect name op args det)
        (io-action
          (generic-capability name)
          (legacy-io-operation name op args)
          (fn (result) (legacy-evidence det))))
      
      ;; Sequential composition
      (case (effect-seq e1 cont)
        (compose-paths
          (effect-to-action e1)
          (fn (sigma-val)
            (let ((result (sigma-first sigma-val)))
              (effect-to-action (cont result))))
          (legacy-coherence)))
      
      ;; Parallel composition  
      (case (effect-par e1 e2)
        (parallel-paths
          (effect-to-action e1)
          (effect-to-action e2)
          (legacy-independence)))
      
      ;; Choice
      (case (effect-choice e1 e2)
        (choice-paths
          (effect-to-action e1)
          (effect-to-action e2)
          (legacy-choice-coherence))))))

;; Convert new ComputationalAction to old Effect (lossy)
(type action-to-effect (-> (ComputationalAction A) (Effect A)))
(define action-to-effect
  (fn (action)
    (match action
      ;; Pure computation becomes pure effect
      (case (pure-computation v _)
        (pure-effect v))
      
      ;; IO action becomes io effect (loses evidence)
      (case (io-action cap op _)
        (io-effect
          (capability-name cap)
          (operation-name op)
          (operation-args op)
          (operation-determinism op)))
      
      ;; Sequential composition
      (case (compose-paths a1 cont _)
        (effect-seq
          (action-to-effect a1)
          (fn (v) (action-to-effect (cont (make-sigma v (dummy-evidence)))))))
      
      ;; Parallel composition
      (case (parallel-paths a1 a2 _)
        (effect-par
          (action-to-effect a1)
          (action-to-effect a2)))
      
      ;; Choice
      (case (choice-paths a1 a2 _)
        (effect-choice
          (action-to-effect a1)
          (action-to-effect a2))))))

;; ============================================================================
;; LEGACY SUPPORT STRUCTURES
;; ============================================================================

;; Generic capability for legacy effects
(type generic-capability (-> String IOCapability))
(define generic-capability
  (fn (name)
    (match name
      (case "file" (file-capability "*" (list read-permission write-permission) 
                                   (legacy-permission-proof)))
      (case "print" (console-capability (legacy-console-proof)))
      (case "input" (console-capability (legacy-console-proof)))
      (case "env" (env-capability (list "*") (legacy-env-proof)))
      (case _ (console-capability (legacy-console-proof))))))

;; Legacy IO operation wrapper
(data LegacyIOOperation (-> Type U0)
  (case legacy-io-op (-> String String (List Value) (LegacyIOOperation A))))

(type legacy-io-operation (-> String String (List Value) (IOOperation A)))
(define legacy-io-operation
  (fn (name op args)
    (legacy-io-op name op args)))

;; Legacy evidence constructor
(type legacy-evidence (-> Determinism (ComputationEvidence A)))
(define legacy-evidence
  (fn (det)
    (match det
      (case deterministic 
        (deterministic-evidence (legacy-determinism-proof)))
      (case semi-deterministic
        (state-transition-evidence initial-state initial-state (refl _ _)))
      (case non-deterministic
        (resource-evidence (no-resources) (no-usage-proof))))))

;; Legacy coherence proofs (stubs)
(type legacy-coherence (-> CompositionCoherence))
(define legacy-coherence
  (fn () (make-coherence (sorry))))

(type legacy-independence (-> IndependenceProof))
(define legacy-independence  
  (fn () (make-independence (sorry))))

(type legacy-choice-coherence (-> ChoiceCoherence))
(define legacy-choice-coherence
  (fn () (make-choice-coherence (sorry))))

;; ============================================================================
;; COMPATIBILITY API
;; ============================================================================

;; Keep old names working with new implementation
(type print-compat (-> String (Effect Unit)))
(define print-compat
  (fn (s)
    (action-to-effect (print s))))

(type read-file-compat (-> String (Effect String)))
(define read-file-compat
  (fn (path)
    (action-to-effect
      (>>= (read-file path)
           (fn (sigma) (pure (sigma-first sigma)))))))

(type write-file-compat (-> String String (Effect Unit)))
(define write-file-compat
  (fn (path content)
    (action-to-effect
      (>>= (write-file path content)
           (fn (sigma) (pure unit))))))

;; ============================================================================
;; MIGRATION HELPERS
;; ============================================================================

;; Check if using old or new system
(type is-legacy-effect? (-> Value Bool))
(define is-legacy-effect?
  (fn (v)
    (match v
      (case (effect-value _) true)
      (case (computation-value _) false)
      (case _ false))))

;; Unified effect executor (works with both systems)
(type execute-unified (-> Value (List EffectHandler) (Result Value)))
(define execute-unified
  (fn (eff-val handlers)
    (if (is-legacy-effect? eff-val)
        (execute-effect (extract-effect eff-val) handlers)
        (execute-computation (extract-computation eff-val) handlers))))

;; Extract effect from value
(type extract-effect (-> Value (Effect A)))
(define extract-effect
  (fn (v)
    (match v
      (case (effect-value eff) eff)
      (case _ (error "Not an effect value")))))

;; Extract computation from value  
(type extract-computation (-> Value (ComputationalAction A)))
(define extract-computation
  (fn (v)
    (match v
      (case (computation-value comp) comp)
      (case _ (error "Not a computation value")))))

;; ============================================================================
;; EXPORTS
;; ============================================================================

;; Export both old and new APIs during transition
(export effect-to-action)
(export action-to-effect)
(export print-compat)
(export read-file-compat)
(export write-file-compat)
(export execute-unified)