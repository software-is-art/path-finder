;; ============================================================================
;; INTERMEDIATE REPRESENTATION (IR) CORE
;; ============================================================================
;; HoTT-native IR that preserves computational evidence through compilation
;; This IR serves as the foundation for evidence-aware optimizations

(import types types)
(import effects computation-as-effect)

;; ============================================================================
;; IR VALUE REPRESENTATION
;; ============================================================================

(data IRValue U0
  ;; Constants
  (case ir-nat (-> Nat IRValue))
  (case ir-bool (-> Bool IRValue))
  (case ir-string (-> String IRValue))
  (case ir-unit IRValue)
  
  ;; Variables with De Bruijn indices for correct scoping
  (case ir-var (-> (name : String) (index : Nat) IRValue))
  
  ;; Constructors with evidence of well-formedness
  (case ir-constructor (-> (name : String) 
                          (args : List IRValue)
                          (evidence : TerminationEvidence)
                          IRValue))
  
  ;; Type representations
  (case ir-type (-> (level : Nat) IRValue))
  (case ir-arrow (-> (from : IRValue) (to : IRValue) IRValue))
  
  ;; Function values (closures)
  (case ir-closure (-> (params : List String)
                      (body : IRComputation)
                      (env : IREnvironment)
                      (evidence : ComplexityEvidence)
                      IRValue)))

;; ============================================================================
;; IR COMPUTATION REPRESENTATION
;; ============================================================================

(data IRComputation U0
  ;; Return a value
  (case ir-return (-> IRValue IRComputation))
  
  ;; Let binding with evidence
  (case ir-let (-> (name : String)
                  (value : IRComputation)
                  (body : IRComputation)
                  (evidence : SpaceEvidence)
                  IRComputation))
  
  ;; Function application with evidence
  (case ir-app (-> (func : IRValue)
                  (arg : IRValue)
                  (evidence : ComputationEvidence)
                  IRComputation))
  
  ;; Eliminators with evidence
  (case ir-nat-elim (-> (motive : IRValue)
                       (base : IRValue)
                       (step : IRValue)
                       (target : IRValue)
                       (evidence : TerminationEvidence)
                       IRComputation))
  
  (case ir-bool-elim (-> (motive : IRValue)
                        (false-case : IRValue)
                        (true-case : IRValue)
                        (target : IRValue)
                        (evidence : ComplexityEvidence)
                        IRComputation))
  
  ;; Sequencing for effects
  (case ir-sequence (-> (first : IRComputation)
                       (second : IRComputation)
                       IRComputation))
  
  ;; Conditional with evidence of branch selection
  (case ir-if (-> (condition : IRValue)
                 (then-branch : IRComputation)
                 (else-branch : IRComputation)
                 (evidence : BranchEvidence)
                 IRComputation)))

;; ============================================================================
;; IR EFFECT REPRESENTATION
;; ============================================================================

(data IREffect U0
  ;; Perform an effect with evidence
  (case ir-perform (-> (effect : EffectDescriptor)
                      (evidence : EffectEvidence)
                      IREffect))
  
  ;; Bind effect result
  (case ir-bind (-> (effect : IREffect)
                   (continuation : IRComputation)
                   IREffect))
  
  ;; Pure computation as effect
  (case ir-pure (-> (computation : IRComputation) IREffect)))

;; ============================================================================
;; IR MODULE REPRESENTATION
;; ============================================================================

(data IRModule U0
  (case ir-module (-> (name : String)
                     (imports : List Import)
                     (definitions : List IRDefinition)
                     (exports : List String)
                     (evidence : ModuleEvidence)
                     IRModule)))

(data IRDefinition U0
  ;; Value definition with evidence
  (case ir-def-value (-> (name : String)
                        (type : IRValue)
                        (value : IRComputation)
                        (evidence : DefinitionEvidence)
                        IRDefinition))
  
  ;; Type definition
  (case ir-def-type (-> (name : String)
                       (params : List String)
                       (type : IRValue)
                       IRDefinition))
  
  ;; Effect handler definition
  (case ir-def-handler (-> (name : String)
                          (effect-type : EffectType)
                          (handler : IRComputation)
                          IRDefinition)))

;; ============================================================================
;; EVIDENCE IN IR
;; ============================================================================

;; Combined evidence for computations
(data ComputationEvidence U0
  (case comp-evidence (-> (termination : TerminationEvidence)
                         (complexity : ComplexityEvidence)
                         (space : SpaceEvidence)
                         ComputationEvidence)))

;; Evidence for conditional branches
(data BranchEvidence U0
  ;; Static branch (known at compile time)
  (case static-branch (-> (taken : Bool) BranchEvidence))
  
  ;; Dynamic branch with probability
  (case dynamic-branch (-> (probability : Rational) BranchEvidence))
  
  ;; Unknown branch
  (case unknown-branch BranchEvidence))

;; Evidence for effects
(data EffectEvidence U0
  ;; Pure effect (no side effects)
  (case pure-effect EffectEvidence)
  
  ;; IO effect with determinism info
  (case io-effect (-> (deterministic : Bool) EffectEvidence))
  
  ;; State effect
  (case state-effect (-> (read-only : Bool) EffectEvidence)))

;; Evidence for definitions
(data DefinitionEvidence U0
  (case def-evidence (-> (usage-count : Nat)
                        (inline-benefit : Nat)
                        (size : Nat)
                        DefinitionEvidence)))

;; Evidence for modules
(data ModuleEvidence U0
  (case module-evidence (-> (total-complexity : ComplexityEvidence)
                           (total-space : SpaceEvidence)
                           (effect-summary : List EffectEvidence)
                           ModuleEvidence)))

;; ============================================================================
;; IR ENVIRONMENT
;; ============================================================================

(data IREnvironment U0
  (case ir-empty-env IREnvironment)
  (case ir-extend-env (-> (name : String)
                         (value : IRValue)
                         (rest : IREnvironment)
                         IREnvironment)))

;; ============================================================================
;; EFFECT DESCRIPTORS
;; ============================================================================

(data EffectDescriptor U0
  ;; Print effect
  (case effect-print (-> (message : IRValue) EffectDescriptor))
  
  ;; File operations
  (case effect-file-read (-> (path : IRValue) EffectDescriptor))
  (case effect-file-write (-> (path : IRValue) (content : IRValue) EffectDescriptor))
  
  ;; General IO effect
  (case effect-io (-> (category : String)
                     (operation : String)
                     (args : List IRValue)
                     EffectDescriptor)))

;; ============================================================================
;; EFFECT TYPES
;; ============================================================================

(data EffectType U0
  (case print-effect-type EffectType)
  (case file-effect-type EffectType)
  (case io-effect-type EffectType)
  (case computation-effect-type EffectType))

;; ============================================================================
;; RATIONAL NUMBERS FOR PROBABILITIES
;; ============================================================================

(data Rational U0
  (case ratio (-> (numerator : Nat) (denominator : Nat) Rational)))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export IRValue)
(export IRComputation)
(export IREffect)
(export IRModule)
(export IRDefinition)
(export IREnvironment)
(export ComputationEvidence)
(export BranchEvidence)
(export EffectEvidence)
(export DefinitionEvidence)
(export ModuleEvidence)
(export EffectDescriptor)
(export EffectType)
(export Rational)