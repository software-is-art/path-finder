;; ============================================================================
;; PURE MATHEMATICAL HoTT-NATIVE TYPE INFERENCE AND ELABORATION (S-EXPRESSION VERSION)
;; ============================================================================
;; This replaces hott-native-inference.rkt with pure mathematical HoTT notation.
;; Uses PathFinder's native HoTT foundations: type families, identity types,
;; computational evidence, and universe polymorphism.

;; Import dependencies
(import types types)
(import evaluator values)
(import core ast)
(import types families)

;; ============================================================================
;; INFERENCE UNIVERSE: HoTT-NATIVE CONSTRAINT SYSTEM
;; ============================================================================

;; Inference universe: a universe containing type variables and constraints
;; Following HoTT: inference happens within a universe at a specific level
(data InferenceUniverse U0
  (case inference-universe (-> Nat                        ;; universe level
                              TypeEnvironment             ;; type environment
                              (List UniverseConstraint)   ;; constraints
                              EvidenceContext             ;; evidence context
                              Tier                        ;; execution tier
                              InferenceUniverse)))

;; Type variable: a term in the universe waiting for evidence
(data TypeVariable U0
  (case type-variable (-> String                          ;; variable name
                         Nat                              ;; universe level
                         (List EvidenceRequirement)       ;; evidence requirements
                         TypeVariable)))

;; Proof obligation: computational evidence that must be provided
(data ProofObligation U0
  (case proof-obligation (-> Type                         ;; obligation type
                            (Maybe Value)                 ;; witness (if found)
                            Goal                          ;; goal to prove
                            InferenceContext              ;; local context
                            ProofObligation)))

;; Evidence context: tracks computational proofs and witnesses
(data EvidenceContext U0
  (case evidence-context (-> (List (Pair Type Evidence))               ;; type-evidence pairs
                            (List (Triple Type Evidence HoTT-AST))     ;; witnesses with sources
                            (List ProofObligation)                      ;; pending obligations
                            EvidenceContext)))

;; Evidence requirement for type variables
(data EvidenceRequirement U0
  (case requires-proof (-> Type EvidenceRequirement))
  (case requires-witness (-> Type EvidenceRequirement))
  (case requires-computation (-> Type EvidenceRequirement)))

;; Goal representation
(data Goal U0
  (case prove-equal (-> Type Type Goal))
  (case prove-inhabited (-> Type Goal))
  (case prove-computation (-> Type Value Goal)))

;; Inference context for local reasoning
(data InferenceContext U0
  (case local-context (-> (List (Pair String Type)) InferenceContext)))

;; Evidence for computational proofs
(data Evidence U0
  (case literal-evidence (-> Value Evidence))
  (case formation-evidence (-> String (List Type) Nat Evidence))
  (case elimination-evidence (-> HoTT-AST HoTT-AST Evidence))
  (case constraint-evidence (-> Type Type Evidence)))

;; ============================================================================
;; INFERENCE UNIVERSE CONSTRUCTION
;; ============================================================================

;; Create inference universe at specified level
(type make-inference-universe (-> Nat TypeEnvironment Tier InferenceUniverse))
(define make-inference-universe
  (fn (level type-env tier)
    (inference-universe level type-env nil 
      (evidence-context nil nil nil) tier)))

;; ============================================================================
;; HoTT TYPE SYNTHESIS (INFERENCE)
;; ============================================================================

;; HoTT-native type synthesis using computational evidence
(type hott-infer (-> HoTT-AST InferenceUniverse (Pair Type InferenceUniverse)))
(define hott-infer
  (fn (ast universe)
    (hott-ast-eliminator (Pair Type InferenceUniverse) ast
      ;; Variable case: lookup with universe level checking
      (fn (name)
        (universe-lookup universe name))
      
      ;; Application case: Pi-elimination with path computation
      (fn (func-ast arg-ast)
        (hott-infer-pi-elimination (cons func-ast (cons arg-ast nil)) universe))
      
      ;; Lambda case: Pi-introduction with computational evidence
      (fn (param body-ast)
        (hott-infer-pi-introduction param body-ast universe))
      
      ;; Pi-type case: Pi-type formation
      (fn (var domain-ast codomain-ast)
        (hott-infer-pi-formation var domain-ast codomain-ast universe))
      
      ;; Sigma-type case: Sigma-type formation
      (fn (var first-ast second-ast)
        (hott-infer-sigma-formation var first-ast second-ast universe))
      
      ;; Identity-type case: identity type formation
      (fn (type-ast left-ast right-ast)
        (hott-infer-identity-formation type-ast left-ast right-ast universe))
      
      ;; Eliminator case: HoTT eliminator inference
      (fn (target cases)
        (hott-infer-eliminator target cases universe))
      
      ;; Type application case: type family application
      (fn (family-name args)
        (hott-infer-type-family-application family-name args universe))
      
      ;; Constructor case: inductive type constructor inference
      (fn (constructor-name args)
        (hott-infer-constructor constructor-name args universe))
      
      ;; Literal case: canonical types with computational evidence
      (fn (value)
        (hott-infer-literal value universe))
      
      ;; Effect case: effect type inference
      (fn (eff)
        (hott-infer-effect eff universe)))))

;; ============================================================================
;; HoTT TYPE CHECKING WITH COMPUTATIONAL EVIDENCE
;; ============================================================================

;; HoTT-native type checking: verify computational evidence
(type hott-check (-> HoTT-AST Type InferenceUniverse (Pair Evidence InferenceUniverse)))
(define hott-check
  (fn (ast expected-type universe)
    (let ((infer-result (hott-infer ast universe)))
      (let ((inferred-type (first infer-result)))
        (let ((updated-universe (second infer-result)))
          (let ((constraint-result (generate-identity-constraint updated-universe 
                                                               inferred-type 
                                                               expected-type ast)))
            (let ((evidence (constraint-evidence inferred-type expected-type)))
              (pair evidence constraint-result))))))))

;; ============================================================================
;; Pi-TYPE FORMATION AND ELIMINATION (HoTT-NATIVE)
;; ============================================================================

;; Lambda inference: Pi-introduction with computational evidence
(type hott-infer-pi-introduction 
      (-> String HoTT-AST InferenceUniverse (Pair Type InferenceUniverse)))
(define hott-infer-pi-introduction
  (fn (param body-ast universe)
    (let ((param-type-var (fresh-type-variable universe)))
      (let ((extended-universe (extend-universe-binding universe param param-type-var)))
        (let ((body-result (hott-infer body-ast extended-universe)))
          (let ((body-type (first body-result)))
            (let ((final-universe (second body-result)))
              (let ((pi-type (make-pi-type param param-type-var body-type)))
                (let ((evidence (formation-evidence "pi-intro" 
                                  (cons param-type-var (cons body-type nil)) zero)))
                  (let ((universe-with-evidence 
                         (record-computational-evidence final-universe pi-type evidence 
                                                      (lambda param body-ast))))
                    (pair pi-type universe-with-evidence)))))))))))

;; Function application: Pi-elimination with path computation
(type hott-infer-pi-elimination 
      (-> (List HoTT-AST) InferenceUniverse (Pair Type InferenceUniverse)))
(define hott-infer-pi-elimination
  (fn (elements universe)
    (List-elim elements
      ;; Empty elements: error
      (pair sorry universe)
      (fn (func-ast rest _)
        (List-elim rest
          ;; Single element: return its type
          (hott-infer func-ast universe)
          (fn (arg-ast rest-args _)
            (let ((func-result (hott-infer func-ast universe)))
              (let ((func-type (first func-result)))
                (let ((universe1 (second func-result)))
                  (let ((arg-result (hott-infer arg-ast universe1)))
                    (let ((arg-type (first arg-result)))
                      (let ((universe2 (second arg-result)))
                        ;; Handle function type elimination
                        (Type-elim (Pair Type InferenceUniverse) func-type
                          ;; Universe case: not a function
                          (fn (n) (pair sorry universe2))  ;; Error
                          ;; Pi-type case: dependent function application
                          (fn (var domain codomain)
                            (let ((constraint-universe 
                                   (generate-identity-constraint universe2 arg-type 
                                                               domain arg-ast)))
                              (let ((result-type (substitute-in-type codomain var 
                                                                    arg-ast constraint-universe)))
                                (let ((evidence (elimination-evidence func-ast arg-ast)))
                                  (let ((final-universe 
                                         (record-computational-evidence constraint-universe 
                                                                      result-type evidence 
                                                                      (app func-ast arg-ast))))
                                    (pair result-type final-universe))))))
                          ;; Other type cases: not functions
                          (fn (var first second) (pair sorry universe2))
                          (fn (left right) (pair sorry universe2))
                          (fn (A x y) (pair sorry universe2))
                          (pair sorry universe2)
                          (pair sorry universe2)
                          (fn (name constructors) (pair sorry universe2))
                          (fn (base req opt) (pair sorry universe2))))))))))))))

;; ============================================================================
;; TYPE FAMILY INFERENCE (ADAPTIVE DISPATCH)
;; ============================================================================

(type hott-infer-type-family-application 
      (-> String (List HoTT-AST) InferenceUniverse (Pair Type InferenceUniverse)))
(define hott-infer-type-family-application
  (fn (family-name args universe)
    (let ((family-maybe (get-type-family-by-name family-name)))
      (Maybe-elim family-maybe
        ;; Unknown family: error
        (pair sorry universe)
        (fn (family)
          (let ((arg-results (infer-list-types args universe)))
            (let ((arg-types (first arg-results)))
              (let ((updated-universe (second arg-results)))
                (let ((tier (InferenceUniverse-elim updated-universe
                               (fn (level env constraints evidence t) t))))
                  ;; Dispatch based on execution tier
                  (Tier-elim (Pair Type InferenceUniverse) tier
                    (tier0-type-family-instantiation family arg-types updated-universe)
                    (tier1-type-family-instantiation family arg-types updated-universe)
                    (tier2-type-family-instantiation family arg-types updated-universe)
                    (tier3-type-family-instantiation family arg-types updated-universe)))))))))))

;; Tier 0: Pure mathematical instantiation
(type tier0-type-family-instantiation 
      (-> TypeFamily (List Type) InferenceUniverse (Pair Type InferenceUniverse)))
(define tier0-type-family-instantiation
  (fn (family arg-types universe)
    (let ((instantiated-type (instantiate-type-family family arg-types)))
      (let ((evidence (formation-evidence "type-family-tier0" arg-types zero)))
        (let ((universe-with-evidence 
               (record-computational-evidence universe instantiated-type evidence sorry)))
          (pair instantiated-type universe-with-evidence))))))

;; Tier 1: Compile-time type family instantiation
(type tier1-type-family-instantiation 
      (-> TypeFamily (List Type) InferenceUniverse (Pair Type InferenceUniverse)))
(define tier1-type-family-instantiation
  (fn (family arg-types universe)
    (let ((instantiated-type (instantiate-type-family family arg-types)))
      (let ((evidence (formation-evidence "type-family-tier1" arg-types one)))
        (let ((universe-with-evidence 
               (record-computational-evidence universe instantiated-type evidence sorry)))
          (pair instantiated-type universe-with-evidence))))))

;; Tier 2: Type-resolution time type family instantiation
(type tier2-type-family-instantiation 
      (-> TypeFamily (List Type) InferenceUniverse (Pair Type InferenceUniverse)))
(define tier2-type-family-instantiation
  (fn (family arg-types universe)
    (let ((instantiated-type (instantiate-type-family family arg-types)))
      (let ((evidence (formation-evidence "type-family-tier2" arg-types two)))
        (let ((universe-with-evidence 
               (record-computational-evidence universe instantiated-type evidence sorry)))
          (pair instantiated-type universe-with-evidence))))))

;; Tier 3: Runtime type family instantiation
(type tier3-type-family-instantiation 
      (-> TypeFamily (List Type) InferenceUniverse (Pair Type InferenceUniverse)))
(define tier3-type-family-instantiation
  (fn (family arg-types universe)
    (let ((instantiated-type (instantiate-type-family family arg-types)))
      (let ((evidence (formation-evidence "type-family-tier3" arg-types three)))
        (let ((universe-with-evidence 
               (record-computational-evidence universe instantiated-type evidence sorry)))
          (pair instantiated-type universe-with-evidence))))))

;; ============================================================================
;; DEPENDENT TYPE FORMATION (UNIVERSE POLYMORPHIC)
;; ============================================================================

;; Sigma-type formation: (Sigma (x : A) B)
(type hott-infer-sigma-formation 
      (-> String HoTT-AST HoTT-AST InferenceUniverse (Pair Type InferenceUniverse)))
(define hott-infer-sigma-formation
  (fn (var domain-ast body-ast universe)
    (let ((domain-result (hott-infer domain-ast universe)))
      (let ((domain-type (first domain-result)))
        (let ((universe1 (second domain-result)))
          (let ((extended-universe (extend-universe-binding universe1 var domain-type)))
            (let ((codomain-result (hott-infer body-ast extended-universe)))
              (let ((codomain-type (first codomain-result)))
                (let ((universe2 (second codomain-result)))
                  ;; Universe level calculation with computational evidence
                  (let ((domain-level (type-universe-level domain-type)))
                    (let ((codomain-level (type-universe-level codomain-type)))
                      (let ((result-level (nat-max domain-level codomain-level)))
                        (let ((sigma-type (make-sigma-type var domain-type codomain-type)))
                          (let ((evidence (formation-evidence "sigma-formation" 
                                            (cons domain-type (cons codomain-type nil)) 
                                            result-level)))
                            (let ((final-universe 
                                   (record-computational-evidence universe2 sigma-type evidence 
                                                                (sigma-type var domain-ast body-ast))))
                              (pair sigma-type final-universe))))))))))))))))

;; Pi-type formation: (Pi (x : A) B)
(type hott-infer-pi-formation 
      (-> String HoTT-AST HoTT-AST InferenceUniverse (Pair Type InferenceUniverse)))
(define hott-infer-pi-formation
  (fn (var domain-ast body-ast universe)
    (let ((domain-result (hott-infer domain-ast universe)))
      (let ((domain-type (first domain-result)))
        (let ((universe1 (second domain-result)))
          (let ((extended-universe (extend-universe-binding universe1 var domain-type)))
            (let ((codomain-result (hott-infer body-ast extended-universe)))
              (let ((codomain-type (first codomain-result)))
                (let ((universe2 (second codomain-result)))
                  ;; Universe level calculation with computational evidence
                  (let ((domain-level (type-universe-level domain-type)))
                    (let ((codomain-level (type-universe-level codomain-type)))
                      (let ((result-level (nat-max domain-level codomain-level)))
                        (let ((pi-type (make-pi-type var domain-type codomain-type)))
                          (let ((evidence (formation-evidence "pi-formation" 
                                            (cons domain-type (cons codomain-type nil)) 
                                            result-level)))
                            (let ((final-universe 
                                   (record-computational-evidence universe2 pi-type evidence 
                                                                (pi-type var domain-ast body-ast))))
                              (pair pi-type final-universe))))))))))))))))

;; Identity type formation with computational evidence
(type hott-infer-identity-formation 
      (-> HoTT-AST HoTT-AST HoTT-AST InferenceUniverse (Pair Type InferenceUniverse)))
(define hott-infer-identity-formation
  (fn (type-ast left-ast right-ast universe)
    (let ((type-result (hott-infer type-ast universe)))
      (let ((type-expr (first type-result)))
        (let ((universe1 (second type-result)))
          (let ((left-result (hott-infer left-ast universe1)))
            (let ((left-type (first left-result)))
              (let ((universe2 (second left-result)))
                (let ((right-result (hott-infer right-ast universe2)))
                  (let ((right-type (first right-result)))
                    (let ((universe3 (second right-result)))
                      ;; Generate identity constraints
                      (let ((universe4 (generate-identity-constraint universe3 left-type 
                                                                    type-expr left-ast)))
                        (let ((universe5 (generate-identity-constraint universe4 right-type 
                                                                      type-expr right-ast)))
                          ;; Form identity type with universe level evidence
                          (let ((type-level (type-universe-level type-expr)))
                            (let ((id-type (make-identity-type type-expr sorry sorry)))  ;; Would use actual terms
                              (let ((evidence (formation-evidence "identity-formation" 
                                                (cons type-expr nil) type-level)))
                                (let ((final-universe 
                                       (record-computational-evidence universe5 id-type evidence 
                                                                    (id-type type-ast left-ast right-ast))))
                                  (pair id-type final-universe)))))))))))))))))))))

;; ============================================================================
;; LITERAL AND EFFECT INFERENCE
;; ============================================================================

;; Infer types for literal values
(type hott-infer-literal (-> Value InferenceUniverse (Pair Type InferenceUniverse)))
(define hott-infer-literal
  (fn (value universe)
    (Value-elim (Pair Type InferenceUniverse) value
      ;; Constructor value case
      (fn (name args type)
        (let ((evidence (literal-evidence value)))
          (let ((universe-with-evidence 
                 (with-computational-evidence universe type evidence (literal value))))
            (pair type universe-with-evidence))))
      ;; Other value cases
      (fn (params body env)
        (pair sorry universe))  ;; Function closure
      (fn (name arity type)
        (let ((evidence (literal-evidence value)))
          (let ((universe-with-evidence 
                 (with-computational-evidence universe type evidence (literal value))))
            (pair type universe-with-evidence))))
      ;; Unit value
      (let ((evidence (literal-evidence value)))
        (let ((universe-with-evidence 
               (with-computational-evidence universe unit-type evidence (literal value))))
          (pair unit-type universe-with-evidence)))
      ;; String value
      (fn (str)
        (let ((string-type (inductive-type "String" nil)))
          (let ((evidence (literal-evidence value)))
            (let ((universe-with-evidence 
                   (with-computational-evidence universe string-type evidence (literal value))))
              (pair string-type universe-with-evidence)))))
      ;; Other cases
      (fn (eff) (pair sorry universe))
      (fn (type start end proof) (pair type universe))
      (fn (type-a type-b forward quasi-inverse) (pair sorry universe)))))

;; Infer types for effects
(type hott-infer-effect (-> Effect InferenceUniverse (Pair Type InferenceUniverse)))
(define hott-infer-effect
  (fn (eff universe)
    (Effect-elim (Pair Type InferenceUniverse) eff
      (fn (name arg-type return-type)
        (let ((effect-type (make-effect-type arg-type return-type)))
          (pair effect-type universe))))))

;; Infer constructor types
(type hott-infer-constructor 
      (-> String (List HoTT-AST) InferenceUniverse (Pair Type InferenceUniverse)))
(define hott-infer-constructor
  (fn (constructor-name args universe)
    ;; Look up constructor signature and infer result type
    (let ((constructor-maybe (lookup-constructor constructor-name)))
      (Maybe-elim constructor-maybe
        (pair sorry universe)  ;; Unknown constructor
        (fn (constructor-type)
          (let ((arg-results (infer-list-types args universe)))
            (let ((arg-types (first arg-results)))
              (let ((updated-universe (second arg-results)))
                ;; Apply constructor type to arguments
                (let ((result-type (apply-constructor-type constructor-type arg-types)))
                  (pair result-type updated-universe))))))))))

;; ============================================================================
;; COMPUTATIONAL EVIDENCE AND PROOF OBLIGATIONS
;; ============================================================================

;; Attach computational evidence to inferred type
(type with-computational-evidence 
      (-> InferenceUniverse Type Evidence HoTT-AST InferenceUniverse))
(define with-computational-evidence
  (fn (universe type evidence ast)
    (InferenceUniverse-elim universe
      (fn (level env constraints evidence-ctx tier)
        (EvidenceContext-elim evidence-ctx
          (fn (proofs witnesses obligations)
            (let ((new-proofs (cons (pair type evidence) proofs)))
              (let ((new-evidence-ctx (evidence-context new-proofs witnesses obligations)))
                (inference-universe level env constraints new-evidence-ctx tier)))))))))

;; Record computational evidence for complex type formations
(type record-computational-evidence 
      (-> InferenceUniverse Type Evidence HoTT-AST InferenceUniverse))
(define record-computational-evidence
  (fn (universe type evidence source)
    (InferenceUniverse-elim universe
      (fn (level env constraints evidence-ctx tier)
        (EvidenceContext-elim evidence-ctx
          (fn (proofs witnesses obligations)
            (let ((new-witnesses (cons (triple type evidence source) witnesses)))
              (let ((new-evidence-ctx (evidence-context proofs new-witnesses obligations)))
                (inference-universe level env constraints new-evidence-ctx tier)))))))))

;; Generate identity type constraint (equality proof obligation)
(type generate-identity-constraint 
      (-> InferenceUniverse Type Type HoTT-AST InferenceUniverse))
(define generate-identity-constraint
  (fn (universe type1 type2 source)
    (if (type-equal? type1 type2)
        universe  ;; Types already equal
        ;; Generate proof obligation
        (InferenceUniverse-elim universe
          (fn (level env constraints evidence-ctx tier)
            (let ((id-type (make-identity-type (universe-of-types type1 type2) sorry sorry)))
              (let ((obligation (proof-obligation id-type nothing 
                                                (prove-equal type1 type2) 
                                                (local-context nil))))
                (EvidenceContext-elim evidence-ctx
                  (fn (proofs witnesses obligations)
                    (let ((new-obligations (cons obligation obligations)))
                      (let ((new-evidence-ctx (evidence-context proofs witnesses new-obligations)))
                        (inference-universe level env constraints new-evidence-ctx tier))))))))))))

;; ============================================================================
;; UNIVERSE OPERATIONS AND TYPE VARIABLES
;; ============================================================================

;; Universe-aware variable lookup
(type universe-lookup (-> InferenceUniverse String (Pair Type InferenceUniverse)))
(define universe-lookup
  (fn (universe name)
    (InferenceUniverse-elim universe
      (fn (level env constraints evidence-ctx tier)
        (let ((type-maybe (lookup-type env name)))
          (Maybe-elim type-maybe
            ;; Create fresh type variable in current universe
            (let ((var (fresh-type-variable universe)))
              (let ((new-env (type-env-extend name var env)))
                (let ((new-universe (inference-universe level new-env constraints 
                                                       evidence-ctx tier)))
                  (pair var new-universe))))
            ;; Variable found
            (fn (var-type) (pair var-type universe))))))))

;; Create fresh type variable in universe
(type fresh-type-variable (-> InferenceUniverse Type))
(define fresh-type-variable
  (fn (universe)
    (InferenceUniverse-elim universe
      (fn (level env constraints evidence-ctx tier)
        (let ((var-name (generate-fresh-name "TVar")))
          (let ((type-var (type-variable var-name level nil)))
            (universe level)))))))  ;; Return universe type for now

;; Extend universe with single binding
(type extend-universe-binding (-> InferenceUniverse String Type InferenceUniverse))
(define extend-universe-binding
  (fn (universe var var-type)
    (InferenceUniverse-elim universe
      (fn (level env constraints evidence-ctx tier)
        (let ((new-env (type-env-extend var var-type env)))
          (inference-universe level new-env constraints evidence-ctx tier))))))

;; ============================================================================
;; HoTT-NATIVE AST ELABORATION
;; ============================================================================

;; Elaborate AST with HoTT computational evidence
(type hott-elaborate (-> HoTT-AST InferenceUniverse HoTT-AST))
(define hott-elaborate
  (fn (ast universe)
    ;; Solve identity constraints first
    (let ((solved-universe (solve-identity-constraints universe)))
      ;; Elaborate with computational evidence
      (elaborate-with-evidence ast solved-universe))))

(type elaborate-with-evidence (-> HoTT-AST InferenceUniverse HoTT-AST))
(define elaborate-with-evidence
  (fn (ast universe)
    (hott-ast-eliminator HoTT-AST ast
      ;; Variable case: may have resolved types
      (fn (name)
        (InferenceUniverse-elim universe
          (fn (level env constraints evidence-ctx tier)
            (let ((type-maybe (lookup-type env name)))
              (Maybe-elim type-maybe
                (var name)  ;; No change
                (fn (var-type)
                  ;; Could elaborate with type information
                  (var name)))))))
      
      ;; Application case: recursive elaboration
      (fn (func-ast arg-ast)
        (app (elaborate-with-evidence func-ast universe)
             (elaborate-with-evidence arg-ast universe)))
      
      ;; Other cases: recursive elaboration
      (fn (param body-ast)
        (lambda param (elaborate-with-evidence body-ast universe)))
      
      (fn (var domain-ast codomain-ast)
        (pi-type var (elaborate-with-evidence domain-ast universe)
                     (elaborate-with-evidence codomain-ast universe)))
      
      (fn (var first-ast second-ast)
        (sigma-type var (elaborate-with-evidence first-ast universe)
                        (elaborate-with-evidence second-ast universe)))
      
      (fn (type-ast left-ast right-ast)
        (id-type (elaborate-with-evidence type-ast universe)
                 (elaborate-with-evidence left-ast universe)
                 (elaborate-with-evidence right-ast universe)))
      
      (fn (target cases)
        (eliminator (elaborate-with-evidence target universe)
                   (list-map HoTT-AST HoTT-AST cases 
                     (fn (case) (elaborate-with-evidence case universe)))))
      
      (fn (type-name args)
        (type-app type-name 
          (list-map HoTT-AST HoTT-AST args 
            (fn (arg) (elaborate-with-evidence arg universe)))))
      
      (fn (constructor-name args)
        (constructor constructor-name 
          (list-map HoTT-AST HoTT-AST args 
            (fn (arg) (elaborate-with-evidence arg universe)))))
      
      ;; Literals and effects: unchanged
      (fn (value) (literal value))
      (fn (eff) (effect eff)))))

;; ============================================================================
;; HoTT ELIMINATOR INFERENCE
;; ============================================================================

;; Infer type for pattern matching using HoTT eliminators
(type hott-infer-eliminator 
      (-> HoTT-AST (List HoTT-AST) InferenceUniverse (Pair Type InferenceUniverse)))
(define hott-infer-eliminator
  (fn (scrutinee cases universe)
    (let ((scrutinee-result (hott-infer scrutinee universe)))
      (let ((scrutinee-type (first scrutinee-result)))
        (let ((universe1 (second scrutinee-result)))
          (let ((motive-type (fresh-type-variable universe1)))
            ;; Generate eliminator constraints for each case
            (let ((final-universe (generate-eliminator-constraints cases scrutinee-type 
                                                                 motive-type universe1)))
              (pair motive-type final-universe))))))))

;; Generate constraints for all eliminator cases
(type generate-eliminator-constraints 
      (-> (List HoTT-AST) Type Type InferenceUniverse InferenceUniverse))
(define generate-eliminator-constraints
  (fn (cases scrutinee-type motive-type universe)
    (List-elim cases
      universe  ;; No cases
      (fn (case rest rec)
        (let ((universe-with-case (generate-eliminator-constraint case scrutinee-type 
                                                                motive-type universe)))
          (generate-eliminator-constraints rest scrutinee-type motive-type 
                                         universe-with-case))))))

;; Generate constraint for single eliminator case
(type generate-eliminator-constraint 
      (-> HoTT-AST Type Type InferenceUniverse InferenceUniverse))
(define generate-eliminator-constraint
  (fn (case scrutinee-type motive-type universe)
    ;; Simplified eliminator constraint generation
    universe))

;; ============================================================================
;; UTILITY FUNCTIONS
;; ============================================================================

;; Infer types for list of ASTs
(type infer-list-types 
      (-> (List HoTT-AST) InferenceUniverse (Pair (List Type) InferenceUniverse)))
(define infer-list-types
  (fn (asts universe)
    (List-elim asts
      ;; Empty list
      (pair nil universe)
      ;; Non-empty list
      (fn (head tail rec)
        (let ((head-result (hott-infer head universe)))
          (let ((head-type (first head-result)))
            (let ((universe1 (second head-result)))
              (let ((tail-result (infer-list-types tail universe1)))
                (let ((tail-types (first tail-result)))
                  (let ((final-universe (second tail-result)))
                    (pair (cons head-type tail-types) final-universe)))))))))))

;; Substitute term in type (for dependent types)
(type substitute-in-type (-> Type String HoTT-AST InferenceUniverse Type))
(define substitute-in-type
  (fn (type var term universe)
    ;; Simplified substitution - full implementation would handle all type constructors
    type))

;; Universe of two types (for identity type formation)
(type universe-of-types (-> Type Type Type))
(define universe-of-types
  (fn (type1 type2)
    (let ((level1 (type-universe-level type1)))
      (let ((level2 (type-universe-level type2)))
        (universe (nat-max level1 level2))))))

;; Solve identity constraints using HoTT path computation
(type solve-identity-constraints (-> InferenceUniverse InferenceUniverse))
(define solve-identity-constraints
  (fn (universe)
    (InferenceUniverse-elim universe
      (fn (level env constraints evidence-ctx tier)
        (EvidenceContext-elim evidence-ctx
          (fn (proofs witnesses obligations)
            ;; Process each obligation
            (let ((solved-obligations (attempt-proof-synthesis-for-all obligations universe)))
              (let ((new-evidence-ctx (evidence-context proofs witnesses solved-obligations)))
                (inference-universe level env constraints new-evidence-ctx tier)))))))))

;; Attempt proof synthesis for all obligations
(type attempt-proof-synthesis-for-all 
      (-> (List ProofObligation) InferenceUniverse (List ProofObligation)))
(define attempt-proof-synthesis-for-all
  (fn (obligations universe)
    (list-map ProofObligation ProofObligation obligations 
      (fn (obligation) (attempt-proof-synthesis obligation universe)))))

;; Attempt to synthesize proof for obligation
(type attempt-proof-synthesis (-> ProofObligation InferenceUniverse ProofObligation))
(define attempt-proof-synthesis
  (fn (obligation universe)
    ;; Simplified proof synthesis - real implementation would use HoTT tactics
    obligation))

;; ============================================================================
;; CONSTRUCTOR AND TYPE UTILITIES
;; ============================================================================

;; Instantiate type family with arguments
(type instantiate-type-family (-> TypeFamily (List Type) Type))
(define instantiate-type-family
  (fn (family arg-types)
    (TypeFamily-elim family
      (fn (name arity instantiation instances)
        (instantiation arg-types)))))

;; Apply constructor type to arguments
(type apply-constructor-type (-> Type (List Type) Type))
(define apply-constructor-type
  (fn (constructor-type arg-types)
    ;; Simplified application - would handle dependent constructor types
    constructor-type))

;; Lookup constructor signature - implemented in bootstrap-registry.sexp
;; TODO: Import from types/bootstrap-registry module

;; Generate fresh name
(type generate-fresh-name (-> String String))
(define generate-fresh-name
  (fn (prefix)
    (string-append prefix "0")))  ;; Simplified

;; Natural number literals
(type one Nat)
(define one (succ zero))

(type two Nat)
(define two (succ one))

(type three Nat)
(define three (succ two))

;; Type helper functions
(type make-effect-type (-> Type Type Type))
(define make-effect-type
  (fn (arg-type return-type)
    (effect-type arg-type empty-effect-set empty-effect-set)))

;; Triple constructor
(type triple (-> Type Type Type A B C (Triple A B C)))
(define triple
  (fn (A B C a b c)
    (pair (pair a b) c)))

;; This establishes the pure mathematical HoTT-native inference system for PathFinder