;; ============================================================================
;; PATHFINDER MLIR DIALECT DEFINITION
;; ============================================================================
;; Defines PathFinder types and operations in MLIR with evidence preservation

(import compiler.ir core)
(import types types)
(import effects computation-as-effect)

;; ============================================================================
;; MLIR TYPE DEFINITIONS
;; ============================================================================

(data MLIRType U0
  ;; Basic PathFinder types
  (case mlir-nat MLIRType)
  (case mlir-bool MLIRType)
  (case mlir-string MLIRType)
  (case mlir-unit MLIRType)
  
  ;; Type universe
  (case mlir-type (-> (level : Nat) MLIRType))
  
  ;; Function types
  (case mlir-closure (-> (param-types : List MLIRType) 
                        (result-type : MLIRType) 
                        MLIRType))
  
  ;; Evidence container
  (case mlir-evidence (-> (kind : EvidenceKind) MLIRType))
  
  ;; Identity types for HoTT
  (case mlir-path (-> (type : MLIRType) (a : MLIRValue) (b : MLIRValue) MLIRType))
  
  ;; Inductive types
  (case mlir-inductive (-> (name : String) (params : List MLIRType) MLIRType)))

;; ============================================================================
;; MLIR OPERATION DEFINITIONS
;; ============================================================================

(data MLIROp U0
  ;; Constants
  (case mlir-const-nat (-> (value : Nat) (result : MLIRValue) MLIROp))
  (case mlir-const-bool (-> (value : Bool) (result : MLIRValue) MLIROp))
  (case mlir-const-string (-> (value : String) (result : MLIRValue) MLIROp))
  
  ;; Constructors
  (case mlir-constructor (-> (name : String) 
                           (args : List MLIRValue)
                           (result : MLIRValue)
                           (evidence : MLIRAttribute)
                           MLIROp))
  
  ;; Eliminators with evidence
  (case mlir-nat-elim (-> (motive : MLIRValue)
                         (base : MLIRValue)
                         (step : MLIRValue)
                         (target : MLIRValue)
                         (result : MLIRValue)
                         (evidence : MLIRAttribute)
                         MLIROp))
  
  (case mlir-bool-elim (-> (motive : MLIRValue)
                          (false-case : MLIRValue)
                          (true-case : MLIRValue)
                          (target : MLIRValue)
                          (result : MLIRValue)
                          (evidence : MLIRAttribute)
                          MLIROp))
  
  ;; Function operations
  (case mlir-apply (-> (func : MLIRValue)
                      (arg : MLIRValue)
                      (result : MLIRValue)
                      (evidence : MLIRAttribute)
                      MLIROp))
  
  (case mlir-closure-create (-> (captures : List MLIRValue)
                               (body : MLIRRegion)
                               (result : MLIRValue)
                               (evidence : MLIRAttribute)
                               MLIROp))
  
  ;; Effects
  (case mlir-perform (-> (effect : MLIREffect)
                        (result : MLIRValue)
                        (evidence : MLIRAttribute)
                        MLIROp))
  
  ;; Runtime-at-compile-time operations
  (case mlir-compute-at-compile-time (-> (computation : MLIRRegion)
                                        (cache-key : String)
                                        (result : MLIRValue)
                                        MLIROp))
  
  (case mlir-cache-lookup (-> (key : String)
                             (result : MLIRValue)
                             (found : MLIRValue)
                             MLIROp))
  
  (case mlir-cache-store (-> (key : String)
                            (value : MLIRValue)
                            MLIROp))
  
  ;; Control flow
  (case mlir-branch (-> (target : MLIRBlock) MLIROp))
  (case mlir-cond-branch (-> (condition : MLIRValue)
                            (true-block : MLIRBlock)
                            (false-block : MLIRBlock)
                            MLIROp))
  (case mlir-return (-> (values : List MLIRValue) MLIROp)))

;; ============================================================================
;; MLIR ATTRIBUTES (EVIDENCE)
;; ============================================================================

(data MLIRAttribute U0
  ;; Termination evidence
  (case mlir-termination (-> (steps : Nat) MLIRAttribute))
  (case mlir-structural-termination (-> (measure : Nat) MLIRAttribute))
  
  ;; Complexity evidence
  (case mlir-constant-complexity (-> (ops : Nat) MLIRAttribute))
  (case mlir-linear-complexity (-> (factor : Nat) MLIRAttribute))
  (case mlir-polynomial-complexity (-> (degree : Nat) MLIRAttribute))
  
  ;; Purity evidence
  (case mlir-pure (-> (deterministic : Bool) MLIRAttribute))
  (case mlir-effect-free MLIRAttribute)
  
  ;; Space evidence
  (case mlir-constant-space (-> (size : Nat) MLIRAttribute))
  (case mlir-linear-space (-> (factor : Nat) MLIRAttribute))
  
  ;; Composite evidence
  (case mlir-evidence-bundle (-> (termination : MLIRAttribute)
                                (complexity : MLIRAttribute)
                                (purity : MLIRAttribute)
                                (space : MLIRAttribute)
                                MLIRAttribute))
  
  ;; Cache evidence
  (case mlir-cached-value (-> (hash : String) (value : MLIRLiteral) MLIRAttribute)))

;; ============================================================================
;; MLIR EFFECTS
;; ============================================================================

(data MLIREffect U0
  ;; I/O effects
  (case mlir-print-effect (-> (message : MLIRValue) MLIREffect))
  (case mlir-read-effect (-> (path : MLIRValue) MLIREffect))
  (case mlir-write-effect (-> (path : MLIRValue) (content : MLIRValue) MLIREffect))
  
  ;; Computation effect (for general effects)
  (case mlir-computation-effect (-> (desc : MLIRValue) MLIREffect)))

;; ============================================================================
;; MLIR STRUCTURE
;; ============================================================================

(data MLIRModule U0
  (case mlir-module (-> (name : String)
                       (attributes : List MLIRAttribute)
                       (functions : List MLIRFunction)
                       (globals : List MLIRGlobal)
                       MLIRModule)))

(data MLIRFunction U0
  (case mlir-function (-> (name : String)
                         (type : MLIRType)
                         (attributes : List MLIRAttribute)
                         (body : MLIRRegion)
                         MLIRFunction)))

(data MLIRGlobal U0
  (case mlir-global (-> (name : String)
                       (type : MLIRType)
                       (value : MLIRLiteral)
                       (attributes : List MLIRAttribute)
                       MLIRGlobal)))

(data MLIRRegion U0
  (case mlir-region (-> (blocks : List MLIRBlock) MLIRRegion)))

(data MLIRBlock U0
  (case mlir-block (-> (label : String)
                      (args : List (Pair String MLIRType))
                      (ops : List MLIROp)
                      MLIRBlock)))

(data MLIRValue U0
  ;; SSA value
  (case mlir-ssa-value (-> (name : String) MLIRValue))
  ;; Block argument
  (case mlir-block-arg (-> (name : String) (index : Nat) MLIRValue))
  ;; Literal value
  (case mlir-literal (-> (lit : MLIRLiteral) MLIRValue)))

(data MLIRLiteral U0
  (case mlir-nat-lit (-> Nat MLIRLiteral))
  (case mlir-bool-lit (-> Bool MLIRLiteral))
  (case mlir-string-lit (-> String MLIRLiteral))
  (case mlir-unit-lit MLIRLiteral))

(data EvidenceKind U0
  (case termination-evidence-kind EvidenceKind)
  (case complexity-evidence-kind EvidenceKind)
  (case space-evidence-kind EvidenceKind))

;; ============================================================================
;; DIALECT METADATA
;; ============================================================================

(define pathfinder-dialect-name "pathfinder")

(define pathfinder-dialect-namespace "pf")

;; Type names in MLIR syntax
(define mlir-type-syntax
  (fn (ty)
    (match ty
      (case mlir-nat "!pf.nat")
      (case mlir-bool "!pf.bool")
      (case mlir-string "!pf.string")
      (case mlir-unit "!pf.unit")
      (case (mlir-type level)
        (string-append "!pf.type<" (string-append (nat-to-string level) ">")))
      (case (mlir-closure params result)
        (string-append "!pf.closure<(" 
          (string-append (string-join ", " (map mlir-type-syntax params))
            (string-append ") -> " (string-append (mlir-type-syntax result) ">")))))
      (case (mlir-evidence kind)
        (string-append "!pf.evidence<" (string-append (evidence-kind-name kind) ">")))
      (case _ "!pf.unknown"))))

;; Operation names in MLIR syntax
(define mlir-op-syntax
  (fn (op)
    (match op
      (case (mlir-const-nat _ _) "pf.const_nat")
      (case (mlir-const-bool _ _) "pf.const_bool")
      (case (mlir-const-string _ _) "pf.const_string")
      (case (mlir-constructor _ _ _ _) "pf.constructor")
      (case (mlir-nat-elim _ _ _ _ _ _) "pf.nat_elim")
      (case (mlir-bool-elim _ _ _ _ _ _) "pf.bool_elim")
      (case (mlir-apply _ _ _ _) "pf.apply")
      (case (mlir-closure-create _ _ _ _) "pf.closure")
      (case (mlir-perform _ _ _) "pf.perform")
      (case (mlir-compute-at-compile-time _ _ _) "pf.compute_at_compile_time")
      (case (mlir-cache-lookup _ _ _) "pf.cache_lookup")
      (case (mlir-cache-store _ _) "pf.cache_store")
      (case _ "pf.unknown"))))

;; Attribute syntax
(define mlir-attribute-syntax
  (fn (attr)
    (match attr
      (case (mlir-termination steps)
        (string-append "#pf.termination<steps: " (string-append (nat-to-string steps) ">")))
      (case (mlir-constant-complexity ops)
        (string-append "#pf.complexity<constant: " (string-append (nat-to-string ops) ">")))
      (case (mlir-pure det)
        (string-append "#pf.pure<deterministic: " (string-append (bool-to-string det) ">")))
      (case mlir-effect-free
        "#pf.effect_free")
      (case (mlir-cached-value hash value)
        (string-append "#pf.cached<" (string-append hash ">")))
      (case _ "#pf.unknown"))))

;; ============================================================================
;; HELPERS
;; ============================================================================

(define evidence-kind-name
  (fn (kind)
    (match kind
      (case termination-evidence-kind "termination")
      (case complexity-evidence-kind "complexity")
      (case space-evidence-kind "space"))))

(define nat-to-string (fn (n) "n"))
(define bool-to-string (fn (b) (if b "true" "false")))
(define string-join (fn (sep lst) "joined"))
(define map (fn (f lst)
  (match lst
    (case nil nil)
    (case (cons x xs) (cons (f x) (map f xs))))))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export MLIRType)
(export MLIROp)
(export MLIRAttribute)
(export MLIREffect)
(export MLIRModule)
(export MLIRFunction)
(export MLIRRegion)
(export MLIRBlock)
(export MLIRValue)
(export MLIRLiteral)
(export pathfinder-dialect-name)
(export pathfinder-dialect-namespace)
(export mlir-type-syntax)
(export mlir-op-syntax)
(export mlir-attribute-syntax)