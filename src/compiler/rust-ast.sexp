;; ============================================================================
;; RUST AST REPRESENTATION
;; ============================================================================
;; Data types for representing Rust code that PathFinder will generate
;; This enables PathFinder to compile itself to efficient Rust code

(import types types)
(import core foundations)

;; ============================================================================
;; RUST TYPES
;; ============================================================================

(data RustType U0
  ;; Primitive types
  (case rust-unit RustType)
  (case rust-bool RustType)
  (case rust-usize RustType)
  (case rust-string RustType)
  
  ;; Reference types
  (case rust-ref (-> RustType RustType))
  (case rust-mut-ref (-> RustType RustType))
  (case rust-box (-> RustType RustType))
  
  ;; Complex types
  (case rust-vec (-> RustType RustType))
  (case rust-option (-> RustType RustType))
  (case rust-result (-> RustType RustType RustType))
  
  ;; Named types
  (case rust-named (-> String RustType))
  
  ;; Function types
  (case rust-fn (-> (List RustType) RustType RustType)))

;; ============================================================================
;; RUST PATTERNS
;; ============================================================================

(data RustPattern U0
  ;; Variable pattern
  (case rust-var-pat (-> String RustPattern))
  
  ;; Wildcard pattern
  (case rust-wildcard RustPattern)
  
  ;; Struct pattern
  (case rust-struct-pat (-> String (List (Pair String RustPattern)) RustPattern))
  
  ;; Enum pattern
  (case rust-enum-pat (-> String (List RustPattern) RustPattern))
  
  ;; Tuple pattern
  (case rust-tuple-pat (-> (List RustPattern) RustPattern)))

;; ============================================================================
;; RUST EXPRESSIONS
;; ============================================================================

(data RustExpr U0
  ;; Literals
  (case rust-unit-lit RustExpr)
  (case rust-bool-lit (-> Bool RustExpr))
  (case rust-usize-lit (-> Nat RustExpr))
  (case rust-string-lit (-> String RustExpr))
  
  ;; Variables
  (case rust-var (-> String RustExpr))
  
  ;; Function calls
  (case rust-call (-> RustExpr (List RustExpr) RustExpr))
  (case rust-method-call (-> RustExpr String (List RustExpr) RustExpr))
  
  ;; Struct construction
  (case rust-struct (-> String (List (Pair String RustExpr)) RustExpr))
  
  ;; Enum construction
  (case rust-enum (-> String String (List RustExpr) RustExpr))
  
  ;; Control flow
  (case rust-if (-> RustExpr RustExpr RustExpr RustExpr))
  (case rust-match (-> RustExpr (List RustMatchArm) RustExpr))
  (case rust-block (-> (List RustStmt) RustExpr))
  
  ;; References
  (case rust-ref (-> RustExpr RustExpr))
  (case rust-deref (-> RustExpr RustExpr))
  
  ;; Boxing
  (case rust-box-new (-> RustExpr RustExpr))
  
  ;; Cloning
  (case rust-clone (-> RustExpr RustExpr))
  
  ;; Operators
  (case rust-binop (-> String RustExpr RustExpr RustExpr)))

;; Match arm
(data RustMatchArm U0
  (case rust-match-arm (-> RustPattern RustExpr RustMatchArm)))

;; ============================================================================
;; RUST STATEMENTS
;; ============================================================================

(data RustStmt U0
  ;; Let binding
  (case rust-let (-> RustPattern (Option RustType) RustExpr RustStmt))
  
  ;; Expression statement
  (case rust-expr-stmt (-> RustExpr RustStmt))
  
  ;; Return statement
  (case rust-return (-> RustExpr RustStmt)))

;; ============================================================================
;; RUST ITEMS (TOP-LEVEL DECLARATIONS)
;; ============================================================================

(data RustItem U0
  ;; Function definition
  (case rust-fn-def (-> String 
                        (List (Pair String RustType)) 
                        RustType 
                        (List RustStmt)
                        RustItem))
  
  ;; Struct definition
  (case rust-struct-def (-> String 
                           (List (Pair String RustType)) 
                           RustItem))
  
  ;; Enum definition
  (case rust-enum-def (-> String 
                         (List RustVariant) 
                         RustItem))
  
  ;; Implementation block
  (case rust-impl (-> String 
                      (List RustItem) 
                      RustItem))
  
  ;; Trait definition
  (case rust-trait-def (-> String 
                          (List RustItem) 
                          RustItem))
  
  ;; Use statement
  (case rust-use (-> String RustItem)))

;; Enum variant
(data RustVariant U0
  (case rust-variant (-> String (List RustType) RustVariant)))

;; ============================================================================
;; RUST MODULE
;; ============================================================================

(data RustModule U0
  (case rust-module (-> String 
                       (List RustUse)
                       (List RustItem)
                       RustModule)))

;; Use declarations
(data RustUse U0
  (case rust-use-item (-> String RustUse)))

;; ============================================================================
;; HELPER CONSTRUCTORS
;; ============================================================================

;; Create a simple function
(type make-rust-fn (-> String (List (Pair String RustType)) RustType RustExpr RustItem))
(define make-rust-fn
  (fn (name params ret-type body)
    (rust-fn-def name params ret-type 
                 (list (rust-return body)))))

;; Create a match expression
(type make-rust-match (-> RustExpr (List (Pair RustPattern RustExpr)) RustExpr))
(define make-rust-match
  (fn (expr arms)
    (rust-match expr 
                (list-map (fn (arm) 
                            (rust-match-arm (fst arm) (snd arm)))
                          arms))))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export RustType)
(export RustPattern)
(export RustExpr)
(export RustStmt)
(export RustItem)
(export RustModule)
(export RustMatchArm)
(export RustVariant)
(export RustUse)

(export make-rust-fn)
(export make-rust-match)