;; ============================================================================
;; PURE MATHEMATICAL HOTT FOUNDATIONS (S-EXPRESSION VERSION)
;; ============================================================================
;; This is the foundational layer - no dependencies
;; Uses clean s-expression syntax without LISP legacy

;; Universe hierarchy
(define U0 (Universe 0))
(define U1 (Universe 1))
(define U2 (Universe 2))
(define U3 (Universe 3))

;; Identity type (central to HoTT)
(type Id (-> Type0 (-> Value Value Type0)))
(define Id 
  (fn (A) 
    (fn (x y) 
      (IdType A x y))))

;; Basic inductive types
(data Nat U0
  (case zero Nat)
  (case succ (-> Nat Nat)))

(data Bool U0
  (case false Bool)
  (case true Bool))

(data Unit U0
  (case unit Unit))

(data Empty U0)
  ;; no constructors

;; HoTT value hierarchy  
(data Value U1
  (case constructor-value (-> String (List Value) Type Value))
  (case closure-value (-> (List String) AST Environment Value))
  (case builtin-value (-> String Nat Type Value))
  (case unit-value Value)
  (case path-value (-> Type Value Value Proof Value))
  (case string-value (-> String Value))
  (case effect-value (-> Effect Value)))

;; HoTT type hierarchy
(data Type U1
  (case universe (-> Nat Type))
  (case pi-type (-> String Type Type Type))
  (case sigma-type (-> String Type Type Type))
  (case sum-type (-> Type Type Type))
  (case id-type (-> Type Value Value Type))
  (case unit-type Type)
  (case empty-type Type)
  (case inductive-type (-> String (List Constructor) Type))
  (case effect-type (-> Type (List String) (List String) Type)))

;; AST representation
(data AST U0
  (case var (-> String AST))
  (case app (-> AST AST AST))
  (case lambda (-> String Type AST AST))
  (case pi (-> String AST AST AST))
  (case sigma (-> String AST AST AST))
  (case pair (-> AST AST AST))
  (case let-expr (-> String AST AST AST))
  (case constructor (-> String (List AST) AST))
  (case case-split (-> AST (List CaseBranch) AST)))

;; Environment for evaluation
(data Environment U0
  (case empty-env Environment)
  (case extend-env (-> String Value Environment Environment)))

;; Constructor for inductive types
(data Constructor U0
  (case make-constructor (-> String (List Type) String Constructor)))

;; Effect representation
(data Effect U0
  (case make-effect (-> String (List Value) Bool Effect)))

;; Proof objects for path types
(data Proof U0
  (case refl-proof (-> Value Proof))
  (case path-comp (-> Proof Proof Proof))
  (case path-inv (-> Proof Proof)))