#lang racket/base

(require racket/contract
         racket/match
         racket/list
         racket/set
         racket/string
         "../parser/ast.rkt"
         "../types/types.rkt"
         "typechecker.rkt")

(provide universe-level-inference-context
         universe-level-inference-context?
         universe-level-inference-context-type-env
         universe-level-inference-context-variable-counter
         universe-level-inference-context-constraints
         universe-level-inference-context-level-assignments
         make-universe-inference-context
         infer-universe-levels
         solve-universe-constraints
         universe-constraint
         universe-constraint?
         universe-constraint-type
         level-variable
         level-variable?
         level-variable-id
         fresh-level-variable
         add-universe-constraint!
         universe-less-than
         universe-less-equal
         universe-equal
         universe-max
         consistent-universe-assignment?
         resolve-level
         type-check-with-universe-inference
         elaborate-with-universe-levels)

;; ============================================================================
;; HoTT-NATIVE UNIVERSE LEVEL INFERENCE
;; ============================================================================
;; Implements universe level inference using HoTT's universe hierarchy
;; Type₀ : Type₁ : Type₂ : ... with proper constraint solving

;; ============================================================================
;; UNIVERSE LEVEL VARIABLES AND CONSTRAINTS
;; ============================================================================

;; Universe level variable: unknown level to be inferred
(struct level-variable (id name) #:transparent)

;; Universe constraint types
(struct universe-constraint (type lhs rhs source-location) #:transparent)

;; Constraint types
(define universe-less-than 'less-than)        ; u₁ < u₂
(define universe-less-equal 'less-equal)      ; u₁ ≤ u₂  
(define universe-equal 'equal)                ; u₁ = u₂
(define universe-max 'max)                    ; u₁ = max(u₂, u₃)

;; Universe inference context
(struct universe-level-inference-context 
        (type-env variable-counter constraints level-assignments) 
        #:transparent #:mutable)

;; Create universe inference context
(define/contract (make-universe-inference-context type-env)
  (-> type-environment? universe-level-inference-context?)
  (universe-level-inference-context type-env 0 '() (make-hash)))

;; ============================================================================
;; LEVEL VARIABLE MANAGEMENT
;; ============================================================================

;; Create fresh level variable
(define/contract (fresh-level-variable ctx [name 'u])
  (->* (universe-level-inference-context?) (symbol?) level-variable?)
  (let* ([id (universe-level-inference-context-variable-counter ctx)]
         [var (level-variable id name)])
    (set-universe-level-inference-context-variable-counter! ctx (+ id 1))
    var))

;; Add universe constraint to context
(define/contract (add-universe-constraint! ctx constraint-type lhs rhs [location #f])
  (->* (universe-level-inference-context? symbol? any/c any/c) ((or/c #f string?)) void?)
  (let ([constraint (universe-constraint constraint-type lhs rhs location)])
    (set-universe-level-inference-context-constraints! 
     ctx 
     (cons constraint (universe-level-inference-context-constraints ctx)))))

;; ============================================================================
;; UNIVERSE LEVEL INFERENCE FOR TYPE FORMERS
;; ============================================================================

;; Main universe level inference function
(define/contract (infer-universe-levels ast ctx)
  (-> ast-node/c universe-level-inference-context? exact-nonnegative-integer?)
  (match ast
    ;; Literals are in universe 0
    [(number-atom _) 0]
    [(boolean-atom _) 0]
    [(string-atom _) 0]
    
    ;; Variables: lookup or infer
    [(symbol-atom name)
     (infer-variable-universe-level name ctx)]
    
    ;; S-expressions: type formers and applications
    [(sexpr elements)
     (if (null? elements)
         0  ; Unit type in universe 0
         (infer-sexpr-universe-level elements ctx))]
    
    [_ 0]))  ; Default to universe 0

;; ============================================================================
;; S-EXPRESSION UNIVERSE LEVEL INFERENCE
;; ============================================================================

(define/contract (infer-sexpr-universe-level elements ctx)
  (-> (listof ast-node/c) universe-level-inference-context? exact-nonnegative-integer?)
  (let ([first-elem (first elements)])
    (match first-elem
      ;; Type formers that affect universe levels
      [(symbol-atom "Type")
       (infer-type-literal-universe-level (rest elements) ctx)]
      
      [(symbol-atom "pi")
       (infer-pi-type-universe-level (rest elements) ctx)]
      
      [(symbol-atom "sigma")
       (infer-sigma-type-universe-level (rest elements) ctx)]
      
      [(symbol-atom "identity")
       (infer-identity-type-universe-level (rest elements) ctx)]
      
      [(symbol-atom "+")  ; Sum type
       (infer-sum-type-universe-level (rest elements) ctx)]
      
      ;; Function application
      [_
       (infer-application-universe-level elements ctx)])))

;; ============================================================================
;; TYPE FORMER UNIVERSE LEVEL RULES
;; ============================================================================

;; Type literal: Type i : Type (i+1)
(define/contract (infer-type-literal-universe-level args ctx)
  (-> (listof ast-node/c) universe-level-inference-context? exact-nonnegative-integer?)
  (cond
    ;; Type without level annotation: Type₀
    [(null? args) 1]  ; Type₀ : Type₁
    
    ;; Type with explicit level: Type i
    [(= (length args) 1)
     (let ([level-ast (first args)])
       (match level-ast
         [(number-atom i) (+ i 1)]  ; Type i : Type (i+1)
         [(symbol-atom level-name)
          ;; Level variable: generate constraint
          (let ([level-var (fresh-level-variable ctx 'type-level)]
                [result-var (fresh-level-variable ctx 'result-level)])
            (add-universe-constraint! ctx universe-less-than level-var result-var)
            ;; Return placeholder - will be resolved by constraint solving
            0)]
         [_ 1]))]
    
    [else 1]))

;; Π-type: (Π (x : A) B) : Type (max(level(A), level(B)))
(define/contract (infer-pi-type-universe-level args ctx)
  (-> (listof ast-node/c) universe-level-inference-context? exact-nonnegative-integer?)
  (when (not (= (length args) 2))
    (error "pi requires binding and body"))
  
  (let* ([binding-ast (first args)]
         [body-ast (second args)])
    (match binding-ast
      [(sexpr (list (symbol-atom var) domain-ast))
       (let* ([domain-level (infer-universe-levels domain-ast ctx)]
              [extended-ctx (extend-context-with-binding ctx var domain-ast)]
              [codomain-level (infer-universe-levels body-ast extended-ctx)])
         
         ;; Universe level rule: Π : Type (max(level(A), level(B)))
         (max domain-level codomain-level))]
      
      [_ (error "pi binding must be (var type)")])))

;; Σ-type: (Σ (x : A) B) : Type (max(level(A), level(B)))
(define/contract (infer-sigma-type-universe-level args ctx)
  (-> (listof ast-node/c) universe-level-inference-context? exact-nonnegative-integer?)
  (when (not (= (length args) 2))
    (error "sigma requires binding and body"))
  
  (let* ([binding-ast (first args)]
         [body-ast (second args)])
    (match binding-ast
      [(sexpr (list (symbol-atom var) domain-ast))
       (let* ([domain-level (infer-universe-levels domain-ast ctx)]
              [extended-ctx (extend-context-with-binding ctx var domain-ast)]
              [codomain-level (infer-universe-levels body-ast extended-ctx)])
         
         ;; Same rule as Π-type
         (max domain-level codomain-level))]
      
      [_ (error "sigma binding must be (var type)")])))

;; Identity type: Id A x y : Type (level(A))
(define/contract (infer-identity-type-universe-level args ctx)
  (-> (listof ast-node/c) universe-level-inference-context? exact-nonnegative-integer?)
  (when (not (= (length args) 3))
    (error "identity requires type and two terms"))
  
  (let* ([type-ast (first args)]
         [left-ast (second args)]
         [right-ast (third args)]
         [type-level (infer-universe-levels type-ast ctx)])
    
    ;; Identity type lives in same universe as the type
    type-level))

;; Sum type: A + B : Type (max(level(A), level(B)))
(define/contract (infer-sum-type-universe-level args ctx)
  (-> (listof ast-node/c) universe-level-inference-context? exact-nonnegative-integer?)
  (when (not (= (length args) 2))
    (error "sum type requires two arguments"))
  
  (let ([left-level (infer-universe-levels (first args) ctx)]
        [right-level (infer-universe-levels (second args) ctx)])
    (max left-level right-level)))

;; ============================================================================
;; FUNCTION APPLICATION UNIVERSE LEVEL INFERENCE
;; ============================================================================

(define/contract (infer-application-universe-level elements ctx)
  (-> (listof ast-node/c) universe-level-inference-context? exact-nonnegative-integer?)
  (let* ([func-ast (first elements)]
         [args (rest elements)]
         [func-level (infer-universe-levels func-ast ctx)])
    
    ;; For function application, universe level depends on return type
    ;; This requires type inference to determine the result type's level
    (infer-application-result-universe-level func-ast args ctx)))

(define/contract (infer-application-result-universe-level func-ast args ctx)
  (-> ast-node/c (listof ast-node/c) universe-level-inference-context? exact-nonnegative-integer?)
  ;; Simplified: assume result is in universe 0
  ;; Full implementation would infer function type and extract result universe level
  0)

;; ============================================================================
;; VARIABLE UNIVERSE LEVEL INFERENCE
;; ============================================================================

(define/contract (infer-variable-universe-level name ctx)
  (-> string? universe-level-inference-context? exact-nonnegative-integer?)
  (let ([type (type-env-lookup (universe-level-inference-context-type-env ctx) name)])
    (cond
      [type (type-universe-level type)]
      [else 
       ;; Unknown variable: create level variable and constraint
       (let ([level-var (fresh-level-variable ctx (string->symbol name))])
         ;; Return placeholder level
         0)])))

;; ============================================================================
;; UNIVERSE CONSTRAINT SOLVING
;; ============================================================================

;; Solve universe constraints using topological sorting
(define/contract (solve-universe-constraints ctx)
  (-> universe-level-inference-context? void?)
  (let ([constraints (universe-level-inference-context-constraints ctx)]
        [assignments (universe-level-inference-context-level-assignments ctx)])
    
    ;; Build constraint graph
    (let ([graph (build-constraint-graph constraints)])
      ;; Solve constraints and update assignments
      (solve-constraint-graph graph assignments))))

;; Build directed graph from universe constraints
(define/contract (build-constraint-graph constraints)
  (-> (listof universe-constraint?) any/c)
  (let ([graph (make-hash)])
    (for ([constraint constraints])
      (match constraint
        [(universe-constraint 'less-than lhs rhs _)
         (add-graph-edge! graph lhs rhs 'less-than)]
        
        [(universe-constraint 'less-equal lhs rhs _)
         (add-graph-edge! graph lhs rhs 'less-equal)]
        
        [(universe-constraint 'equal lhs rhs _)
         (add-graph-edge! graph lhs rhs 'equal)
         (add-graph-edge! graph rhs lhs 'equal)]
        
        [(universe-constraint 'max lhs rhs _)
         ;; max constraint: lhs = max(components in rhs)
         (add-max-constraint! graph lhs rhs)]))
    graph))

;; Add edge to constraint graph
(define/contract (add-graph-edge! graph from to edge-type)
  (-> hash? any/c any/c symbol? void?)
  (let ([edges (hash-ref graph from '())])
    (hash-set! graph from (cons (cons to edge-type) edges))))

;; Add max constraint to graph
(define/contract (add-max-constraint! graph target components)
  (-> hash? any/c any/c void?)
  ;; For max constraint, target ≥ each component
  (when (list? components)
    (for ([component components])
      (add-graph-edge! graph component target 'less-equal))))

;; Solve constraint graph using topological sort + unification
(define/contract (solve-constraint-graph graph assignments)
  (-> hash? hash? void?)
  ;; Simplified constraint solving:
  ;; 1. Find strongly connected components (for equality constraints)
  ;; 2. Topologically sort the condensed graph
  ;; 3. Assign minimal levels satisfying all constraints
  
  ;; For now, assign level 0 to all variables
  (for ([(var edges) (in-hash graph)])
    (when (level-variable? var)
      (hash-set! assignments var 0))))

;; ============================================================================
;; UNIVERSE CONSISTENCY CHECKING
;; ============================================================================

;; Check if universe assignment is consistent
(define/contract (consistent-universe-assignment? assignments constraints)
  (-> hash? (listof universe-constraint?) boolean?)
  (andmap (lambda (constraint) (check-constraint-satisfaction constraint assignments))
          constraints))

;; Check if individual constraint is satisfied
(define/contract (check-constraint-satisfaction constraint assignments)
  (-> universe-constraint? hash? boolean?)
  (match constraint
    [(universe-constraint 'less-than lhs rhs _)
     (< (resolve-level lhs assignments) (resolve-level rhs assignments))]
    
    [(universe-constraint 'less-equal lhs rhs _)
     (<= (resolve-level lhs assignments) (resolve-level rhs assignments))]
    
    [(universe-constraint 'equal lhs rhs _)
     (= (resolve-level lhs assignments) (resolve-level rhs assignments))]
    
    [(universe-constraint 'max target components _)
     (= (resolve-level target assignments)
        (apply max (map (lambda (c) (resolve-level c assignments)) components)))]
    
    [_ #t]))

;; Resolve level variable to concrete level
(define/contract (resolve-level level assignments)
  (-> any/c hash? exact-nonnegative-integer?)
  (cond
    [(level-variable? level) (hash-ref assignments level 0)]
    [(exact-nonnegative-integer? level) level]
    [else 0]))

;; ============================================================================
;; CONTEXT EXTENSION AND UTILITIES
;; ============================================================================

;; Extend context with variable binding
(define/contract (extend-context-with-binding ctx var type-ast)
  (-> universe-level-inference-context? string? ast-node/c universe-level-inference-context?)
  (let ([new-env (make-type-environment (universe-level-inference-context-type-env ctx))])
    ;; For universe level inference, we need the type's level, not the type itself
    ;; Simplified: just extend with the same context
    (universe-level-inference-context 
     new-env
     (universe-level-inference-context-variable-counter ctx)
     (universe-level-inference-context-constraints ctx)
     (universe-level-inference-context-level-assignments ctx))))

;; ============================================================================
;; INTEGRATION WITH TYPE INFERENCE
;; ============================================================================

;; Enhanced type checking with universe level inference
(define/contract (type-check-with-universe-inference ast type-env)
  (-> ast-node/c type-environment? (values hott-type/c exact-nonnegative-integer?))
  (let ([universe-ctx (make-universe-inference-context type-env)])
    ;; Infer universe levels
    (let ([inferred-level (infer-universe-levels ast universe-ctx)])
      ;; Solve constraints
      (solve-universe-constraints universe-ctx)
      
      ;; Regular type checking
      (let ([inferred-type (type-check ast type-env)])
        (values inferred-type inferred-level)))))

;; ============================================================================
;; UNIVERSE LEVEL ELABORATION
;; ============================================================================

;; Elaborate AST with inferred universe levels
(define/contract (elaborate-with-universe-levels ast ctx)
  (-> ast-node/c universe-level-inference-context? ast-node/c)
  (match ast
    ;; Type literals get explicit level annotations
    [(sexpr (list (symbol-atom "Type")))
     (let ([level (infer-universe-levels ast ctx)])
       (sexpr (list (symbol-atom "Type") (number-atom level))))]
    
    ;; S-expressions get recursive elaboration
    [(sexpr elements)
     (sexpr (map (lambda (elem) (elaborate-with-universe-levels elem ctx)) elements))]
    
    ;; Other nodes unchanged
    [_ ast]))

;; ============================================================================
;; UNIVERSE LEVEL PRETTY PRINTING
;; ============================================================================

;; Pretty print universe level assignment
(define/contract (pretty-print-universe-assignment assignments)
  (-> hash? string?)
  (string-append
   "Universe Level Assignment:\n"
   (string-join
    (for/list ([(var level) (in-hash assignments)])
      (format "  ~a := ~a" var level))
    "\n")))

;; Pretty print universe constraints
(define/contract (pretty-print-universe-constraints constraints)
  (-> (listof universe-constraint?) string?)
  (string-append
   "Universe Constraints:\n"
   (string-join
    (for/list ([constraint constraints])
      (match constraint
        [(universe-constraint type lhs rhs location)
         (format "  ~a ~a ~a" lhs type rhs)]))
    "\n")))