#lang racket

;; ============================================================================
;; HoTT-NATIVE BIDIRECTIONAL INFERENCE INTEGRATION
;; ============================================================================
;; Integrates all HoTT-native inference components:
;; - HoTT-native bidirectional foundation (hott-native-inference.rkt)
;; - Type family parameter inference (type-family-inference.rkt) 
;; - Universe level inference (universe-level-inference.rkt)
;; - Proof obligation generation and AST elaboration

(require "hott-native-inference.rkt"
         "type-family-inference.rkt"
         "universe-level-inference.rkt"
         "typechecker.rkt"
         "../parser/ast.rkt"
         "../types/types.rkt"
         "../types/type-families.rkt"
         "../evaluator/values.rkt"
         racket/contract
         racket/match)

(provide make-bidirectional-inference-context
         bidirectional-inference-context?
         elaborate-ast-with-inference
         generate-proof-obligations
         complete-ast-elaboration
         infer-and-elaborate
         check-and-elaborate
         bidirectional-inference-context-hott
         bidirectional-inference-context-type-family
         bidirectional-inference-context-universe
         elaborated-ast-node
         elaborated-ast-node?
         elaborated-ast-node-original
         elaborated-ast-node-inferred-type
         elaborated-ast-node-universe-level
         elaborated-ast-node-proof-obligations
         elaborated-ast-node-type-family-params
         is-type-family-constructor?)

;; ============================================================================
;; CONTRACTS
;; ============================================================================

(define ast-node/c
  (or/c number-atom? boolean-atom? string-atom? symbol-atom? sexpr?))

;; ============================================================================
;; INTEGRATED INFERENCE CONTEXT  
;; ============================================================================

;; Unified context that combines all inference systems
(struct bidirectional-inference-context (hott type-family universe) #:transparent)

;; Elaborated AST node with complete inference information
(struct elaborated-ast-node (original inferred-type universe-level proof-obligations type-family-params) #:transparent)

;; Create integrated inference context
(define/contract (make-bidirectional-inference-context type-env)
  (-> type-environment? bidirectional-inference-context?)
  (let ([hott-ctx (make-inference-universe 0 type-env)]
        [type-family-ctx (make-tf-inference-context type-env)]
        [universe-ctx (make-universe-inference-context type-env)])
    (bidirectional-inference-context hott-ctx type-family-ctx universe-ctx)))

;; ============================================================================
;; PROOF OBLIGATION GENERATION
;; ============================================================================

;; Generate proof obligations for AST node and its subterms
(define/contract (generate-proof-obligations ast ctx)
  (-> ast-node/c bidirectional-inference-context? (listof proof-obligation?))
  (let ([hott-ctx (bidirectional-inference-context-hott ctx)])
    (match ast
      ;; Literals have no proof obligations
      [(number-atom _) '()]
      [(boolean-atom _) '()]
      [(string-atom _) '()]
      
      ;; Variables may need dependent type proof obligations
      [(symbol-atom name)
       (generate-variable-obligations name hott-ctx)]
      
      ;; S-expressions: complex proof obligations
      [(sexpr elements)
       (if (null? elements)
           '()
           (append (generate-application-obligations elements hott-ctx)
                   (apply append (map (lambda (elem)
                                      (generate-proof-obligations elem ctx))
                                    elements))))])))

;; Generate obligations for variable lookup
(define/contract (generate-variable-obligations name hott-ctx)
  (-> string? inference-universe? (listof proof-obligation?))
  (let* ([type-env (inference-universe-type-env hott-ctx)]
         [var-type (type-env-lookup type-env name)])
    (if var-type
        ;; Variable exists - may need dependent type obligations
        (if (pi-type? var-type)
            ;; Dependent type - generate proof obligations for dependencies
            (list (proof-obligation var-type #f 
                                   `(prove-dependencies ,name ,var-type)
                                   hott-ctx))
            '())
        ;; Variable doesn't exist - obligation to prove it exists
        (list (proof-obligation Type0 #f
                               `(prove-exists ,name)
                               hott-ctx)))))

;; Generate obligations for function application
(define/contract (generate-application-obligations elements hott-ctx)
  (-> (listof ast-node/c) inference-universe? (listof proof-obligation?))
  (when (null? elements)
    (error "generate-application-obligations: empty elements list"))
  
  (let ([func-ast (first elements)]
        [args (rest elements)])
    ;; Function application needs:
    ;; 1. Proof that function exists and has correct type
    ;; 2. Proof that arguments satisfy dependent constraints
    ;; 3. Proof that result type is well-formed
    (list (proof-obligation Type0 #f
                           `(prove-application ,func-ast ,@args)
                           hott-ctx))))

;; ============================================================================
;; AST ELABORATION WITH INFERENCE
;; ============================================================================

;; Elaborate AST node with complete inference information
(define/contract (elaborate-ast-with-inference ast ctx)
  (-> ast-node/c bidirectional-inference-context? elaborated-ast-node?)
  (let* ([hott-ctx (bidirectional-inference-context-hott ctx)]
         [type-family-ctx (bidirectional-inference-context-type-family ctx)]
         [universe-ctx (bidirectional-inference-context-universe ctx)]
         
         ;; Infer type using HoTT-native inference
         [inferred-type (hott-infer ast hott-ctx)]
         
         ;; Infer universe level
         [universe-level (infer-universe-levels ast universe-ctx)]
         
         ;; Generate proof obligations
         [proof-obligations (generate-proof-obligations ast ctx)]
         
         ;; Infer type family parameters if applicable
         [type-family-params (infer-type-family-parameters-for-ast ast type-family-ctx)])
    
    (elaborated-ast-node ast inferred-type universe-level proof-obligations type-family-params)))

;; Infer type family parameters for AST node if it uses type families
(define/contract (infer-type-family-parameters-for-ast ast ctx)
  (-> ast-node/c type-family-inference-context? (or/c (listof hott-type/c) #f))
  (match ast
    [(sexpr elements)
     (when (and (not (null? elements))
                (symbol-atom? (first elements)))
       (let ([func-name (symbol-atom-value (first elements))]
             [args (rest elements)])
         (if (is-type-family-constructor? func-name)
             (infer-type-family-parameters func-name args #f ctx)
             #f)))]
    [_ #f]))

;; Check if function name is a type family constructor
(define/contract (is-type-family-constructor? name)
  (-> string? boolean?)
  (member name '("list-cons" "list-nil" "vec-cons" "vec-nil" "equal-refl") #t))

;; ============================================================================
;; BIDIRECTIONAL INFERENCE AND ELABORATION
;; ============================================================================

;; Infer type and elaborate AST (synthesis mode)
(define/contract (infer-and-elaborate ast ctx)
  (-> ast-node/c bidirectional-inference-context? elaborated-ast-node?)
  (elaborate-ast-with-inference ast ctx))

;; Check against expected type and elaborate AST (checking mode)
(define/contract (check-and-elaborate ast expected-type ctx)
  (-> ast-node/c hott-type/c bidirectional-inference-context? elaborated-ast-node?)
  (let* ([elaborated (elaborate-ast-with-inference ast ctx)]
         [inferred-type (elaborated-ast-node-inferred-type elaborated)]
         [hott-ctx (bidirectional-inference-context-hott ctx)])
    
    ;; Check type compatibility and add constraint if needed
    (unless (hott-type-equal? inferred-type expected-type)
      (generate-identity-constraint hott-ctx inferred-type expected-type ast))
    
    ;; Return elaborated AST with checking constraint
    elaborated))

;; Complete AST elaboration with all inference systems
(define/contract (complete-ast-elaboration ast ctx)
  (-> ast-node/c bidirectional-inference-context? elaborated-ast-node?)
  (let* ([elaborated (elaborate-ast-with-inference ast ctx)]
         [hott-ctx (bidirectional-inference-context-hott ctx)]
         [universe-ctx (bidirectional-inference-context-universe ctx)])
    
    ;; Solve universe constraints
    (solve-universe-constraints universe-ctx)
    
    ;; Attempt proof synthesis for obligations
    (for-each (lambda (obligation)
                (attempt-proof-synthesis obligation hott-ctx))
              (elaborated-ast-node-proof-obligations elaborated))
    
    elaborated))