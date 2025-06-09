#lang racket/base

(require racket/contract
         racket/match
         racket/list
         racket/string
         "../parser/ast.rkt"
         "../types/types.rkt"
         "../types/type-families.rkt"
         "../evaluator/values.rkt"
         "typechecker.rkt")

(provide infer-type-family-parameters
         infer-constructor-types
         infer-implicit-arguments
         type-family-inference-context
         type-family-inference-context?
         type-family-inference-context-type-env
         type-family-inference-context-known-types
         type-family-inference-context-inferred-parameters
         make-tf-inference-context
         resolve-type-parameters
         type-check-with-inference
         type-check-with-inference-context
         safe-infer-type
         compatible-list-types?
         extract-element-type-from-name
         extract-list-element-type
         infer-list-cons-type
         infer-list-nil-type
         infer-nonempty-list-element-type
         implicit-parameter?
         infer-array-length
         unify-types
         type-family-constructor?
         infer-from-argument-types
         infer-from-expected-type
         infer-from-usage-context
         infer-list-element-type)

;; ============================================================================
;; TYPE FAMILY PARAMETER INFERENCE - HIGH VALUE IMPLEMENTATION
;; ============================================================================
;; Focused on the main verbosity issues: type family parameters and constructors
;; Addresses the 50-60% annotation reduction opportunity identified

;; Helper function for string suffix checking
(define/contract (string-suffix? str suffix)
  (-> string? string? boolean?)
  (let ([str-len (string-length str)]
        [suffix-len (string-length suffix)])
    (and (>= str-len suffix-len)
         (string=? (substring str (- str-len suffix-len)) suffix))))

;; ============================================================================
;; TYPE FAMILY INFERENCE CONTEXT
;; ============================================================================

;; Context for tracking type family parameter inference
(struct type-family-inference-context (type-env known-types inferred-parameters) #:transparent #:mutable)

;; Make inference context
(define/contract (make-tf-inference-context type-env)
  (-> type-environment? type-family-inference-context?)
  (type-family-inference-context type-env (make-hash) (make-hash)))

;; ============================================================================
;; TYPE FAMILY PARAMETER INFERENCE
;; ============================================================================

;; Main entry point: infer type family parameters from usage context
(define/contract (infer-type-family-parameters family-name args expected-type ctx)
  (-> string? (listof ast-node/c) (or/c hott-type/c #f) type-family-inference-context? 
      (listof hott-type/c))
  (let ([family (get-type-family (string->symbol family-name))])
    (cond
      [family
       ;; Try different inference strategies
       (or (infer-from-argument-types family args ctx)
           (infer-from-expected-type family expected-type ctx)
           (infer-from-usage-context family args ctx)
           ;; Fallback: require explicit parameters
           (error (format "Cannot infer type parameters for ~a" family-name)))]
      [else 
       (error (format "Unknown type family: ~a" family-name))])))

;; Strategy 1: Infer from argument value types
(define/contract (infer-from-argument-types family args ctx)
  (-> any/c (listof ast-node/c) type-family-inference-context? (or/c (listof hott-type/c) #f))
  (match (type-family-name family)
    ;; List type family: infer element type from first non-nil argument
    ['List (infer-list-element-type args ctx)]
    
    ;; NonEmptyList: infer from first element
    ['NonEmptyList (infer-nonempty-list-element-type args ctx)]
    
    ;; BoundedArray: infer element type and length
    ['BoundedArray (infer-bounded-array-parameters args ctx)]
    
    ;; Generic type families
    [_ (infer-generic-type-family-parameters family args ctx)]))

;; Strategy 2: Infer from expected result type
(define/contract (infer-from-expected-type family expected-type ctx)
  (-> any/c (or/c hott-type/c #f) type-family-inference-context? (or/c (listof hott-type/c) #f))
  (when expected-type
    (match expected-type
      ;; If expecting List[T], extract T
      [(inductive-type list-name _) #:when (string-prefix? list-name "List-")
       (list (extract-element-type-from-name list-name))]
      
      ;; If expecting specific instantiated type family
      [_ (extract-parameters-from-instantiated-type expected-type family)])))

;; Strategy 3: Infer from usage context (function calls, assignments)
(define/contract (infer-from-usage-context family args ctx)
  (-> any/c (listof ast-node/c) type-family-inference-context? (or/c (listof hott-type/c) #f))
  ;; Look for patterns like (list-cons ? x existing-list) where existing-list has known type
  (match args
    ;; Pattern: operation on existing typed structure
    [(list _ ... existing-structure)
     (let ([existing-type (safe-infer-type existing-structure ctx)])
       (when existing-type
         (extract-type-family-parameters existing-type family)))]
    [_ #f]))

;; ============================================================================
;; LIST TYPE FAMILY INFERENCE (HIGH IMPACT)
;; ============================================================================

;; Infer List element type from arguments
(define/contract (infer-list-element-type args ctx)
  (-> (listof ast-node/c) type-family-inference-context? (or/c (listof hott-type/c) #f))
  (cond
    ;; list-nil: no inference possible, needs context
    [(null? args) #f]
    
    ;; list-cons with explicit elements
    [(>= (length args) 1)
     (let ([first-arg-type (safe-infer-type (first args) ctx)])
       (when first-arg-type
         (list first-arg-type)))]
    
    [else #f]))

;; Infer NonEmptyList element type (always has at least one element)
(define/contract (infer-nonempty-list-element-type args ctx)
  (-> (listof ast-node/c) type-family-inference-context? (or/c (listof hott-type/c) #f))
  (when (>= (length args) 1)
    (let ([first-elem-type (safe-infer-type (first args) ctx)])
      (when first-elem-type
        (list first-elem-type)))))

;; Infer BoundedArray parameters (element type + length)
(define/contract (infer-bounded-array-parameters args ctx)
  (-> (listof ast-node/c) type-family-inference-context? (or/c (listof hott-type/c) #f))
  (when (>= (length args) 2)
    (let ([element-type (safe-infer-type (first args) ctx)]
          [length-value (second args)])
      (when element-type
        ;; Try to infer length from literal or context
        (let ([length-type (infer-array-length length-value ctx)])
          (when length-type
            (list element-type length-type)))))))

;; ============================================================================
;; CONSTRUCTOR TYPE INFERENCE (HIGH IMPACT)
;; ============================================================================

;; Infer constructor types from arguments (eliminates constructor disambiguation)
(define/contract (infer-constructor-types constructor-name args ctx)
  (-> string? (listof ast-node/c) type-family-inference-context? (or/c hott-type/c #f))
  (match constructor-name
    ;; List constructors
    ["list-cons" (infer-list-cons-type args ctx)]
    ["list-nil" (infer-list-nil-type args ctx)]
    
    ;; NonEmptyList constructors  
    ["nonempty-list-cons" (infer-nonempty-cons-type args ctx)]
    
    ;; BoundedArray constructors
    ["make-bounded-array" (infer-bounded-array-cons-type args ctx)]
    
    ;; Generic constructor inference
    [_ (infer-generic-constructor-type constructor-name args ctx)]))

;; Infer list-cons type from element and tail
(define/contract (infer-list-cons-type args ctx)
  (-> (listof ast-node/c) type-family-inference-context? (or/c hott-type/c #f))
  (when (= (length args) 2)
    (let ([elem-type (safe-infer-type (first args) ctx)]
          [tail-type (safe-infer-type (second args) ctx)])
      (cond
        ;; If both types known, verify compatibility and return List[elem-type]
        [(and elem-type tail-type)
         (when (compatible-list-types? elem-type tail-type)
           (make-list-type elem-type))]
        
        ;; If only element type known, return List[elem-type]
        [elem-type (make-list-type elem-type)]
        
        ;; If only tail type known, extract element type
        [tail-type (extract-list-element-type tail-type)]
        
        [else #f]))))

;; Infer list-nil type from context (needs expected type or usage)
(define/contract (infer-list-nil-type args ctx)
  (-> (listof ast-node/c) type-family-inference-context? (or/c hott-type/c #f))
  ;; list-nil needs context to infer element type
  (hash-ref (type-family-inference-context-inferred-parameters ctx) 'list-nil-context #f))

;; Infer nonempty-list-cons type from elements
(define/contract (infer-nonempty-cons-type args ctx)
  (-> (listof ast-node/c) type-family-inference-context? (or/c hott-type/c #f))
  (when (>= (length args) 1)
    (let ([first-elem-type (safe-infer-type (first args) ctx)])
      (when first-elem-type
        ;; Create NonEmptyList type with inferred element type
        first-elem-type))))  ; Simplified - would create proper NonEmptyList type

;; Infer bounded array constructor type
(define/contract (infer-bounded-array-cons-type args ctx)
  (-> (listof ast-node/c) type-family-inference-context? (or/c hott-type/c #f))
  (when (>= (length args) 2)
    (let ([elem-type (safe-infer-type (first args) ctx)]
          [length-ast (second args)])
      (when elem-type
        ;; Create BoundedArray type with inferred parameters
        elem-type))))  ; Simplified - would create proper BoundedArray type

;; ============================================================================
;; IMPLICIT ARGUMENT INFERENCE (Π/Σ-TYPES)
;; ============================================================================

;; Infer implicit arguments for dependent types
(define/contract (infer-implicit-arguments pi-type-arg explicit-args ctx)
  (-> pi-type? (listof ast-node/c) type-family-inference-context? (listof hott-type/c))
  (if (pi-type? pi-type-arg)
      (let ([var (pi-type-var-name pi-type-arg)]
            [domain (pi-type-domain pi-type-arg)]
            [codomain (pi-type-codomain pi-type-arg)])
        (if (implicit-parameter? var)
            ;; Try to infer implicit parameter from explicit arguments
            (let ([inferred (infer-implicit-from-context var domain explicit-args ctx)])
              (if inferred
                  (cons inferred (infer-remaining-implicit-args codomain explicit-args ctx))
                  (error (format "Cannot infer implicit parameter ~a" var))))
            '()))  ; Not implicit
      '()))  ; Not a pi-type

;; Check if parameter is implicit (marked with {})
(define/contract (implicit-parameter? var-name)
  (-> string? boolean?)
  (and (string-prefix? var-name "{") 
       (string-suffix? var-name "}")))

;; Infer implicit parameter from explicit argument types
(define/contract (infer-implicit-from-context var domain explicit-args ctx)
  (-> string? hott-type/c (listof ast-node/c) type-family-inference-context? (or/c hott-type/c #f))
  (when (not (null? explicit-args))
    (let ([first-arg-type (safe-infer-type (first explicit-args) ctx)])
      (when first-arg-type
        ;; If domain is Type and we have a concrete type, infer that type
        (match domain
          [(universe _) first-arg-type]  ; {A : Type} inferred from concrete type
          [_ (unify-types domain first-arg-type ctx)])))))

;; ============================================================================
;; TYPE RESOLUTION AND UNIFICATION
;; ============================================================================

;; Resolve all inferred type parameters
(define/contract (resolve-type-parameters ctx)
  (-> type-family-inference-context? void?)
  ;; Apply any pending type parameter resolutions
  (let ([parameters (type-family-inference-context-inferred-parameters ctx)])
    (for ([(key value) (in-hash parameters)])
      (when (unresolved-type? value)
        (hash-set! parameters key (resolve-unresolved-type value ctx))))))

;; Safe type inference that doesn't fail on unknown types
(define/contract (safe-infer-type ast ctx)
  (-> ast-node/c type-family-inference-context? (or/c hott-type/c #f))
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (type-check ast (type-family-inference-context-type-env ctx))))

;; Check if two types are compatible for list operations
(define/contract (compatible-list-types? elem-type tail-type)
  (-> hott-type/c hott-type/c boolean?)
  (match tail-type
    [(inductive-type list-name _) #:when (string-prefix? list-name "List-")
     (let ([tail-elem-type (extract-element-type-from-name list-name)])
       (hott-type-equal? elem-type tail-elem-type))]
    [_ #f]))

;; Extract element type from List type name
(define/contract (extract-element-type-from-name list-name)
  (-> string? hott-type/c)
  (match list-name
    ["List-Nat" Nat]
    ["List-Bool" Bool]
    ["List-String" (inductive-type "String" '())]
    [_ Nat]))  ; Default fallback

;; Extract list element type from list type
(define/contract (extract-list-element-type list-type)
  (-> hott-type/c (or/c hott-type/c #f))
  (match list-type
    [(inductive-type list-name _) #:when (string-prefix? list-name "List-")
     (extract-element-type-from-name list-name)]
    [_ #f]))

;; Infer array length from value context
(define/contract (infer-array-length length-ast ctx)
  (-> ast-node/c type-family-inference-context? (or/c hott-type/c #f))
  (match length-ast
    [(number-atom n) (nat-from-number n)]
    [(symbol-atom name) (safe-infer-type length-ast ctx)]
    [_ #f]))

;; Convert number to Nat type
(define/contract (nat-from-number n)
  (-> exact-nonnegative-integer? hott-type/c)
  ;; Return Nat type - in full implementation would create specific Nat value
  Nat)

;; Generic type family parameter inference
(define/contract (infer-generic-type-family-parameters family args ctx)
  (-> any/c (listof ast-node/c) type-family-inference-context? (or/c (listof hott-type/c) #f))
  ;; Try to infer from argument types
  (when (not (null? args))
    (let ([arg-types (filter values (map (lambda (arg) (safe-infer-type arg ctx)) args))])
      (when (not (null? arg-types))
        arg-types))))

;; Extract parameters from instantiated type family
(define/contract (extract-parameters-from-instantiated-type type family)
  (-> hott-type/c any/c (or/c (listof hott-type/c) #f))
  ;; Simplified extraction - full implementation would parse type structure
  #f)

;; Extract type family parameters from known type
(define/contract (extract-type-family-parameters type family)
  (-> hott-type/c any/c (or/c (listof hott-type/c) #f))
  ;; Simplified extraction - look for matching type family instances
  (match type
    [(inductive-type list-name _) #:when (string-prefix? list-name "List-")
     (list (extract-element-type-from-name list-name))]
    [_ #f]))

;; Generic constructor type inference
(define/contract (infer-generic-constructor-type constructor-name args ctx)
  (-> string? (listof ast-node/c) type-family-inference-context? (or/c hott-type/c #f))
  ;; Look up constructor signature and infer result type
  #f)

;; Remaining implicit argument inference
(define/contract (infer-remaining-implicit-args codomain explicit-args ctx)
  (-> hott-type/c (listof ast-node/c) type-family-inference-context? (listof hott-type/c))
  ;; Simplified - full implementation would handle chained implicit parameters
  '())

;; Unify two types
(define/contract (unify-types type1 type2 ctx)
  (-> hott-type/c hott-type/c type-family-inference-context? (or/c hott-type/c #f))
  (if (hott-type-equal? type1 type2)
      type1
      #f))

;; Check if type is unresolved
(define/contract (unresolved-type? type)
  (-> any/c boolean?)
  ;; Simplified check - full implementation would have proper unresolved type structure
  #f)

;; Resolve unresolved type
(define/contract (resolve-unresolved-type type ctx)
  (-> any/c type-family-inference-context? hott-type/c)
  ;; Return the type as-is for now
  type)

;; ============================================================================
;; INTEGRATION WITH EXISTING TYPE CHECKER
;; ============================================================================

;; Enhanced type checking with type family parameter inference
(define/contract (type-check-with-inference ast type-env)
  (-> ast-node/c type-environment? hott-type/c)
  (let ([ctx (make-tf-inference-context type-env)])
    (type-check-with-inference-context ast ctx)))

;; Type checking with inference context
(define/contract (type-check-with-inference-context ast ctx)
  (-> ast-node/c type-family-inference-context? hott-type/c)
  (match ast
    ;; Enhanced S-expression handling with type family inference
    [(sexpr elements) #:when (not (null? elements))
     (let ([first-elem (first elements)])
       (match first-elem
         ;; Type family constructors with parameter inference
         [(symbol-atom name) #:when (type-family-constructor? name)
          (infer-and-type-check-constructor name (rest elements) ctx)]
         
         ;; Regular type checking
         [_ (type-check ast (type-family-inference-context-type-env ctx))]))]
    
    ;; Default to regular type checking
    [_ (type-check ast (type-family-inference-context-type-env ctx))]))

;; Check if symbol is a type family constructor
(define/contract (type-family-constructor? name)
  (-> string? boolean?)
  (and (member name '("list-cons" "list-nil" "nonempty-list-cons" "make-bounded-array")) #t))

;; Infer and type check constructor with parameters
(define/contract (infer-and-type-check-constructor name args ctx)
  (-> string? (listof ast-node/c) type-family-inference-context? hott-type/c)
  (let ([inferred-type (infer-constructor-types name args ctx)])
    (if inferred-type
        inferred-type
        ;; Fallback to regular type checking
        (type-check (sexpr (cons (symbol-atom name) args)) 
                    (type-family-inference-context-type-env ctx)))))