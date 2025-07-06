;; ============================================================================
;; AST TO IR TRANSLATION
;; ============================================================================
;; Converts PathFinder AST to evidence-preserving IR

(import core ast)
(import compiler.ir core)
(import compiler.ir builder)
(import types types)
(import effects computation-as-effect)

;; ============================================================================
;; TRANSLATION CONTEXT
;; ============================================================================

(data TranslationContext U0
  (case trans-context
    (-> (env : IREnvironment)           ;; Current environment
        (var-map : List (Pair String Nat)) ;; Variable name to index mapping
        (type-env : List (Pair String IRValue)) ;; Type environment
        (in-function : Bool)            ;; Are we inside a function?
        TranslationContext)))

;; Create initial translation context
(define make-trans-context
  (fn ()
    (trans-context ir-empty-env nil nil false)))

;; ============================================================================
;; AST TO IR TRANSLATION
;; ============================================================================

;; Translate top-level AST to IR module
(define ast-to-ir-module
  (fn (name ast-list)
    (let ((defs (translate-definitions ast-list (make-trans-context))))
      (make-ir-module name nil defs (extract-exports ast-list)))))

;; Translate list of definitions
(define translate-definitions
  (fn (ast-list ctx)
    (match ast-list
      (case nil nil)
      (case (cons ast rest)
        (match ast
          ;; Skip imports and exports - handled separately
          (case (import _)
            (translate-definitions rest ctx))
          (case (export _)
            (translate-definitions rest ctx))
          ;; Translate actual definitions
          (case _
            (let ((def-pair (translate-definition ast ctx)))
              (match def-pair
                (case (pair def new-ctx)
                  (cons def (translate-definitions rest new-ctx)))))))))))

;; Translate single definition
(define translate-definition
  (fn (ast ctx)
    (match ast
      ;; Value definition
      (case (define name value)
        (let ((ir-value (translate-ast value ctx)))
          (let ((ir-type (infer-type ir-value)))
            (let ((def (make-ir-def-value name ir-type (ir-return ir-value))))
              (pair def (extend-context name ir-value ctx))))))
      
      ;; Type definition
      (case (data name params constructors)
        (let ((ir-type (translate-data-type name params constructors)))
          (let ((def (make-ir-def-type name params ir-type)))
            (pair def ctx))))
      
      ;; Function definition (sugar for define with lambda)
      (case (defun name params body)
        (translate-definition (define name (lambda params body)) ctx))
      
      ;; Other forms
      (case _
        (pair (make-ir-def-value "unknown" ir-unit (ir-return ir-unit)) ctx)))))

;; ============================================================================
;; EXPRESSION TRANSLATION
;; ============================================================================

;; Main AST to IR translation
(define translate-ast
  (fn (ast ctx)
    (match ast
      ;; Literals
      (case (nat-lit n)
        (make-ir-nat n))
      
      (case (bool-lit b)
        (make-ir-bool b))
      
      (case (string-lit s)
        (make-ir-string s))
      
      ;; Variables
      (case (var name)
        (make-ir-var name ctx))
      
      ;; Constructors
      (case (constructor name args)
        (let ((ir-args (map (fn (arg) (translate-ast arg ctx)) args)))
          (make-ir-constructor name ir-args)))
      
      ;; Lambda
      (case (lambda params body)
        (let ((new-ctx (extend-params params ctx)))
          (let ((ir-body (translate-computation body new-ctx)))
            (make-ir-closure params ir-body (get-env ctx)))))
      
      ;; Application
      (case (app func arg)
        (let ((ir-func (translate-ast func ctx)))
          (let ((ir-arg (translate-ast arg ctx)))
            (make-ir-app ir-func ir-arg))))
      
      ;; Let binding
      (case (let-bind name value body)
        (let ((ir-value (translate-computation value ctx)))
          (let ((new-ctx (extend-context name ir-value ctx)))
            (let ((ir-body (translate-computation body new-ctx)))
              (make-ir-let name ir-value ir-body)))))
      
      ;; If expression
      (case (if-expr cond then else)
        (let ((ir-cond (translate-ast cond ctx)))
          (let ((ir-then (translate-computation then ctx)))
            (let ((ir-else (translate-computation else ctx)))
              (make-ir-if ir-cond ir-then ir-else)))))
      
      ;; Pattern matching (simplified for now)
      (case (match-expr expr cases)
        (translate-match expr cases ctx))
      
      ;; Effects
      (case (perform effect)
        (translate-effect effect ctx))
      
      ;; Special forms
      (case zero
        (make-ir-nat zero))
      
      (case (succ n)
        (make-ir-constructor "succ" (list (translate-ast n ctx))))
      
      (case true
        (make-ir-bool true))
      
      (case false
        (make-ir-bool false))
      
      (case nil
        (make-ir-constructor "nil" nil))
      
      (case (cons x xs)
        (make-ir-constructor "cons" 
          (list (translate-ast x ctx) (translate-ast xs ctx))))
      
      ;; Type expressions
      (case (type-expr level)
        (ir-type level))
      
      (case (arrow-type from to)
        (make-ir-function-type 
          (translate-ast from ctx)
          (translate-ast to ctx)))
      
      ;; Default
      (case _
        ir-unit))))

;; Translate computation (expression that might have effects)
(define translate-computation
  (fn (ast ctx)
    (let ((val (translate-ast ast ctx)))
      (if (is-computation? ast)
          val
          (make-ir-return val)))))

;; ============================================================================
;; PATTERN MATCHING TRANSLATION
;; ============================================================================

(define translate-match
  (fn (expr cases ctx)
    (let ((target (translate-ast expr ctx)))
      (match cases
        ;; Simple nat matching -> nat-elim
        (case ((match-case zero zero-body)
               (match-case (succ n) succ-body))
          (let ((motive (make-ir-closure (list "_") 
                          (ir-return (infer-type (translate-ast zero-body ctx)))
                          ir-empty-env)))
            (let ((base (translate-ast zero-body ctx)))
              (let ((step (make-ir-closure (list n "rec")
                            (translate-computation succ-body 
                              (extend-context n ir-unit ctx))
                            (get-env ctx))))
                (make-ir-nat-elim motive base step target)))))
        
        ;; Simple bool matching -> bool-elim
        (case ((match-case false false-body)
               (match-case true true-body))
          (let ((motive (make-ir-closure (list "_")
                          (ir-return (infer-type (translate-ast false-body ctx)))
                          ir-empty-env)))
            (let ((false-case (translate-ast false-body ctx)))
              (let ((true-case (translate-ast true-body ctx)))
                (make-ir-bool-elim motive false-case true-case target)))))
        
        ;; Default: translate to nested if-then-else
        (case _
          (translate-match-cases target cases ctx))))))

(define translate-match-cases
  (fn (target cases ctx)
    ;; Simplified - would need proper pattern compilation
    (make-ir-return ir-unit)))

;; ============================================================================
;; EFFECT TRANSLATION
;; ============================================================================

(define translate-effect
  (fn (effect ctx)
    (match effect
      ;; Print effect
      (case (print msg)
        (let ((ir-msg (translate-ast msg ctx)))
          (make-ir-perform (effect-print ir-msg))))
      
      ;; File effects
      (case (file-read path)
        (let ((ir-path (translate-ast path ctx)))
          (make-ir-perform (effect-file-read ir-path))))
      
      (case (file-write path content)
        (let ((ir-path (translate-ast path ctx)))
          (let ((ir-content (translate-ast content ctx)))
            (make-ir-perform (effect-file-write ir-path ir-content)))))
      
      ;; General IO
      (case (io-effect cat op args)
        (let ((ir-args (map (fn (arg) (translate-ast arg ctx)) args)))
          (make-ir-perform (effect-io cat op ir-args))))
      
      ;; Default
      (case _
        (make-ir-return ir-unit)))))

;; ============================================================================
;; TYPE INFERENCE (SIMPLIFIED)
;; ============================================================================

(define infer-type
  (fn (ir-value)
    ;; Simplified type inference
    (match ir-value
      (case (ir-nat _) (ir-type zero))
      (case (ir-bool _) (ir-type zero))
      (case (ir-string _) (ir-type zero))
      (case ir-unit (ir-type zero))
      (case _ (ir-type zero)))))

;; ============================================================================
;; CONTEXT OPERATIONS
;; ============================================================================

(define extend-context
  (fn (name value ctx)
    (match ctx
      (case (trans-context env vars types in-func)
        (trans-context 
          (ir-extend-env name value env)
          (cons (pair name (length vars)) vars)
          types
          in-func)))))

(define extend-params
  (fn (params ctx)
    (match params
      (case nil ctx)
      (case (cons p ps)
        (extend-params ps (extend-context p ir-unit ctx))))))

(define get-env
  (fn (ctx)
    (match ctx
      (case (trans-context env _ _ _) env))))

(define is-computation?
  (fn (ast)
    ;; Check if AST node represents a computation
    (match ast
      (case (let-bind _ _ _) true)
      (case (if-expr _ _ _) true)
      (case (match-expr _ _) true)
      (case (perform _) true)
      (case _ false))))

;; ============================================================================
;; HELPER FUNCTIONS
;; ============================================================================

(define extract-exports
  (fn (ast-list)
    ;; Extract export names from AST list
    nil))

(define translate-data-type
  (fn (name params constructors)
    ;; Translate data type definition
    (ir-type zero)))

(define map
  (fn (f lst)
    (match lst
      (case nil nil)
      (case (cons x xs) (cons (f x) (map f xs))))))

(define list
  (fn (x) (cons x nil)))

(define pair
  (fn (x y) (cons x y)))

(define length
  (fn (lst)
    (match lst
      (case nil zero)
      (case (cons _ xs) (succ (length xs))))))

;; ============================================================================
;; PUBLIC API
;; ============================================================================

;; Translate a complete module
(define translate-module
  (fn (module-name ast-list)
    (ast-to-ir-module module-name ast-list)))

;; Translate a single expression
(define translate-expression
  (fn (ast)
    (translate-ast ast (make-trans-context))))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export translate-module)
(export translate-expression)
(export ast-to-ir-module)
(export translate-ast)
(export translate-computation)