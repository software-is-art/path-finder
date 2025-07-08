;; ============================================================================
;; CONSTANT FOLDING OPTIMIZATION PASS
;; ============================================================================
;; Evaluates pure computations at compile time using evidence

(import compiler.ir core)
(import compiler.ir builder)
(import types types)
(import effects computation-as-effect)

;; ============================================================================
;; CONSTANT FOLDING CONTEXT
;; ============================================================================

(data FoldContext U0
  (case fold-context
    (-> (bindings : List (Pair String IRValue))  ;; Known constant bindings
        (changed : Bool)                          ;; Did we make changes?
        (fuel : Nat)                             ;; Evaluation fuel (prevents loops)
        FoldContext)))

;; Create initial folding context
(define make-fold-context
  (fn ()
    (fold-context nil false thousand)))

;; ============================================================================
;; MAIN CONSTANT FOLDING PASS
;; ============================================================================

;; Fold constants in a module
(define fold-module
  (fn (module)
    (match module
      (case (ir-module name imports defs exports evidence)
        (let ((folded-defs-ctx (fold-definitions defs (make-fold-context))))
          (match folded-defs-ctx
            (case (pair folded-defs ctx)
              (ir-module name imports folded-defs exports evidence))))))))

;; Fold constants in definitions
(define fold-definitions
  (fn (defs ctx)
    (match defs
      (case nil (pair nil ctx))
      (case (cons def rest)
        (let ((folded-def-ctx (fold-definition def ctx)))
          (match folded-def-ctx
            (case (pair folded-def new-ctx)
              (let ((rest-result (fold-definitions rest new-ctx)))
                (match rest-result
                  (case (pair rest-defs final-ctx)
                    (pair (cons folded-def rest-defs) final-ctx)))))))))))

;; Fold constants in a single definition
(define fold-definition
  (fn (def ctx)
    (match def
      (case (ir-def-value name type value evidence)
        (let ((folded-comp-ctx (fold-computation value ctx)))
          (match folded-comp-ctx
            (case (pair folded-comp new-ctx)
              ;; If the computation reduced to a constant, record it
              (let ((binding-ctx (maybe-add-binding name folded-comp new-ctx)))
                (pair (ir-def-value name type folded-comp evidence) binding-ctx))))))
      
      ;; Other definitions pass through
      (case _ (pair def ctx)))))

;; ============================================================================
;; COMPUTATION FOLDING
;; ============================================================================

(define fold-computation
  (fn (comp ctx)
    (match ctx
      (case (fold-context bindings changed fuel)
        (if (is-zero? fuel)
            (pair comp ctx)  ;; Out of fuel, stop folding
            (match comp
              ;; Return value - fold the value
              (case (ir-return val)
                (let ((folded-val-ctx (fold-value val ctx)))
                  (match folded-val-ctx
                    (case (pair folded-val new-ctx)
                      (pair (ir-return folded-val) new-ctx)))))
              
              ;; Let binding - fold value and body
              (case (ir-let name value body space)
                (let ((folded-val-ctx (fold-computation value ctx)))
                  (match folded-val-ctx
                    (case (pair folded-val val-ctx)
                      (let ((bind-ctx (maybe-add-computation-binding name folded-val val-ctx)))
                        (let ((folded-body-ctx (fold-computation body bind-ctx)))
                          (match folded-body-ctx
                            (case (pair folded-body body-ctx)
                              ;; If value is constant, substitute in body
                              (if (is-constant-computation? folded-val)
                                  (pair folded-body (set-changed body-ctx))
                                  (pair (ir-let name folded-val folded-body space) body-ctx))))))))))
              
              ;; Application - try to evaluate if possible
              (case (ir-app func arg evidence)
                (let ((folded-func-ctx (fold-value func ctx)))
                  (match folded-func-ctx
                    (case (pair folded-func func-ctx)
                      (let ((folded-arg-ctx (fold-value arg func-ctx)))
                        (match folded-arg-ctx
                          (case (pair folded-arg arg-ctx)
                            (try-eval-app folded-func folded-arg evidence arg-ctx))))))))
              
              ;; Nat elimination - try to evaluate if target is constant
              (case (ir-nat-elim motive base step target evidence)
                (let ((folded-target-ctx (fold-value target ctx)))
                  (match folded-target-ctx
                    (case (pair folded-target target-ctx)
                      (if (is-nat-constant? folded-target)
                          (eval-nat-elim motive base step folded-target evidence target-ctx)
                          (pair (ir-nat-elim motive base step folded-target evidence) target-ctx))))))
              
              ;; Bool elimination - try to evaluate if target is constant
              (case (ir-bool-elim motive false-case true-case target evidence)
                (let ((folded-target-ctx (fold-value target ctx)))
                  (match folded-target-ctx
                    (case (pair folded-target target-ctx)
                      (if (is-bool-constant? folded-target)
                          (eval-bool-elim motive false-case true-case folded-target evidence target-ctx)
                          (pair (ir-bool-elim motive false-case true-case folded-target evidence) target-ctx))))))
              
              ;; Conditional - evaluate if condition is constant
              (case (ir-if condition then-branch else-branch evidence)
                (let ((folded-cond-ctx (fold-value condition ctx)))
                  (match folded-cond-ctx
                    (case (pair folded-cond cond-ctx)
                      (if (is-bool-constant? folded-cond)
                          (match folded-cond
                            (case (ir-bool true)
                              (let ((result (fold-computation then-branch cond-ctx)))
                                (match result
                                  (case (pair comp new-ctx)
                                    (pair comp (set-changed new-ctx))))))
                            (case (ir-bool false)
                              (let ((result (fold-computation else-branch cond-ctx)))
                                (match result
                                  (case (pair comp new-ctx)
                                    (pair comp (set-changed new-ctx))))))
                            (case _ (pair comp ctx)))
                          (pair comp ctx))))))
              
              ;; Sequence - fold both parts
              (case (ir-sequence first second)
                (let ((folded-first-ctx (fold-computation first ctx)))
                  (match folded-first-ctx
                    (case (pair folded-first first-ctx)
                      (let ((folded-second-ctx (fold-computation second first-ctx)))
                        (match folded-second-ctx
                          (case (pair folded-second second-ctx)
                            (pair (ir-sequence folded-first folded-second) second-ctx))))))))
              
              ;; Default - no folding
              (case _ (pair comp ctx))))))))

;; ============================================================================
;; VALUE FOLDING
;; ============================================================================

(define fold-value
  (fn (val ctx)
    (match val
      ;; Constants pass through
      (case (ir-nat _) (pair val ctx))
      (case (ir-bool _) (pair val ctx))
      (case (ir-string _) (pair val ctx))
      (case ir-unit (pair val ctx))
      
      ;; Look up variables in constant bindings
      (case (ir-var name index)
        (let ((maybe-const (lookup-binding name ctx)))
          (match maybe-const
            (case (some const-val)
              (pair const-val (set-changed ctx)))
            (case none
              (pair val ctx)))))
      
      ;; Fold constructor arguments
      (case (ir-constructor name args evidence)
        (let ((folded-args-ctx (fold-value-list args ctx)))
          (match folded-args-ctx
            (case (pair folded-args new-ctx)
              (pair (ir-constructor name folded-args evidence) new-ctx)))))
      
      ;; Other values pass through
      (case _ (pair val ctx)))))

;; Fold a list of values
(define fold-value-list
  (fn (vals ctx)
    (match vals
      (case nil (pair nil ctx))
      (case (cons v vs)
        (let ((folded-v-ctx (fold-value v ctx)))
          (match folded-v-ctx
            (case (pair folded-v v-ctx)
              (let ((folded-vs-ctx (fold-value-list vs v-ctx)))
                (match folded-vs-ctx
                  (case (pair folded-vs vs-ctx)
                    (pair (cons folded-v folded-vs) vs-ctx)))))))))))

;; ============================================================================
;; EVALUATION HELPERS
;; ============================================================================

;; Try to evaluate function application
(define try-eval-app
  (fn (func arg evidence ctx)
    ;; For now, we don't evaluate user functions
    ;; Could be extended to handle simple cases
    (pair (ir-app func arg evidence) ctx)))

;; Evaluate nat-elim with constant target
(define eval-nat-elim
  (fn (motive base step target evidence ctx)
    (match target
      (case (ir-nat n)
        (let ((result (eval-nat-elim-helper motive base step n ctx)))
          (match result
            (case (pair val new-ctx)
              (pair (ir-return val) (set-changed new-ctx))))))
      (case _
        (pair (ir-nat-elim motive base step target evidence) ctx)))))

;; Helper to evaluate nat-elim
(define eval-nat-elim-helper
  (fn (motive base step n ctx)
    (nat-elim (fn (_) (Pair IRValue FoldContext))
              (pair base ctx)
              (fn (pred rec)
                (match rec
                  (case (pair rec-val rec-ctx)
                    ;; Apply step function
                    ;; This is simplified - would need proper beta reduction
                    (pair rec-val rec-ctx))))
              n)))

;; Evaluate bool-elim with constant target
(define eval-bool-elim
  (fn (motive false-case true-case target evidence ctx)
    (match target
      (case (ir-bool b)
        (if b
            (pair (ir-return true-case) (set-changed ctx))
            (pair (ir-return false-case) (set-changed ctx))))
      (case _
        (pair (ir-bool-elim motive false-case true-case target evidence) ctx)))))

;; ============================================================================
;; CONTEXT HELPERS
;; ============================================================================

(define lookup-binding
  (fn (name ctx)
    (match ctx
      (case (fold-context bindings _ _)
        (lookup-in-list name bindings)))))

(define lookup-in-list
  (fn (name bindings)
    (match bindings
      (case nil none)
      (case (cons (pair bind-name val) rest)
        (if (string-equal? name bind-name)
            (some val)
            (lookup-in-list name rest))))))

(define maybe-add-binding
  (fn (name comp ctx)
    (match comp
      (case (ir-return val)
        (if (is-constant-value? val)
            (add-binding name val ctx)
            ctx))
      (case _ ctx))))

(define maybe-add-computation-binding
  (fn (name comp ctx)
    (match comp
      (case (ir-return val)
        (if (is-constant-value? val)
            (add-binding name val ctx)
            ctx))
      (case _ ctx))))

(define add-binding
  (fn (name val ctx)
    (match ctx
      (case (fold-context bindings changed fuel)
        (fold-context (cons (pair name val) bindings) changed fuel)))))

(define set-changed
  (fn (ctx)
    (match ctx
      (case (fold-context bindings _ fuel)
        (fold-context bindings true fuel)))))

(define decrement-fuel
  (fn (ctx)
    (match ctx
      (case (fold-context bindings changed fuel)
        (fold-context bindings changed (pred fuel))))))

;; ============================================================================
;; PREDICATES
;; ============================================================================

(define is-constant-value?
  (fn (val)
    (match val
      (case (ir-nat _) true)
      (case (ir-bool _) true)
      (case (ir-string _) true)
      (case ir-unit true)
      (case (ir-constructor name args _)
        (all-constant? args))
      (case _ false))))

(define is-constant-computation?
  (fn (comp)
    (match comp
      (case (ir-return val)
        (is-constant-value? val))
      (case _ false))))

(define is-nat-constant?
  (fn (val)
    (match val
      (case (ir-nat _) true)
      (case _ false))))

(define is-bool-constant?
  (fn (val)
    (match val
      (case (ir-bool _) true)
      (case _ false))))

(define all-constant?
  (fn (vals)
    (match vals
      (case nil true)
      (case (cons v vs)
        (if (is-constant-value? v)
            (all-constant? vs)
            false)))))

;; ============================================================================
;; HELPERS
;; ============================================================================

(define thousand (mult hundred ten))
(define hundred (mult ten ten))
(define ten (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ zero))))))))))

(define mult
  (fn (x y)
    (nat-elim (fn (_) Nat)
              zero
              (fn (_ acc) (add x acc))
              y)))

(define add
  (fn (x y)
    (nat-elim (fn (_) Nat)
              x
              (fn (_ acc) (succ acc))
              y)))

(define is-zero?
  (fn (n)
    (match n
      (case zero true)
      (case _ false))))

(define pred
  (fn (n)
    (match n
      (case zero zero)
      (case (succ p) p))))

(define string-equal?
  (fn (s1 s2) true))  ;; Stub

(define pair (fn (x y) (cons x y)))

(data Option U0
  (case none Option)
  (case some (-> IRValue Option)))

;; ============================================================================
;; PUBLIC API
;; ============================================================================

;; Run constant folding on a module
(define optimize-constant-fold
  (fn (module)
    (fold-module module)))

;; Check if folding made changes
(define folding-changed?
  (fn (ctx)
    (match ctx
      (case (fold-context _ changed _) changed))))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export optimize-constant-fold)
(export fold-module)
(export fold-computation)
(export fold-value)
(export folding-changed?)