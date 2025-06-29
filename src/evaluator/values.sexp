;; ============================================================================
;; PURE MATHEMATICAL HOTT VALUE SYSTEM (S-EXPRESSION VERSION)
;; ============================================================================
;; Runtime values for PathFinder HoTT-based evaluation
;; Depends on: types/types

(import types types)

;; ============================================================================
;; VALUE HIERARCHY AS HOTT INDUCTIVE TYPE
;; ============================================================================

;; The type of runtime values
(data Value U0
  ;; Constructor applications (for inductive types)
  (case constructor-value (-> String (List Value) Type Value))
  
  ;; Function closures
  (case closure-value (-> (List String) AST Environment Value))
  
  ;; Built-in function values (name, arity, type)
  (case builtin-value (-> String Nat Type Value))
  
  ;; Unit value (the unique inhabitant of Unit)
  (case unit-value Value)
  
  ;; String values (for effects and I/O)
  (case string-value (-> String Value))
  
  ;; Effect values (algebraic effects)
  (case effect-value (-> Effect Value))
  
  ;; Path values (inhabitants of identity types)
  (case path-runtime-value (-> Type Value Value Proof Value))
  
  ;; Equivalence values
  (case equivalence-runtime-value (-> Type Type Value Value Value)))

;; ============================================================================
;; VALUE PREDICATES
;; ============================================================================

;; Check if value is a constructor
(define is-constructor?
  (fn (v)
    (match v
      (case (constructor-value _ _ _) true)
      (case _ false))))

;; Check if value is a function (closure or builtin)
(define is-function?
  (fn (v)
    (match v
      (case (closure-value _ _ _) true)
      (case (builtin-value _ _ _) true)
      (case _ false))))

;; ============================================================================
;; VALUE CONSTRUCTORS
;; ============================================================================

;; Create zero value
(define zero-value
  (constructor-value "zero" nil (inductive-type "Nat" nat-constructors)))

;; Create successor value
(define succ-value
  (fn (n)
    (constructor-value "succ" (list n) (inductive-type "Nat" nat-constructors))))

;; Create boolean values
(define true-value
  (constructor-value "true" nil (inductive-type "Bool" bool-constructors)))

(define false-value
  (constructor-value "false" nil (inductive-type "Bool" bool-constructors)))

;; ============================================================================
;; VALUE OPERATIONS
;; ============================================================================

;; Apply a function value to an argument
(define apply-value
  (fn (func arg)
    (match func
      (case (closure-value params body env)
        (match params
          (case nil (error "Too many arguments"))
          (case (cons p rest)
            (if (null? rest)
                ;; Last parameter - evaluate body
                (eval-with-env body (extend-env env p arg))
                ;; More parameters - return new closure
                (closure-value rest body (extend-env env p arg))))))
      (case (builtin-value name arity type)
        (apply-builtin name arity type arg))
      (case _ (error "Not a function")))))

;; ============================================================================
;; VALUE EQUALITY
;; ============================================================================

;; Structural equality for values
(define value-equal?
  (fn (v1 v2)
    (match (pair v1 v2)
      ;; Constructor equality
      (case (pair (constructor-value n1 args1 t1) 
                  (constructor-value n2 args2 t2))
        (and (string-equal? n1 n2)
             (list-equal? value-equal? args1 args2)))
      
      ;; Unit equality
      (case (pair unit-value unit-value) true)
      
      ;; String equality
      (case (pair (string-value s1) (string-value s2))
        (string-equal? s1 s2))
      
      ;; Different constructors
      (case _ false))))

;; ============================================================================
;; VALUE CONVERSION
;; ============================================================================

;; Convert value to string representation
(define value->string
  (fn (v)
    (match v
      (case (constructor-value name args _)
        (if (null? args)
            name
            (string-append "(" name " " 
                          (join-strings (map value->string args) " ")
                          ")")))
      (case (closure-value params _ _)
        (string-append "<closure:" (join-strings params ",") ">"))
      (case (builtin-value name _ _)
        (string-append "<builtin:" name ">"))
      (case unit-value "unit")
      (case (string-value s) (string-append "\"" s "\""))
      (case _ "<value>"))))