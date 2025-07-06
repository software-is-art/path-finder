;; ============================================================================
;; COMPUTATIONAL PRIMITIVES
;; ============================================================================
;; Every primitive operation returns a computation with evidence
;; This replaces direct primitive operations with evidence-carrying versions

(import types types)
(import effects computation-as-effect)
(import evaluator values-computation)
(import core evidence-propagation)

;; ============================================================================
;; ARITHMETIC PRIMITIVES WITH EVIDENCE
;; ============================================================================

;; Addition with evidence
(type plus-computation (-> Value Value (Computation Value)))
(define plus-computation
  (fn (x y)
    (arithmetic-computation
      plus-op
      (list (lift-to-computation x) (lift-to-computation y))
      (make-arith-evidence
        (arithmetic-correctness-proof plus-op x y)
        no-overflow
        (constant-complexity 1)))))

;; Subtraction with evidence (handles underflow)
(type minus-computation (-> Value Value (Computation Value)))
(define minus-computation
  (fn (x y)
    (arithmetic-computation
      minus-op
      (list (lift-to-computation x) (lift-to-computation y))
      (make-arith-evidence
        (arithmetic-correctness-proof minus-op x y)
        (check-underflow x y)
        (constant-complexity 1)))))

;; Multiplication with evidence
(type times-computation (-> Value Value (Computation Value)))
(define times-computation
  (fn (x y)
    (arithmetic-computation
      times-op
      (list (lift-to-computation x) (lift-to-computation y))
      (make-arith-evidence
        (arithmetic-correctness-proof times-op x y)
        (check-overflow-mult x y)
        (multiplication-complexity x y)))))

;; Division with evidence (handles division by zero)
(type divide-computation (-> Value Value (Computation Value)))
(define divide-computation
  (fn (x y)
    (match (is-zero? y)
      (case true
        (error-computation "Division by zero"
          (division-error-evidence x y)))
      (case false
        (arithmetic-computation
          divide-op
          (list (lift-to-computation x) (lift-to-computation y))
          (make-arith-evidence
            (division-correctness-proof x y)
            no-overflow
            (division-complexity)))))))

;; ============================================================================
;; COMPARISON PRIMITIVES WITH EVIDENCE
;; ============================================================================

;; Equality with evidence
(type equal-computation (-> Value Value (Computation Value)))
(define equal-computation
  (fn (x y)
    (comparison-computation
      equal-op
      x y
      (make-comparison-evidence
        (equality-decidable x y)
        (equality-complexity (type-of x))))))

;; Less-than with evidence
(type less-than-computation (-> Value Value (Computation Value)))
(define less-than-computation
  (fn (x y)
    (comparison-computation
      less-op
      x y
      (make-comparison-evidence
        (ordering-decidable x y)
        (constant-complexity 1)))))

;; ============================================================================
;; BOOLEAN PRIMITIVES WITH EVIDENCE
;; ============================================================================

;; Boolean AND with evidence
(type and-computation (-> Value Value (Computation Value)))
(define and-computation
  (fn (x y)
    (match (extract-raw x)
      ;; Short-circuit evaluation evidence
      (case (raw-constructor "false" _ _)
        (value-computation false-value
          (short-circuit-evidence "and" x)))
      (case _
        (boolean-computation
          and-op x y
          (and-evidence x y))))))

;; Boolean OR with evidence  
(type or-computation (-> Value Value (Computation Value)))
(define or-computation
  (fn (x y)
    (match (extract-raw x)
      ;; Short-circuit evaluation evidence
      (case (raw-constructor "true" _ _)
        (value-computation true-value
          (short-circuit-evidence "or" x)))
      (case _
        (boolean-computation
          or-op x y
          (or-evidence x y))))))

;; Boolean NOT with evidence
(type not-computation (-> Value (Computation Value)))
(define not-computation
  (fn (x)
    (boolean-computation
      not-op (list x)
      (not-evidence x))))

;; ============================================================================
;; LIST PRIMITIVES WITH EVIDENCE
;; ============================================================================

;; Cons with evidence
(type cons-computation (-> Value Value (Computation Value)))
(define cons-computation
  (fn (head tail)
    (constructor-computation
      "cons"
      (list (lift-to-computation head) 
            (lift-to-computation tail))
      (cons-evidence head tail))))

;; Head with evidence (handles empty list)
(type head-computation (-> Value (Computation Value)))
(define head-computation
  (fn (lst)
    (match (extract-raw lst)
      (case (raw-constructor "nil" _ _)
        (error-computation "Head of empty list"
          (empty-list-evidence "head")))
      (case (raw-constructor "cons" (list h _) _)
        (value-computation h
          (head-evidence lst))))))

;; Tail with evidence
(type tail-computation (-> Value (Computation Value)))
(define tail-computation
  (fn (lst)
    (match (extract-raw lst)
      (case (raw-constructor "nil" _ _)
        (error-computation "Tail of empty list"
          (empty-list-evidence "tail")))
      (case (raw-constructor "cons" (list _ t) _)
        (value-computation t
          (tail-evidence lst))))))

;; List length with evidence
(type length-computation (-> Value (Computation Value)))
(define length-computation
  (fn (lst)
    (match (extract-raw lst)
      (case (raw-constructor "nil" _ _)
        (value-computation zero-value
          (length-base-evidence)))
      (case (raw-constructor "cons" (list _ t) _)
        (bind-computation
          (length-computation t)
          (fn (t-length)
            (succ-computation t-length))
          (length-inductive-evidence lst))))))

;; ============================================================================
;; FUNCTION PRIMITIVES WITH EVIDENCE
;; ============================================================================

;; Function composition with evidence
(type compose-computation (-> Value Value (Computation Value)))
(define compose-computation
  (fn (f g)
    (value-computation
      (make-closure-value
        (list "x")
        (app (lift-value f) (app (lift-value g) (var "x")))
        empty-env)
      (composition-evidence f g))))

;; Identity function with evidence
(type identity-computation (-> Type (Computation Value)))
(define identity-computation
  (fn (type)
    (value-computation
      (make-closure-value
        (list "x")
        (var "x")
        empty-env)
      (identity-evidence type))))

;; ============================================================================
;; EVIDENCE CONSTRUCTION HELPERS
;; ============================================================================

;; Lift a value to a trivial computation
(type lift-to-computation (-> Value (Computation Value)))
(define lift-to-computation
  (fn (v)
    (value-computation v
      (trivial-evidence))))

;; Make arithmetic evidence
(type make-arith-evidence (-> Proof OverflowBehavior ComplexityEvidence ArithmeticEvidence))
(define make-arith-evidence
  (fn (correctness overflow complexity)
    (arith-evidence correctness overflow complexity)))

;; Make comparison evidence
(type make-comparison-evidence (-> Proof ComplexityEvidence ComparisonEvidence))
(define make-comparison-evidence
  (fn (decidability complexity)
    (comp-evidence decidability complexity)))

;; ============================================================================
;; COMPLEXITY HELPERS
;; ============================================================================

;; Multiplication complexity (could be O(n²) for big numbers)
(type multiplication-complexity (-> Value Value ComplexityEvidence))
(define multiplication-complexity
  (fn (x y)
    (if (and (is-small-nat? x) (is-small-nat? y))
        (constant-complexity 1)
        (polynomial-complexity 2))))

;; Division complexity
(type division-complexity (-> ComplexityEvidence))
(define division-complexity
  (fn ()
    (logarithmic-complexity)))

;; ============================================================================
;; ERROR EVIDENCE
;; ============================================================================

;; Evidence for computation errors
(type error-computation (-> String Evidence (Computation Value)))
(define error-computation
  (fn (msg evidence)
    (bind-computation
      (io-computation
        (error-primitive msg)
        (error-io-evidence msg))
      (fn (_)
        (value-computation
          (error-value msg)
          evidence))
      (error-bind-evidence))))

;; ============================================================================
;; PRIMITIVE OPERATION TABLE
;; ============================================================================

;; Map primitive names to computations
(type primitive-computation (-> String (Computation Value)))
(define primitive-computation
  (fn (name)
    (match name
      (case "+" (binary-primitive plus-computation))
      (case "-" (binary-primitive minus-computation))
      (case "*" (binary-primitive times-computation))
      (case "/" (binary-primitive divide-computation))
      (case "=" (binary-primitive equal-computation))
      (case "<" (binary-primitive less-than-computation))
      (case "and" (binary-primitive and-computation))
      (case "or" (binary-primitive or-computation))
      (case "not" (unary-primitive not-computation))
      (case "cons" (binary-primitive cons-computation))
      (case "head" (unary-primitive head-computation))
      (case "tail" (unary-primitive tail-computation))
      (case "length" (unary-primitive length-computation))
      (case "compose" (binary-primitive compose-computation))
      (case "identity" (nullary-primitive identity-computation))
      (case _ (error-computation 
                (string-append "Unknown primitive: " name)
                (unknown-primitive-evidence name))))))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export plus-computation)
(export minus-computation)
(export times-computation)
(export divide-computation)
(export equal-computation)
(export less-than-computation)
(export and-computation)
(export or-computation)
(export not-computation)
(export cons-computation)
(export head-computation)
(export tail-computation)
(export length-computation)
(export compose-computation)
(export identity-computation)
(export primitive-computation)
(export lift-to-computation)
(export error-computation)