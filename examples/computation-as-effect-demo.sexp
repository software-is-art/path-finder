;; ============================================================================
;; COMPUTATION AS EFFECT DEMONSTRATION
;; ============================================================================
;; Shows how all computation carries evidence in PathFinder

(import core computational-primitives)
(import core evaluator-computation)
(import effects computation-as-effect)

;; ============================================================================
;; EXAMPLE 1: SIMPLE ARITHMETIC WITH EVIDENCE
;; ============================================================================

;; Even 2 + 2 carries computational evidence!
(define simple-addition
  (plus-computation (nat-value 2) (nat-value 2)))

;; Extract and examine the evidence
(define addition-evidence
  (extract-evidence (run-with-evidence simple-addition)))

;; Query computational properties
(define addition-properties
  (list
    (cons "Termination" (extract-termination addition-evidence))
    (cons "Complexity" (extract-complexity addition-evidence))
    (cons "Space" (extract-space addition-evidence))
    (cons "Deterministic?" (is-deterministic? addition-evidence))))

;; Result: 
;; - Terminates in 1 step
;; - O(1) time complexity
;; - O(1) space complexity  
;; - Fully deterministic

;; ============================================================================
;; EXAMPLE 2: FUNCTION APPLICATION WITH EVIDENCE
;; ============================================================================

;; Define a function that doubles a number
(define double
  (make-closure-value 
    (list "x")
    (app (app (var "+") (var "x")) (var "x"))
    empty-env))

;; Apply the function - this builds evidence!
(define double-application
  (apply-computation
    (lift-to-computation double)
    (lift-to-computation (nat-value 5))
    (application-evidence double (nat-value 5))))

;; The evidence shows:
;; - Function application terminates
;; - Complexity includes function call overhead
;; - Stack space used for call frame

;; ============================================================================
;; EXAMPLE 3: RECURSIVE FUNCTION WITH TERMINATION EVIDENCE
;; ============================================================================

;; Factorial with evidence of termination
(define factorial
  (fix-computation
    (fn (fact)
      (make-closure-value
        (list "n")
        (match-expr (var "n")
          (list
            (match-case (constructor-pattern "zero" nil)
              (literal (nat-value 1)))
            (match-case (constructor-pattern "succ" (list "m"))
              (app (app (var "*") 
                        (app (var "n"))
                        (app fact (var "m")))))))))
    ;; Termination evidence: structural recursion on Nat
    (structural-termination)))

;; Computing factorial(5) carries evidence
(define fact-5
  (apply-computation
    (lift-to-computation factorial)
    (lift-to-computation (nat-value 5))
    (factorial-evidence 5)))

;; Evidence includes:
;; - Proof that recursion terminates (decreasing on n)
;; - O(n) time complexity
;; - O(n) stack space

;; ============================================================================
;; EXAMPLE 4: ERROR HANDLING WITH EVIDENCE
;; ============================================================================

;; Division by zero carries evidence of the error
(define div-by-zero
  (divide-computation (nat-value 10) (nat-value 0)))

;; The evidence shows:
;; - Computation fails (does not terminate normally)
;; - Error type: Division by zero
;; - Point of failure in computation

;; Safe division with evidence
(define safe-divide
  (fn (x y)
    (match-computation
      (equal-computation y (nat-value 0))
      (list
        (match-case (constructor-pattern "true" nil)
          (error-computation "Division by zero"
            (division-error-evidence x y)))
        (match-case (constructor-pattern "false" nil)
          (divide-computation x y)))
      (exhaustiveness-proof)
      (match-evidence))))

;; ============================================================================
;; EXAMPLE 5: OPTIMIZATION VIA EVIDENCE
;; ============================================================================

;; Constant expression that can be optimized
(define constant-expr
  (plus-computation
    (times-computation (nat-value 3) (nat-value 4))
    (minus-computation (nat-value 10) (nat-value 5))))

;; The optimizer can:
;; 1. See all sub-expressions are constant (via evidence)
;; 2. Prove the computation is deterministic
;; 3. Replace with pre-computed value

(define optimized-expr
  (optimize-computation constant-expr))
;; Result: (value-computation (nat-value 17) (constant-folded-evidence))

;; ============================================================================
;; EXAMPLE 6: PARALLEL COMPUTATION WITH INDEPENDENCE
;; ============================================================================

;; Two independent computations
(define comp1 (factorial-computation (nat-value 10)))
(define comp2 (fibonacci-computation (nat-value 15)))

;; Sequential execution
(define sequential
  (bind-computation comp1
    (fn (r1)
      (bind-computation comp2
        (fn (r2)
          (pair-computation r1 r2))))))

;; Parallel execution (optimizer detects independence)
(define parallel
  (parallel-computation comp1 comp2
    (independence-proof comp1 comp2)))

;; Evidence shows:
;; - Parallel version has better time complexity
;; - Space usage is sum (both execute at once)
;; - Result is provably the same

;; ============================================================================
;; EXAMPLE 7: EVIDENCE QUERIES
;; ============================================================================

;; Users can query computational properties
(define fibonacci-20-info
  (let ((comp (fibonacci-computation (nat-value 20))))
    (list
      (cons "Time complexity" (complexity comp))         ;; O(2^n)
      (cons "Space complexity" (space-usage comp))      ;; O(n)
      (cons "Terminates?" (definitely-terminates? comp)) ;; true
      (cons "Deterministic?" (is-deterministic? comp))  ;; true
      (cons "Can parallelize?" (has-parallelism? comp)) ;; true (recursive calls)
      (cons "Cache benefit?" (worth-caching? comp)))))   ;; true (deterministic)

;; ============================================================================
;; EXAMPLE 8: EVIDENCE-BASED CONTRACTS
;; ============================================================================

;; Function with complexity requirements
(define quick-sort
  (with-evidence-contract
    (requires (complexity-bound (n-log-n-complexity)))
    (ensures (sorted? output))
    (fn (lst)
      (match lst
        (case nil (value-computation nil))
        (case (cons pivot rest)
          (let ((smaller (filter (less-than? pivot) rest))
                (larger (filter (greater-than? pivot) rest)))
            (append-computation
              (quick-sort smaller)
              (cons-computation pivot (quick-sort larger)))))))))

;; Contract violation if complexity exceeds O(n log n)
;; This is checked at compile time using evidence!

;; ============================================================================
;; SUMMARY
;; ============================================================================

;; In PathFinder with computation-as-effect:
;;
;; 1. Every operation builds evidence
;; 2. Evidence enables optimization
;; 3. Properties are queryable
;; 4. Contracts are enforceable
;; 5. Errors carry proof of failure
;; 6. Recursion requires termination proof
;; 7. Parallelism requires independence proof
;;
;; This makes PathFinder the first language where
;; computation itself is a first-class mathematical object!