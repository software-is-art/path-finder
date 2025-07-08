;; ============================================================================
;; EVIDENCE PROPAGATION SYSTEM
;; ============================================================================
;; Rules for how evidence flows through computational operations
;; Ensures evidence is preserved and composed correctly

(import types types)
(import effects computation-as-effect)
(import core ast)

;; ============================================================================
;; EVIDENCE PROPAGATION RULES
;; ============================================================================

;; Rule: Sequential composition combines evidence
(type sequence-evidence-rule 
  (-> Evidence Evidence Evidence))
(define sequence-evidence-rule
  (fn (ev1 ev2)
    (match (pair ev1 ev2)
      ;; Termination: sum of steps
      (case (pair (termination-evidence-ast (always-terminates n1))
                  (termination-evidence-ast (always-terminates n2)))
        (termination-evidence-ast (always-terminates (+ n1 n2))))
      
      ;; Complexity: compose complexities
      (case (pair (complexity-evidence-ast c1)
                  (complexity-evidence-ast c2))
        (complexity-evidence-ast (compose-complexity c1 c2)))
      
      ;; Combined evidence
      (case _ 
        (combined-evidence-ast (list ev1 ev2))))))

;; Rule: Parallel composition takes maximum
(type parallel-evidence-rule
  (-> Evidence Evidence Evidence))
(define parallel-evidence-rule
  (fn (ev1 ev2)
    (match (pair ev1 ev2)
      ;; Termination: max of steps
      (case (pair (termination-evidence-ast (always-terminates n1))
                  (termination-evidence-ast (always-terminates n2)))
        (termination-evidence-ast (always-terminates (max n1 n2))))
      
      ;; Space: sum (both execute simultaneously)
      (case (pair (space-evidence-ast (constant-space s1))
                  (space-evidence-ast (constant-space s2)))
        (space-evidence-ast (constant-space (+ s1 s2))))
      
      ;; Combined
      (case _ 
        (combined-evidence-ast (list ev1 ev2))))))

;; Rule: Function application evidence
(type application-evidence-rule
  (-> Evidence Evidence Evidence))
(define application-evidence-rule
  (fn (func-ev arg-ev)
    (combined-evidence-ast
      (list
        ;; Function evaluation
        func-ev
        ;; Argument evaluation  
        arg-ev
        ;; Application step
        (termination-evidence-ast (always-terminates 1))
        ;; Stack frame
        (space-evidence-ast (constant-space 1))))))

;; ============================================================================
;; COMPLEXITY COMPOSITION
;; ============================================================================

;; Compose complexity bounds
(type compose-complexity (-> ComplexityBound ComplexityBound ComplexityBound))
(define compose-complexity
  (fn (c1 c2)
    (match (pair c1 c2)
      ;; O(1) + O(1) = O(1)
      (case (pair constant-time constant-time)
        constant-time)
      
      ;; O(1) + O(n) = O(n)
      (case (pair constant-time linear-time)
        linear-time)
      (case (pair linear-time constant-time)
        linear-time)
      
      ;; O(n) + O(n) = O(n)
      (case (pair linear-time linear-time)
        linear-time)
      
      ;; O(log n) + O(1) = O(log n)
      (case (pair logarithmic-time constant-time)
        logarithmic-time)
      (case (pair constant-time logarithmic-time)
        logarithmic-time)
      
      ;; O(n^k) + O(n^j) = O(n^max(k,j))
      (case (pair (polynomial-time k) (polynomial-time j))
        (polynomial-time (max k j)))
      
      ;; Default: take worse complexity
      (case _ (worse-complexity c1 c2)))))

;; ============================================================================
;; EVIDENCE INFERENCE FOR COMMON PATTERNS
;; ============================================================================

;; Infer evidence for arithmetic operations
(type arithmetic-operation-evidence (-> String (List Evidence) Evidence))
(define arithmetic-operation-evidence
  (fn (op arg-evidences)
    (match op
      ;; Addition/multiplication are constant time
      (case "+" (constant-arithmetic-evidence))
      (case "*" (constant-arithmetic-evidence))
      (case "-" (constant-arithmetic-evidence))
      
      ;; Division might fail (evidence includes possibility)
      (case "/" (division-evidence arg-evidences))
      
      ;; Modulo similar to division
      (case "%" (division-evidence arg-evidences))
      
      ;; Comparisons are constant
      (case "<" (constant-arithmetic-evidence))
      (case ">" (constant-arithmetic-evidence))
      (case "=" (constant-arithmetic-evidence))
      
      ;; Unknown operation
      (case _ (unknown-operation-evidence op)))))

;; Evidence for constant-time arithmetic
(type constant-arithmetic-evidence (-> Evidence))
(define constant-arithmetic-evidence
  (fn ()
    (combined-evidence-ast
      (list
        (termination-evidence-ast (always-terminates 1))
        (complexity-evidence-ast constant-time)
        (space-evidence-ast (constant-space 0))
        (determinism-evidence-ast fully-deterministic)))))

;; Evidence for division (might fail)
(type division-evidence (-> (List Evidence) Evidence))
(define division-evidence
  (fn (arg-evs)
    (combined-evidence-ast
      (list
        (termination-evidence-ast (always-terminates 1))
        (complexity-evidence-ast constant-time)
        (space-evidence-ast (constant-space 0))
        (determinism-evidence-ast fully-deterministic)
        ;; Division by zero possibility
        (correctness-evidence-ast (requires-nonzero (second arg-evs)))))))

;; ============================================================================
;; EVIDENCE FOR CONTROL FLOW
;; ============================================================================

;; Pattern matching evidence
(type match-evidence (-> (List (Pair Pattern Evidence)) Evidence))
(define match-evidence
  (fn (cases)
    (combined-evidence-ast
      (list
        ;; Termination: max of all branches
        (termination-evidence-ast
          (max-termination (map (fn (c) (extract-termination (snd c))) cases)))
        ;; Complexity: O(number of patterns) + max branch complexity
        (complexity-evidence-ast
          (match-complexity (length cases) (map (fn (c) (extract-complexity (snd c))) cases)))
        ;; Space: max of all branches
        (space-evidence-ast
          (max-space (map (fn (c) (extract-space (snd c))) cases)))))))

;; Let binding evidence
(type let-binding-evidence (-> String Evidence))
(define let-binding-evidence
  (fn (var)
    (combined-evidence-ast
      (list
        ;; Binding is constant time
        (termination-evidence-ast (always-terminates 1))
        (complexity-evidence-ast constant-time)
        ;; One stack slot for binding
        (space-evidence-ast (constant-space 1))))))

;; ============================================================================
;; EVIDENCE EXTRACTION
;; ============================================================================

;; Extract termination bound from evidence
(type extract-termination (-> Evidence TerminationBound))
(define extract-termination
  (fn (ev)
    (match ev
      (case (termination-evidence-ast t) t)
      (case (combined-evidence-ast evs)
        (fold-termination (map extract-termination evs)))
      (case _ structurally-decreasing))))

;; Extract complexity from evidence
(type extract-complexity (-> Evidence ComplexityBound))
(define extract-complexity
  (fn (ev)
    (match ev
      (case (complexity-evidence-ast c) c)
      (case (combined-evidence-ast evs)
        (fold-complexity (map extract-complexity evs)))
      (case _ linear-time))))

;; Extract space from evidence
(type extract-space (-> Evidence SpaceBound))
(define extract-space
  (fn (ev)
    (match ev
      (case (space-evidence-ast s) s)
      (case (combined-evidence-ast evs)
        (fold-space (map extract-space evs)))
      (case _ linear-space))))

;; ============================================================================
;; EVIDENCE VALIDATION
;; ============================================================================

;; Check if evidence is consistent
(type validate-evidence (-> Evidence Bool))
(define validate-evidence
  (fn (ev)
    (match ev
      ;; Check basic constraints
      (case (termination-evidence-ast (always-terminates n))
        (>= n 0))
      
      ;; Check complexity bounds
      (case (complexity-evidence-ast (polynomial-time degree))
        (>= degree 0))
      
      ;; Validate combined evidence
      (case (combined-evidence-ast evs)
        (all validate-evidence evs))
      
      ;; Default: assume valid
      (case _ true))))

;; ============================================================================
;; EVIDENCE OPTIMIZATION
;; ============================================================================

;; Simplify redundant evidence
(type simplify-evidence (-> Evidence Evidence))
(define simplify-evidence
  (fn (ev)
    (match ev
      ;; Flatten nested combined evidence
      (case (combined-evidence-ast evs)
        (combined-evidence-ast (flatten-evidence evs)))
      
      ;; Remove duplicate evidence
      (case _ ev))))

;; Flatten evidence lists
(type flatten-evidence (-> (List Evidence) (List Evidence)))
(define flatten-evidence
  (fn (evs)
    (flat-map
      (fn (ev)
        (match ev
          (case (combined-evidence-ast inner) inner)
          (case _ (list ev))))
      evs)))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export sequence-evidence-rule)
(export parallel-evidence-rule)
(export application-evidence-rule)
(export arithmetic-operation-evidence)
(export match-evidence)
(export let-binding-evidence)
(export extract-termination)
(export extract-complexity)
(export extract-space)
(export validate-evidence)
(export simplify-evidence)