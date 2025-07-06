;; ============================================================================
;; MINIMAL ARITHMETIC BOOTSTRAP
;; ============================================================================
;; Pure HoTT arithmetic implementation for bootstrapping
;; This will be slow but correct - PathFinder will optimize it later

(import types types)
(import core foundations)
(import core eliminators)

;; ============================================================================
;; NATURAL NUMBER ARITHMETIC
;; ============================================================================

;; Addition using Peano recursion
(type pathfinder-add (-> Nat Nat Nat))
(define pathfinder-add
  (fn (x y)
    (nat-elim (fn (_) Nat)
              x
              (fn (n rec) (succ rec))
              y)))

;; Define the primitive operations that computational-primitives expects
(define plus-op pathfinder-add)
(define add-op pathfinder-add)

;; Multiplication using repeated addition
(type pathfinder-mult (-> Nat Nat Nat))
(define pathfinder-mult
  (fn (x y)
    (nat-elim (fn (_) Nat)
              zero
              (fn (n rec) (pathfinder-add x rec))
              y)))

(define times-op pathfinder-mult)
(define mult-op pathfinder-mult)

;; Subtraction (saturating at zero)
(type pathfinder-sub (-> Nat Nat Nat))
(define pathfinder-sub
  (fn (x y)
    (nat-elim (fn (_) Nat)
              x
              (fn (n rec) (pathfinder-pred rec))
              y)))

(define minus-op pathfinder-sub)
(define sub-op pathfinder-sub)

;; Predecessor
(type pathfinder-pred (-> Nat Nat))
(define pathfinder-pred
  (fn (n)
    (nat-elim (fn (_) Nat)
              zero
              (fn (m _) m)
              n)))

;; ============================================================================
;; COMPARISON OPERATIONS
;; ============================================================================

;; Equality check using double elimination
(type pathfinder-equal (-> Nat Nat Bool))
(define pathfinder-equal
  (fn (x y)
    (nat-elim (fn (x) (-> Nat Bool))
              ;; x is zero
              (nat-elim (fn (_) Bool)
                        true      ;; zero = zero
                        (fn (_ _) false)  ;; zero ≠ succ n
                        y)
              ;; x is succ n
              (fn (n rec-n)
                (nat-elim (fn (_) Bool)
                          false   ;; succ n ≠ zero
                          (fn (m _) (rec-n m))  ;; succ n = succ m iff n = m
                          y))
              x)))

(define equal-op pathfinder-equal)

;; Less than
(type pathfinder-less (-> Nat Nat Bool))
(define pathfinder-less
  (fn (x y)
    (nat-elim (fn (y) (-> Nat Bool))
              ;; y is zero: nothing is less than zero
              (fn (x) false)
              ;; y is succ m
              (fn (m rec-m)
                (nat-elim (fn (_) Bool)
                          true      ;; zero < succ m
                          (fn (n _) (rec-m n))  ;; succ n < succ m iff n < m
                          x))
              y)))

(define less-op pathfinder-less)

;; Less than or equal
(type pathfinder-leq (-> Nat Nat Bool))
(define pathfinder-leq
  (fn (x y)
    (bool-or (pathfinder-less x y)
             (pathfinder-equal x y))))

;; ============================================================================
;; BOOLEAN OPERATIONS
;; ============================================================================

;; Boolean OR
(type bool-or (-> Bool Bool Bool))
(define bool-or
  (fn (x y)
    (bool-elim (fn (_) Bool)
               y        ;; false ∨ y = y
               true     ;; true ∨ y = true
               x)))

;; Boolean AND
(type bool-and (-> Bool Bool Bool))
(define bool-and
  (fn (x y)
    (bool-elim (fn (_) Bool)
               false    ;; false ∧ y = false
               y        ;; true ∧ y = y
               x)))

;; ============================================================================
;; WRAPPER FUNCTIONS FOR RUST VM INTEGRATION
;; ============================================================================

;; When Rust returns symbolic arithmetic, evaluate it
(type evaluate-arithmetic (-> Value Value))
(define evaluate-arithmetic
  (fn (expr)
    (match expr
      ;; Handle symbolic addition
      (case (constructor "+" (list x y) _)
        (pathfinder-add (evaluate-arithmetic x) 
                       (evaluate-arithmetic y)))
      
      ;; Handle symbolic multiplication
      (case (constructor "*" (list x y) _)
        (pathfinder-mult (evaluate-arithmetic x)
                        (evaluate-arithmetic y)))
      
      ;; Handle symbolic subtraction
      (case (constructor "-" (list x y) _)
        (pathfinder-sub (evaluate-arithmetic x)
                       (evaluate-arithmetic y)))
      
      ;; Natural numbers pass through
      (case (constructor "zero" nil _) expr)
      (case (constructor "succ" args _) expr)
      
      ;; Nested arithmetic
      (case (constructor "add" (list x y) _)
        (pathfinder-add (evaluate-arithmetic x)
                       (evaluate-arithmetic y)))
      
      ;; Default: return as-is
      (case _ expr))))

;; ============================================================================
;; HELPER OPERATIONS
;; ============================================================================

;; Check if zero
(type is-zero? (-> Nat Bool))
(define is-zero?
  (fn (n)
    (nat-elim (fn (_) Bool)
              true
              (fn (_ _) false)
              n)))

;; Division operation (placeholder - complex to implement properly)
(type pathfinder-div (-> Nat Nat Nat))
(define pathfinder-div
  (fn (x y)
    zero))  ;; TODO: Implement proper division

(define divide-op pathfinder-div)

;; ============================================================================
;; ARITHMETIC WITH EVIDENCE (PLACEHOLDER)
;; ============================================================================

;; For now, just compute without evidence
;; PathFinder will generate optimized versions with evidence later
(define add-with-evidence pathfinder-add)
(define mult-with-evidence pathfinder-mult)
(define sub-with-evidence pathfinder-sub)

;; ============================================================================
;; EXPORTS
;; ============================================================================

;; Basic operations
(export pathfinder-add)
(export pathfinder-mult)
(export pathfinder-sub)
(export pathfinder-pred)
(export pathfinder-equal)
(export pathfinder-less)
(export pathfinder-leq)
(export pathfinder-div)

;; Operation aliases expected by computational-primitives
(export plus-op)
(export add-op)
(export times-op)
(export mult-op)
(export minus-op)
(export sub-op)
(export divide-op)
(export equal-op)
(export less-op)

;; Helper functions
(export is-zero?)
(export bool-or)
(export bool-and)
(export evaluate-arithmetic)

;; Evidence versions
(export add-with-evidence)
(export mult-with-evidence)
(export sub-with-evidence)