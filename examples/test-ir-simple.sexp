;; ============================================================================
;; SIMPLE IR TEST EXAMPLE
;; ============================================================================
;; Tests the basic IR translation and optimization pipeline

;; Simple arithmetic function
(define add-two
  (fn (n)
    (succ (succ n))))

;; Constant that should be folded
(define five
  (add-two (succ (succ (succ zero)))))

;; Boolean function
(define is-zero
  (fn (n)
    (nat-elim (fn (_) Bool)
              true           ;; zero is zero
              (fn (_ _) false) ;; succ n is not zero
              n)))

;; Test constant folding in conditionals
(define test-constant-if
  (if (is-zero zero)
      (succ zero)      ;; This branch should be selected
      zero))           ;; This branch should be eliminated

;; Test with let binding
(define test-let
  (let x (succ (succ zero))
    (let y (succ x)
      (add-two y))))

;; Effect example
(define test-effect
  (begin
    (perform (print "Testing IR compilation"))
    (perform (print "Constant folding should optimize this"))
    five))

;; Export our definitions
(export add-two)
(export five)
(export is-zero)
(export test-constant-if)
(export test-let)
(export test-effect)