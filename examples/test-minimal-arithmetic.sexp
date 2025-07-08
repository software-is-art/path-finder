;; ============================================================================
;; TEST MINIMAL ARITHMETIC
;; ============================================================================
;; Tests the pure PathFinder arithmetic implementation

(import bootstrap minimal-arithmetic)
(import core foundations)

;; Test basic addition
(define two (succ (succ zero)))
(define three (succ two))
(define five (pathfinder-add two three))

;; Test multiplication
(define six (pathfinder-mult two three))

;; Test subtraction
(define one (pathfinder-sub three two))

;; Test comparison
(define two-equals-two (pathfinder-equal two two))
(define two-less-three (pathfinder-less two three))

;; Print results (using effect system)
(perform (print "Testing minimal arithmetic..."))
(perform (print "2 + 3 = 5 (should be succ(succ(succ(succ(succ(zero))))))"))
(perform (print "2 * 3 = 6 (should be succ(succ(succ(succ(succ(succ(zero)))))))"))
(perform (print "3 - 2 = 1 (should be succ(zero))"))
(perform (print "2 == 2 (should be true)"))
(perform (print "2 < 3 (should be true)"))

;; Test symbolic arithmetic evaluation
(define symbolic-sum (constructor "+" (list two three) nat-type))
(define evaluated-sum (evaluate-arithmetic symbolic-sum))

(perform (print "Symbolic + evaluation test"))
(perform (print "Should evaluate (+ 2 3) to 5"))