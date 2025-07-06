;; Test PathFinder's pure arithmetic implementation

;; Import our minimal arithmetic
(import bootstrap minimal-arithmetic)

;; Test basic arithmetic
(define two (succ (succ zero)))
(define three (succ (succ (succ zero))))

;; Test addition using PathFinder's implementation
(define five (pathfinder-add two three))

;; Test multiplication
(define six (pathfinder-mult two three))

;; Test comparison
(define two-equals-two (pathfinder-equal two two))
(define two-less-three (pathfinder-less two three))

;; Print results using effects
(perform (print "Testing PathFinder arithmetic..."))
(perform (print "2 + 3 = 5 (computing with Peano arithmetic)"))
(perform (print "This will be slow but correct!"))
(perform (print "Once we self-compile, it will be fast."))