;; Test importing PathFinder arithmetic module

;; Import the minimal arithmetic module
(import (bootstrap minimal-arithmetic))

;; Now we should have access to PathFinder arithmetic functions
(define two (succ (succ zero)))
(define three (succ (succ (succ zero))))

;; Test PathFinder addition
(define five (pathfinder-add two three))

;; Print to confirm it works
(perform (print "Testing module import:"))
(perform (print "Successfully imported minimal-arithmetic"))
(perform (print "2 + 3 = 5 (using PathFinder arithmetic)"))