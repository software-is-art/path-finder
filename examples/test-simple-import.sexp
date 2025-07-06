;; Test simple import without dependencies

;; Import simple arithmetic
(import (bootstrap simple-arithmetic))

;; Define some numbers
(define two (succ (succ zero)))
(define three (succ (succ (succ zero))))

;; Use imported function
(define sum (simple-add two three))

;; Print results
(perform (print "Simple import test:"))
(perform (print "Imported simple-arithmetic successfully"))
(perform (print "Created symbolic sum"))