;; Simple test to verify PathFinder is working

;; Test basic values
(define x (succ (succ zero)))
(define y (succ zero))

;; Test that built-in arithmetic returns symbolic values
(define sum (+ x y))

;; Print to verify
(perform (print "PathFinder minimal VM test"))
(perform (print "x = 2 (succ (succ zero))"))
(perform (print "y = 1 (succ zero)"))
(perform (print "x + y should return symbolic constructor"))