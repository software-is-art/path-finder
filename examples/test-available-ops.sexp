;; Test what operations are available in minimal VM

;; Natural numbers work
(define two (succ (succ zero)))
(define three (succ (succ (succ zero))))

;; Try the + operator (should return symbolic constructor)
(define sum-symbolic (+ two three))

;; Effects should still work
(perform (print "Minimal VM Test Results:"))
(perform (print "- Created natural numbers using succ/zero"))
(perform (print "- The + operator should return a symbolic constructor"))
(perform (print "- Not actual addition, just the AST node"))
(perform (print "Success!"))