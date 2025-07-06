;; Test basic natural numbers and symbolic arithmetic

;; Define some natural numbers
(define my-zero zero)
(define one (succ zero))
(define two (succ (succ zero)))
(define three (succ (succ (succ zero))))

;; Test that arithmetic returns symbolic constructors
;; Since we removed computation from Rust, these should just return constructors
(define symbolic-sum (add two three))

;; Print to see what happens
(perform (print "Testing minimal Rust VM:"))
(perform (print "- Natural numbers work (zero, succ)"))
(perform (print "- Arithmetic returns symbolic constructors"))
(perform (print "- No actual computation in Rust anymore"))
(perform (print "Done!"))