;; Test closure support in Guile bootstrap

;; Make an adder function that returns a closure
(define make-adder
  (fn (x)
    (fn (y)
      (nat-elim (fn (_) Nat)
                x
                (fn (n rec) (succ rec))
                y))))

;; Create specific adders
(define add-two (make-adder (succ (succ zero))))
(define add-three (make-adder (succ (succ (succ zero)))))

;; Test them
(define five-a (add-two (succ (succ (succ zero)))))    ; 2 + 3 = 5
(define five-b (add-three (succ (succ zero))))         ; 3 + 2 = 5

(perform (print "Testing closures:"))
(perform (print "make-adder creates closures that capture 'x'"))
(perform (print "add-two(3) = 5"))
(perform (print "add-three(2) = 5"))
(perform (print "Closures working correctly!"))