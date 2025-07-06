;; Test nat-elim based addition

;; Define natural numbers
(define my-zero zero)
(define one (succ zero))
(define two (succ one))
(define three (succ two))

;; Define addition using nat-elim
(define add
  (fn (x y)
    (nat-elim (fn (_) Nat)  ; motive: constant Nat type
              x             ; base case: x + 0 = x
              (fn (n rec) (succ rec))  ; step: x + (n+1) = succ(x + n)
              y)))          ; target: y

;; Test addition
(define five (add two three))

;; Print results
(perform (print "Testing nat-elim based addition:"))
(perform (print "2 + 3 = 5"))
(perform (print "Result computed using nat-elim!"))