;; Debug test to see actual values

;; Test basic values
(define my-zero zero)
(define one (succ zero))
(define two (succ one))

;; What do these actually look like?
(perform (print "=== Debugging PathFinder Values ==="))
(perform (print "What is zero?"))
(perform (print zero))
(perform (print "What is one?"))
(perform (print one))
(perform (print "What is two?"))
(perform (print two))

;; Test nat-elim directly
(perform (print ""))
(perform (print "Testing nat-elim on two:"))
(define nat-elim-result 
  (nat-elim (fn (_) Nat)
            zero           ; if n=0, return 0
            (fn (pred rec) (succ rec))  ; else return succ of recursive call
            two))          ; eliminate two

(perform (print "Result of nat-elim on two:"))
(perform (print nat-elim-result))

;; Test function application
(perform (print ""))
(perform (print "Testing function application:"))
(define identity (fn (x) x))
(define id-two (identity two))
(perform (print "identity(two) = "))
(perform (print id-two))

;; Test closure
(perform (print ""))
(perform (print "Testing closure:"))
(define make-constant (fn (x) (fn (y) x)))
(define always-two (make-constant two))
(define result (always-two zero))
(perform (print "always-two(zero) = "))
(perform (print result))