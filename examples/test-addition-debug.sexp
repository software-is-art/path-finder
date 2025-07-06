;; Debug addition computation

(define one (succ zero))
(define two (succ one))
(define three (succ two))

;; Define addition
(define add
  (fn (x y)
    (nat-elim (fn (_) Nat)
              x             ; base: x + 0 = x
              (fn (n rec) (succ rec))  ; step: x + (n+1) = succ(x + n)
              y)))          ; eliminate on y

(perform (print "Testing addition step by step:"))
(perform (print ""))

;; Test 2 + 0
(define two-plus-zero (add two zero))
(perform (print "2 + 0 = "))
(perform (print two-plus-zero))

;; Test 2 + 1
(define two-plus-one (add two one))
(perform (print ""))
(perform (print "2 + 1 = "))
(perform (print two-plus-one))

;; Test 2 + 3
(define two-plus-three (add two three))
(perform (print ""))
(perform (print "2 + 3 = "))
(perform (print two-plus-three))

;; Let's manually trace what should happen for 2 + 1:
;; nat-elim Nat 2 (fn n rec => succ rec) 1
;; Since 1 = succ zero, we get:
;; step 0 (nat-elim Nat 2 (fn...) 0) = succ 2 = 3

(perform (print ""))
(perform (print "Expected: 2 + 1 = succ(succ(succ(zero))) = 3"))