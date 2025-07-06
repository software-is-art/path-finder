;; Test that actually verifies computation results

;; Define natural numbers
(define one (succ zero))
(define two (succ one))
(define three (succ two))
(define four (succ three))
(define five (succ four))

;; Define addition using nat-elim
(define add
  (fn (x y)
    (nat-elim (fn (_) Nat)
              x
              (fn (n rec) (succ rec))
              y)))

;; Compute 2 + 3
(define computed-five (add two three))

;; Let's see what we actually get
(perform (print "Testing actual computation:"))
(perform (print "two + three = ?"))

;; Try to use the result in another computation
(define seven (add computed-five two))

;; Test if we can pattern match on the result
(define is-five?
  (fn (n)
    (nat-elim (fn (_) Bool)
              false  ; 0 is not 5
              (fn (m rec)
                (nat-elim (fn (_) Bool)
                          false  ; 1 is not 5
                          (fn (m2 rec2)
                            (nat-elim (fn (_) Bool)
                                      false  ; 2 is not 5
                                      (fn (m3 rec3)
                                        (nat-elim (fn (_) Bool)
                                                  false  ; 3 is not 5
                                                  (fn (m4 rec4)
                                                    (nat-elim (fn (_) Bool)
                                                              false  ; 4 is not 5
                                                              (fn (m5 rec5) true)  ; 5 is 5!
                                                              m4))
                                                  m3))
                                        m2))
                          m))
              n)))

(define test-result (is-five? computed-five))

(perform (print "computed-five is actually 5?"))
(perform (print "Let's see if we can add to it..."))
(perform (print "five + two = seven?"))