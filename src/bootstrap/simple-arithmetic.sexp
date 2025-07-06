;; ============================================================================
;; SIMPLE ARITHMETIC FOR BOOTSTRAP TESTING
;; ============================================================================
;; No imports, uses only primitives available in bootstrap

;; Simple addition - just creates a symbolic constructor
(define simple-add
  (fn (x y)
    (cons 'add (cons x (cons y nil)))))

;; Simple multiplication - symbolic
(define simple-mult
  (fn (x y)
    (cons 'mult (cons x (cons y nil)))))

;; Export our functions
(export simple-add)
(export simple-mult)