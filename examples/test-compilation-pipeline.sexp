;; Test the compilation pipeline exists

;; Try to import the compiler modules
(import compiler pipeline)

;; Simple program to compile
(define test-program
  "(define add
     (fn (x y)
       (nat-elim (fn (_) Nat)
                 x
                 (fn (n rec) (succ rec))
                 y)))
   
   (define five (add (succ (succ zero)) (succ (succ (succ zero)))))
   
   (export add)
   (export five)")

;; Try to compile it
(perform (print "Testing compilation pipeline..."))

(define compile-result 
  (compile-from-string test-program 
    (compile-options
      target-javascript
      opt-full
      ".test-cache"
      "test-output.js"
      false
      true
      false
      false)))

(perform (print "Compilation completed!"))
(perform (print "Result:"))
(perform (print compile-result))