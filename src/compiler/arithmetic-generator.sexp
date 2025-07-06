;; ============================================================================
;; ARITHMETIC CODE GENERATOR
;; ============================================================================
;; Generates optimized Rust code for arithmetic operations
;; Transforms slow Peano arithmetic into efficient machine arithmetic

(import types types)
(import compiler rust-ast)
(import compiler code-generator)
(import compiler evidence-compiler)
(import bootstrap minimal-arithmetic)
(import core foundations)

;; ============================================================================
;; ARITHMETIC OPTIMIZATION PATTERNS
;; ============================================================================

;; Generate optimized natural number representation
(type generate-optimized-nat (-> RustItem))
(define generate-optimized-nat
  (fn ()
    (rust-enum-def "OptimizedNat"
                  (list
                    ;; Small numbers use machine integers
                    (rust-variant "Small" (list rust-usize))
                    ;; Large numbers use Peano representation
                    (rust-variant "Large" (list (rust-box (rust-named "HottValue"))))))))

;; Generate conversion from HoTT Nat to OptimizedNat
(type generate-nat-converter (-> RustItem))
(define generate-nat-converter
  (fn ()
    (make-rust-fn
      "hott_to_optimized"
      (list (pair "n" (rust-ref (rust-named "HottValue"))))
      (rust-named "OptimizedNat")
      (rust-block
        (list
          ;; Count the depth of Succ constructors
          (rust-let (rust-var-pat "depth")
                   (some rust-usize)
                   (rust-call (rust-var "count_successors")
                             (list (rust-var "n"))))
          ;; If small enough, use machine representation
          (rust-return
            (rust-if (rust-binop "<" (rust-var "depth") (rust-usize-lit 1000))
                    (rust-enum "OptimizedNat" "Small" 
                              (list (rust-var "depth")))
                    (rust-enum "OptimizedNat" "Large"
                              (list (rust-box-new (rust-clone (rust-var "n"))))))))))))

;; ============================================================================
;; OPTIMIZED ARITHMETIC OPERATIONS
;; ============================================================================

;; Generate optimized addition
(type generate-optimized-add (-> RustItem))
(define generate-optimized-add
  (fn ()
    (make-rust-fn
      "optimized_add"
      (list (pair "x" (rust-ref (rust-named "OptimizedNat")))
            (pair "y" (rust-ref (rust-named "OptimizedNat"))))
      (rust-named "OptimizedNat")
      (make-rust-match (rust-tuple-pat (list (rust-var "x") (rust-var "y")))
        (list
          ;; Both small: use machine addition
          (pair (rust-tuple-pat (list (rust-enum-pat "OptimizedNat::Small" 
                                                    (list (rust-var-pat "a")))
                                    (rust-enum-pat "OptimizedNat::Small" 
                                                    (list (rust-var-pat "b")))))
                (rust-enum "OptimizedNat" "Small" 
                          (list (rust-binop "+" (rust-var "a") (rust-var "b")))))
          
          ;; Mixed or large: fallback to Peano
          (pair rust-wildcard
                (rust-call (rust-var "fallback_add")
                          (list (rust-var "x") (rust-var "y")))))))))

;; Generate optimized multiplication
(type generate-optimized-mult (-> RustItem))
(define generate-optimized-mult
  (fn ()
    (make-rust-fn
      "optimized_mult"
      (list (pair "x" (rust-ref (rust-named "OptimizedNat")))
            (pair "y" (rust-ref (rust-named "OptimizedNat"))))
      (rust-named "OptimizedNat")
      (make-rust-match (rust-tuple-pat (list (rust-var "x") (rust-var "y")))
        (list
          ;; Both small: use machine multiplication
          (pair (rust-tuple-pat (list (rust-enum-pat "OptimizedNat::Small"
                                                    (list (rust-var-pat "a")))
                                    (rust-enum-pat "OptimizedNat::Small"
                                                    (list (rust-var-pat "b")))))
                (rust-if (rust-binop "<" 
                                   (rust-binop "*" (rust-var "a") (rust-var "b"))
                                   (rust-usize-lit 1000))
                        (rust-enum "OptimizedNat" "Small"
                                  (list (rust-binop "*" (rust-var "a") (rust-var "b"))))
                        (rust-call (rust-var "overflow_to_peano")
                                  (list (rust-var "a") (rust-var "b")))))
          
          ;; Mixed or large: fallback
          (pair rust-wildcard
                (rust-call (rust-var "fallback_mult")
                          (list (rust-var "x") (rust-var "y")))))))))

;; ============================================================================
;; COMPARISON OPERATIONS
;; ============================================================================

;; Generate optimized equality check
(type generate-optimized-equal (-> RustItem))
(define generate-optimized-equal
  (fn ()
    (make-rust-fn
      "optimized_equal"
      (list (pair "x" (rust-ref (rust-named "OptimizedNat")))
            (pair "y" (rust-ref (rust-named "OptimizedNat"))))
      rust-bool
      (make-rust-match (rust-tuple-pat (list (rust-var "x") (rust-var "y")))
        (list
          ;; Both small: direct comparison
          (pair (rust-tuple-pat (list (rust-enum-pat "OptimizedNat::Small"
                                                    (list (rust-var-pat "a")))
                                    (rust-enum-pat "OptimizedNat::Small"
                                                    (list (rust-var-pat "b")))))
                (rust-binop "==" (rust-var "a") (rust-var "b")))
          
          ;; Mixed: can't be equal
          (pair (rust-tuple-pat (list (rust-enum-pat "OptimizedNat::Small" 
                                                    (list rust-wildcard))
                                    (rust-enum-pat "OptimizedNat::Large"
                                                    (list rust-wildcard))))
                (rust-bool-lit false))
          (pair (rust-tuple-pat (list (rust-enum-pat "OptimizedNat::Large"
                                                    (list rust-wildcard))
                                    (rust-enum-pat "OptimizedNat::Small"
                                                    (list rust-wildcard))))
                (rust-bool-lit false))
          
          ;; Both large: structural comparison
          (pair rust-wildcard
                (rust-call (rust-var "peano_equal")
                          (list (rust-var "x") (rust-var "y")))))))))

;; ============================================================================
;; MODULE GENERATION
;; ============================================================================

;; Generate complete optimized arithmetic module
(type generate-arithmetic-module (-> RustModule))
(define generate-arithmetic-module
  (fn ()
    (rust-module "optimized_arithmetic"
                (list 
                  (rust-use-item "crate::hott_values::*")
                  (rust-use-item "std::cmp::Ordering"))
                (list
                  ;; Type definitions
                  (generate-optimized-nat)
                  
                  ;; Converters
                  (generate-nat-converter)
                  
                  ;; Arithmetic operations
                  (generate-optimized-add)
                  (generate-optimized-mult)
                  
                  ;; Comparison operations
                  (generate-optimized-equal)
                  
                  ;; Helper: count successors
                  (make-rust-fn
                    "count_successors"
                    (list (pair "n" (rust-ref (rust-named "HottValue"))))
                    rust-usize
                    (rust-match (rust-var "n")
                               (list
                                 (rust-match-arm
                                   (rust-enum-pat "HottValue::Constructor"
                                                 (list (rust-struct-pat "ConstructorData"
                                                                      (list (pair "name" (rust-string-lit "zero"))))))
                                   (rust-usize-lit 0))
                                 (rust-match-arm
                                   (rust-enum-pat "HottValue::Constructor"
                                                 (list (rust-struct-pat "ConstructorData"
                                                                      (list (pair "name" (rust-string-lit "succ"))
                                                                            (pair "args" (rust-var-pat "args"))))))
                                   (rust-binop "+" 
                                             (rust-usize-lit 1)
                                             (rust-call (rust-var "count_successors")
                                                       (list (rust-method-call (rust-var "args")
                                                                             "first"
                                                                             nil)))))
                                 (rust-match-arm
                                   rust-wildcard
                                   (rust-usize-lit 0)))))))))

;; ============================================================================
;; WRITE GENERATED CODE TO FILE
;; ============================================================================

;; Effect to write generated Rust code
(type write-generated-arithmetic (-> String Effect))
(define write-generated-arithmetic
  (fn (filename)
    (let ((module (generate-arithmetic-module))
          (code (gen-rust-module module)))
      (perform-effect 'file-write
                     filename
                     code))))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export generate-optimized-nat)
(export generate-nat-converter)
(export generate-optimized-add)
(export generate-optimized-mult)
(export generate-optimized-equal)
(export generate-arithmetic-module)
(export write-generated-arithmetic)