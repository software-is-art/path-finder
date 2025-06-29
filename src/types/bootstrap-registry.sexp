;; ============================================================================
;; BOOTSTRAP TYPE FAMILY REGISTRY
;; ============================================================================
;; Minimal registry implementation for bootstrapping

(import types types)
(import types families)
(import types list)

;; Global registry with standard type families
(type standard-registry TypeFamilyRegistry)
(define standard-registry
  (let ((empty-reg (family-registry nil)))
    (let ((reg1 (register-family empty-reg "List" 
                  (make-type-family "List" 1
                    (fn (args)
                      (List-elim args
                        (error "List requires 1 argument")
                        (fn (elem-type rest _)
                          (if (list-empty? rest)
                              (make-list-type elem-type)
                              (error "List requires exactly 1 argument")))))))))
      (let ((reg2 (register-family reg1 "Maybe"
                    (make-type-family "Maybe" 1
                      (fn (args)
                        (List-elim args
                          (error "Maybe requires 1 argument")
                          (fn (elem-type rest _)
                            (if (list-empty? rest)
                                (make-maybe-type elem-type)
                                (error "Maybe requires exactly 1 argument")))))))))
        (let ((reg3 (register-family reg2 "Either"
                      (make-type-family "Either" 2
                        (fn (args)
                          (List-elim args
                            (error "Either requires 2 arguments")
                            (fn (left-type rest1 _)
                              (List-elim rest1
                                (error "Either requires 2 arguments")
                                (fn (right-type rest2 _)
                                  (if (list-empty? rest2)
                                      (make-either-type left-type right-type)
                                      (error "Either requires exactly 2 arguments")))))))))))
          reg3)))))

;; Implementation of get-type-family-by-name
(type get-type-family-by-name (-> String (Maybe TypeFamily)))
(define get-type-family-by-name
  (fn (name)
    (lookup-family standard-registry name)))

;; Check if list is empty
(type list-empty? (-> (List A) Bool))
(define list-empty?
  (fn (lst)
    (List-elim lst
      true
      (fn (head tail rec) false))))

;; Make List type (proper version)
(type make-list-type (-> Type Type))
(define make-list-type
  (fn (elem-type)
    (let ((nil-cons (type-constructor "nil" nil elem-type))
          (cons-cons (type-constructor "cons" 
                       (cons elem-type (cons (make-list-type elem-type) nil))
                       elem-type)))
      (inductive-type "List" (cons nil-cons (cons cons-cons nil))))))

;; Make Maybe type
(type make-maybe-type (-> Type Type))
(define make-maybe-type
  (fn (elem-type)
    (let ((nothing-cons (type-constructor "nothing" nil elem-type))
          (just-cons (type-constructor "just" (cons elem-type nil) elem-type)))
      (inductive-type "Maybe" (cons nothing-cons (cons just-cons nil))))))

;; Make Either type
(type make-either-type (-> Type Type Type))
(define make-either-type
  (fn (left-type right-type)
    (let ((left-cons (type-constructor "left" (cons left-type nil) (sum-type left-type right-type)))
          (right-cons (type-constructor "right" (cons right-type nil) (sum-type left-type right-type))))
      (inductive-type "Either" (cons left-cons (cons right-cons nil))))))

;; Implementation of lookup-constructor
(type lookup-constructor (-> String (Maybe Type)))
(define lookup-constructor
  (fn (name)
    ;; For bootstrap, just handle common constructors
    (cond
      ((string-equal? name "zero") (just Nat))
      ((string-equal? name "succ") (just (pi-type "n" Nat Nat)))
      ((string-equal? name "true") (just Bool))
      ((string-equal? name "false") (just Bool))
      ((string-equal? name "nil") (just (pi-type "A" (universe 0) (make-list-type (var "A")))))
      ((string-equal? name "cons") (just (pi-type "A" (universe 0) 
                                           (pi-type "x" (var "A")
                                             (pi-type "xs" (make-list-type (var "A"))
                                               (make-list-type (var "A")))))))
      ((string-equal? name "nothing") (just (pi-type "A" (universe 0) (make-maybe-type (var "A")))))
      ((string-equal? name "just") (just (pi-type "A" (universe 0)
                                          (pi-type "x" (var "A") (make-maybe-type (var "A"))))))
      (else nothing))))

;; Helper for cond macro (if not available)
(type cond-helper (-> (List (Pair Bool A)) A A))
(define cond-helper
  (fn (cases default)
    (List-elim cases
      default
      (fn (case rest rec)
        (if (first case)
            (second case)
            rec)))))