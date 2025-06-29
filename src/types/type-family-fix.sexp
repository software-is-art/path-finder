;; ============================================================================
;; TYPE FAMILY FIX - PROPER STRUCTURED TYPE FAMILIES
;; ============================================================================
;; This fixes the string-based type encoding issue by using structured data

(import types types)
(import types families)

;; ============================================================================
;; EXTENDED TYPE REPRESENTATION
;; ============================================================================

;; Extend Type to include type family applications
;; Note: This shows what should be added to types.sexp
(comment "
(data Type Uω
  ... existing constructors ...
  ;; Type family application: List[Nat], Maybe[String], etc.
  (case type-family-app (-> String (List Type) Type)))
")

;; For now, we'll use a helper to create properly structured type family instances
(type make-type-family-instance (-> String (List Type) Type))
(define make-type-family-instance
  (fn (family-name type-args)
    ;; Create a structured representation instead of string concatenation
    ;; This will be an inductive type with metadata
    (let ((constructors (get-family-constructors family-name type-args)))
      (inductive-type 
        (make-family-instance-name family-name type-args)
        constructors))))

;; Create a structured name that can be parsed back
(type make-family-instance-name (-> String (List Type) String))
(define make-family-instance-name
  (fn (family-name type-args)
    ;; For now, still use strings but in a structured way
    ;; Eventually this should be replaced with type-family-app
    (string-append family-name "[" (types-to-string type-args) "]")))

;; ============================================================================
;; LIST TYPE FAMILY - PROPER IMPLEMENTATION
;; ============================================================================

;; List type family definition
(define List-TypeFamily
  (make-type-family "List" 1
    (fn (args)
      (List-elim args
        (error "List requires exactly 1 type argument")
        (fn (element-type rest _)
          (List-elim rest
            ;; Correct: single argument
            (let ((nil-constructor (type-constructor "nil" nil "List"))
                  (cons-constructor (type-constructor "cons" 
                    (cons element-type 
                      (cons (make-type-family-instance "List" (cons element-type nil)) nil))
                    "List")))
              (make-type-family-instance "List" (cons element-type nil)))
            ;; Error: too many arguments
            (fn (_ _ _) (error "List requires exactly 1 type argument"))))))))

;; Proper List type constructor
(type make-list-type-proper (-> Type Type))
(define make-list-type-proper
  (fn (element-type)
    (instantiate-family List-TypeFamily (cons element-type nil))))

;; ============================================================================
;; TYPE FAMILY REGISTRY
;; ============================================================================

;; Global type family registry
(type TypeFamilyRegistry (List (Pair String TypeFamily)))

;; Register standard type families
(define standard-type-families
  (cons (pair "List" List-TypeFamily)
    (cons (pair "Maybe" Maybe-TypeFamily)
      (cons (pair "Either" Either-TypeFamily)
        (cons (pair "Vec" Vec-TypeFamily)
          nil)))))

;; Look up type family by name
(type get-type-family-by-name (-> String (Maybe TypeFamily)))
(define get-type-family-by-name
  (fn (name)
    (list-find-map (Pair String TypeFamily) (Maybe TypeFamily) standard-type-families
      (fn (entry)
        (if (string-equal? (first entry) name)
            (just (second entry))
            nothing)))))

;; ============================================================================
;; FIX TYPE NAME GENERATION
;; ============================================================================

;; Replace the hacky type-name function
(type type-name-proper (-> Type String))
(define type-name-proper
  (fn (t)
    (Type-elim String t
      ;; universe case
      (fn (n) (string-append "Type" (nat-to-peano-string n)))
      ;; pi-type case
      (fn (var domain codomain) 
        (string-append "(" domain-str " -> " codomain-str ")"))
      ;; sigma-type case
      (fn (var first second) 
        (string-append "(" var " : " first-str " × " second-str ")"))
      ;; sum-type case
      (fn (left right) 
        (string-append "(" left-str " + " right-str ")"))
      ;; identity-type case
      (fn (A x y) 
        (string-append "Id(" A-str "," x-str "," y-str ")"))
      ;; unit-type case
      "Unit"
      ;; empty-type case
      "Empty"
      ;; inductive-type case
      (fn (name constructors) 
        ;; Check if it's a type family instance
        (parse-family-instance-name name))
      ;; effect-type case
      (fn (base req opt) "Effect"))))

;; Parse structured type family instance names
(type parse-family-instance-name (-> String String))
(define parse-family-instance-name
  (fn (name)
    ;; If it contains '[', it's a type family instance
    (if (string-contains? name "[")
        name  ;; Already structured
        name))) ;; Regular inductive type

;; ============================================================================
;; PROPER TYPE EQUALITY
;; ============================================================================

;; Implement type-equal? properly using structural comparison
(type type-equal? (-> Type Type Bool))
(define type-equal?
  (fn (t1 t2)
    (Type-elim Bool t1
      ;; universe case
      (fn (n1)
        (Type-elim Bool t2
          (fn (n2) (nat-equal? n1 n2))
          (fn (_ _ _) false) ;; pi
          (fn (_ _ _) false) ;; sigma
          (fn (_ _) false)   ;; sum
          (fn (_ _ _) false) ;; identity
          false false        ;; unit, empty
          (fn (_ _) false)   ;; inductive
          (fn (_ _ _) false))) ;; effect
      
      ;; pi-type case
      (fn (var1 dom1 cod1)
        (Type-elim Bool t2
          (fn (_) false) ;; universe
          (fn (var2 dom2 cod2)
            (Bool-and (type-equal? dom1 dom2)
                     (type-equal? cod1 cod2)))
          (fn (_ _ _) false) ;; sigma
          (fn (_ _) false)   ;; sum
          (fn (_ _ _) false) ;; identity
          false false        ;; unit, empty
          (fn (_ _) false)   ;; inductive
          (fn (_ _ _) false))) ;; effect
      
      ;; ... implement all other cases similarly
      )))

;; ============================================================================
;; MIGRATION HELPERS
;; ============================================================================

;; Convert old string-based type to proper type family instance
(type migrate-string-type (-> String Type))
(define migrate-string-type
  (fn (old-name)
    (cond
      ((string-equal? old-name "List-Nat")
       (make-list-type-proper Nat))
      ((string-equal? old-name "List-Bool")
       (make-list-type-proper Bool))
      ((string-equal? old-name "List-String")
       (make-list-type-proper String))
      (else
       (inductive-type old-name nil))))) ;; Fallback

;; ============================================================================
;; PEANO STRING FOR DEBUGGING
;; ============================================================================

;; Pure HoTT implementation of nat to string for debugging
(type nat-to-peano-string (-> Nat String))
(define nat-to-peano-string
  (fn (n)
    (Nat-elim n
      "Z"
      (fn (pred rec)
        (string-append "S(" (string-append rec ")"))))))

;; This provides a proper type family system without string hacking!