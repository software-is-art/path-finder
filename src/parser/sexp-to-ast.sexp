;; ============================================================================
;; S-EXPRESSION TO AST CONVERTER
;; ============================================================================
;; Convert parsed S-expressions into PathFinder HoTT-AST

(import core foundations)
(import core ast)
(import types list)
(import types string)
(import parser sexp-parser)

;; ============================================================================
;; CONVERSION RESULT
;; ============================================================================

(data ConversionResult U0
  (case conv-ok (-> HoTT-AST ConversionResult))
  (case conv-error (-> String ConversionResult)))

;; ============================================================================
;; MAIN CONVERTER
;; ============================================================================

;; Convert S-expression to AST
(define sexp-to-ast
  (fn (sexpr)
    (match sexpr
      ;; Literals
      (case (number n) 
        (conv-ok (literal (nat-value n))))
      
      (case (string s)
        (conv-ok (literal (string-value s))))
      
      ;; Special atoms
      (case (atom "zero")
        (conv-ok (constructor "zero" nil)))
      
      (case (atom "true")
        (conv-ok (constructor "true" nil)))
      
      (case (atom "false")
        (conv-ok (constructor "false" nil)))
      
      (case (atom "nil")
        (conv-ok (constructor "nil" nil)))
      
      (case (atom "Unit")
        (conv-ok (var "Unit")))
      
      (case (atom "Nat")
        (conv-ok (var "Nat")))
      
      (case (atom "Bool")
        (conv-ok (var "Bool")))
      
      ;; Variables
      (case (atom name)
        (conv-ok (var name)))
      
      ;; Quoted expressions
      (case (quoted expr)
        (conv-ok (literal (quote-value expr))))
      
      ;; Lists - check for special forms
      (case (list elems)
        (convert-list elems))
      
      ;; Empty list
      (case (list nil)
        (conv-error "Empty list not allowed")))))

;; Convert list forms
(define convert-list
  (fn (elems)
    (match elems
      (case nil
        (conv-error "Empty list"))
      
      (case (cons head tail)
        (match head
          ;; Check for special forms
          (case (atom form-name)
            (convert-special-form form-name tail))
          
          ;; Function application
          (case _
            (convert-application elems)))))))

;; Convert special forms
(define convert-special-form
  (fn (form-name args)
    (match form-name
      ;; Lambda/fn
      (case "fn"
        (convert-fn args))
      
      (case "lambda"
        (convert-lambda args))
      
      ;; Let binding
      (case "let"
        (convert-let args))
      
      ;; Define (top-level)
      (case "define"
        (convert-define args))
      
      ;; Type declaration
      (case "type"
        (convert-type args))
      
      ;; Data type
      (case "data"
        (convert-data args))
      
      ;; Case/match
      (case "match"
        (convert-match args))
      
      (case "case"
        (convert-case args))
      
      ;; Import
      (case "import"
        (convert-import args))
      
      ;; Export
      (case "export"
        (convert-export args))
      
      ;; Constructors
      (case "succ"
        (convert-unary-constructor "succ" args))
      
      (case "cons"
        (convert-binary-constructor "cons" args))
      
      ;; Effects
      (case "perform"
        (convert-perform args))
      
      ;; Eliminators
      (case "nat-elim"
        (convert-eliminator "nat-elim" args))
      
      (case "bool-elim"
        (convert-eliminator "bool-elim" args))
      
      ;; Type expressions
      (case "->"
        (convert-arrow-type args))
      
      ;; If expression
      (case "if"
        (convert-if args))
      
      ;; Default - treat as application
      (case _
        (convert-application (cons (atom form-name) args))))))

;; Convert fn form: (fn (params...) body)
(define convert-fn
  (fn (args)
    (match args
      (case (cons params (cons body nil))
        (match params
          ;; Single parameter
          (case (atom param)
            (conv-ok (lambda param (must-convert body))))
          
          ;; Multiple parameters - curry
          (case (list param-list)
            (convert-lambda-list param-list body))
          
          (case _
            (conv-error "Invalid fn parameters"))))
      
      (case _
        (conv-error "Invalid fn form")))))

;; Convert lambda form: (lambda (params...) body)
(define convert-lambda
  (fn (args)
    (match args
      (case (cons params (cons body nil))
        (match params
          (case (list param-list)
            (convert-lambda-list param-list body))
          (case _
            (conv-error "Invalid lambda parameters"))))
      (case _
        (conv-error "Invalid lambda form")))))

;; Convert lambda with parameter list (curried)
(define convert-lambda-list
  (fn (params body)
    (match params
      (case nil
        (conv-error "Empty parameter list"))
      
      (case (cons (atom p) nil)
        (conv-ok (lambda p (must-convert body))))
      
      (case (cons (atom p) rest)
        (conv-ok (lambda p (must-convert (list (cons (atom "lambda") 
                                               (cons (list rest) 
                                                    (cons body nil))))))))
      
      (case _
        (conv-error "Invalid parameter in lambda")))))

;; Convert let form: (let var value body)
(define convert-let
  (fn (args)
    (match args
      (case (cons (atom var) (cons value (cons body nil)))
        (conv-ok (let-expr var (must-convert value) (must-convert body))))
      (case _
        (conv-error "Invalid let form")))))

;; Convert function application
(define convert-application
  (fn (elems)
    (match elems
      (case nil
        (conv-error "Empty application"))
      
      (case (cons func nil)
        (sexp-to-ast func))
      
      (case (cons func args)
        (convert-apply-multi func args)))))

;; Convert multi-argument application (left-associative)
(define convert-apply-multi
  (fn (func args)
    (match args
      (case nil
        (sexp-to-ast func))
      
      (case (cons arg nil)
        (conv-ok (app (must-convert func) (must-convert arg))))
      
      (case (cons arg rest)
        (convert-apply-multi 
          (list (cons func (cons arg nil)))
          rest)))))

;; Convert unary constructor
(define convert-unary-constructor
  (fn (name args)
    (match args
      (case (cons arg nil)
        (conv-ok (app (var name) (must-convert arg))))
      (case _
        (conv-error (string-append name " requires exactly one argument"))))))

;; Convert binary constructor
(define convert-binary-constructor
  (fn (name args)
    (match args
      (case (cons arg1 (cons arg2 nil))
        (conv-ok (app (app (var name) (must-convert arg1)) (must-convert arg2))))
      (case _
        (conv-error (string-append name " requires exactly two arguments"))))))

;; Convert perform: (perform expr)
(define convert-perform
  (fn (args)
    (match args
      (case (cons expr nil)
        (conv-ok (effect (perform-effect (must-convert expr)))))
      (case _
        (conv-error "Invalid perform form")))))

;; Convert eliminator
(define convert-eliminator
  (fn (name args)
    (conv-ok (eliminator (var name) (map-must-convert args)))))

;; Convert arrow type: (-> t1 t2 ...)
(define convert-arrow-type
  (fn (args)
    (match args
      (case nil
        (conv-error "Empty arrow type"))
      
      (case (cons t nil)
        (sexp-to-ast t))
      
      (case (cons t1 (cons t2 nil))
        (conv-ok (pi-type "_" (must-convert t1) (must-convert t2))))
      
      (case (cons t1 rest)
        (conv-ok (pi-type "_" (must-convert t1) 
                         (must-convert (cons (atom "->") rest))))))))

;; Convert if expression
(define convert-if
  (fn (args)
    (match args
      (case (cons test (cons then-branch (cons else-branch nil)))
        (conv-ok (match-expr (must-convert test)
                            (list (match-case (constructor-pattern "true" nil)
                                            (must-convert then-branch))
                                  (match-case (constructor-pattern "false" nil)
                                            (must-convert else-branch))))))
      (case _
        (conv-error "Invalid if form")))))

;; Convert match expression
(define convert-match
  (fn (args)
    (match args
      (case (cons expr cases)
        (conv-ok (match-expr (must-convert expr)
                            (map-convert-case cases))))
      (case _
        (conv-error "Invalid match form")))))

;; Convert case
(define convert-case
  (fn (args)
    (conv-error "Case must be inside match")))

;; Convert define (for now, just return the AST as-is)
(define convert-define
  (fn (args)
    (conv-error "Define not supported in expressions")))

;; Convert type declaration
(define convert-type
  (fn (args)
    (conv-error "Type declaration not supported in expressions")))

;; Convert data declaration
(define convert-data
  (fn (args)
    (conv-error "Data declaration not supported in expressions")))

;; Convert import
(define convert-import
  (fn (args)
    (conv-error "Import not supported in expressions")))

;; Convert export
(define convert-export
  (fn (args)
    (conv-error "Export not supported in expressions")))

;; ============================================================================
;; HELPERS
;; ============================================================================

;; Convert or error
(define must-convert
  (fn (sexpr)
    (match (sexp-to-ast sexpr)
      (case (conv-ok ast) ast)
      (case (conv-error msg) 
        (error (string-append "Conversion error: " msg))))))

;; Map conversion over list
(define map-must-convert
  (fn (sexprs)
    (match sexprs
      (case nil nil)
      (case (cons s rest)
        (cons (must-convert s) (map-must-convert rest))))))

;; Convert match case
(define convert-match-case
  (fn (case-expr)
    (match case-expr
      (case (list (cons (atom "case") (cons pattern (cons expr nil))))
        (match-case (convert-pattern pattern) (must-convert expr)))
      (case _
        (error "Invalid case form")))))

;; Map case conversion
(define map-convert-case
  (fn (cases)
    (match cases
      (case nil nil)
      (case (cons c rest)
        (cons (convert-match-case c) (map-convert-case rest))))))

;; Convert pattern
(define convert-pattern
  (fn (sexpr)
    (match sexpr
      (case (atom "_") wildcard-pattern)
      (case (atom name) (var-pattern name))
      (case (number n) (literal-pattern (nat-value n)))
      (case (list (cons (atom ctor) args))
        (constructor-pattern ctor (map-convert-pattern args)))
      (case _ wildcard-pattern))))

;; Map pattern conversion
(define map-convert-pattern
  (fn (sexprs)
    (match sexprs
      (case nil nil)
      (case (cons s rest)
        (cons (convert-pattern s) (map-convert-pattern rest))))))

;; ============================================================================
;; VALUE CONSTRUCTORS (STUBS)
;; ============================================================================

;; These would be defined in core
(define nat-value (fn (n) n))
(define string-value (fn (s) s))
(define quote-value (fn (e) e))
(define perform-effect (fn (e) e))

;; ============================================================================
;; TOP-LEVEL CONVERSION
;; ============================================================================

;; Convert a file (list of top-level forms)
(define convert-file
  (fn (sexprs)
    (convert-top-level-forms sexprs nil)))

(define convert-top-level-forms
  (fn (sexprs acc)
    (match sexprs
      (case nil (reverse acc))
      (case (cons sexpr rest)
        (let ((ast (convert-top-level-form sexpr)))
          (convert-top-level-forms rest (cons ast acc)))))))

;; Convert a single top-level form
(define convert-top-level-form
  (fn (sexpr)
    (match sexpr
      ;; Special handling for top-level define
      (case (list (cons (atom "define") (cons (atom name) (cons value nil))))
        (define-node name (must-convert value)))
      
      ;; Type declaration
      (case (list (cons (atom "type") (cons (atom name) (cons type nil))))
        (type-decl-node name (must-convert type)))
      
      ;; Import
      (case (list (cons (atom "import") args))
        (import-node (convert-import-spec args)))
      
      ;; Export  
      (case (list (cons (atom "export") (cons (atom name) nil)))
        (export-node name))
      
      ;; Other expressions
      (case _
        (must-convert sexpr)))))

;; Convert import spec
(define convert-import-spec
  (fn (args)
    (match args
      (case (cons (atom name) nil) name)
      (case (cons (string path) nil) path)
      (case (cons (list (cons (atom cat) (cons (atom mod) nil))) nil)
        (string-append cat "/" mod))
      (case _ "unknown"))))

;; ============================================================================
;; AST NODE CONSTRUCTORS (STUBS)
;; ============================================================================

(define define-node (fn (name ast) ast))
(define type-decl-node (fn (name type) ast))
(define import-node (fn (spec) (var "import")))
(define export-node (fn (name) (var "export")))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export sexp-to-ast)
(export convert-file)
(export convert-top-level-form)
(export ConversionResult)
(export conv-ok)
(export conv-error)