;;; PathFinder S-expression to AST parser
;;; Converts PathFinder syntax into evaluatable AST nodes

(use-modules (ice-9 match)
             (srfi srfi-1))  ; for fold-left

(define (parse-pathfinder sexp)
  "Parse a PathFinder s-expression into an AST"
  (match sexp
    ;; Literals
    ((? number? n) `(literal ,n))
    ((? string? s) `(literal ,s))
    ((? boolean? b) `(literal ,b))
    
    ;; Special atoms
    ('zero '(constructor "zero"))
    ('true '(constructor "true"))
    ('false '(constructor "false"))
    ('nil '(constructor "nil"))
    ('Unit '(type-expr "Unit"))
    ('Nat '(type-expr "Nat"))
    ('Bool '(type-expr "Bool"))
    
    ;; Quoted symbols
    (('quote sym) `(literal ',sym))
    
    ;; Variables
    ((? symbol? s) `(var ,(symbol->string s)))
    
    ;; Define forms
    (('define name value)
     `(define ,(symbol->string name) ,(parse-pathfinder value)))
    
    ;; Lambda/fn forms
    (('fn params body)
     (let ((param-list (if (list? params) params (list params))))
       `(lambda ,(map symbol->string param-list) ,(parse-pathfinder body))))
    
    (('lambda params body)
     `(lambda ,(map symbol->string params) ,(parse-pathfinder body)))
    
    ;; Let binding
    (('let var val body)
     `(let ,(symbol->string var) ,(parse-pathfinder val) ,(parse-pathfinder body)))
    
    ;; Match expressions
    (('match target cases ...)
     `(match ,(parse-pathfinder target) ,@(map parse-match-case cases)))
    
    ;; Function application
    (('app func arg)
     `(app ,(parse-pathfinder func) ,(parse-pathfinder arg)))
    
    ;; Special forms
    (('succ n)
     `(app (var "succ") ,(parse-pathfinder n)))
    
    (('cons a b)
     `(app (app (var "cons") ,(parse-pathfinder a)) ,(parse-pathfinder b)))
    
    (('perform effect)
     `(perform ,(parse-pathfinder effect)))
    
    (('print msg)
     `(app (var "perform") 
           (constructor "print" ,(parse-pathfinder msg))))
    
    ;; Eliminator applications
    (('nat-elim motive base step target)
     (let* ((nat-elim-3 `(app (var "nat-elim") ,(parse-pathfinder motive)))
            (nat-elim-2 `(app ,nat-elim-3 ,(parse-pathfinder base)))
            (nat-elim-1 `(app ,nat-elim-2 ,(parse-pathfinder step))))
       `(app ,nat-elim-1 ,(parse-pathfinder target))))
    
    (('bool-elim motive false-case true-case target)
     (let* ((bool-elim-3 `(app (var "bool-elim") ,(parse-pathfinder motive)))
            (bool-elim-2 `(app ,bool-elim-3 ,(parse-pathfinder false-case)))
            (bool-elim-1 `(app ,bool-elim-2 ,(parse-pathfinder true-case))))
       `(app ,bool-elim-1 ,(parse-pathfinder target))))
    
    ;; General function application (f arg1 arg2 ...)
    ((func args ...)
     (fold
      (lambda (a f) `(app ,f ,(parse-pathfinder a)))
      (parse-pathfinder func)
      args))
    
    ;; Unknown forms - pass through as literals
    (_
     `(literal ,sexp))))

;; Helper to convert PathFinder strings to Scheme strings
(define (pathfinder-string->scheme-string pf-str)
  "Convert a PathFinder string (list of chars) to a Scheme string"
  (match pf-str
    (('constructor "empty-string")
     "")
    (('constructor "string-cons" char-val rest)
     (string-append 
      (pathfinder-char->string char-val)
      (pathfinder-string->scheme-string rest)))
    ;; If it's already a Scheme string, return it
    ((? string? s) s)
    (_ "")))

;; Parse a match case
(define (parse-match-case case-expr)
  "Parse a match case expression"
  (match case-expr
    (('case pattern body)
     `(case ,(parse-pattern pattern) ,(parse-pathfinder body)))
    (_
     (error "Invalid match case syntax:" case-expr))))

;; Parse patterns for match expressions
(define (parse-pattern pattern)
  "Parse a pattern in a match expression"
  (match pattern
    ;; Variable patterns
    ((? symbol? s)
     (if (eq? s '_)
         '_  ; wildcard
         s)) ; variable binding
    
    ;; Literal patterns
    ((? number? n) n)
    ((? string? s) s)
    
    ;; Constructor patterns (for matching)
    ('zero 'zero)
    ('true 'true)
    ('false 'false)
    ('nil 'nil)
    
    ;; Successor pattern
    (('succ pat)
     `(constructor "succ" ,(parse-pattern pat)))
    
    ;; List patterns
    (('cons head tail)
     `(constructor "cons" ,(parse-pattern head) ,(parse-pattern tail)))
    
    ;; General constructor patterns
    ((name args ...)
     (if (symbol? name)
         `(constructor ,(symbol->string name) ,@(map parse-pattern args))
         (error "Invalid pattern:" pattern)))
    
    ;; Unknown patterns
    (_
     (error "Unknown pattern type:" pattern))))

(define (pathfinder-char->string char-val)
  "Convert a PathFinder character to a string"
  (match char-val
    (('constructor "char" nat-val)
     (string (integer->char (pathfinder-nat->number nat-val))))
    (_ "?")))

(define (pathfinder-nat->number nat-val)
  "Convert a PathFinder natural number to a Scheme number"
  (match nat-val
    (('constructor "zero") 0)
    (('constructor "succ" pred)
     (+ 1 (pathfinder-nat->number pred)))
    (('literal n) n)  ; Already a number
    (_ 0)))