;;; Core PathFinder evaluator
;;; This implements the HoTT evaluation semantics with proper lexical scoping

(use-modules (ice-9 match)
             (srfi srfi-1))  ; for fold

;; Main evaluation function
(define (pathfinder-eval ast env)
  "Evaluate a PathFinder AST in an environment"
  (match ast
    ;; Variables
    (('var name)
     (env-lookup env name))
    
    ;; Define - returns the value and should be handled by caller to extend environment
    (('define name value-ast)
     (let ((value (pathfinder-eval value-ast env)))
       `(define-result ,name ,value)))
    
    ;; Lambda - creates a closure capturing the current environment
    (('lambda params body)
     `(closure ,params ,body ,env))
    
    ;; Function application
    (('app func-ast arg-ast)
     (let ((func (pathfinder-eval func-ast env))
           (arg (pathfinder-eval arg-ast env)))
       (apply-function func arg env)))
    
    ;; Let binding
    (('let var value-ast body-ast)
     (let* ((value (pathfinder-eval value-ast env))
            (new-env (env-extend env var value)))
       (pathfinder-eval body-ast new-env)))
    
    ;; Match expressions
    (('match target-ast . cases)
     (let ((target (pathfinder-eval target-ast env)))
       (pathfinder-match target cases env)))
    
    ;; Constructors
    (('constructor name . args)
     (if (null? args)
         `(constructor ,name)
         (let ((eval-args (map (lambda (arg) (pathfinder-eval arg env)) args)))
           `(constructor ,name ,@eval-args))))
    
    ;; Literals
    (('literal value)
     value)
    
    ;; Effects
    (('perform effect-ast)
     (let ((effect (pathfinder-eval effect-ast env)))
       `(effect ,effect)))
    
    ;; Type expressions (evaluate to themselves for now)
    (('type-expr . _)
     ast)
    
    ;; Unknown forms
    (_
     (error "Unknown AST form:" ast))))

;; Apply a function to an argument
(define (apply-function func arg env)
  "Apply a function value to an argument"
  (match func
    ;; User-defined function (closure)
    (('closure (param) body closure-env)
     ;; Extend the closure's environment, not the current one!
     (let ((new-env (env-extend closure-env param arg)))
       (pathfinder-eval body new-env)))
    
    ;; Multi-parameter closure (currying)
    (('closure (param . rest-params) body closure-env)
     (let ((new-env (env-extend closure-env param arg)))
       (if (null? rest-params)
           (pathfinder-eval body new-env)
           `(closure ,rest-params ,body ,new-env))))
    
    ;; Built-in functions
    (('builtin "succ" 1)
     `(constructor "succ" ,arg))
    
    (('builtin "cons" 2)
     ;; Cons needs 2 args, so return a partial application
     `(builtin-partial "cons" 1 ,arg))
    
    (('builtin-partial "cons" 1 first-arg)
     ;; Now we have both args for cons
     `(constructor "cons" ,first-arg ,arg))
    
    (('builtin "nat-elim" 4)
     ;; nat-elim needs 4 args: motive, base, step, target
     `(builtin-partial "nat-elim" 3 ,arg))
    
    (('builtin-partial "nat-elim" 3 motive)
     `(builtin-partial "nat-elim" 2 ,motive ,arg))
    
    (('builtin-partial "nat-elim" 2 motive base)
     `(builtin-partial "nat-elim" 1 ,motive ,base ,arg))
    
    (('builtin-partial "nat-elim" 1 motive base step)
     ;; All args collected, apply nat-elim
     (apply-nat-elim motive base step arg env))
    
    (('builtin "bool-elim" 4)
     `(builtin-partial "bool-elim" 3 ,arg))
    
    (('builtin-partial "bool-elim" 3 motive)
     `(builtin-partial "bool-elim" 2 ,motive ,arg))
    
    (('builtin-partial "bool-elim" 2 motive false-case)
     `(builtin-partial "bool-elim" 1 ,motive ,false-case ,arg))
    
    (('builtin-partial "bool-elim" 1 motive false-case true-case)
     ;; All args collected, apply bool-elim
     (apply-bool-elim motive false-case true-case arg env))
    
    (('builtin "perform" 1)
     ;; Perform wraps its argument as an effect
     `(effect ,arg))
    
    ;; Partial application of user functions
    (('app partial-func arg2)
     (apply-function (pathfinder-eval partial-func env) arg env))
    
    ;; Error cases
    (_
     (error "Cannot apply non-function:" func))))

;; Natural number eliminator
(define (apply-nat-elim motive base step target env)
  "Apply nat-elim to a natural number"
  (match target
    (('constructor "zero")
     base)
    (('constructor "succ" pred)
     ;; step takes two arguments: predecessor and recursive result
     (let* ((rec-result (apply-nat-elim motive base step pred env))
            (step-with-pred (apply-function step pred env)))
       (apply-function step-with-pred rec-result env)))
    (_
     (error "nat-elim expects a natural number, got:" target))))

;; Boolean eliminator  
(define (apply-bool-elim motive false-case true-case target env)
  "Apply bool-elim to a boolean"
  (match target
    (('constructor "false")
     false-case)
    (('constructor "true")
     true-case)
    (_
     (error "bool-elim expects a boolean, got:" target))))

;; Helper to check if something is an effect
(define (effect? value)
  (and (pair? value) (eq? (car value) 'effect)))

;; ============================================================================
;; PATTERN MATCHING IMPLEMENTATION
;; ============================================================================

;; Main pattern matching function
(define (pathfinder-match target cases env)
  "Match a target value against a list of cases"
  (if (null? cases)
      (error "Non-exhaustive match, no case matched:" target)
      (let ((case-expr (car cases)))
        (match case-expr
          ;; Each case has form: (case pattern body)
          (('case pattern body)
           (let ((match-result (match-pattern pattern target '())))
             (if (car match-result)  ; successful match?
                 (let ((bindings (cdr match-result)))
                   ;; Extend environment with pattern bindings
                   (let ((new-env (fold (lambda (binding env)
                                          (env-extend env (car binding) (cdr binding)))
                                        env
                                        bindings)))
                     (pathfinder-eval body new-env)))
                 ;; Try next case
                 (pathfinder-match target (cdr cases) env))))
          (_
           (error "Invalid match case syntax:" case-expr))))))

;; Pattern matching compiler
(define (match-pattern pattern value bindings)
  "Match a pattern against a value, returning (success? . bindings)"
  (cond
    ;; Wildcard pattern - always matches, no binding
    ((eq? pattern '_)
     (cons #t bindings))
    
    ;; Atom patterns (for matching specific constructor atoms like 'zero, 'true, etc.)
    ;; Must come before variable pattern check!
    ((and (symbol? pattern) (memq pattern '(zero true false nil)))
     (match value
       (('constructor name)
        (if (string=? (symbol->string pattern) name)
            (cons #t bindings)
            (cons #f bindings)))
       (_ (cons #f bindings))))
    
    ;; Variable pattern - always matches, creates binding
    ((symbol? pattern)
     (cons #t (cons (cons (symbol->string pattern) value) bindings)))
    
    ;; Constructor pattern
    ((and (pair? pattern) (eq? (car pattern) 'constructor))
     (match pattern
       (('constructor name . pat-args)
        (match value
          (('constructor val-name . val-args)
           (if (string=? name val-name)
               ;; Names match, check arguments
               (match-constructor-args pat-args val-args bindings)
               ;; Names don't match
               (cons #f bindings)))
          (_ (cons #f bindings))))))
    
    ;; Literal patterns (numbers, strings)
    ((or (number? pattern) (string? pattern))
     (if (equal? pattern value)
         (cons #t bindings)
         (cons #f bindings)))
    
    ;; List pattern for matching PathFinder lists
    ((and (pair? pattern) (memq (car pattern) '(nil cons)))
     (match-list-pattern pattern value bindings))
    
    ;; Unknown pattern
    (else
     (error "Unknown pattern type:" pattern))))

;; Match constructor arguments recursively
(define (match-constructor-args pat-args val-args bindings)
  "Match argument lists from constructor patterns"
  (cond
    ((and (null? pat-args) (null? val-args))
     ;; Both empty - success
     (cons #t bindings))
    ((or (null? pat-args) (null? val-args))
     ;; Different lengths - fail
     (cons #f bindings))
    (else
     ;; Match first argument
     (let ((result (match-pattern (car pat-args) (car val-args) bindings)))
       (if (car result)
           ;; First arg matched, continue with rest
           (match-constructor-args (cdr pat-args) (cdr val-args) (cdr result))
           ;; First arg didn't match
           result)))))

;; Match list patterns (nil and cons)
(define (match-list-pattern pattern value bindings)
  "Match PathFinder list patterns"
  (match pattern
    ('nil
     (match value
       (('constructor "nil") (cons #t bindings))
       (_ (cons #f bindings))))
    (('cons head-pat tail-pat)
     (match value
       (('constructor "cons" head tail)
        (let ((head-result (match-pattern head-pat head bindings)))
          (if (car head-result)
              (match-pattern tail-pat tail (cdr head-result))
              head-result)))
       (_ (cons #f bindings))))
    (_ (error "Invalid list pattern:" pattern))))