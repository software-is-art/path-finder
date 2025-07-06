;;; Environment management for PathFinder
;;; Provides lexically scoped variable bindings

(use-modules (ice-9 match))

;; Environment is an association list
;; ((name . value) ...)

(define (make-empty-environment)
  "Create an empty environment"
  '())

(define (env-extend env name value)
  "Extend environment with a new binding"
  (cons (cons name value) env))

(define (env-lookup env name)
  "Look up a variable in the environment"
  (let ((binding (assoc name env)))
    (if binding
        (cdr binding)
        (error "Unbound variable:" name))))

(define (env-lookup-safe env name)
  "Look up a variable, returning #f if not found"
  (let ((binding (assoc name env)))
    (and binding (cdr binding))))

(define (make-initial-environment)
  "Create the initial environment with primitives"
  (let ((env (make-empty-environment)))
    ;; Add constructors
    (set! env (env-extend env "zero" '(constructor "zero")))
    (set! env (env-extend env "true" '(constructor "true")))
    (set! env (env-extend env "false" '(constructor "false")))
    (set! env (env-extend env "nil" '(constructor "nil")))
    
    ;; Add builtin functions
    (set! env (env-extend env "succ" '(builtin "succ" 1)))
    (set! env (env-extend env "cons" '(builtin "cons" 2)))
    (set! env (env-extend env "nat-elim" '(builtin "nat-elim" 4)))
    (set! env (env-extend env "bool-elim" '(builtin "bool-elim" 4)))
    (set! env (env-extend env "perform" '(builtin "perform" 1)))
    
    ;; Type constructors (for reference)
    (set! env (env-extend env "Nat" '(type "Nat")))
    (set! env (env-extend env "Bool" '(type "Bool")))
    (set! env (env-extend env "Type" '(type "Type")))
    (set! env (env-extend env "Unit" '(type "Unit")))
    
    env))

;; Environment utilities
(define (env-keys env)
  "Get all bound names"
  (map car env))

(define (env-size env)
  "Get number of bindings"
  (length env))

(define (env-merge env1 env2)
  "Merge two environments (env2 takes precedence)"
  (append env2 env1))

(define (env-update env var value)
  "Update an existing binding in the environment"
  (map (lambda (binding)
         (if (equal? (car binding) var)
             (cons var value)
             binding))
       env))