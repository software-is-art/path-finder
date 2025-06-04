#lang racket/base

(require racket/contract
         racket/match
         racket/list
         "../parser/ast.rkt")

(provide evaluate
         make-environment
         make-global-environment
         environment?
         closure?)

;; Value types for evaluation results
(struct closure (params body-list env) #:transparent)
(struct builtin-func (name proc) #:transparent)

;; Value contract - all possible evaluation results
(define value/c
  (or/c number? boolean? string? closure? builtin-func?))

;; Environment for lexical scoping
(struct environment (bindings parent) #:transparent)

;; Environment operations
(define/contract (make-environment [parent #f])
  (->* () ((or/c environment? #f)) environment?)
  (environment (make-hash) parent))

(define/contract (env-lookup env name)
  (-> environment? string? (or/c value/c #f))
  (or (hash-ref (environment-bindings env) name #f)
      (and (environment-parent env)
           (env-lookup (environment-parent env) name))))

(define/contract (env-define! env name value)
  (-> environment? string? value/c void?)
  (hash-set! (environment-bindings env) name value))

(define/contract (env-extend env params args)
  (-> environment? (listof string?) (listof value/c) environment?)
  (let ([new-env (make-environment env)])
    (for ([param params] [arg args])
      (env-define! new-env param arg))
    new-env))

;; Built-in functions
(define builtin-environment
  (let ([env (make-environment)])
    (env-define! env "+" (builtin-func "+" +))
    (env-define! env "-" (builtin-func "-" -))
    (env-define! env "*" (builtin-func "*" *))
    (env-define! env "/" (builtin-func "/" /))
    (env-define! env "=" (builtin-func "=" =))
    (env-define! env "<" (builtin-func "<" <))
    (env-define! env ">" (builtin-func ">" >))
    env))

;; Create a new environment that extends the builtin environment
(define/contract (make-global-environment)
  (-> environment?)
  (make-environment builtin-environment))

;; Main evaluation function
(define/contract (evaluate ast [env (make-global-environment)])
  (->* (ast-node/c) (environment?) value/c)
  (match ast
    ;; Literals
    [(number-atom value) value]
    [(boolean-atom value) value]
    [(string-atom value) value]
    
    ;; Symbol lookup
    [(symbol-atom name)
     (let ([value (env-lookup env name)])
       (if value
           value
           (error "Undefined variable: " name)))]
    
    ;; S-expressions (function calls and special forms)
    [(sexpr elements)
     (if (null? elements)
         '() ; empty list
         (let ([first-elem (first elements)])
           (match first-elem
             ;; Special forms
             [(symbol-atom "define")
              (when (< (length elements) 3)
                (error "define requires at least 2 arguments"))
              (let* ([name-elem (second elements)]
                     [value-elem (third elements)])
                (match name-elem
                  [(symbol-atom name)
                   (let ([value (evaluate value-elem env)])
                     (env-define! env name value)
                     value)]
                  [_ (error "define expects a symbol as first argument")]))]
             
             [(symbol-atom "lambda")
              (when (< (length elements) 3)
                (error "lambda requires at least 2 arguments"))
              (let* ([params-elem (second elements)]
                     [body-elems (drop elements 2)]) ; Multiple body expressions
                (match params-elem
                  [(sexpr param-nodes)
                   (let ([params (map (lambda (node)
                                       (match node
                                         [(symbol-atom name) name]
                                         [_ (error "lambda parameters must be symbols")]))
                                     param-nodes)])
                     (closure params body-elems env))]
                  [_ (error "lambda parameters must be a list")]))]
             
             [(symbol-atom "if")
              (when (not (= (length elements) 4))
                (error "if requires exactly 3 arguments"))
              (let* ([cond-elem (second elements)]
                     [then-elem (third elements)]
                     [else-elem (fourth elements)]
                     [cond-value (evaluate cond-elem env)])
                (if cond-value
                    (evaluate then-elem env)
                    (evaluate else-elem env)))]
             
             ;; Function calls
             [_
              (let* ([func (evaluate first-elem env)]
                     [args (map (lambda (arg) (evaluate arg env)) (rest elements))])
                (match func
                  [(closure params body-list closure-env)
                   (when (not (= (length params) (length args)))
                     (error "Wrong number of arguments"))
                   (let ([extended-env (env-extend closure-env params args)])
                     ;; Evaluate all body expressions, return the last one
                     (let loop ([exprs body-list] [result #f])
                       (if (null? exprs)
                           result
                           (loop (rest exprs) (evaluate (first exprs) extended-env)))))]
                  
                  [(builtin-func name proc)
                   (apply proc args)]
                  
                  [_ (error "Cannot call non-function value: " func)]))])))]))