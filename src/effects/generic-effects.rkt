#lang racket/base

(require racket/contract
         racket/match
         racket/list
         "../types/types.rkt")

(provide defeffect-syntax
         defhandler-syntax
         perform
         perform-deterministic
         perform-non-deterministic
         handle
         with-handler
         with-execution-context
         current-execution-context
         resolve-handler
         register-multi-context-handler!
         global-effect-registry
         effect-determinism
         set-effect-determinism!)

;; Global effect registry for the system
(define global-effect-registry (make-effect-registry))

;; ============================================================================
;; EXECUTION CONTEXT SYSTEM
;; ============================================================================

;; Current execution context parameter
(define current-execution-context (make-parameter 'runtime))

;; Predefined execution contexts
(define valid-execution-contexts 
  '(compile-time runtime test debug profile universal))

;; Execution context switching
(define-syntax with-execution-context
  (syntax-rules ()
    [(with-execution-context context body ...)
     (parameterize ([current-execution-context context])
       body ...)]))

;; Context validation
(define/contract (validate-execution-context context)
  (-> any/c boolean?)
  (or (member context valid-execution-contexts)
      (and (list? context) 
           (andmap (lambda (ctx) (member ctx valid-execution-contexts)) context))))

;; Context matching - does handler context support execution context?
(define/contract (context-matches? handler-context execution-context)
  (-> any/c symbol? boolean?)
  (cond
    [(eq? handler-context 'universal) #t]
    [(symbol? handler-context) (eq? handler-context execution-context)]
    [(list? handler-context) (member execution-context handler-context)]
    [else #f]))

;; Macro for defining effects (user-facing syntax)
(define-syntax defeffect-syntax
  (syntax-rules ()
    [(defeffect-syntax name (op-name input-types ... -> output-type) ...)
     (begin
       (define name-effect
         (defeffect 'name
           (defop 'op-name (list input-types ...) output-type) ...))
       (register-effect! global-effect-registry name-effect)
       'name)]))

;; Macro for defining handlers (enhanced for multi-context)
(define-syntax defhandler-syntax
  (syntax-rules ()
    [(defhandler-syntax handler-name effect-name handler-type
       [(op-name args ...) body] ...)
     (begin
       (define handler-name
         (defhandler 'effect-name 'handler-type
           (cons 'op-name (lambda (args ...) body)) ...))
       (register-multi-context-handler! global-effect-registry handler-name)
       'handler-name)]))

;; ============================================================================
;; MULTI-CONTEXT HANDLER RESOLUTION
;; ============================================================================

;; Enhanced handler resolution that supports multi-context handlers
(define/contract (resolve-handler registry effect-name op-name [execution-context (current-execution-context)])
  (->* (effect-registry? symbol? symbol?) (symbol?) (or/c procedure? #f))
  (let ([handlers (effect-registry-handlers registry)])
    (or 
      ;; 1. Look for exact context match
      (find-exact-context-handler handlers effect-name op-name execution-context)
      
      ;; 2. Look for multi-context handler that includes this context
      (find-multi-context-handler handlers effect-name op-name execution-context)
      
      ;; 3. Look for universal handler
      (find-universal-handler handlers effect-name op-name)
      
      ;; 4. No handler found
      #f)))

;; Find handler with exact context match
(define/contract (find-exact-context-handler handlers effect-name op-name context)
  (-> hash? symbol? symbol? symbol? (or/c procedure? #f))
  (let* ([key (cons effect-name context)]
         [handler (hash-ref handlers key #f)])
    (and handler
         (let ([impl (assoc op-name (effect-handler-implementations handler))])
           (and impl (cdr impl))))))

;; Find multi-context handler that supports this context
(define/contract (find-multi-context-handler handlers effect-name op-name context)
  (-> hash? symbol? symbol? symbol? (or/c procedure? #f))
  (let ([matching-handlers 
         (filter (lambda (entry)
                   (let ([handler-key (car entry)]
                         [handler (cdr entry)])
                     (and (eq? (car handler-key) effect-name)
                          (context-matches? (effect-handler-handler-type handler) context))))
                 (hash->list handlers))])
    (and (not (null? matching-handlers))
         (let* ([handler (cdar matching-handlers)]
                [impl (assoc op-name (effect-handler-implementations handler))])
           (and impl (cdr impl))))))

;; Find universal handler
(define/contract (find-universal-handler handlers effect-name op-name)
  (-> hash? symbol? symbol? (or/c procedure? #f))
  (find-exact-context-handler handlers effect-name op-name 'universal))

;; Enhanced handler registration for multi-context handlers
(define/contract (register-multi-context-handler! registry handler)
  (-> effect-registry? effect-handler? void?)
  (let ([effect-name (effect-handler-effect-name handler)]
        [handler-type (effect-handler-handler-type handler)])
    (cond
      ;; Single context or universal
      [(symbol? handler-type)
       (let ([key (cons effect-name handler-type)])
         (hash-set! (effect-registry-handlers registry) key handler))]
      
      ;; Multiple contexts - register for each
      [(list? handler-type)
       (for ([context handler-type])
         (let ([key (cons effect-name context)])
           (hash-set! (effect-registry-handlers registry) key handler)))]
      
      [else (error "Invalid handler type: " handler-type)])))

;; Perform an effect operation
(define/contract (perform effect-name op-name . args)
  (->* (symbol? symbol?) () #:rest (listof any/c) effect-instance?)
  (apply invoke-effect global-effect-registry effect-name op-name args))

;; Handle an effect using context-aware resolution
(define/contract (handle effect-instance [handler-context (current-execution-context)])
  (->* (effect-instance?) (symbol?) any/c)
  (let* ([effect-name (effect-instance-effect-name effect-instance)]
         [op-name (effect-instance-operation-name effect-instance)]
         [args (effect-instance-arguments effect-instance)]
         [handler-proc (resolve-handler global-effect-registry effect-name op-name handler-context)])
    (if handler-proc
        (apply handler-proc args)
        (error "No handler found for effect: " effect-name " operation: " op-name " in context: " handler-context))))

;; Handle an effect with a specific named handler (legacy compatibility)
(define/contract (handle-with-named-handler effect-instance handler-name)
  (-> effect-instance? symbol? any/c)
  (let* ([key (cons (effect-instance-effect-name effect-instance) handler-name)]
         [handler (hash-ref (effect-registry-handlers global-effect-registry) key #f)])
    (if handler
        (let* ([op-name (effect-instance-operation-name effect-instance)]
               [args (effect-instance-arguments effect-instance)]
               [implementations (effect-handler-implementations handler)]
               [impl (assoc op-name implementations)])
          (if impl
              (apply (cdr impl) args)
              (error "Handler does not implement operation: " op-name)))
        (error "Handler not found: " handler-name))))

;; With-handler form for scoped effect handling
(define-syntax with-handler
  (syntax-rules ()
    [(with-handler handler-name body ...)
     (parameterize ([current-handler 'handler-name])
       body ...)]))

;; Current handler parameter
(define current-handler (make-parameter #f))

;; ============================================================================
;; EXAMPLE: User-Defined Effects
;; ============================================================================

#|
;; User defines their own FileIO effect:
(defeffect-syntax FileIO
  (read-file String -> String)
  (write-file String String -> Unit)
  (file-exists String -> Bool))

;; User defines compile-time handler:
(defhandler-syntax compile-time-file-handler FileIO compile-time
  [(read-file path) 
   (if (file-exists? path)
       (file->string path)
       (error "Build failed: File not found"))]
  [(write-file path content)
   (error "Build failed: Cannot write files at compile time")]
  [(file-exists path)
   (file-exists? path)])

;; User defines runtime handler:
(defhandler-syntax runtime-file-handler FileIO runtime
  [(read-file path)
   (with-handlers ([exn:fail? (lambda (e) (error "Runtime file read failed"))])
     (file->string path))]
  [(write-file path content)
   (with-handlers ([exn:fail? (lambda (e) (error "Runtime file write failed"))])
     (display-to-file content path))]
  [(file-exists path)
   (file-exists? path)])

;; User code using effects:
(define (process-config)
  (perform 'FileIO 'read-file "config.json"))

;; Type of process-config: Unit →{FileIO} String

;; Handling at compile time:
(handle (process-config) 'compile-time-file-handler)

;; Handling at runtime:
(handle (process-config) 'runtime-file-handler)
|#

;; ============================================================================
;; EFFECT DETERMINISM SUPPORT
;; ============================================================================

;; Effect determinism registry - tracks which effects are deterministic
(define effect-determinism-registry (make-hash))

;; Get determinism status of an effect
(define/contract (effect-determinism effect-name operation)
  (-> symbol? symbol? symbol?)
  (let ([key (cons effect-name operation)])
    (hash-ref effect-determinism-registry key 'non-deterministic)))

;; Set determinism status of an effect operation
(define/contract (set-effect-determinism! effect-name operation determinism)
  (-> symbol? symbol? symbol? void?)
  (let ([key (cons effect-name operation)])
    (hash-set! effect-determinism-registry key determinism)))

;; Perform an effect with explicit deterministic annotation
(define/contract (perform-deterministic effect-name operation . args)
  (->* (symbol? symbol?) () #:rest (listof any/c) any/c)
  ;; Mark this operation as deterministic for caching
  (set-effect-determinism! effect-name operation 'deterministic)
  (apply perform effect-name operation args))

;; Perform an effect with explicit non-deterministic annotation
(define/contract (perform-non-deterministic effect-name operation . args)
  (->* (symbol? symbol?) () #:rest (listof any/c) any/c)
  ;; Mark this operation as non-deterministic (no caching)
  (set-effect-determinism! effect-name operation 'non-deterministic)
  (apply perform effect-name operation args))

;; Enhanced perform that checks determinism for caching decisions
(define/contract (perform-with-determinism effect-name operation determinism . args)
  (->* (symbol? symbol? symbol?) () #:rest (listof any/c) any/c)
  ;; Set determinism for this operation
  (set-effect-determinism! effect-name operation determinism)
  ;; Perform the effect
  (apply perform effect-name operation args))

;; ============================================================================
;; CACHE-AWARE EFFECT PERFORMANCE HELPERS
;; ============================================================================

;; Check if an effect operation should be cached
(define/contract (effect-cacheable? effect-name operation)
  (-> symbol? symbol? boolean?)
  (eq? (effect-determinism effect-name operation) 'deterministic))

;; Register common deterministic effects
(define/contract (register-deterministic-effects!)
  (-> void?)
  ;; File operations that depend on file content (deterministic if file doesn't change)
  (set-effect-determinism! 'FileIO 'read-file 'deterministic)
  (set-effect-determinism! 'FileIO 'file-exists 'deterministic)
  
  ;; Configuration and environment (deterministic within execution session)
  (set-effect-determinism! 'Environment 'get-env 'deterministic)
  (set-effect-determinism! 'Config 'read-config 'deterministic)
  
  ;; Network operations (potentially deterministic with TTL)
  (set-effect-determinism! 'Network 'http-get 'deterministic)  ; With TTL
  
  ;; Computation operations (always deterministic)
  (set-effect-determinism! 'Compute 'hash 'deterministic)
  (set-effect-determinism! 'Compute 'compress 'deterministic))

;; Register common non-deterministic effects
(define/contract (register-non-deterministic-effects!)
  (-> void?)
  ;; Time-based operations
  (set-effect-determinism! 'Time 'current-timestamp 'non-deterministic)
  (set-effect-determinism! 'Time 'current-time 'non-deterministic)
  
  ;; Random operations
  (set-effect-determinism! 'Random 'random-number 'non-deterministic)
  (set-effect-determinism! 'Random 'uuid 'non-deterministic)
  
  ;; User interaction
  (set-effect-determinism! 'User 'prompt 'non-deterministic)
  (set-effect-determinism! 'User 'read-input 'non-deterministic)
  
  ;; File modification operations
  (set-effect-determinism! 'FileIO 'write-file 'non-deterministic)
  (set-effect-determinism! 'FileIO 'delete-file 'non-deterministic))

;; Initialize effect determinism registry with common patterns
(register-deterministic-effects!)
(register-non-deterministic-effects!)