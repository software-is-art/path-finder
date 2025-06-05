#lang racket/base

(require rackunit
         racket/list
         "../src/main.rkt"
         "../src/types/types.rkt"
         "../src/effects/generic-effects.rkt"
         "../src/typecheck/effect-checker.rkt")

;; Test suite for Generic Effect System

(test-case "User-defined effects"
  
  ;; User can define custom effects
  (let ([file-io-effect (defeffect 'FileIO
                          (defop 'read-file (list (inductive-type "String" '())) 
                                           (inductive-type "String" '()))
                          (defop 'write-file (list (inductive-type "String" '()) 
                                                  (inductive-type "String" '())) 
                                            Unit))])
    (check-true (effect-definition? file-io-effect)
                "Should create effect definition")
    (check-equal? (effect-definition-name file-io-effect) 'FileIO
                  "Effect should have correct name")
    (check-equal? (length (effect-definition-operations file-io-effect)) 2
                  "FileIO should have 2 operations"))
  
  ;; Effects are not hardcoded in the type system
  (let ([registry (make-effect-registry)])
    (check-false (effect-registered? registry 'FileIO)
                 "FileIO should not be registered by default")
    (register-effect! registry (defeffect 'FileIO))
    (check-true (effect-registered? registry 'FileIO)
                "FileIO should be registered after user defines it")))

(test-case "Generic effect handlers"
  
  ;; User can define handlers for their effects
  (let ([mock-handler (defhandler 'FileIO 'test
                        (cons 'read-file (lambda (path) "mock content"))
                        (cons 'write-file (lambda (path content) 'ok)))])
    (check-true (effect-handler? mock-handler)
                "Should create effect handler")
    (check-equal? (effect-handler-effect-name mock-handler) 'FileIO
                  "Handler should be for FileIO effect")
    (check-equal? (effect-handler-handler-type mock-handler) 'test
                  "Handler should be test type")))

(test-case "Effect type inference"
  
  (let ([env (make-effect-environment)])
    ;; Pure expressions have no effects
    (check-true (effect-empty? (infer-effects (number-atom 42) env))
                "Literals should have no effects")
    
    ;; Generic perform adds effects
    (let ([perform-expr (sexpr (list (symbol-atom "perform")
                                    (string-atom "FileIO") 
                                    (string-atom "read-file")
                                    (string-atom "test.txt")))])
      (let ([effects (infer-effects perform-expr env)])
        (check-false (effect-empty? effects)
                     "Perform should infer effects")
        (check-true (member 'FileIO (effect-set-effects effects))
                    "Should infer FileIO effect from perform")))))

(test-case "No hardcoded effects in type checker"
  
  ;; The type checker doesn't know about specific effects
  (let ([env (make-effect-environment)])
    ;; Can't find hardcoded file-read effect
    (check-false (type-env-lookup (effect-environment-type-env env) "file-read")
                 "No hardcoded file-read in type environment")
    (check-false (type-env-lookup (effect-environment-type-env env) "http-get")
                 "No hardcoded http-get in type environment")
    
    ;; Only generic effect operations
    (check-true (type-env-lookup (effect-environment-type-env env) "perform")
                "Should have generic perform operation")
    (check-true (type-env-lookup (effect-environment-type-env env) "handle")
                "Should have generic handle operation")))

(test-case "Effect composition with user-defined effects"
  
  ;; Register some user effects
  (let ([registry (make-effect-registry)])
    (register-effect! registry (defeffect 'FileIO))
    (register-effect! registry (defeffect 'NetworkIO))
    (register-effect! registry (defeffect 'Console))
    
    ;; Effects compose generically
    (let ([e1 (make-effect-set 'FileIO)]
          [e2 (make-effect-set 'NetworkIO)]
          [e3 (make-effect-set 'Console)])
      (let ([combined (effect-union e1 (effect-union e2 e3))])
        (check-equal? (length (effect-set-effects combined)) 3
                      "Should combine all user-defined effects")
        (check-true (andmap (lambda (e) (member e (effect-set-effects combined)))
                           '(FileIO NetworkIO Console))
                    "Should contain all effects")))))

(test-case "Handler resolution is generic"
  
  (let ([registry (make-effect-registry)])
    ;; Register effect and handlers
    (register-effect! registry 
                      (defeffect 'Database
                        (defop 'query (list (inductive-type "String" '())) 
                                     (inductive-type "List" '()))))
    
    (register-handler! registry
                       (defhandler 'Database 'mock
                         (cons 'query (lambda (sql) '(mock data)))))
    
    (register-handler! registry
                       (defhandler 'Database 'production
                         (cons 'query (lambda (sql) (error "Real DB query")))))
    
    ;; Different handlers for same effect
    (check-true (hash-has-key? (effect-registry-handlers registry) 
                              (cons 'Database 'mock))
                "Should have mock handler")
    (check-true (hash-has-key? (effect-registry-handlers registry)
                              (cons 'Database 'production))
                "Should have production handler")))

(test-case "True extensibility"
  
  ;; Users can add arbitrary effects without modifying compiler
  (let ([registry (make-effect-registry)])
    ;; Define a completely custom effect
    (let ([quantum-effect 
           (defeffect 'Quantum
             (defop 'superposition (list Unit) (inductive-type "Qubit" '()))
             (defop 'entangle (list (inductive-type "Qubit" '()) 
                                   (inductive-type "Qubit" '())) 
                              (inductive-type "EntangledPair" '()))
             (defop 'measure (list (inductive-type "Qubit" '())) 
                            Bool))])
      
      (register-effect! registry quantum-effect)
      (check-true (effect-registered? registry 'Quantum)
                  "Can register custom Quantum effect")
      
      ;; Define handler for quantum effect
      (let ([quantum-handler
             (defhandler 'Quantum 'simulator
               (cons 'superposition (lambda () '|0>))
               (cons 'entangle (lambda (q1 q2) (list q1 q2)))
               (cons 'measure (lambda (q) (= 0 (random 2)))))])
        
        (register-handler! registry quantum-handler)
        (check-true (hash-has-key? (effect-registry-handlers registry)
                                  (cons 'Quantum 'simulator))
                    "Can register handler for custom effect")))))

;; Run the tests
(printf "Running Generic Effect System tests...~n")