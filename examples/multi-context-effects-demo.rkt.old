#lang racket/base

(require "../src/effects/generic-effects.rkt"
         "../src/types/types.rkt")

;; ============================================================================
;; DEMONSTRATION: Enhanced 3-Tier Effect System
;; ============================================================================
;; 
;; Tier 1: Pure Computational CIFs (HoTT-native mathematical operations)
;; Tier 2+3: Unified Generic Effect System with Multi-Context Handlers
;;
;; Key Innovation: Handlers can work in:
;; - Single context: 'compile-time, 'runtime, 'test, etc.
;; - Multiple contexts: '(compile-time runtime)
;; - Universal context: 'universal (works everywhere)

;; ============================================================================
;; EFFECT DEFINITIONS (User-Defined)
;; ============================================================================

;; Configuration effect - used in all contexts
(define config-effect
  (defeffect 'Config
    (defop 'get-setting (list (inductive-type "String" '())) (inductive-type "String" '()))))

(register-effect! global-effect-registry config-effect)

;; Logging effect - context-aware
(define logging-effect
  (defeffect 'Logging
    (defop 'log-info (list (inductive-type "String" '())) Unit)
    (defop 'log-error (list (inductive-type "String" '())) Unit)))

(register-effect! global-effect-registry logging-effect)

;; File system effect - different implementations per context
(define filesystem-effect
  (defeffect 'FileSystem
    (defop 'read-file (list (inductive-type "String" '())) (inductive-type "String" '()))
    (defop 'file-exists? (list (inductive-type "String" '())) Bool)))

(register-effect! global-effect-registry filesystem-effect)

;; ============================================================================
;; HANDLER IMPLEMENTATIONS
;; ============================================================================

;; Universal config handler - adapts to execution context
(define universal-config-handler
  (defhandler 'Config 'universal
    (cons 'get-setting (lambda (key)
                         (case (current-execution-context)
                           ['compile-time (string-append "build-" key)]
                           ['runtime (string-append "prod-" key)]
                           ['test (string-append "test-" key)]
                           [else (string-append "default-" key)])))))

(register-multi-context-handler! global-effect-registry universal-config-handler)

;; Multi-context logging handler - works in development contexts
(define dev-logging-handler
  (defhandler 'Logging '(compile-time runtime debug)
    (cons 'log-info (lambda (msg) 
                      (printf "[~a] INFO: ~a~n" (current-execution-context) msg)))
    (cons 'log-error (lambda (msg) 
                       (printf "[~a] ERROR: ~a~n" (current-execution-context) msg)))))

(register-multi-context-handler! global-effect-registry dev-logging-handler)

;; Test-specific logging handler - overrides for test context
(define test-logging-handler
  (defhandler 'Logging 'test
    (cons 'log-info (lambda (msg) 
                      (printf "[TEST-CAPTURE] ~a~n" msg)))
    (cons 'log-error (lambda (msg) 
                       (printf "[TEST-CAPTURE] ERROR: ~a~n" msg)))))

(register-multi-context-handler! global-effect-registry test-logging-handler)

;; Context-specific file system handlers
(define compile-time-fs-handler
  (defhandler 'FileSystem 'compile-time
    (cons 'read-file (lambda (path) 
                       (string-append "// Generated asset: " path)))
    (cons 'file-exists? (lambda (path) #t))))

(register-multi-context-handler! global-effect-registry compile-time-fs-handler)

(define runtime-fs-handler
  (defhandler 'FileSystem 'runtime
    (cons 'read-file (lambda (path) 
                       (string-append "Runtime file content: " path)))
    (cons 'file-exists? (lambda (path) #t))))

(register-multi-context-handler! global-effect-registry runtime-fs-handler)

(define test-fs-handler
  (defhandler 'FileSystem 'test
    (cons 'read-file (lambda (path) 
                       (string-append "Mock file content: " path)))
    (cons 'file-exists? (lambda (path) #f))))

(register-multi-context-handler! global-effect-registry test-fs-handler)

;; Universal fallback file system handler
(define universal-fs-handler
  (defhandler 'FileSystem 'universal
    (cons 'read-file (lambda (path) 
                       (string-append "Universal file content: " path)))
    (cons 'file-exists? (lambda (path) #t))))

(register-multi-context-handler! global-effect-registry universal-fs-handler)

;; ============================================================================
;; DEMONSTRATION FUNCTIONS
;; ============================================================================

(define (demo-application)
  (printf "=== Demo Application Running ===~n")
  
  ;; Get configuration
  (let ([api-url-effect (perform 'Config 'get-setting "api-url")])
    (printf "API URL: ~a~n" (handle api-url-effect)))
  
  ;; Log some information
  (handle (perform 'Logging 'log-info "Application started"))
  
  ;; Check if a file exists and read it
  (let* ([file-path "config.json"]
         [exists-effect (perform 'FileSystem 'file-exists? file-path)])
    (if (handle exists-effect)
        (let ([content-effect (perform 'FileSystem 'read-file file-path)])
          (printf "File content: ~a~n" (handle content-effect)))
        (handle (perform 'Logging 'log-error "Config file not found"))))
  
  (handle (perform 'Logging 'log-info "Application finished"))
  (printf "=== Demo Complete ===~n~n"))

;; ============================================================================
;; DEMONSTRATION
;; ============================================================================

(printf "=== Enhanced 3-Tier Effect System Demonstration ===~n~n")

(printf "1. COMPILE-TIME CONTEXT:~n")
(printf "   - Uses build configuration~n")
(printf "   - Reads from asset pipeline~n")
(printf "   - Logs to build system~n")
(with-execution-context 'compile-time
  (demo-application))

(printf "2. RUNTIME CONTEXT:~n")
(printf "   - Uses production configuration~n")
(printf "   - Reads actual files~n")
(printf "   - Logs to application logger~n")
(with-execution-context 'runtime
  (demo-application))

(printf "3. TEST CONTEXT:~n")
(printf "   - Uses test configuration~n")
(printf "   - Uses mock file system~n")
(printf "   - Captures test logs~n")
(with-execution-context 'test
  (demo-application))

(printf "4. DEBUG CONTEXT:~n")
(printf "   - Falls back to universal handlers~n")
(printf "   - Uses default behaviors~n")
(with-execution-context 'debug
  (demo-application))

(printf "=== Key Benefits Demonstrated ===~n")
(printf "✓ Same effect definitions work in all contexts~n")
(printf "✓ Handlers can be universal, multi-context, or specific~n")
(printf "✓ Automatic handler resolution by execution context~n")
(printf "✓ Easy testing with mock handlers~n")
(printf "✓ Compile-time vs runtime separation maintained~n")
(printf "✓ Fully user-extensible - no hardcoded effects~n")