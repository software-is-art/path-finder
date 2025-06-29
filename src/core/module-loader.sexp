;; ============================================================================
;; MODULE LOADER - INTEGRATION WITH EFFECTS AND CACHING
;; ============================================================================
;; High-level module loading with dependency resolution and caching

(import types types)
(import effects effects)
(import core modules)
(import parser parser)
(import core evaluator)

;; ============================================================================
;; MODULE LOADING CONTEXT
;; ============================================================================

;; Loading context tracks state during module loading
(data LoadingContext U0
  (case loading-context (-> ModuleCache           ;; Module cache
                           (List ModuleId)         ;; Loading stack (cycle detection)
                           Environment             ;; Global environment
                           LoadingContext)))

;; ============================================================================
;; HIGH-LEVEL MODULE LOADING
;; ============================================================================

;; Load module with all dependencies
(type load-module-with-deps (-> String LoadingContext (Effect (Pair Module LoadingContext))))
(define load-module-with-deps
  (fn (path ctx)
    (match ctx
      (case (loading-context cache stack env)
        ;; Check for circular dependency
        (if (is-loading? path stack)
            (error-effect (string-append "Circular dependency: " path))
            ;; Load the module
            (>>= (load-module path cache)
                 (fn (module-pair)
                   (let ((module (first module-pair)))
                     (let ((new-cache (second module-pair)))
                       (let ((mod-id (get-module-id module)))
                         (let ((new-stack (cons mod-id stack)))
                           (let ((new-ctx (loading-context new-cache new-stack env)))
                             ;; Load dependencies
                             (>>= (load-dependencies module new-ctx)
                                  (fn (deps-ctx)
                                    ;; Remove from stack after loading
                                    (let ((final-ctx (pop-loading-stack deps-ctx)))
                                      (pure (pair module final-ctx)))))))))))))))))

;; Load all dependencies of a module
(type load-dependencies (-> Module LoadingContext (Effect LoadingContext)))
(define load-dependencies
  (fn (module ctx)
    (let ((imports (get-module-imports module)))
      (load-import-list imports ctx))))

;; Load a list of imports
(type load-import-list (-> (List Import) LoadingContext (Effect LoadingContext)))
(define load-import-list
  (fn (imports ctx)
    (List-elim imports
      (pure ctx)
      (fn (import rest rec)
        (>>= (load-single-import import ctx)
             (fn (new-ctx)
               (load-import-list rest new-ctx)))))))

;; Load a single import
(type load-single-import (-> Import LoadingContext (Effect LoadingContext)))
(define load-single-import
  (fn (import ctx)
    (let ((path (import-path import)))
      (>>= (load-module-with-deps path ctx)
           (fn (module-ctx-pair)
             (pure (second module-ctx-pair)))))))

;; ============================================================================
;; MODULE EXECUTION
;; ============================================================================

;; Execute module in environment (apply all definitions)
(type execute-module (-> Module Environment (Effect Environment)))
(define execute-module
  (fn (module env)
    (let ((exports (module-exports module)))
      (pure (merge-environments env exports)))))

;; ============================================================================
;; REPL INTEGRATION
;; ============================================================================

;; REPL command to load a module
(type repl-load-module (-> String Environment (Effect Environment)))
(define repl-load-module
  (fn (path env)
    (let ((initial-ctx (loading-context empty-cache nil env)))
      (>>= (load-module-with-deps path initial-ctx)
           (fn (module-ctx-pair)
             (let ((module (first module-ctx-pair)))
               (execute-module module env)))))))

;; ============================================================================
;; BOOTSTRAP SUPPORT
;; ============================================================================

;; Load the initial bootstrap modules
(type load-bootstrap-modules (-> (Effect Environment)))
(define load-bootstrap-modules
  (let ((bootstrap-paths (list "core/types.sexp"
                              "core/foundations.sexp"
                              "core/eliminators.sexp"
                              "types/types.sexp"
                              "evaluator/values.sexp")))
    (load-module-list bootstrap-paths empty-env)))

;; Load a list of modules in order
(type load-module-list (-> (List String) Environment (Effect Environment)))
(define load-module-list
  (fn (paths env)
    (List-elim paths
      (pure env)
      (fn (path rest rec)
        (>>= (repl-load-module path env)
             (fn (new-env)
               (load-module-list rest new-env)))))))

;; ============================================================================
;; HELPER FUNCTIONS
;; ============================================================================

;; Get module ID from module
(type get-module-id (-> Module ModuleId))
(define get-module-id
  (fn (mod)
    (match mod
      (case (module id imports env) id))))

;; Get module imports
(type get-module-imports (-> Module (List Import)))
(define get-module-imports
  (fn (mod)
    (match mod
      (case (module id imports env) imports))))

;; Get import path
(type import-path (-> Import String))
(define import-path
  (fn (import)
    (match import
      (case (import-all path) path)
      (case (import-qualified path qual) path)
      (case (import-selective path names) path))))

;; Check if module is being loaded
(type is-loading? (-> String (List ModuleId) Bool))
(define is-loading?
  (fn (path stack)
    (List-elim stack
      false
      (fn (mod-id rest rec)
        (match mod-id
          (case (module-id mod-path hash)
            (Bool-or (string-equal? path mod-path) rec)))))))

;; Pop loading stack
(type pop-loading-stack (-> LoadingContext LoadingContext))
(define pop-loading-stack
  (fn (ctx)
    (match ctx
      (case (loading-context cache stack env)
        (match stack
          (case nil ctx)  ;; Empty stack
          (case (cons _ rest)
            (loading-context cache rest env)))))))

;; Empty module cache
(type empty-cache ModuleCache)
(define empty-cache nil)

;; Error effect
(type error-effect (-> String (Effect A)))
(define error-effect
  (fn (msg)
    (io "error" "throw" (list (string-value msg)) non-deterministic)))

;; This provides a complete module loading system that:
;; 1. Tracks dependencies and detects cycles
;; 2. Caches compiled modules by content hash
;; 3. Integrates with the effect system for I/O
;; 4. Supports different import styles
;; 5. Maintains pure functional semantics