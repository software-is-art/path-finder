#!/usr/bin/env guile
!#

;;; PathFinder Bootstrap VM in Guile Scheme
;;; This is a minimal but correct implementation that demonstrates
;;; PathFinder's metacircular compilation capabilities.

(use-modules (ice-9 match)
             (ice-9 pretty-print)
             (ice-9 rdelim)
             (srfi srfi-1))

;; Load our modules
(add-to-load-path (dirname (current-filename)))
(load "evaluator.scm")
(load "primitives.scm") 
(load "parser.scm")
(load "effects.scm")
(load "environment.scm")

(define (read-all-sexps port)
  "Read all s-expressions from a port"
  (let loop ((sexps '()))
    (let ((sexp (read port)))
      (if (eof-object? sexp)
          (reverse sexps)
          (loop (cons sexp sexps))))))

(define (run-bootstrap filename)
  "Run PathFinder bootstrap on a file"
  (format #t "PathFinder Bootstrap VM (Guile Edition)~%")
  (format #t "Loading: ~a~%~%" filename)
  
  (let* ((sexps (call-with-input-file filename read-all-sexps))
         (env (make-initial-environment))
         (defines '())
         (others '()))
    
    ;; Pass 1: Categorize top-level forms
    (for-each
      (lambda (sexp)
        (cond
          ;; Collect defines
          ((and (pair? sexp) (eq? (car sexp) 'define))
           (set! defines (cons sexp defines)))
          ;; Everything else
          (else
           (set! others (cons sexp others)))))
      sexps)
    
    ;; Pass 2: Create environment with all define names
    (let ((full-env env))
      ;; Add placeholder for each define
      (for-each
        (lambda (define-sexp)
          (let ((name (cadr define-sexp)))
            (set! full-env 
                  (env-extend full-env 
                              (symbol->string name) 
                              `(placeholder ,name)))))
        (reverse defines))
      
      ;; Pass 3: Evaluate all defines with full environment
      (for-each
        (lambda (define-sexp)
          (catch #t
            (lambda ()
              (let* ((name (cadr define-sexp))
                     (value-sexp (caddr define-sexp))
                     (ast (parse-pathfinder value-sexp))
                     (value (pathfinder-eval ast full-env)))
                (set! full-env 
                      (env-update full-env 
                                  (symbol->string name) 
                                  value))
                (format #t "Defined: ~a~%" name)))
            (lambda (key . args)
              (format #t "Error evaluating ~a: ~a~%" define-sexp args)
              ;; For critical errors in defines, we should probably exit
              ;; but for now just continue to be backwards compatible
              )))
        (reverse defines))
      
      ;; Process other forms (imports, expressions, etc)
      (for-each
        (lambda (sexp)
          (catch #t
            (lambda ()
              (cond
                ;; Handle type declarations (just for documentation)
                ((and (pair? sexp) (eq? (car sexp) 'type))
                 (format #t "Type: ~a : ~a~%" (cadr sexp) (caddr sexp)))
                
                ;; Handle imports
                ((and (pair? sexp) (eq? (car sexp) 'import))
                 (let ((new-env (handle-import (cadr sexp) full-env)))
                   (set! full-env new-env)))
                
                ;; Evaluate other expressions
                (else
                 (let* ((ast (parse-pathfinder sexp))
                        (result (pathfinder-eval ast full-env)))
                   ;; Handle different result types
                   (cond
                     ((effect? result)
                      (execute-effect result))
                     ;; Constructor returned from top-level - silently ignore
                     ((and (pair? result) (eq? (car result) 'constructor))
                      #t)
                     (else
                      ;; For debugging, show other results
                      (format #t "Result: ~a~%" result)))))))
            
            (lambda (key . args)
              (format #t "Error evaluating ~a: ~a~%" sexp args))))
        (reverse others)))
    
    (format #t "~%Bootstrap complete!~%")))

;; Track loaded modules to prevent circular dependencies
(define loaded-modules (make-hash-table))

;; Track exported symbols from modules
(define module-exports (make-hash-table))

;; Load a module file and return its environment
(define (load-module filename base-env)
  (if (not (file-exists? filename))
      (error "Module file not found:" filename)
      (let* ((sexps (call-with-input-file filename read-all-sexps))
             (module-env base-env)
             (exports '())
             (defines '())
             (imports '())
             (others '()))
        
        ;; Pass 1: Categorize forms
        (for-each
          (lambda (sexp)
            (cond
              ;; Track exports
              ((and (pair? sexp) (eq? (car sexp) 'export))
               (set! exports (cons (cadr sexp) exports)))
              
              ;; Collect defines for two-pass evaluation
              ((and (pair? sexp) (eq? (car sexp) 'define))
               (set! defines (cons sexp defines)))
              
              ;; Collect imports
              ((and (pair? sexp) (eq? (car sexp) 'import))
               (set! imports (cons sexp imports)))
              
              ;; Collect other forms
              (else 
               (set! others (cons sexp others)))))
          sexps)
        
        ;; Process imports first
        (for-each
          (lambda (import-sexp)
            (set! module-env (handle-import (cadr import-sexp) module-env)))
          (reverse imports))
        
        ;; Pass 2: Create placeholders for all defines
        ;; This allows mutual recursion
        (let ((placeholder-env module-env))
          (for-each
            (lambda (define-sexp)
              (let ((name (cadr define-sexp)))
                ;; Add placeholder binding
                (set! placeholder-env 
                      (env-extend placeholder-env 
                                  (symbol->string name) 
                                  `(placeholder ,name)))))
            (reverse defines))
          
          ;; Pass 3: Evaluate all defines with full environment
          (for-each
            (lambda (define-sexp)
              (let* ((name (cadr define-sexp))
                     (value-sexp (caddr define-sexp))
                     (ast (parse-pathfinder value-sexp))
                     ;; Evaluate in environment with all names available
                     (value (pathfinder-eval ast placeholder-env)))
                ;; Update the binding with actual value
                (set! placeholder-env 
                      (env-update placeholder-env 
                                  (symbol->string name) 
                                  value))))
            (reverse defines))
          
          ;; Process other forms
          (for-each
            (lambda (sexp)
              (let ((ast (parse-pathfinder sexp)))
                (pathfinder-eval ast placeholder-env)))
            (reverse others))
          
          ;; Store exports for this module
          (hash-set! module-exports filename exports)
          placeholder-env))))

;; Extract only exported symbols from module environment
(define (extract-exports module-env base-env filename)
  (let ((exports (hash-ref module-exports filename '())))
    (if (null? exports)
        ;; If no exports specified, export nothing (or could export all)
        base-env
        ;; Add only exported symbols to base environment
        (fold (lambda (export-name env)
                (let ((value (env-lookup-safe module-env (symbol->string export-name))))
                  (if value
                      (env-extend env (symbol->string export-name) value)
                      env)))
              base-env
              exports))))

;; Handle imports with enhanced path support
(define (handle-import import-spec env)
  (match import-spec
    ;; String path (relative or absolute)
    ((? string? path)
     (let* ((filename (resolve-module-path path))
            (module-id (path->module-id filename)))
       (format #t "Importing: ~a~%" filename)
       (load-module-with-checks module-id filename env)))
    
    ;; Simple symbol name
    ((? symbol? name)
     (let* ((filename (find-module-file (symbol->string name)))
            (module-id (symbol->string name)))
       (if filename
           (begin
             (format #t "Importing: ~a from ~a~%" name filename)
             (load-module-with-checks module-id filename env))
           (begin
             (format #t "ERROR: Module not found: ~a~%" name)
             (format #t "  Searched in: src/, ./, ./src/bootstrap/~%")
             env))))
    
    ;; Original (category module) format
    (((? symbol? category) (? symbol? module))
     (let* ((module-path (format #f "~a/~a" category module))
            (filename (format #f "src/~a/~a.sexp" category module)))
       (format #t "Importing: ~a from ~a~%" module-path filename)
       (load-module-with-checks module-path filename env)))
    
    (_ 
     (format #t "ERROR: Invalid import format: ~a~%" import-spec)
     (format #t "  Expected: (import \"path\"), (import name), or (import (category module))~%")
     env)))

;; Helper: Resolve module path
(define (resolve-module-path path)
  "Resolve a module path relative to current directory"
  (cond
    ;; Absolute path
    ((absolute-file-name? path) path)
    ;; Relative path with .sexp extension
    ((string-suffix? ".sexp" path) path)
    ;; Relative path without extension
    (else (string-append path ".sexp"))))

;; Helper: Find module file by name
(define (find-module-file name)
  "Search for a module file in standard locations"
  (let ((search-paths (list
                       (format #f "~a.sexp" name)
                       (format #f "./~a.sexp" name)
                       (format #f "src/~a.sexp" name)
                       (format #f "src/bootstrap/~a.sexp" name))))
    (find file-exists? search-paths)))

;; Helper: Convert path to module ID for tracking
(define (path->module-id path)
  "Convert a file path to a module ID"
  ;; Use the path as-is for now (could normalize later)
  path)

;; Helper: Load module with dependency checks
(define (load-module-with-checks module-id filename env)
  "Load a module with circular dependency checking"
  (let ((status (hash-ref loaded-modules module-id #f)))
    (cond
      ;; Already loaded
      ((eq? status 'loaded)
       (format #t "  (already loaded)~%")
       env)
      ;; Currently loading - circular dependency!
      ((eq? status 'loading)
       (format #t "ERROR: Circular dependency detected for ~a~%" module-id)
       (error "Circular dependency:" module-id))
      ;; Not loaded yet
      (else
       (if (file-exists? filename)
           (catch #t
             (lambda ()
               ;; Mark as loading to detect circular deps
               (hash-set! loaded-modules module-id 'loading)
               
               ;; Load and evaluate the module
               (let* ((module-env (load-module filename env))
                      ;; Extract only exported symbols
                      (exported-env (extract-exports module-env env filename)))
                 
                 ;; Mark as loaded
                 (hash-set! loaded-modules module-id 'loaded)
                 
                 ;; Return environment with imported symbols
                 exported-env))
             (lambda (key . args)
               (hash-remove! loaded-modules module-id)
               (format #t "Error loading module ~a: ~a~%" module-id args)
               env))
           (begin
             (format #t "ERROR: Module file not found: ~a~%" filename)
             env))))))

;; Command line handling
(define (main args)
  (match args
    ((program filename)
     (if (file-exists? filename)
         (run-bootstrap filename)
         (format #t "Error: File not found: ~a~%" filename)))
    ((program "-e" expr)
     ;; Evaluate a single expression
     (let* ((sexp (call-with-input-string expr read))
            (ast (parse-pathfinder sexp))
            (env (make-initial-environment))
            (result (pathfinder-eval ast env)))
       (pretty-print result)))
    (_
     (format #t "Usage: ~a <file.sexp>~%" (car args))
     (format #t "   or: ~a -e '<expression>'~%" (car args))
     (exit 1))))

;; Run main with command-line arguments
(main (command-line))