#lang racket/base

(require racket/system
         racket/file
         racket/string
         racket/match)

;; Custom linting script for PathFinder LISP
;; Runs multiple static analysis tools

(define (run-command cmd)
  "Run a shell command and return success status"
  (printf "Running: ~a~n" cmd)
  (system cmd))

(define (find-racket-files dir)
  "Find all .rkt files in directory"
  (find-files (lambda (path) (string-suffix? (path->string path) ".rkt")) dir))

(define (lint-file file)
  "Lint a single Racket file"
  (printf "Linting ~a...~n" file)
  (let ([result (run-command (format "racket -e '(require \"~a\")' > /dev/null 2>&1" file))])
    (if (= result 0)
        (printf "✓ ~a~n" file)
        (printf "✗ ~a~n" file))
    (= result 0)))

(define (main)
  "Main linting function"
  (printf "PathFinder LISP Static Analysis~n")
  (printf "================================~n~n")
  
  ;; Check dependencies
  (printf "1. Checking dependencies...~n")
  (run-command "raco check-deps src/")
  
  ;; Syntax check all files
  (printf "~n2. Syntax checking...~n")
  (let ([files (append (find-racket-files "src/") (find-racket-files "tests/"))])
    (let ([results (map lint-file files)])
      (let ([passed (length (filter identity results))]
            [total (length results)])
        (printf "~n=== Lint Results ===~n")
        (printf "Files checked: ~a~n" total)
        (printf "Passed: ~a~n" passed)
        (if (= passed total)
            (printf "All files passed linting! ✓~n")
            (printf "Some files failed linting. ✗~n")))))
  
  ;; Format check
  (printf "~n3. Format checking...~n")
  (run-command "find src tests -name '*.rkt' -exec raco fmt --check {} \\; || echo 'Run: devbox run fmt'"))

(main)