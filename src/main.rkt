#lang racket/base

(require racket/cmdline
         racket/port
         racket/string
         racket/file
         "lexer/lexer.rkt"
         "parser/parser.rkt"
         "evaluator/evaluator.rkt")

;; PathFinder LISP - Main Entry Point
;; A HoTT-based functional programming language with algebraic effects

;; Module exports
(provide main
         pathfinder-version
         start-repl
         evaluate-file
         evaluate-string
         tokenize
         parse)

;; Version information
(define pathfinder-version "0.1.0")

;; Tokenization (implemented in lexer/lexer.rkt)
;; tokenize function is now imported from lexer module

;; Parser (implemented in parser/parser.rkt)
;; parse function is now imported from parser module

(define (type-check ast)
  ;; Task 4: Type checker implementation
  (error "Type checker not yet implemented"))

;; evaluate function is now imported from evaluator module

;; Main evaluation pipeline
(define (evaluate-string input)
  "Evaluate a PathFinder LISP expression from string"
  (let* ([tokens (tokenize input)]
         [ast (parse tokens)])
    ;; Skip type checking for now - Task 4
    (evaluate ast)))

;; File evaluation
(define (evaluate-file filename)
  "Evaluate a PathFinder LISP file"
  (let ([input (file->string filename)])
    (evaluate-string input)))

;; REPL implementation
(define (start-repl)
  "Start the PathFinder LISP Read-Eval-Print Loop"
  (displayln (string-append "PathFinder LISP v" pathfinder-version))
  (displayln "A HoTT-based functional language with algebraic effects")
  (displayln "Type (exit) to quit")
  (newline)
  (repl-loop (make-global-environment)))

(define (repl-loop env)
  "Main REPL loop with persistent environment"
  (display "pathfinder> ")
  (flush-output)
  (let ([input (read-line)])
    (cond
      [(eof-object? input) (displayln "\nGoodbye!")]
      [(string=? (string-trim input) "(exit)") (displayln "Goodbye!")]
      [(string=? (string-trim input) "") (repl-loop env)]
      [else
       (with-handlers ([exn:fail? (lambda (e)
                                   (displayln (string-append "Error: " (exn-message e))))])
         (let* ([tokens (tokenize input)]
                [ast (parse tokens)]
                [result (evaluate ast env)])
           (displayln (format "~a" result))))
       (repl-loop env)])))

;; Command line interface
(define (main . args)
  "Main entry point for command line usage"
  (command-line
   #:program "pathfinder"
   #:once-each
   [("-v" "--version") "Show version information"
    (displayln (string-append "PathFinder LISP v" pathfinder-version))
    (exit 0)]
   [("-i" "--interactive") "Start interactive REPL"
    (start-repl)
    (exit 0)]
   #:args filename
   (cond
     [(null? filename) (start-repl)]
     [(= (length filename) 1)
      (with-handlers ([exn:fail:filesystem? 
                      (lambda (e) 
                        (displayln (string-append "Error: Cannot read file " (car filename)))
                        (exit 1))]
                     [exn:fail? 
                      (lambda (e)
                        (displayln (string-append "Error: " (exn-message e)))
                        (exit 1))])
        (evaluate-file (car filename)))]
     [else
      (displayln "Error: Too many arguments")
      (exit 1)])))

;; If run directly, start main
(module+ main
  (main (vector->list (current-command-line-arguments))))