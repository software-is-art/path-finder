#!/usr/bin/env racket
#lang racket/base

(require racket/cmdline
         racket/pretty
         racket/match
         racket/file
         racket/port
         racket/list
         json)

;; Simple S-Expression CLI Operations
;; Usage: racket simple-sexp-cli.rkt <operation> <file> [args...]

;; Helper functions for path navigation
(define (get-at-path expr path)
  "Navigate to expression at given path (list of indices)"
  (if (null? path)
      expr
      (let ([index (car path)]
            [rest (cdr path)])
        (if (list? expr)
            (if (< index (length expr))
                (get-at-path (list-ref expr index) rest)
                (error "Path index out of bounds"))
            (error "Cannot navigate into non-list")))))

(define (set-at-path expr path new-expr)
  "Replace expression at given path with new expression"
  (if (null? path)
      new-expr
      (let ([index (car path)]
            [rest (cdr path)])
        (if (list? expr)
            (if (< index (length expr))
                (let ([before (take expr index)]
                      [after (drop expr (+ index 1))]
                      [modified-item (set-at-path (list-ref expr index) rest new-expr)])
                  (append before (list modified-item) after))
                (error "Path index out of bounds"))
            (error "Cannot navigate into non-list")))))

(define (wrap-at-path expr path wrapper-symbol)
  "Wrap expression at path with a new list starting with wrapper-symbol"
  (let ([target (get-at-path expr path)])
    (set-at-path expr path (list (string->symbol wrapper-symbol) target))))

;; JSON utilities
(define (parse-path-arg str)
  "Parse path argument from JSON string"
  (string->jsexpr str))

(define (output-result success result [error-msg #f])
  "Output JSON result"
  (displayln (jsexpr->string 
              (hash 'success success
                    'result (if success result #f)
                    'error (if success #f error-msg)))))

;; Main operations
(define (cmd-read file-path)
  "Read and parse S-expression from file"
  (with-handlers ([exn:fail? (lambda (e) (output-result #f #f (exn-message e)))])
    (let ([expr (call-with-input-file file-path read)])
      (output-result #t (format "~s" expr)))))

(define (cmd-format file-path)
  "Format S-expression file"
  (with-handlers ([exn:fail? (lambda (e) (output-result #f #f (exn-message e)))])
    (let ([expr (call-with-input-file file-path read)])
      (output-result #t (format "~a" (pretty-format expr))))))

(define (cmd-modify file-path path-str expr-str)
  "Modify S-expression at path"
  (with-handlers ([exn:fail? (lambda (e) (output-result #f #f (exn-message e)))])
    (let* ([path (parse-path-arg path-str)]
           [new-expr (read (open-input-string expr-str))]
           [expr (call-with-input-file file-path read)]
           [modified (set-at-path expr path new-expr)])
      (call-with-output-file file-path #:exists 'replace
        (lambda (out) (pretty-print modified out)))
      (output-result #t (format "~a" (pretty-format modified))))))

(define (cmd-wrap file-path path-str wrapper)
  "Wrap S-expression at path"
  (with-handlers ([exn:fail? (lambda (e) (output-result #f #f (exn-message e)))])
    (let* ([path (parse-path-arg path-str)]
           [expr (call-with-input-file file-path read)]
           [modified (wrap-at-path expr path wrapper)])
      (call-with-output-file file-path #:exists 'replace
        (lambda (out) (pretty-print modified out)))
      (output-result #t (format "~a" (pretty-format modified))))))

;; Main command dispatcher
(define (main args)
  (when (< (length args) 2)
    (output-result #f #f "Usage: racket simple-sexp-cli.rkt <operation> <file> [args...]")
    (exit 1))
  
  (let ([operation (car args)]
        [file-path (cadr args)]
        [rest-args (cddr args)])
    (case (string->symbol operation)
      [(read) (cmd-read file-path)]
      [(format) (cmd-format file-path)]
      [(modify) 
       (when (< (length rest-args) 2)
         (output-result #f #f "modify requires path and expression")
         (exit 1))
       (cmd-modify file-path (car rest-args) (cadr rest-args))]
      [(wrap)
       (when (< (length rest-args) 2)
         (output-result #f #f "wrap requires path and wrapper symbol")
         (exit 1))
       (cmd-wrap file-path (car rest-args) (cadr rest-args))]
      [else (output-result #f #f (format "Unknown operation: ~a" operation))])))

;; Run if called directly
(module+ main
  (main (vector->list (current-command-line-arguments))))