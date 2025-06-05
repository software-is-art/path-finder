#!/usr/bin/env racket
#lang racket/base

(require racket/cmdline
         racket/pretty
         racket/match
         racket/file
         racket/port
         racket/list
         json)

;; S-Expression CLI Operations
;; Usage: racket sexp-cli.rkt <operation> <file> [args...]

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
                (let ([lst (list->vector expr)])
                  (vector-set! lst index (set-at-path (vector-ref lst index) rest new-expr))
                  (vector->list lst))
                (error "Path index out of bounds"))
            (error "Cannot navigate into non-list")))))

(define (insert-at-path expr path index new-expr)
  "Insert new expression at given path and index"
  (if (null? path)
      (if (list? expr)
          (append (take expr index)
                  (list new-expr)
                  (drop expr index))
          (error "Cannot insert into non-list"))
      (let ([idx (car path)]
            [rest (cdr path)])
        (if (list? expr)
            (if (< idx (length expr))
                (let ([lst (list->vector expr)])
                  (vector-set! lst idx (insert-at-path (vector-ref lst idx) rest index new-expr))
                  (vector->list lst))
                (error "Path index out of bounds"))
            (error "Cannot navigate into non-list")))))

(define (delete-at-path expr path)
  "Delete expression at given path"
  (if (= (length path) 1)
      (let ([index (car path)])
        (if (list? expr)
            (if (< index (length expr))
                (append (take expr index) (drop expr (+ index 1)))
                (error "Path index out of bounds"))
            (error "Cannot delete from non-list")))
      (let ([index (car path)]
            [rest (cdr path)])
        (if (list? expr)
            (if (< index (length expr))
                (let ([lst (list->vector expr)])
                  (vector-set! lst index (delete-at-path (list-ref expr index) rest))
                  (vector->list lst))
                (error "Path index out of bounds"))
            (error "Cannot navigate into non-list")))))

(define (wrap-at-path expr path wrapper-symbol)
  "Wrap expression at path with a new list starting with wrapper-symbol"
  (let ([target (get-at-path expr path)])
    (set-at-path expr path (list (string->symbol wrapper-symbol) target))))

;; Advanced operations (paredit-style)
(define (slurp-forward expr path)
  "Move next sibling into this list"
  (let* ([parent-path (drop-right path 1)]
         [index (last path)]
         [parent (get-at-path expr parent-path)]
         [target-list (list-ref parent index)])
    (if (and (list? parent) (< (+ index 1) (length parent)) (list? target-list))
        (let ([next-sibling (list-ref parent (+ index 1))]
              [new-target (append target-list (list next-sibling))]
              [new-parent (append (take parent index)
                                  (list new-target)
                                  (drop parent (+ index 2)))])
          (set-at-path expr parent-path new-parent))
        (error "Cannot slurp forward"))))

(define (barf-forward expr path)
  "Move last element out of this list"
  (let* ([target-list (get-at-path expr path)]
         [parent-path (drop-right path 1)]
         [index (last path)]
         [parent (get-at-path expr parent-path)])
    (if (and (list? target-list) (> (length target-list) 1) (list? parent))
        (let ([last-elem (last target-list)]
              [new-target (drop-right target-list 1)]
              [new-parent (append (take parent (+ index 1))
                                  (list last-elem)
                                  (drop parent (+ index 1)))])
          (set-at-path (set-at-path expr path new-target) parent-path new-parent))
        (error "Cannot barf forward"))))

(define (splice expr path)
  "Remove wrapping parentheses, moving children up one level"
  (let* ([target-list (get-at-path expr path)]
         [parent-path (drop-right path 1)]
         [index (last path)]
         [parent (get-at-path expr parent-path)])
    (if (and (list? target-list) (list? parent))
        (let ([new-parent (append (take parent index)
                                  target-list
                                  (drop parent (+ index 1)))])
          (set-at-path expr parent-path new-parent))
        (error "Cannot splice"))))

;; JSON utilities
(define (parse-json-args args)
  "Parse JSON arguments from command line"
  (cond
    [(null? args) '()]
    [(string=? (car args) "--path")
     (cons (cons 'path (string->jsexpr (cadr args))) (parse-json-args (cddr args)))]
    [(string=? (car args) "--index")
     (cons (cons 'index (string->number (cadr args))) (parse-json-args (cddr args)))]
    [(string=? (car args) "--wrapper")
     (cons (cons 'wrapper (cadr args)) (parse-json-args (cddr args)))]
    [(string=? (car args) "--expr")
     (cons (cons 'expr (read (open-input-string (cadr args)))) (parse-json-args (cddr args)))]
    [else (parse-json-args (cdr args))]))

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

(define (cmd-modify file-path args)
  "Modify S-expression at path"
  (with-handlers ([exn:fail? (lambda (e) (output-result #f #f (exn-message e)))])
    (let* ([parsed-args (parse-json-args args)]
           [path (cdr (assoc 'path parsed-args))]
           [new-expr (cdr (assoc 'expr parsed-args))]
           [expr (call-with-input-file file-path read)]
           [modified (set-at-path expr path new-expr)])
      (call-with-output-file file-path #:exists 'replace
        (lambda (out) (pretty-print modified out)))
      (output-result #t (format "~a" (pretty-format modified))))))

(define (cmd-insert file-path args)
  "Insert S-expression at path"
  (with-handlers ([exn:fail? (lambda (e) (output-result #f #f (exn-message e)))])
    (let* ([parsed-args (parse-json-args args)]
           [path (cdr (assoc 'path parsed-args))]
           [index (cdr (assoc 'index parsed-args))]
           [new-expr (cdr (assoc 'expr parsed-args))]
           [expr (call-with-input-file file-path read)]
           [modified (insert-at-path expr path index new-expr)])
      (call-with-output-file file-path #:exists 'replace
        (lambda (out) (pretty-print modified out)))
      (output-result #t (format "~a" (pretty-format modified))))))

(define (cmd-delete file-path args)
  "Delete S-expression at path"
  (with-handlers ([exn:fail? (lambda (e) (output-result #f #f (exn-message e)))])
    (let* ([parsed-args (parse-json-args args)]
           [path (cdr (assoc 'path parsed-args))]
           [expr (call-with-input-file file-path read)]
           [modified (delete-at-path expr path)])
      (call-with-output-file file-path #:exists 'replace
        (lambda (out) (pretty-print modified out)))
      (output-result #t (format "~a" (pretty-format modified))))))

(define (cmd-wrap file-path args)
  "Wrap S-expression at path"
  (with-handlers ([exn:fail? (lambda (e) (output-result #f #f (exn-message e)))])
    (let* ([parsed-args (parse-json-args args)]
           [path (cdr (assoc 'path parsed-args))]
           [wrapper (cdr (assoc 'wrapper parsed-args))]
           [expr (call-with-input-file file-path read)]
           [modified (wrap-at-path expr path wrapper)])
      (call-with-output-file file-path #:exists 'replace
        (lambda (out) (pretty-print modified out)))
      (output-result #t (format "~a" (pretty-format modified))))))

(define (cmd-slurp-forward file-path args)
  "Slurp forward operation"
  (with-handlers ([exn:fail? (lambda (e) (output-result #f #f (exn-message e)))])
    (let* ([parsed-args (parse-json-args args)]
           [path (cdr (assoc 'path parsed-args))]
           [expr (call-with-input-file file-path read)]
           [modified (slurp-forward expr path)])
      (call-with-output-file file-path #:exists 'replace
        (lambda (out) (pretty-print modified out)))
      (output-result #t (format "~a" (pretty-format modified))))))

(define (cmd-barf-forward file-path args)
  "Barf forward operation"
  (with-handlers ([exn:fail? (lambda (e) (output-result #f #f (exn-message e)))])
    (let* ([parsed-args (parse-json-args args)]
           [path (cdr (assoc 'path parsed-args))]
           [expr (call-with-input-file file-path read)]
           [modified (barf-forward expr path)])
      (call-with-output-file file-path #:exists 'replace
        (lambda (out) (pretty-print modified out)))
      (output-result #t (format "~a" (pretty-format modified))))))

(define (cmd-splice file-path args)
  "Splice operation"
  (with-handlers ([exn:fail? (lambda (e) (output-result #f #f (exn-message e)))])
    (let* ([parsed-args (parse-json-args args)]
           [path (cdr (assoc 'path parsed-args))]
           [expr (call-with-input-file file-path read)]
           [modified (splice expr path)])
      (call-with-output-file file-path #:exists 'replace
        (lambda (out) (pretty-print modified out)))
      (output-result #t (format "~a" (pretty-format modified))))))

;; Main command dispatcher
(define (main args)
  (when (< (length args) 2)
    (output-result #f #f "Usage: racket sexp-cli.rkt <operation> <file> [args...]")
    (exit 1))
  
  (let ([operation (car args)]
        [file-path (cadr args)]
        [rest-args (cddr args)])
    (case (string->symbol operation)
      [(read) (cmd-read file-path)]
      [(format) (cmd-format file-path)]
      [(modify) (cmd-modify file-path rest-args)]
      [(insert) (cmd-insert file-path rest-args)]
      [(delete) (cmd-delete file-path rest-args)]
      [(wrap) (cmd-wrap file-path rest-args)]
      [(slurp-forward) (cmd-slurp-forward file-path rest-args)]
      [(barf-forward) (cmd-barf-forward file-path rest-args)]
      [(splice) (cmd-splice file-path rest-args)]
      [else (output-result #f #f (format "Unknown operation: ~a" operation))])))

;; Run if called directly
(module+ main
  (main (vector->list (current-command-line-arguments))))