#lang racket/base

(require racket/file
         racket/port
         racket/match
         racket/list
         json
         (file "/Users/cgalbreath@simplemachines.co.nz/git/path-finder/src/lexer/lexer.rkt")
         (file "/Users/cgalbreath@simplemachines.co.nz/git/path-finder/src/parser/parser.rkt")
         (file "/Users/cgalbreath@simplemachines.co.nz/git/path-finder/src/parser/ast.rkt"))

;; Convert AST to JSON-serializable structure
(define (ast->json ast)
  (match ast
    [(number-atom value)
     (hash 'type "atom" 'atomType "number" 'value value)]
    
    [(boolean-atom value)
     (hash 'type "atom" 'atomType "boolean" 'value value)]
    
    [(string-atom value)
     (hash 'type "atom" 'atomType "string" 'value value)]
    
    [(symbol-atom value)
     (hash 'type "atom" 'atomType "symbol" 'value value)]
    
    [(sexpr elements)
     (hash 'type "list" 'children (map ast->json elements))]
    
    [(match-expr scrutinee cases)
     (hash 'type "list" 
           'children (cons (hash 'type "atom" 'atomType "symbol" 'value "match")
                          (cons (ast->json scrutinee)
                                (map (lambda (case)
                                       (hash 'type "list"
                                             'children (list (pattern->json (match-case-pattern case))
                                                           (ast->json (match-case-body case)))))
                                     cases))))]
    
    [_ (hash 'type "atom" 'atomType "unknown" 'value (format "~a" ast))]))

;; Convert pattern to JSON
(define (pattern->json pattern)
  (match pattern
    [(wildcard-pattern)
     (hash 'type "atom" 'atomType "symbol" 'value "_")]
    
    [(variable-pattern name)
     (hash 'type "atom" 'atomType "symbol" 'value name)]
    
    [(literal-pattern value)
     (cond
       [(number? value) (hash 'type "atom" 'atomType "number" 'value value)]
       [(boolean? value) (hash 'type "atom" 'atomType "boolean" 'value value)]
       [(string? value) (hash 'type "atom" 'atomType "string" 'value value)]
       [else (hash 'type "atom" 'atomType "unknown" 'value (format "~a" value))])]
    
    [(zero-pattern)
     (hash 'type "list" 'children (list (hash 'type "atom" 'atomType "symbol" 'value "zero")))]
    
    [(successor-pattern sub-pattern)
     (hash 'type "list" 'children (list (hash 'type "atom" 'atomType "symbol" 'value "successor")
                                       (pattern->json sub-pattern)))]
    
    [(true-pattern)
     (hash 'type "list" 'children (list (hash 'type "atom" 'atomType "symbol" 'value "true")))]
    
    [(false-pattern)
     (hash 'type "list" 'children (list (hash 'type "atom" 'atomType "symbol" 'value "false")))]
    
    [(constructor-pattern constructor-name sub-patterns)
     (hash 'type "list" 
           'children (cons (hash 'type "atom" 'atomType "symbol" 'value constructor-name)
                          (map pattern->json sub-patterns)))]
    
    [_ (hash 'type "atom" 'atomType "unknown" 'value (format "~a" pattern))]))

;; Main parsing function
(define (parse-file filepath)
  (with-handlers ([exn:fail? (lambda (e)
                              (hash 'success #f 
                                    'error (exn-message e)))])
    (let* ([content (file->string filepath)]
           [tokens (tokenize content)]
           [ast (parse tokens)]
           [json-ast (ast->json ast)])
      (hash 'success #t 'result json-ast))))

;; Main entry point
(define (main)
  (let ([args (current-command-line-arguments)])
    (if (> (vector-length args) 0)
        (let* ([filepath (vector-ref args 0)]
               [result (parse-file filepath)])
          (write-json result))
        (write-json (hash 'success #f 
                         'error "No file path provided")))))

;; Run main function
(main)