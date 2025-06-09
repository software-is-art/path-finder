#lang racket/base

(require racket/string
         racket/file
         racket/match)

;; Script to automatically replace simple Racket contracts with HoTT dependent types
;; Usage: racket scripts/replace-contracts.rkt <file.rkt>

(define (replace-simple-contract-definition line)
  "Replace (define/contract (func arg) (-> type1 type2) body) with HoTT style"
  (let ([pattern #px"^\\(define/contract \\(([^)]+)\\)\\s*\\(-> ([^)]+) ([^)]+)\\)$"])
    (match (regexp-match pattern line)
      [(list _ func-signature input-type output-type)
       (let* ([func-name (car (string-split func-signature))]
              [args (cdr (string-split func-signature))]
              [arg-checks (map (lambda (arg) 
                                (format "  (unless (~a ~a) (error \"~a: ~a must be a ~a\" ~a))"
                                        (type->predicate input-type) arg func-name arg input-type arg))
                              args)])
         (format "(define (~a)~a" func-signature (string-join arg-checks "\n")))]
      [else line])))

(define (type->predicate type-str)
  "Convert type string to predicate function name"
  (match type-str
    ["any/c" "any/c"]
    ["hott-type/c" "hott-type?"]
    ["constructor-value?" "constructor-value?"]
    ["string?" "string?"]
    ["boolean?" "boolean?"]
    ["value/c" "hott-value?"]
    [_ type-str]))

(define (process-file filepath)
  "Process a Racket file and replace simple contracts"
  (let* ([content (file->string filepath)]
         [lines (string-split content "\n")]
         [processed-lines (map replace-simple-contract-definition lines)]
         [new-content (string-join processed-lines "\n")])
    (display-to-file new-content filepath #:exists 'replace)
    (printf "Processed ~a\n" filepath)))

;; Main entry point  
(when (not (empty? (current-command-line-arguments)))
  (let ([filepath (vector-ref (current-command-line-arguments) 0)])
    (process-file filepath)))