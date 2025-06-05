#lang racket/base

(require racket/port
         racket/string
         racket/file)

;; Show the structure of a Racket file with indentation levels
(define (show-structure filename start-line end-line)
  (define lines (file->lines filename))
  (define stack '())
  (define indent-level 0)
  
  (for ([line (in-list lines)]
        [line-num (in-naturals 1)])
    (when (and (>= line-num start-line) (<= line-num end-line))
      (define trimmed (string-trim line))
      (unless (or (string=? trimmed "") (string-prefix? trimmed ";"))
        ;; Count opening and closing brackets
        (define opens (+ (for/sum ([c (in-string line)] #:when (char=? c #\()) 1)
                        (for/sum ([c (in-string line)] #:when (char=? c #\[)) 1)))
        (define closes (+ (for/sum ([c (in-string line)] #:when (char=? c #\))) 1)
                         (for/sum ([c (in-string line)] #:when (char=? c #\])) 1)))
        
        ;; Adjust indent for closing brackets at start of line
        (when (and (> closes 0) (regexp-match? #rx"^[\\)\\]]" trimmed))
          (set! indent-level (max 0 (- indent-level closes))))
        
        ;; Print with indentation
        (printf "~a:~a ~a~a\n" 
                line-num
                (make-string (max 0 (* 2 indent-level)) #\space)
                (make-string (max 0 (- 4 (string-length (number->string line-num)))) #\space)
                line)
        
        ;; Update indent level
        (set! indent-level (+ indent-level opens))
        (set! indent-level (- indent-level closes))))))

;; Run on command line arguments
(define args (current-command-line-arguments))
(when (>= (vector-length args) 3)
  (show-structure (vector-ref args 0)
                  (string->number (vector-ref args 1))
                  (string->number (vector-ref args 2))))