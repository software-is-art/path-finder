#lang racket/base

(require racket/port
         racket/file
         racket/list)

;; Simple bracket checker for Racket files
(define (check-brackets filename)
  (define content (file->string filename))
  (define stack '())
  (define line-num 1)
  (define col-num 1)
  (define positions '()) ; Track positions of opening brackets
  
  (for ([char (in-string content)]
        [pos (in-naturals)])
    (cond
      [(char=? char #\newline)
       (set! line-num (+ line-num 1))
       (set! col-num 1)]
      [(member char '(#\( #\[))
       (set! stack (cons char stack))
       (set! positions (cons (list char line-num col-num) positions))
       (set! col-num (+ col-num 1))]
      [(member char '(#\) #\]))
       (cond
         [(null? stack)
          (printf "ERROR: Unexpected closing ~a at line ~a, column ~a\n" 
                  char line-num col-num)]
         [(and (char=? char #\)) (char=? (car stack) #\())
          (set! stack (cdr stack))
          (set! positions (cdr positions))]
         [(and (char=? char #\]) (char=? (car stack) #\[))
          (set! stack (cdr stack))
          (set! positions (cdr positions))]
         [else
          (define opener (car positions))
          (printf "ERROR: Mismatched brackets at line ~a, column ~a\n" line-num col-num)
          (printf "  Expected ~a to match ~a from line ~a, column ~a\n"
                  (if (char=? (car stack) #\() #\) #\])
                  (first opener)
                  (second opener)
                  (third opener))])
       (set! col-num (+ col-num 1))]
      [else
       (set! col-num (+ col-num 1))]))
  
  (unless (null? stack)
    (printf "\nERROR: Unclosed brackets:\n")
    (for ([opener (reverse positions)])
      (printf "  ~a at line ~a, column ~a\n" 
              (first opener) (second opener) (third opener))))
  
  (when (null? stack)
    (printf "All brackets are properly matched!\n")))

;; Run on command line argument
(define args (current-command-line-arguments))
(when (> (vector-length args) 0)
  (check-brackets (vector-ref args 0)))