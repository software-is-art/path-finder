;;; Effect handling for PathFinder
;;; Executes I/O effects in the host environment

(use-modules (ice-9 match)
             (ice-9 textual-ports))

(define (execute-effect effect)
  "Execute a PathFinder effect"
  (match effect
    ;; Double-wrapped effect (from perform)
    (('effect ('effect inner))
     (execute-effect `(effect ,inner)))
    
    ;; Print effect
    (('effect ('constructor "print" msg))
     (let ((str (extract-string msg)))
       (display str)
       (newline))
     '(constructor "unit"))
    
    ;; File operations
    (('effect ('constructor "file-read" path))
     (let ((path-str (extract-string path)))
       (catch #t
         (lambda ()
           (let ((content (call-with-input-file path-str get-string-all)))
             (make-pathfinder-string content)))
         (lambda (key . args)
           (error "Failed to read file:" path-str)))))
    
    (('effect ('constructor "file-write" path content))
     (let ((path-str (extract-string path))
           (content-str (extract-string content)))
       (catch #t
         (lambda ()
           (call-with-output-file path-str
             (lambda (port)
               (display content-str port)))
           '(constructor "unit"))
         (lambda (key . args)
           (error "Failed to write file:" path-str)))))
    
    ;; IO effect format (for compatibility)
    (('effect ('constructor "io-effect" category operation args determinism))
     (execute-io-effect category operation args))
    
    ;; Unknown effects
    (('effect other)
     (format #t "Warning: Unknown effect: ~a~%" other)
     '(constructor "unit"))
    
    ;; Not an effect
    (_
     (error "Not an effect:" effect))))

(define (execute-io-effect category operation args)
  "Execute an IO effect with category and operation"
  (let ((cat-str (extract-string category))
        (op-str (extract-string operation)))
    (match (cons cat-str op-str)
      (("print" . "println")
       (let ((msg-list (pathfinder-list->scheme-list args)))
         (when (not (null? msg-list))
           (display (extract-string (car msg-list)))
           (newline)))
       '(constructor "unit"))
      
      (("file" . "read")
       (let ((args-list (pathfinder-list->scheme-list args)))
         (if (null? args-list)
             (error "file read requires path argument")
             (let ((path (extract-string (car args-list))))
               (make-pathfinder-string 
                (call-with-input-file path get-string-all))))))
      
      (("file" . "write")
       (let ((args-list (pathfinder-list->scheme-list args)))
         (if (< (length args-list) 2)
             (error "file write requires path and content")
             (let ((path (extract-string (car args-list)))
                   (content (extract-string (cadr args-list))))
               (call-with-output-file path
                 (lambda (port)
                   (display content port)))
               '(constructor "unit")))))
      
      (_
       (error "Unknown IO operation:" cat-str op-str)))))

(define (extract-string val)
  "Extract a Scheme string from a PathFinder value"
  (match val
    ;; Already a Scheme string
    ((? string? s) s)
    
    ;; Literal string
    (('literal s) s)
    
    ;; PathFinder string representation
    (('constructor "empty-string")
     "")
    
    (('constructor "string-cons" char rest)
     (string-append 
      (extract-char char)
      (extract-string rest)))
    
    ;; Fallback
    (_ (format #f "~a" val))))

(define (extract-char char-val)
  "Extract a character from PathFinder char representation"
  (match char-val
    (('constructor "char" nat)
     (string (integer->char (extract-nat nat))))
    (('literal c)
     (if (char? c)
         (string c)
         "?"))
    (_ "?")))

(define (extract-nat nat-val)
  "Extract a number from PathFinder nat"
  (match nat-val
    (('constructor "zero") 0)
    (('constructor "succ" pred)
     (+ 1 (extract-nat pred)))
    (('literal n) n)
    (_ 0)))