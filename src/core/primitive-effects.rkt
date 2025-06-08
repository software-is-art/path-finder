#lang racket/base

(require racket/contract
         racket/match
         racket/file
         racket/port
         racket/string
         "../types/types.rkt"
         "../evaluator/values.rkt"
         (prefix-in pure-effects: "../effects/pure-hott-effects.rkt")
         "host-bridge.rkt")

(provide execute-primitive-effect
         register-primitive-effects!
         primitive-effect?
         primitive-effects-registry
         file-io-primitive
         console-io-primitive
         network-primitive
         environment-primitive
         time-primitive
         random-primitive)

;; ============================================================================
;; PRIMITIVE EFFECTS REGISTRY
;; ============================================================================
;; Primitive effects are the minimal set of I/O operations that must be
;; implemented in the host bridge. All other effects compose from these.

(define primitive-effects-registry (make-hash))

;; Register a primitive effect implementation
(define/contract (register-primitive-effect! effect-name operation-name implementation)
  (-> string? string? procedure? void?)
  (let ([key (format "~a.~a" effect-name operation-name)])
    (hash-set! primitive-effects-registry key implementation)))

;; Check if an effect is primitive (implemented in host bridge)
(define/contract (primitive-effect? effect-name operation-name)
  (-> string? string? boolean?)
  (let ([key (format "~a.~a" effect-name operation-name)])
    (hash-has-key? primitive-effects-registry key)))

;; Execute a primitive effect
(define/contract (execute-primitive-effect effect-name operation-name args)
  (-> string? string? (listof constructor-value?) constructor-value?)
  (let ([key (format "~a.~a" effect-name operation-name)])
    (let ([implementation (hash-ref primitive-effects-registry key #f)])
      (if implementation
          (apply implementation args)
          (error "Primitive effect not found: " key)))))

;; ============================================================================
;; FILE I/O PRIMITIVES
;; ============================================================================

;; Read file contents
(define/contract (file-io-read-file path-value)
  (-> constructor-value? constructor-value?)
  (let ([racket-path (hott-string->racket-string path-value)])
    (if (file-exists? racket-path)
        (racket-string->hott-string (file->string racket-path))
        (error "File not found: " racket-path))))

;; Write file contents
(define/contract (file-io-write-file path-value content-value)
  (-> constructor-value? constructor-value? constructor-value?)
  (let ([racket-path (hott-string->racket-string path-value)]
        [racket-content (hott-string->racket-string content-value)])
    (display-to-file racket-content racket-path #:exists 'replace)
    unit))

;; Check if file exists
(define/contract (file-io-file-exists path-value)
  (-> constructor-value? constructor-value?)
  (let ([racket-path (hott-string->racket-string path-value)])
    (if (file-exists? racket-path) true-value false-value)))

;; Get file modification time
(define/contract (file-io-file-mtime path-value)
  (-> constructor-value? constructor-value?)
  (let ([racket-path (hott-string->racket-string path-value)])
    (if (file-exists? racket-path)
        (racket-number->hott-nat (file-or-directory-modify-seconds racket-path))
        zero-value)))

;; Register File I/O primitives
(define/contract (file-io-primitive)
  (-> void?)
  (register-primitive-effect! "FileIO" "read-file" file-io-read-file)
  (register-primitive-effect! "FileIO" "write-file" file-io-write-file)
  (register-primitive-effect! "FileIO" "file-exists" file-io-file-exists)
  (register-primitive-effect! "FileIO" "file-mtime" file-io-file-mtime))

;; ============================================================================
;; CONSOLE I/O PRIMITIVES
;; ============================================================================

;; Print line to console
(define/contract (console-io-print-line message-value)
  (-> constructor-value? constructor-value?)
  (let ([racket-message (hott-string->racket-string message-value)])
    (displayln racket-message)
    unit))

;; Read line from console
(define/contract (console-io-read-line prompt-value)
  (-> constructor-value? constructor-value?)
  (let ([racket-prompt (hott-string->racket-string prompt-value)])
    (display racket-prompt)
    (flush-output)
    (let ([input (read-line)])
      (if (eof-object? input)
          (string-value "")
          (racket-string->hott-string input)))))

;; Print without newline
(define/contract (console-io-print message-value)
  (-> constructor-value? constructor-value?)
  (let ([racket-message (hott-string->racket-string message-value)])
    (display racket-message)
    (flush-output)
    unit))

;; Register Console I/O primitives
(define/contract (console-io-primitive)
  (-> void?)
  (register-primitive-effect! "Console" "print-line" console-io-print-line)
  (register-primitive-effect! "Console" "read-line" console-io-read-line)
  (register-primitive-effect! "Console" "print" console-io-print))

;; ============================================================================
;; NETWORK PRIMITIVES
;; ============================================================================

;; HTTP GET request (placeholder - would use net/http in real implementation)
(define/contract (network-http-get url-value ttl-value)
  (-> constructor-value? constructor-value? constructor-value?)
  (let ([racket-url (hott-string->racket-string url-value)]
        [ttl-seconds (hott-nat->racket-number ttl-value)])
    ;; Placeholder implementation
    (racket-string->hott-string 
     (format "Mock HTTP response for ~a (TTL: ~a)" racket-url ttl-seconds))))

;; HTTP POST request (placeholder)
(define/contract (network-http-post url-value data-value)
  (-> constructor-value? constructor-value? constructor-value?)
  (let ([racket-url (hott-string->racket-string url-value)]
        [racket-data (hott-string->racket-string data-value)])
    (racket-string->hott-string "Mock HTTP POST response")))

;; Register Network primitives
(define/contract (network-primitive)
  (-> void?)
  (register-primitive-effect! "Network" "http-get" network-http-get)
  (register-primitive-effect! "Network" "http-post" network-http-post))

;; ============================================================================
;; ENVIRONMENT PRIMITIVES
;; ============================================================================

;; Get environment variable
(define/contract (environment-get-env var-name-value)
  (-> constructor-value? constructor-value?)
  (let ([racket-var-name (hott-string->racket-string var-name-value)])
    (let ([env-value (getenv racket-var-name)])
      (if env-value
          (racket-string->hott-string env-value)
          (string-value "")))))

;; Set environment variable (for testing/configuration)
(define/contract (environment-set-env var-name-value value-value)
  (-> constructor-value? constructor-value? constructor-value?)
  (let ([racket-var-name (hott-string->racket-string var-name-value)]
        [racket-value (hott-string->racket-string value-value)])
    (putenv racket-var-name racket-value)
    unit))

;; Register Environment primitives
(define/contract (environment-primitive)
  (-> void?)
  (register-primitive-effect! "Environment" "get-env" environment-get-env)
  (register-primitive-effect! "Environment" "set-env" environment-set-env))

;; ============================================================================
;; TIME PRIMITIVES
;; ============================================================================

;; Get current timestamp (seconds since epoch)
(define/contract (time-current-timestamp)
  (-> constructor-value?)
  (racket-number->hott-nat (current-seconds)))

;; Get current milliseconds
(define/contract (time-current-milliseconds)
  (-> constructor-value?)
  (racket-number->hott-nat (inexact->exact (floor (current-inexact-milliseconds)))))

;; Sleep for given seconds
(define/contract (time-sleep seconds-value)
  (-> constructor-value? constructor-value?)
  (let ([racket-seconds (hott-nat->racket-number seconds-value)])
    (sleep racket-seconds)
    unit))

;; Register Time primitives
(define/contract (time-primitive)
  (-> void?)
  (register-primitive-effect! "Time" "current-timestamp" (lambda () (time-current-timestamp)))
  (register-primitive-effect! "Time" "current-milliseconds" (lambda () (time-current-milliseconds)))
  (register-primitive-effect! "Time" "sleep" time-sleep))

;; ============================================================================
;; RANDOM PRIMITIVES
;; ============================================================================

;; Generate random number between 0 and max-1
(define/contract (random-random-number max-value)
  (-> constructor-value? constructor-value?)
  (let ([racket-max (hott-nat->racket-number max-value)])
    (if (> racket-max 0)
        (racket-number->hott-nat (random racket-max))
        zero-value)))

;; Generate random boolean
(define/contract (random-random-boolean)
  (-> constructor-value?)
  (if (= (random 2) 0) false-value true-value))

;; Generate UUID (simplified)
(define/contract (random-uuid)
  (-> constructor-value?)
  (let ([uuid-string (format "~a-~a-~a-~a" 
                             (random 10000) (random 10000) 
                             (random 10000) (random 10000))])
    (racket-string->hott-string uuid-string)))

;; Register Random primitives
(define/contract (random-primitive)
  (-> void?)
  (register-primitive-effect! "Random" "random-number" random-random-number)
  (register-primitive-effect! "Random" "random-boolean" (lambda () (random-random-boolean)))
  (register-primitive-effect! "Random" "uuid" (lambda () (random-uuid))))

;; ============================================================================
;; PROCESS PRIMITIVES
;; ============================================================================

;; Execute external process (placeholder)
(define/contract (process-run-command command-value args-value)
  (-> constructor-value? constructor-value? constructor-value?)
  (let ([racket-command (hott-string->racket-string command-value)])
    ;; Placeholder - would use system/process in real implementation
    (racket-string->hott-string 
     (format "Mock process output for command: ~a" racket-command))))

;; Get current working directory
(define/contract (process-current-directory)
  (-> constructor-value?)
  (racket-string->hott-string (path->string (current-directory))))

;; Change current working directory
(define/contract (process-change-directory path-value)
  (-> constructor-value? constructor-value?)
  (let ([racket-path (hott-string->racket-string path-value)])
    (current-directory racket-path)
    unit))

;; Register Process primitives
(define/contract (process-primitive)
  (-> void?)
  (register-primitive-effect! "Process" "run-command" process-run-command)
  (register-primitive-effect! "Process" "current-directory" (lambda () (process-current-directory)))
  (register-primitive-effect! "Process" "change-directory" process-change-directory))

;; ============================================================================
;; REGISTRATION FUNCTION
;; ============================================================================

;; Register all primitive effects
(define/contract (register-primitive-effects!)
  (-> void?)
  (file-io-primitive)
  (console-io-primitive)
  (network-primitive)
  (environment-primitive)
  (time-primitive)
  (random-primitive)
  (process-primitive)
  (printf "Primitive effects registered: ~a operations~n" 
          (hash-count primitive-effects-registry)))