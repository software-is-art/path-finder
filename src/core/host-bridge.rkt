#lang racket/base

(require racket/contract
         racket/match
         racket/file
         racket/hash
         "../types/types.rkt"
         "../evaluator/values.rkt"
         "../effects/pure-hott-effects.rkt"
         (prefix-in literals: "hott-literals.rkt")
         "hott-evaluator.rkt"
         (prefix-in hott-cache: "hott-cache.rkt")
         "hott-cache-persistence.rkt")

(provide (all-defined-out))

;; ============================================================================
;; HOST BRIDGE - THE ONLY RACKET DEPENDENCY
;; ============================================================================
;; This is the ONLY module that knows about Racket types.
;; Everything else is pure HoTT. This becomes the compiler backend interface.

;; ============================================================================
;; DEPRECATED: Old bridge handler approach - now handled by primitive effects
;; ============================================================================
;; The primitive-effects.rkt system provides the I/O operations.
;; This host-bridge now only provides conversion utilities.

;; ============================================================================
;; CONVERSION FUNCTIONS (Bridge utilities)
;; ============================================================================

;; HoTT <-> Racket conversion functions (ONLY place these should exist)
(define/contract (racket-number->nat-value n)
  (-> exact-nonnegative-integer? constructor-value?)
  (if (= n 0)
      zero-value
      (succ-value (racket-number->nat-value (- n 1)))))

(define/contract (nat-value->racket-number val)
  (-> constructor-value? exact-nonnegative-integer?)
  (match val
    [(constructor-value "zero" '() _) 0]
    [(constructor-value "next" (list pred) _) 
     (+ 1 (nat-value->racket-number pred))]
    [_ (error "Not a natural number value: " val)]))

(define/contract (racket-boolean->bool-value b)
  (-> boolean? constructor-value?)
  (if b true-value false-value))

(define/contract (bool-value->racket-boolean val)
  (-> constructor-value? boolean?)
  (match val
    [(constructor-value "true" '() _) #t]
    [(constructor-value "false" '() _) #f]
    [_ (error "Not a boolean value: " val)]))

;; Convert HoTT string to Racket string
(define/contract (hott-string->racket-string hott-str)
  (-> constructor-value? string?)
  (match hott-str
    [(constructor-value "empty-string" '() _) ""]
    [(constructor-value "string-cons" (list char-val rest-val) _)
     (string-append (hott-char->racket-char char-val)
                    (hott-string->racket-string rest-val))]))

;; Convert HoTT char to Racket char
(define/contract (hott-char->racket-char char-val)
  (-> constructor-value? string?)
  (match char-val
    [(constructor-value "char" (list nat-val) _)
     (let ([codepoint (hott-nat->racket-number nat-val)])
       (string (integer->char codepoint)))]))

;; Convert Racket string to HoTT string
(define/contract (racket-string->hott-string s)
  (-> string? constructor-value?)
  (if (= (string-length s) 0)
      (constructor-value "empty-string" '() String)
      (let ([chars (string->list s)])
        (foldr (lambda (ch acc)
                 (constructor-value "string-cons"
                                   (list (constructor-value "char" 
                                                          (list (racket-number->hott-nat 
                                                                 (char->integer ch)))
                                                          Char)
                                         acc)
                                   String))
               (constructor-value "empty-string" '() String)
               chars))))

;; Convert HoTT nat to Racket number
(define/contract (hott-nat->racket-number n)
  (-> constructor-value? exact-nonnegative-integer?)
  (match n
    [(constructor-value "zero" '() _) 0]
    [(constructor-value "next" (list pred) _)
     (+ 1 (hott-nat->racket-number pred))]))

;; Convert Racket number to HoTT nat
(define/contract (racket-number->hott-nat n)
  (-> exact-nonnegative-integer? constructor-value?)
  (if (= n 0)
      zero-value
      (succ-value (racket-number->hott-nat (- n 1)))))

;; ============================================================================
;; DEPRECATED: Backend abstraction - replaced by primitive effects system
;; ============================================================================
;; Multi-backend support now handled by:
;; 1. Pure HoTT effects (platform-independent descriptions)
;; 2. Primitive effects registry (platform-specific implementations)
;; 3. Host bridge conversion utilities (this file)

;; ============================================================================
;; DEPRECATED: Compile target interface - replaced by pure HoTT compilation
;; ============================================================================
;; Multi-target compilation now handled through:
;; 1. Pure HoTT IL (platform-independent)
;; 2. Target-specific code generators (separate modules)
;; 3. Host bridge for runtime support

;; ============================================================================
;; DEPRECATED: Runtime bridge - replaced by effect executor system
;; ============================================================================
;; Program execution now handled by:
;; 1. Effect Executor (src/effects/effect-executor.rkt)
;; 2. Primitive Effects (src/core/primitive-effects.rkt)
;; 3. Host Bridge conversions (this file)

;; ============================================================================
;; COMPUTATIONAL CACHE PERSISTENCE (Host-specific)
;; ============================================================================

;; Global cache instance (host-managed) - using HoTT cache
(define global-computation-cache (hott-cache:make-empty-cache))

;; Cache file path (host-specific location)
(define cache-file-path ".pathfinder-cache.rkt")

;; Save cache to host filesystem - using HoTT cache persistence
(define/contract (save-cache-to-host cache [file-path cache-file-path])
  (->* (constructor-value?) (string?) void?)
  (save-hott-cache-to-host cache))

;; Load cache from host filesystem - using HoTT cache persistence
(define/contract (load-cache-from-host [file-path cache-file-path])
  (->* () (string?) constructor-value?)
  (load-hott-cache-from-host))

;; Serialize cache to host-compatible format - delegated to HoTT cache
(define/contract (serialize-cache cache)
  (-> constructor-value? hash?)
  (serialize-hott-cache cache))

;; Deserialize cache from host format - delegated to HoTT cache
(define/contract (deserialize-cache cache-data)
  (-> hash? constructor-value?)
  (deserialize-hott-cache cache-data))

;; Get file modification time (host-specific)
(define/contract (host-file-mtime file-path)
  (-> string? (or/c exact-nonnegative-integer? #f))
  (if (file-exists? file-path)
      (file-or-directory-modify-seconds file-path)
      #f))

;; Check if file has changed since cached (host-specific dependency tracking)
(define/contract (file-dependency-valid? file-path cached-mtime)
  (-> string? exact-nonnegative-integer? boolean?)
  (let ([current-mtime (host-file-mtime file-path)])
    (and current-mtime (<= current-mtime cached-mtime))))

;; ============================================================================
;; CACHE-AWARE EFFECT HANDLERS
;; ============================================================================

;; Enhanced file reading with caching using HoTT cache
(define/contract (cached-read-file hott-path determinism [ttl #f])
  (->* (value/c symbol?) ((or/c exact-nonnegative-integer? #f)) value/c)
  (let* ([racket-path (hott-string->racket-string hott-path)]
         [cache-key (hott-cache:compute-content-address (inductive-type "String" '()) hott-path)])
    ;; Try HoTT cache lookup first
    (match (hott-cache:hott-cache-lookup cache-key global-computation-cache)
      [(constructor-value "some" (list cached-value) _)
       ;; Cache hit - validate file dependency if deterministic
       (if (and (eq? determinism 'deterministic)
                (let ([file-mtime (host-file-mtime racket-path)])
                  (and file-mtime #t))) ; Simplified validation for now
           cached-value
           ;; File changed - re-read (simplified)
           (perform-and-cache-file-read racket-path hott-path cache-key determinism ttl))]
      [_
       ;; Cache miss - perform operation and cache result
       (perform-and-cache-file-read racket-path hott-path cache-key determinism ttl)])))

;; Helper: Perform file read and cache the result using HoTT cache
(define/contract (perform-and-cache-file-read racket-path hott-path cache-key determinism ttl)
  (-> string? value/c constructor-value? symbol? (or/c exact-nonnegative-integer? #f) value/c)
  (if (file-exists? racket-path)
      (let* ([content (file->string racket-path)]
             [result (racket-string->hott-string content)])
        ;; Cache the result if deterministic
        (when (eq? determinism 'deterministic)
          (set! global-computation-cache
                (hott-cache:hott-cache-insert cache-key result global-computation-cache)))
        result)
      (error "File not found: " racket-path)))

;; ============================================================================
;; VALUE CONVERSION FOR EFFECTS (Isolated to host bridge)
;; ============================================================================

;; Convert PathFinder value to Racket value for effect operations
(define/contract (value->racket-value val)
  (-> any/c any/c)
  (match val
    [(string-value content) content]
    [(constructor-value "true" '() _) #t]
    [(constructor-value "false" '() _) #f]
    [(constructor-value "zero" '() _) 0]
    [(constructor-value "next" (list pred) _) (+ 1 (value->racket-value pred))]
    [(unit-value) 'unit]
    [_ val]))

;; Convert Racket value to PathFinder value from effect results
(define/contract (racket-value->value rval)
  (-> any/c any/c)
  (match rval
    [(? string?) (string-value rval)]
    [(? boolean?) (racket-boolean->bool-value rval)]
    [(? exact-nonnegative-integer?) (racket-number->nat-value rval)]
    ['unit unit]
    [_ (string-value (format "~a" rval))]))