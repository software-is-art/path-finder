#lang racket/base

(require racket/contract
         racket/match
         racket/hash
         racket/string
         "../evaluator/values.rkt"
         "../types/types.rkt")

(provide computational-cache?
         make-computational-cache
         cache-computation
         lookup-computation
         cache-key
         cache-entry?
         cache-entry-value
         cache-entry-determinism
         cache-entry-timestamp
         cache-entry-ttl
         invalidate-cache-entry
         is-cache-entry-valid?)

;; ============================================================================
;; COMPUTATIONAL VALUE CACHE
;; ============================================================================
;; This module implements content-addressable caching of computational values
;; to enable Tier promotion between compilation passes.

;; Cache entry structure - contains computational evidence with metadata
(struct cache-entry (key 
                     value           ; The cached constructor value with proofs
                     determinism     ; 'deterministic or 'non-deterministic  
                     timestamp       ; When this was cached
                     ttl             ; Time-to-live in seconds (or #f for permanent)
                     dependencies)   ; List of dependency keys for invalidation
        #:transparent)

;; Computational cache - content-addressable storage for constructor values
(struct computational-cache (storage    ; Hash table: cache-key → cache-entry
                            hit-count   ; Performance metrics
                            miss-count) ; Performance metrics
        #:transparent)

;; Create a new computational cache
(define/contract (make-computational-cache)
  (-> computational-cache?)
  (computational-cache (make-hash) 0 0))

;; ============================================================================
;; CACHE KEY GENERATION
;; ============================================================================
;; Content-addressable keys based on computation structure

;; Generate a cache key for a computation
(define/contract (cache-key operation args effect-context determinism)
  (-> string? (listof value/c) symbol? symbol? string?)
  (let* ([op-str (symbol->string operation)]
         [args-str (string-join (map value->cache-string args) ":")]
         [context-str (symbol->string effect-context)]
         [det-str (symbol->string determinism)])
    (string-append op-str ":" args-str ":" context-str ":" det-str)))

;; Convert a value to a cache-friendly string representation
(define/contract (value->cache-string val)
  (-> value/c string?)
  (match val
    [(constructor-value name args type)
     (string-append name "(" (string-join (map value->cache-string args) ",") ")")]
    [(string-value content)
     (string-append "\"" content "\"")]
    [(builtin-value name _ _)
     (string-append "builtin:" name)]
    [(closure-value params body env)
     ;; For closures, we hash the parameters and body structure
     (string-append "closure:" (string-join params ",") ":" (format "~a" body))]
    [_ (format "~a" val)]))

;; ============================================================================
;; CACHE OPERATIONS
;; ============================================================================

;; Cache a computational result
(define/contract (cache-computation cache operation args effect-context determinism result [ttl #f] [deps '()])
  (->* (computational-cache? string? (listof value/c) symbol? symbol? value/c) 
       ((or/c exact-nonnegative-integer? #f) (listof string?))
       computational-cache?)
  (let* ([key (cache-key operation args effect-context determinism)]
         [timestamp (current-seconds)]
         [entry (cache-entry key result determinism timestamp ttl deps)]
         [new-storage (hash-set (computational-cache-storage cache) key entry)])
    (computational-cache new-storage 
                        (computational-cache-hit-count cache)
                        (computational-cache-miss-count cache))))

;; Look up a cached computation
(define/contract (lookup-computation cache operation args effect-context determinism)
  (-> computational-cache? string? (listof value/c) symbol? symbol? 
      (values (or/c cache-entry? #f) computational-cache?))
  (let* ([key (cache-key operation args effect-context determinism)]
         [entry (hash-ref (computational-cache-storage cache) key #f)])
    (cond
      [(and entry (is-cache-entry-valid? entry))
       ;; Cache hit - increment counter
       (values entry 
               (computational-cache (computational-cache-storage cache)
                                   (+ (computational-cache-hit-count cache) 1)
                                   (computational-cache-miss-count cache)))]
      [entry
       ;; Cache entry exists but is invalid - remove it and count as miss
       (let ([new-storage (hash-remove (computational-cache-storage cache) key)])
         (values #f
                 (computational-cache new-storage
                                     (computational-cache-hit-count cache)
                                     (+ (computational-cache-miss-count cache) 1))))]
      [else
       ;; Cache miss - increment counter
       (values #f
               (computational-cache (computational-cache-storage cache)
                                   (computational-cache-hit-count cache)
                                   (+ (computational-cache-miss-count cache) 1)))])))

;; Check if a cache entry is still valid
(define/contract (is-cache-entry-valid? entry)
  (-> cache-entry? boolean?)
  (match entry
    [(cache-entry key value determinism timestamp ttl deps)
     (cond
       ;; Non-deterministic entries are never valid for reuse
       [(eq? determinism 'non-deterministic) #f]
       ;; No TTL means permanent (until explicit invalidation)
       [(not ttl) #t]
       ;; Check if TTL has expired
       [else (< (current-seconds) (+ timestamp ttl))])]))

;; Invalidate a specific cache entry
(define/contract (invalidate-cache-entry cache key)
  (-> computational-cache? string? computational-cache?)
  (let ([new-storage (hash-remove (computational-cache-storage cache) key)])
    (computational-cache new-storage
                        (computational-cache-hit-count cache)
                        (computational-cache-miss-count cache))))

;; ============================================================================
;; CACHE STATISTICS AND INTROSPECTION
;; ============================================================================

;; Get cache statistics
(define/contract (cache-statistics cache)
  (-> computational-cache? hash?)
  (hash 'hit-count (computational-cache-hit-count cache)
        'miss-count (computational-cache-miss-count cache)
        'total-entries (hash-count (computational-cache-storage cache))
        'hit-ratio (if (> (+ (computational-cache-hit-count cache)
                             (computational-cache-miss-count cache)) 0)
                       (/ (computational-cache-hit-count cache)
                          (+ (computational-cache-hit-count cache)
                             (computational-cache-miss-count cache)))
                       0)))

;; List all cache keys (for debugging/introspection)
(define/contract (cache-keys cache)
  (-> computational-cache? (listof string?))
  (hash-keys (computational-cache-storage cache)))

;; ============================================================================
;; INTEGRATION HELPERS
;; ============================================================================

;; Check if an operation is cacheable based on determinism
(define/contract (operation-cacheable? operation determinism)
  (-> string? symbol? boolean?)
  (and (eq? determinism 'deterministic)
       ;; Add operation-specific cacheability rules here
       (not (member operation '("current-timestamp" "random-number" "user-input")))))

;; Extract determinism annotation from effect context
(define/contract (extract-determinism effect-context)
  (-> any/c symbol?)
  ;; Default to non-deterministic for safety
  ;; This can be enhanced with actual determinism analysis
  (match effect-context
    ['deterministic 'deterministic]
    ['non-deterministic 'non-deterministic]
    [_ 'non-deterministic]))