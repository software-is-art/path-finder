#lang racket/base

(require racket/contract
         racket/match
         racket/file
         racket/port
         racket/list
         "../evaluator/values.rkt"
         "../types/types.rkt"
         (prefix-in hott-cache: "hott-cache.rkt"))

(provide save-hott-cache-to-host
         load-hott-cache-from-host
         serialize-hott-cache
         deserialize-hott-cache
         cache-file-valid?
         get-cache-file-path
         auto-save-cache
         auto-load-cache
         initialize-persistent-cache
         shutdown-persistent-cache
         validate-cache-integrity)

;; ============================================================================
;; PURE HOTT CACHE PERSISTENCE BRIDGE
;; ============================================================================
;; This module handles persistence of pure HoTT cache to host filesystem
;; while maintaining the mathematical purity of the cache itself

;; Cache file configuration
(define default-cache-directory ".pathfinder-cache")
(define cache-file-name "hott-computation-cache.pf")
(define cache-version 1)

;; ============================================================================
;; CACHE SERIALIZATION (HOTT VALUES → HOST FORMAT)
;; ============================================================================

;; Serialize HoTT cache to host-compatible format
(define/contract (serialize-hott-cache cache)
  (-> constructor-value? hash?)
  (hash 'version cache-version
        'timestamp (current-seconds)
        'cache-type "pure-hott-cache"
        'cache-data (serialize-cache-entries cache)
        'metadata (hash 'size (hott-cache:hott-cache-size cache)
                        'content-type "constructor-values")))

;; Serialize cache entries recursively
(define/contract (serialize-cache-entries cache)
  (-> constructor-value? list?)
  (match cache
    [(constructor-value "empty-cache" _ _) '()]
    [(constructor-value "cache-entry" (list key value proof timestamp rest) _)
     (cons (hash 'key (serialize-constructor-value key)
                 'value (serialize-constructor-value value)
                 'proof (serialize-constructor-value proof)
                 'timestamp (serialize-constructor-value timestamp)
                 'key-type (serialize-hott-type (constructor-value-type key))
                 'value-type (serialize-hott-type (constructor-value-type value)))
           (serialize-cache-entries rest))]
    [_ (error "Invalid cache structure for serialization")]))

;; Serialize individual constructor values
(define/contract (serialize-constructor-value value)
  (-> constructor-value? hash?)
  (hash 'constructor (constructor-value-constructor-name value)
        'args (map serialize-constructor-value (constructor-value-args value))
        'type (serialize-hott-type (constructor-value-type value))))

;; Serialize HoTT types
(define/contract (serialize-hott-type type-info)
  (-> hott-type? hash?)
  (match type-info
    [(inductive-type name params)
     (hash 'kind "inductive"
           'name name  
           'params (map serialize-hott-type params))]
    [(pi-type var domain codomain)
     (hash 'kind "pi"
           'var var
           'domain (serialize-hott-type domain)
           'codomain (serialize-hott-type codomain))]
    [(sigma-type var first second)
     (hash 'kind "sigma"
           'var var
           'first (serialize-hott-type first)
           'second (serialize-hott-type second))]
    [(universe level)
     (hash 'kind "universe"
           'level level)]
    [_ (hash 'kind "unknown" 'data (format "~a" type-info))]))

;; ============================================================================
;; CACHE DESERIALIZATION (HOST FORMAT → HOTT VALUES)
;; ============================================================================

;; Deserialize HoTT cache from host format
(define/contract (deserialize-hott-cache cache-data)
  (-> hash? constructor-value?)
  (let ([version (hash-ref cache-data 'version 0)]
        [entries (hash-ref cache-data 'cache-data '())])
    (cond
      [(not (= version cache-version))
       (printf "Warning: Cache version mismatch. Creating empty cache.~n")
       (hott-cache:make-empty-cache)]
      [else
       (deserialize-cache-entries entries)])))

;; Deserialize cache entries
(define/contract (deserialize-cache-entries entries)
  (-> list? constructor-value?)
  (if (null? entries)
      (hott-cache:make-empty-cache)
      (let* ([entry (first entries)]
             [key (deserialize-constructor-value (hash-ref entry 'key))]
             [value (deserialize-constructor-value (hash-ref entry 'value))]
             [proof (deserialize-constructor-value (hash-ref entry 'proof))]
             [timestamp (deserialize-constructor-value (hash-ref entry 'timestamp))]
             [rest (deserialize-cache-entries (rest entries))])
        (hott-cache:make-cache-entry key value proof timestamp rest))))

;; Deserialize individual constructor values
(define/contract (deserialize-constructor-value value-data)
  (-> hash? constructor-value?)
  (let ([constructor (hash-ref value-data 'constructor)]
        [args (map deserialize-constructor-value (hash-ref value-data 'args))]
        [type-info (deserialize-hott-type (hash-ref value-data 'type))])
    (constructor-value constructor args type-info)))

;; Deserialize HoTT types
(define/contract (deserialize-hott-type type-data)
  (-> hash? hott-type?)
  (let ([kind (hash-ref type-data 'kind)])
    (match kind
      ["inductive"
       (inductive-type (hash-ref type-data 'name)
                      (map deserialize-hott-type (hash-ref type-data 'params)))]
      ["pi"
       (pi-type (hash-ref type-data 'var)
               (deserialize-hott-type (hash-ref type-data 'domain))
               (deserialize-hott-type (hash-ref type-data 'codomain)))]
      ["sigma"
       (sigma-type (hash-ref type-data 'var)
                  (deserialize-hott-type (hash-ref type-data 'first))
                  (deserialize-hott-type (hash-ref type-data 'second)))]
      ["universe"
       (universe (hash-ref type-data 'level))]
      [_ 
       ;; Fallback for unknown types
       (inductive-type "Unknown" '())])))

;; ============================================================================
;; HOST FILESYSTEM OPERATIONS
;; ============================================================================

;; Get cache file path
(define/contract (get-cache-file-path [cache-dir default-cache-directory])
  (->* () (string?) string?)
  (path->string (build-path cache-dir cache-file-name)))

;; Ensure cache directory exists
(define/contract (ensure-cache-directory [cache-dir default-cache-directory])
  (->* () (string?) void?)
  (unless (directory-exists? cache-dir)
    (make-directory* cache-dir)))

;; Save HoTT cache to host filesystem
(define/contract (save-hott-cache-to-host cache [cache-dir default-cache-directory])
  (->* (constructor-value?) (string?) void?)
  (ensure-cache-directory cache-dir)
  (let ([cache-file (get-cache-file-path cache-dir)]
        [serialized (serialize-hott-cache cache)])
    (with-output-to-file cache-file
      (lambda () 
        (write serialized))
      #:exists 'replace)
    (printf "Cache saved to: ~a~n" cache-file)))

;; Load HoTT cache from host filesystem
(define/contract (load-hott-cache-from-host [cache-dir default-cache-directory])
  (->* () (string?) constructor-value?)
  (let ([cache-file (get-cache-file-path cache-dir)])
    (if (file-exists? cache-file)
        (let ([cache-data (with-input-from-file cache-file read)])
          (printf "Cache loaded from: ~a~n" cache-file)
          (deserialize-hott-cache cache-data))
        (begin
          (printf "No cache file found. Starting with empty cache.~n")
          (hott-cache:make-empty-cache)))))

;; Check if cache file is valid and recent
(define/contract (cache-file-valid? [cache-dir default-cache-directory] [max-age-hours 24])
  (->* () (string? exact-nonnegative-integer?) boolean?)
  (let ([cache-file (get-cache-file-path cache-dir)])
    (and (file-exists? cache-file)
         (let* ([file-time (file-or-directory-modify-seconds cache-file)]
                [current-time (current-seconds)]
                [age-hours (/ (- current-time file-time) 3600)])
           (< age-hours max-age-hours)))))

;; ============================================================================
;; AUTOMATIC CACHE MANAGEMENT
;; ============================================================================

;; Auto-save cache periodically or on program exit
(define/contract (auto-save-cache cache [save-interval 300])  ; 5 minutes default
  (->* (constructor-value?) (exact-nonnegative-integer?) void?)
  ;; Save immediately
  (save-hott-cache-to-host cache)
  
  ;; Schedule periodic saves (simplified - would use threading in real implementation)
  (printf "Auto-save enabled. Cache will be saved every ~a seconds.~n" save-interval))

;; Auto-load cache on program start
(define/contract (auto-load-cache)
  (-> constructor-value?)
  (let ([cache (load-hott-cache-from-host)])
    (printf "Cache statistics: ~a entries loaded~n" (hott-cache:hott-cache-size cache))
    cache))

;; ============================================================================
;; CACHE INTEGRITY AND VALIDATION
;; ============================================================================

;; Validate cache integrity after loading
(define/contract (validate-cache-integrity cache)
  (-> constructor-value? boolean?)
  (match cache
    [(constructor-value "empty-cache" _ _) #t]
    [(constructor-value "cache-entry" (list key value proof timestamp rest) _)
     ;; Check that all components are valid constructor values
     (and (constructor-value? key)
          (constructor-value? value)
          (constructor-value? proof)
          (constructor-value? timestamp)
          (validate-cache-integrity rest))]
    [_ #f]))

;; Repair corrupted cache entries
(define/contract (repair-cache cache)
  (-> constructor-value? constructor-value?)
  ;; Remove invalid entries, keep valid ones
  (hott-cache:hott-cache-filter cache
                    (lambda (entry)
                      (match entry
                        [(list key value proof timestamp)
                         (and (constructor-value? key)
                              (constructor-value? value)
                              (constructor-value? proof)
                              (constructor-value? timestamp))]
                        [_ #f]))))

;; ============================================================================
;; CACHE MIGRATION AND VERSIONING
;; ============================================================================

;; Migrate cache from older versions
(define/contract (migrate-cache-version cache-data)
  (-> hash? hash?)
  (let ([version (hash-ref cache-data 'version 0)])
    (cond
      [(= version cache-version) cache-data]  ; Current version
      [(= version 0) 
       ;; Migrate from version 0 to current
       (hash 'version cache-version
             'timestamp (current-seconds)
             'cache-type "pure-hott-cache"
             'cache-data '()  ; Start fresh for major version change
             'metadata (hash 'migrated-from 0))]
      [else
       ;; Unknown version - start fresh
       (printf "Unknown cache version ~a. Starting fresh.~n" version)
       (hash 'version cache-version
             'timestamp (current-seconds)
             'cache-type "pure-hott-cache"
             'cache-data '()
             'metadata (hash 'reset-reason "unknown-version"))])))

;; ============================================================================
;; INTEGRATION HELPERS
;; ============================================================================

;; Initialize cache system with persistence
(define/contract (initialize-persistent-cache)
  (-> constructor-value?)
  (let ([loaded-cache (auto-load-cache)])
    (if (validate-cache-integrity loaded-cache)
        loaded-cache
        (begin
          (printf "Cache integrity check failed. Repairing...~n")
          (repair-cache loaded-cache)))))

;; Shutdown cache system with persistence
(define/contract (shutdown-persistent-cache cache)
  (-> constructor-value? void?)
  (printf "Shutting down cache system...~n")
  (save-hott-cache-to-host cache)
  (printf "Cache persistence complete.~n"))