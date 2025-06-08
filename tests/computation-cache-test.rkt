#lang racket/base

(require rackunit
         "../src/core/computation-cache.rkt"
         "../src/core/tier-promotion.rkt"
         "../src/evaluator/values.rkt"
         "../src/types/types.rkt")

;; Test suite for Computational Value Caching System

(test-case "Cache key generation"
  
  ;; Test basic cache key generation
  (let ([key1 (cache-key "read-file" (list (string-value "config.json")) 'file-io 'deterministic)]
        [key2 (cache-key "read-file" (list (string-value "config.json")) 'file-io 'deterministic)]
        [key3 (cache-key "read-file" (list (string-value "other.json")) 'file-io 'deterministic)])
    (check-equal? key1 key2 "Same operations should produce same cache keys")
    (check-not-equal? key1 key3 "Different operations should produce different cache keys"))
  
  ;; Test determinism affects cache key
  (let ([det-key (cache-key "read-file" (list (string-value "test.txt")) 'file-io 'deterministic)]
        [non-det-key (cache-key "read-file" (list (string-value "test.txt")) 'file-io 'non-deterministic)])
    (check-not-equal? det-key non-det-key "Determinism should affect cache key")))

(test-case "Basic cache operations"
  
  (let ([cache (make-computational-cache)]
        [test-value (string-value "cached content")]
        [args (list (string-value "test.txt"))])
    
    ;; Test cache miss
    (let-values ([(result new-cache) (lookup-computation cache "read-file" args 'file-io 'deterministic)])
      (check-false result "Should get cache miss for empty cache")
      (set! cache new-cache))
    
    ;; Test caching a value
    (set! cache (cache-computation cache "read-file" args 'file-io 'deterministic test-value))
    
    ;; Test cache hit
    (let-values ([(result new-cache) (lookup-computation cache "read-file" args 'file-io 'deterministic)])
      (check-true (cache-entry? result) "Should get cache hit")
      (check-equal? (cache-entry-value result) test-value "Should return cached value")
      (set! cache new-cache))
    
    ;; Test hit count incremented
    (let ([stats (cache-statistics cache)])
      (check-equal? (hash-ref stats 'hit-count) 1 "Hit count should be incremented"))))

(test-case "Cache entry validation"
  
  (let ([cache (make-computational-cache)]
        [test-value (string-value "test content")]
        [args (list (string-value "test.txt"))])
    
    ;; Test deterministic caching
    (set! cache (cache-computation cache "read-file" args 'file-io 'deterministic test-value))
    (let-values ([(result _) (lookup-computation cache "read-file" args 'file-io 'deterministic)])
      (check-true (cache-entry? result) "Deterministic entries should be valid"))
    
    ;; Test non-deterministic entries are not reused
    (set! cache (cache-computation cache "random" args 'compute 'non-deterministic test-value))
    (let-values ([(result _) (lookup-computation cache "random" args 'compute 'non-deterministic)])
      (check-false result "Non-deterministic entries should not be reused"))))

(test-case "Cache entry TTL"
  
  (let ([cache (make-computational-cache)]
        [test-value (string-value "expiring content")]
        [args (list (string-value "temp.txt"))])
    
    ;; Cache with 1-second TTL
    (set! cache (cache-computation cache "read-file" args 'file-io 'deterministic test-value 1))
    
    ;; Should hit immediately
    (let-values ([(result _) (lookup-computation cache "read-file" args 'file-io 'deterministic)])
      (check-true (cache-entry? result) "Should hit before TTL expires"))
    
    ;; Sleep and check expiration (Note: This is host-dependent, normally we'd mock time)
    (sleep 2)
    (let-values ([(result _) (lookup-computation cache "read-file" args 'file-io 'deterministic)])
      (check-false result "Should miss after TTL expires"))))

(test-case "Tier promotion analysis"
  
  (let ([cache (make-computational-cache)]
        [test-value (constructor-value "next" (list (constructor-value "zero" '() Nat)) Nat)]
        [args (list (constructor-value "zero" '() Nat) (constructor-value "next" (list (constructor-value "zero" '() Nat)) Nat))])
    
    ;; Cache a mathematical operation result
    (set! cache (cache-computation cache "+" args 'computation 'deterministic test-value))
    
    ;; Test tier promotion analysis
    (let ([promotion (analyze-tier-promotion "+" args 'computation 3 cache)])
      (check-true (tier-promotion? promotion) "Should identify tier promotion opportunity")
      (check-equal? (tier-promotion-original-tier promotion) 3 "Should record original tier")
      (check-equal? (tier-promotion-target-tier promotion) 2 "Should target one tier higher")
      (check-equal? (tier-promotion-cached-value promotion) test-value "Should include cached value"))
    
    ;; Test tier promotability check
    (check-true (tier-promotable? "+" args 'computation 3 cache) "Should be promotable with cached result")
    (check-false (tier-promotable? "*" args 'computation 3 cache) "Should not be promotable without cached result")))

(test-case "Cache invalidation"
  
  (let ([cache (make-computational-cache)]
        [test-value (string-value "invalidatable content")]
        [args (list (string-value "config.json"))])
    
    ;; Cache a value
    (set! cache (cache-computation cache "read-file" args 'file-io 'deterministic test-value))
    
    ;; Verify it's cached
    (let-values ([(result _) (lookup-computation cache "read-file" args 'file-io 'deterministic)])
      (check-true (cache-entry? result) "Should be cached initially"))
    
    ;; Invalidate the entry
    (let ([key (cache-key "read-file" args 'file-io 'deterministic)])
      (set! cache (invalidate-cache-entry cache key)))
    
    ;; Verify it's no longer cached
    (let-values ([(result _) (lookup-computation cache "read-file" args 'file-io 'deterministic)])
      (check-false result "Should be invalidated"))))

(test-case "Cache statistics"
  
  (let ([cache (make-computational-cache)]
        [test-value (string-value "stats test")]
        [args (list (string-value "stats.txt"))])
    
    ;; Initial stats
    (let ([stats (cache-statistics cache)])
      (check-equal? (hash-ref stats 'hit-count) 0 "Initial hit count should be 0")
      (check-equal? (hash-ref stats 'miss-count) 0 "Initial miss count should be 0")
      (check-equal? (hash-ref stats 'total-entries) 0 "Initial entries should be 0"))
    
    ;; Cause a miss
    (let-values ([(result new-cache) (lookup-computation cache "read-file" args 'file-io 'deterministic)])
      (set! cache new-cache))
    
    ;; Cache a value
    (set! cache (cache-computation cache "read-file" args 'file-io 'deterministic test-value))
    
    ;; Cause a hit
    (let-values ([(result new-cache) (lookup-computation cache "read-file" args 'file-io 'deterministic)])
      (set! cache new-cache))
    
    ;; Check final stats
    (let ([stats (cache-statistics cache)])
      (check-equal? (hash-ref stats 'hit-count) 1 "Should have 1 hit")
      (check-equal? (hash-ref stats 'miss-count) 1 "Should have 1 miss")
      (check-equal? (hash-ref stats 'total-entries) 1 "Should have 1 entry")
      (check-equal? (hash-ref stats 'hit-ratio) 0.5 "Hit ratio should be 50%"))))

(test-case "Tier promotion plan creation"
  
  (let ([cache (make-computational-cache)]
        [test-operations (list 
                          (list "+" (list (constructor-value "zero" '() Nat) (constructor-value "zero" '() Nat)) 'computation 3)
                          (list "*" (list (constructor-value "next" (list (constructor-value "zero" '() Nat)) Nat) 
                                          (constructor-value "zero" '() Nat)) 'computation 2))])
    
    ;; Cache results for one operation
    (set! cache (cache-computation cache "+" 
                                  (list (constructor-value "zero" '() Nat) (constructor-value "zero" '() Nat))
                                  'computation 'deterministic 
                                  (constructor-value "zero" '() Nat)))
    
    ;; Create promotion plan
    (let ([plan (create-tier-promotion-plan test-operations cache)])
      (check-true (promotion-plan? plan) "Should create promotion plan")
      (check-equal? (length (promotion-plan-promotions plan)) 1 "Should have 1 promotion opportunity")
      (check-true (> (promotion-plan-estimated-savings plan) 0) "Should estimate positive savings"))))

;; Run the tests
(printf "Running Computational Cache tests...~n")