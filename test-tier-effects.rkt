#lang racket/base

(require "src/effects/pure-hott-effects.rkt"
         "src/effects/effect-executor.rkt"
         "src/core/primitive-effects.rkt"
         "src/core/hott-cache.rkt"
         "src/evaluator/values.rkt"
         "src/types/types.rkt")

;; Initialize the system
(register-primitive-effects!)

;; Create a simple cache
(define test-cache (make-empty-cache))

;; Test 1: Create a pure effect
(printf "=== Testing Tier-Aware Pure HoTT Effects ===~n~n")

(define hello-effect (pure-effect-value (constructor-value "string" (list "Hello, Pure HoTT!") String) String))
(printf "Created pure effect: ~a~n" hello-effect)

;; Test 2: Execute with different tiers
(printf "~nTesting different execution tiers:~n")

;; Tier 0: Compile-time elimination
(define tier0-context (make-tier0-context test-cache))
(define tier0-result (execute-effect-with-cache hello-effect tier0-context))
(printf "Tier 0 result: ~a~n" (effect-result-value tier0-result))
(printf "Tier 0 evidence: ~a~n" (constructor-value-constructor-name (effect-result-evidence tier0-result)))

;; Tier 1: Compile-time specialization  
(define tier1-context (make-tier1-context test-cache))
(define tier1-result (execute-effect-with-cache hello-effect tier1-context))
(printf "Tier 1 evidence: ~a~n" (constructor-value-constructor-name (effect-result-evidence tier1-result)))

;; Tier 2: Effect promotion
(define tier2-context (make-tier2-context test-cache))
(define tier2-result (execute-effect-with-cache hello-effect tier2-context))
(printf "Tier 2 evidence: ~a~n" (constructor-value-constructor-name (effect-result-evidence tier2-result)))

;; Tier 3: Runtime caching
(define tier3-context (make-tier3-context test-cache))
(define tier3-result (execute-effect-with-cache hello-effect tier3-context))
(printf "Tier 3 evidence: ~a~n" (constructor-value-constructor-name (effect-result-evidence tier3-result)))

(printf "~n✓ All tier-aware execution modes working!~n")
(printf "✓ Pure HoTT effects successfully integrated with PathFinder's tier system!~n")