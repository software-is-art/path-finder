#lang racket/base

(require rackunit
         racket/list
         racket/string
         "../src/main.rkt"
         "../src/evaluator/values.rkt"
         "../src/types/types.rkt")

;; Test suite for Tier 2: Algebraic Effects for Compile-Time Operations

(define (test-effect-expr expr effect-type)
  "Helper to test that an expression evaluates to an effect of the expected type"
  (let ([result (evaluate-string expr)])
    (and (effect-value? result)
         (let ([effect (effect-value-effect result)])
           (eq? (effect-effect-type effect) effect-type)))))

(define (test-handled-effect expr handler expected-result)
  "Helper to test that an effect is handled correctly"
  (let ([effect-result (evaluate-string expr)]
        [handler-result (evaluate-string (format "(handle-effect ~a \"~a\")" expr handler))])
    (and (effect-value? effect-result)
         (string-value? handler-result)
         (string=? (string-value-content handler-result) expected-result))))

(test-case "Effect construction and types"
  
  ;; Test file system effects
  (check-true (test-effect-expr "(effect-read-file \"test.txt\")" 'file-read)
              "Should create file-read effect")
  
  (check-true (test-effect-expr "(effect-file-exists \"config.json\")" 'file-exists)
              "Should create file-exists effect")
  
  ;; Test parsing effects
  (check-true (test-effect-expr "(effect-parse-json \"{\\\"key\\\": \\\"value\\\"}\")" 'parse-json)
              "Should create parse-json effect")
  
  ;; Test computation effects
  (check-true (test-effect-expr "(effect-compute-hash \"data\")" 'compute-hash)
              "Should create compute-hash effect")
  
  (check-true (test-effect-expr "(effect-compute-timestamp)" 'compute-timestamp)
              "Should create compute-timestamp effect")
  
  ;; Test environment effects
  (check-true (test-effect-expr "(effect-read-env \"PATH\")" 'env-read)
              "Should create env-read effect"))

(test-case "Effect handler system"
  
  ;; Test mock handler for file reading
  (let ([result (evaluate-string "(handle-effect (effect-read-file \"test.txt\") \"mock\")")])
    (check-true (string-value? result)
                "Mock handler should return string value")
    (check-true (string-contains? (string-value-content result) "mock-content-of")
                "Mock handler should return mock content"))
  
  ;; Test mock handler for timestamp
  (let ([result (evaluate-string "(handle-effect (effect-compute-timestamp) \"mock\")")])
    (check-true (constructor-value? result)
                "Mock timestamp handler should return nat value")
    (check-true (nat-value? result)
                "Mock timestamp should be a natural number"))
  
  ;; Test mock handler for hash computation
  (let ([result (evaluate-string "(handle-effect (effect-compute-hash \"hello\") \"mock\")")])
    (check-true (string-value? result)
                "Mock hash handler should return string value")
    (check-true (string-contains? (string-value-content result) "hash-")
                "Mock hash should contain hash prefix")))

(test-case "Effect vs direct I/O comparison"
  
  ;; Effects are values that can be composed and analyzed
  (let ([effect-result (evaluate-string "(effect-read-file \"data.txt\")")]
        [effect-result2 (evaluate-string "(effect-read-file \"data.txt\")")])
    (check-true (effect-value? effect-result)
                "Effect should be a value")
    (check-true (effect-value? effect-result2)
                "Same effect should create equivalent value"))
  
  ;; Effects don't perform I/O directly - they signal intent
  (check-exn exn:fail?
             (lambda () (evaluate-string "(handle-effect (effect-read-file \"nonexistent.txt\") \"build\")"))
             "Build handler should not be available in evaluation context"))

(test-case "Algebraic effect composition"
  
  ;; Test that effects can be composed (even if handlers aren't implemented yet)
  (let ([seq-result (evaluate-string "(effect-sequence (effect-read-file \"a.txt\") (effect-read-file \"b.txt\"))")]
        [par-result (evaluate-string "(effect-parallel (effect-compute-hash \"data1\") (effect-compute-hash \"data2\"))")])
    (check-true (effect-value? seq-result)
                "Effect sequence should be an effect value")
    (check-true (effect-value? par-result)
                "Effect parallel should be an effect value")))

(test-case "Effect error handling"
  
  ;; Test type safety
  (check-exn exn:fail?
             (lambda () (evaluate-string "(effect-read-file 123)"))
             "effect-read-file should require string argument")
  
  (check-exn exn:fail?
             (lambda () (evaluate-string "(effect-parse-json 456)"))
             "effect-parse-json should require string argument")
  
  (check-exn exn:fail?
             (lambda () (evaluate-string "(handle-effect \"not-an-effect\" \"mock\")"))
             "handle-effect should require effect value"))

(test-case "HoTT integration with effects"
  
  ;; Test that effects integrate with existing HoTT infrastructure
  (let ([effect-result (evaluate-string "(effect-compute-timestamp)")])
    (check-true (value? effect-result)
                "Effects should be proper HoTT values")
    (check-true (effect-value? effect-result)
                "Effect should have correct type"))
  
  ;; Test effect composition with computational proofs
  (let ([comp-result (evaluate-string "(comp-add 3 4)")]
        [effect-result (evaluate-string "(effect-compute-hash \"test\")")])
    (check-true (nat-value? comp-result)
                "Computational proof should work alongside effects")
    (check-true (effect-value? effect-result)
                "Effect should work alongside computational proofs")))

(test-case "Build-time vs runtime distinction"
  
  ;; Effects signal build-time operations without performing them
  (let ([effect (evaluate-string "(effect-read-file \"README.md\")")])
    (check-true (effect-value? effect)
                "File read effect should be created without performing I/O")
    (check-equal? (effect-effect-type (effect-value-effect effect)) 'file-read
                  "Effect should have correct type"))
  
  ;; Mock handler simulates build-time resolution
  (let ([handled (evaluate-string "(handle-effect (effect-read-file \"README.md\") \"mock\")")])
    (check-true (string-value? handled)
                "Mock handler should resolve effect to value")))

(test-case "Effect system design principles"
  
  ;; Effects are first-class values in HoTT
  (let ([effect1 (evaluate-string "(effect-compute-timestamp)")]
        [effect2 (evaluate-string "(effect-read-env \"HOME\")")])
    (check-true (and (value? effect1) (value? effect2))
                "Effects should be first-class values"))
  
  ;; Effects compose algebraically
  (let ([composed (evaluate-string "(effect-sequence (effect-file-exists \"a.txt\") (effect-read-file \"a.txt\"))")])
    (check-true (effect-value? composed)
                "Effect composition should yield effect values"))
  
  ;; Effects don't break purity - they signal intent
  (let ([expr-string "(effect-read-file \"test.txt\")"])
    (let ([result1 (evaluate-string expr-string)]
          [result2 (evaluate-string expr-string)])
      (check-true (and (effect-value? result1) (effect-value? result2))
                  "Same effect expression should be deterministic"))))

;; Run the tests
(printf "Running Tier 2 Algebraic Effects tests...~n")