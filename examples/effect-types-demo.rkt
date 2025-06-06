#lang racket/base

;; Demonstration of Effect Types in HoTT-based PathFinder

;; Pure function: String → String (no effects)
;; (define (uppercase str) ...)
;; Type: String → String

;; Effect function: String →{file-read} String (requires file-read effect)
;; (define (read-and-uppercase filename) ...)  
;; Type: String →{file-read} String
;; Return type is still String, but requires file-read to be handled

;; Effect composition: String →{file-read, network} String
;; (define (read-file-then-upload filename url) ...)
;; Type: String →{file-read, network} String
;; Return type is String, but requires both file-read AND network effects

;; Key insights:

;; 1. RETURN TYPE UNCHANGED
;;    read-and-uppercase: String →{file-read} String
;;    Still returns String, just like a pure function
;;    But the type system tracks the file-read requirement

;; 2. EFFECT COMPOSITION
;;    If function A requires {file-read} and calls function B requiring {network}
;;    Then A's effective type becomes String →{file-read, network} String

;; 3. HANDLER ENFORCEMENT
;;    The type system prevents:
;;    - Calling effect functions without handlers
;;    - Forgetting to handle effects before runtime
;;    - Effect leakage (unhandled effects reaching runtime)

;; 4. ALGEBRAIC PROPERTIES
;;    Effects form an algebraic structure:
;;    - Union: {file-read} ∪ {network} = {file-read, network}
;;    - Subsumption: {file-read} ⊆ {file-read, network}
;;    - Identity: {} (no effects) can be used anywhere

;; Example type evolution:

;; Step 1: Pure function
;; uppercase : String → String

;; Step 2: Add file reading (effect added, return type unchanged)
;; read-and-uppercase : String →{file-read} String

;; Step 3: Add validation (more effects, return type still unchanged)  
;; read-validate-uppercase : String →{file-read, validate} String

;; Step 4: Add error handling (different return type, effects preserved)
;; safe-read-validate-uppercase : String →{file-read, validate} String ! {PartialFailure}

;; The power: Effects track **how** computation happens,
;; while return types track **what** computation produces

(printf "Effect Types Demo: Effects change requirements, not return types~n")
(printf "Pure function:     String → String~n")
(printf "Effect function:   String →{file-read} String~n") 
(printf "Complex function:  String →{file-read,network,validate} String~n")
(printf "~n")
(printf "Key insight: Same return type (String), different effect requirements~n")