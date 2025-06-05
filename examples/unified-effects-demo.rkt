#lang racket/base

;; Demonstration: Unified Algebraic Effect System (Unison-style)

;; ============================================================================
;; THREE TIERS, ONE ALGEBRAIC STRUCTURE
;; ============================================================================

;; TIER 1: Pure Computational (No Effects)
;; (comp-add 3 4) : Nat →{} Nat
;; Result: 7 (computed at compile time, injected as constant)

;; TIER 2: Compile-Time Effects (Build fails if unhandled)  
;; (effect-read-config "app.json") : String →{compile-time-file-read} Config
;; Handler: COMPILE-TIME-HANDLER
;; Resolution: Build time - file must exist or build fails
;; Result: Config data injected as compile-time constant

;; TIER 3: Runtime Effects (Dynamic, can fail)
;; (runtime-read-file user-input-path) : String →{runtime-file-read} (Result String)
;; Handler: RUNTIME-HANDLER  
;; Resolution: Runtime - path computed dynamically, can fail gracefully
;; Result: Runtime computation that returns Result<String, Error>

;; ============================================================================
;; UNISON-STYLE CAPABILITIES
;; ============================================================================

;; File capability with read/write permissions
;; file-cap : FileCapability{read, write}

;; Network capability with http-get permission  
;; net-cap : NetworkCapability{http-get}

;; Capability-secured operations
;; (secure-file-read "data.txt" file-cap) : String →{capability-file-read} String
;; (secure-http-get "api.com/data" net-cap) : String →{capability-network} JSON

;; ============================================================================
;; KEY INSIGHTS
;; ============================================================================

;; 1. SAME ALGEBRAIC STRUCTURE
;;    All effects are algebraic - they compose, have handlers, signal intent
;;    The difference is WHEN and HOW they're resolved, not their structure

;; 2. HANDLER TYPE DETERMINES RESOLUTION
;;    - COMPILE-TIME: Resolved during build, failures stop compilation
;;    - RUNTIME: Resolved during execution, failures handled gracefully  
;;    - CAPABILITY: Resolved with permission checking, capabilities required
;;    - TEST: Resolved with mocks, deterministic testing

;; 3. COMPILER ENFORCES CONSUMPTION  
;;    Type system ensures ALL effects are handled before reaching runtime
;;    Whether compile-time, runtime, or capability-based - all must be consumed

;; 4. UNISON-STYLE CAPABILITIES
;;    Fine-grained permissions: FileCapability{read}, NetworkCapability{http-get}
;;    Capability injection: Functions receive capabilities as arguments
;;    Secure by default: No ambient authority, explicit permission flow

;; ============================================================================
;; EXAMPLES
;; ============================================================================

;; TIER 2 (Compile-time) vs TIER 3 (Runtime) - Same effect, different handler:

;; Compile-time file reading (path known at build time)
;; (define config (compile-time-read-file "config.json"))
;; Type: String →{compile-time-file-read} Config
;; Handler: Build system reads file, injects content as constant

;; Runtime file reading (path computed dynamically)  
;; (define content (runtime-read-file (user-input-filename)))
;; Type: String →{runtime-file-read} (Result String Error)
;; Handler: Runtime system reads file, can fail gracefully

;; Capability-based file reading (permission-secured)
;; (define secure-content (capability-read-file path file-capability))
;; Type: String → FileCapability{read} →{capability-file-read} String  
;; Handler: Capability system checks permissions, then reads file

;; ============================================================================
;; EFFECT COMPOSITION WORKS ACROSS TIERS
;; ============================================================================

;; Mixed-tier function:
;; (define (process-user-data username)
;;   (let ([config (compile-time-read-file "app.json")]      ; Tier 2
;;         [user-file (runtime-read-file username)]          ; Tier 3  
;;         [result (comp-hash user-file)])                   ; Tier 1
;;     (capability-log result log-capability)))              ; Capability
;;
;; Type: String →{compile-time-file-read, runtime-file-read, capability-log} Result
;; 
;; All effects must be handled:
;; - compile-time-file-read: Build system
;; - runtime-file-read: Runtime error handling  
;; - capability-log: Log capability injection

(printf "Unified Algebraic Effect System (Unison-style)~n")
(printf "=====================================~n")
(printf "Tier 1: Pure computational →{} Result~n")
(printf "Tier 2: Compile-time →{compile-time-effects} Result~n") 
(printf "Tier 3: Runtime →{runtime-effects} (Result Value Error)~n")
(printf "Capabilities: →{capability-effects} Result (with permissions)~n")
(printf "~n")
(printf "Key: Same algebraic structure, different handler resolution~n")
(printf "Compiler enforces effect consumption across ALL tiers~n")