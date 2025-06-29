;; ============================================================================
;; PATHFINDER BOOTSTRAP MODULE
;; ============================================================================
;; This is the minimal bootstrap module that loads the core system

;; First, let's test basic evaluation works
(define test-nat (succ (succ zero)))

;; Load core modules needed for self-hosting
(import core types)
(import core foundations) 
(import core eliminators)
(import evaluator values)
(import parser parser)
(import core evaluator)
(import core modules)
(import effects effects)

;; Define bootstrap function
(define bootstrap-pathfinder
  (fn ()
    (print "PathFinder Bootstrap Complete!")))

;; Run bootstrap
(bootstrap-pathfinder)