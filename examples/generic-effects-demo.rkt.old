#lang racket/base

;; Demonstration: Truly Generic Effect System

;; ============================================================================
;; USER-DEFINED EFFECTS (Not hardcoded!)
;; ============================================================================

;; User defines their own FileIO effect:
(defeffect FileIO
  (read-file String -> String)
  (write-file String String -> Unit)
  (file-exists String -> Bool))

;; User defines their own NetworkIO effect:
(defeffect NetworkIO  
  (http-get String -> String)
  (http-post String String -> String)
  (download-file String String -> Unit))

;; User defines their own Console effect:
(defeffect Console
  (read-line String -> String)
  (print-line String -> Unit)
  (clear-screen Unit -> Unit))

;; User defines custom Database effect:
(defeffect Database
  (query String -> List)
  (execute String -> Unit)
  (transaction (Unit -> a) -> a))

;; ============================================================================
;; USER-DEFINED HANDLERS (For different contexts)
;; ============================================================================

;; Compile-time handler for FileIO
(defhandler compile-time-file FileIO compile-time
  [(read-file path) 
   (if (file-exists? path)
       (file->string path)
       (error "Build failed: File not found: " path))]
  [(write-file path content)
   (error "Build failed: Cannot write files at compile time")]
  [(file-exists path)
   (file-exists? path)])

;; Runtime handler for FileIO
(defhandler runtime-file FileIO runtime
  [(read-file path)
   (with-handlers ([exn:fail? (lambda (e) 
                                (error "Runtime file read failed: " path))])
     (file->string path))]
  [(write-file path content)
   (with-handlers ([exn:fail? (lambda (e) 
                                (error "Runtime file write failed: " path))])
     (display-to-file content path))]
  [(file-exists path)
   (file-exists? path)])

;; Test handler for FileIO (deterministic testing)
(defhandler test-file FileIO test
  [(read-file path)
   (case path
     [("config.json") "{\"version\": \"1.0\"}"]
     [("data.txt") "test data"]
     [else "mock file content"])]
  [(write-file path content)
   'ok]
  [(file-exists path)
   #t])

;; Mock handler for NetworkIO
(defhandler mock-network NetworkIO test  
  [(http-get url)
   (format "Mock GET response from ~a" url)]
  [(http-post url data)
   (format "Mock POST response from ~a with ~a" url data)]
  [(download-file url path)
   'ok])

;; ============================================================================
;; TYPE SYSTEM INTEGRATION
;; ============================================================================

;; The type checker tracks effects generically:

;; read-config : String →{FileIO} Config
(define (read-config path)
  (perform 'FileIO 'read-file path))

;; fetch-data : String →{NetworkIO} Data  
(define (fetch-data url)
  (perform 'NetworkIO 'http-get url))

;; interactive-prompt : String →{Console} String
(define (interactive-prompt message)
  (perform 'Console 'print-line message)
  (perform 'Console 'read-line ">> "))

;; mixed-effects : String →{FileIO, NetworkIO} Result
(define (backup-to-cloud local-path remote-url)
  (let ([data (perform 'FileIO 'read-file local-path)])
    (perform 'NetworkIO 'http-post remote-url data)))

;; ============================================================================
;; KEY INSIGHTS
;; ============================================================================

;; 1. NO HARDCODED EFFECTS
;;    - Type checker doesn't know about FileIO, NetworkIO, etc.
;;    - Users define their own effects with custom operations
;;    - Effect definitions are first-class values

;; 2. GENERIC TYPE TRACKING
;;    - Type system tracks {EffectName} in function types
;;    - Effect composition works for any user-defined effects
;;    - No special cases in the type checker

;; 3. HANDLER FLEXIBILITY  
;;    - Same effect can have multiple handlers
;;    - compile-time vs runtime vs test vs mock
;;    - Handlers are also user-defined

;; 4. TRUE EXTENSIBILITY
;;    - Add new effects without modifying compiler
;;    - Add new handlers without modifying runtime
;;    - Effects compose algebraically

;; ============================================================================
;; USAGE EXAMPLES
;; ============================================================================

;; Different handlers for different contexts:

;; At compile time (build fails if file missing):
(handle (read-config "app.json") 'compile-time-file)

;; At runtime (graceful failure):
(handle (read-config user-input-path) 'runtime-file)

;; In tests (deterministic):
(handle (read-config "config.json") 'test-file)  ; Always returns same value

;; Mixed effects with multiple handlers:
(with-handlers ([FileIO 'runtime-file]
                [NetworkIO 'mock-network])
  (backup-to-cloud "data.txt" "https://api.example.com/backup"))

(printf "Generic Effect System Demo~n")
(printf "========================~n")
(printf "Effects are user-defined, not hardcoded!~n")
(printf "Type checker only knows about the CONCEPT of effects~n")
(printf "Users define: effects, operations, handlers~n")
(printf "~n")
(printf "Example effects: FileIO, NetworkIO, Console, Database~n")
(printf "Example handlers: compile-time, runtime, test, mock~n")