;; ============================================================================
;; IR PRETTY PRINTER
;; ============================================================================
;; Human-readable output for IR structures with evidence annotations

(import compiler.ir core)
(import types types)

;; ============================================================================
;; PRETTY PRINTING CONFIGURATION
;; ============================================================================

(data PrintConfig U0
  (case print-config (-> (indent-width : Nat)
                        (show-evidence : Bool)
                        (show-types : Bool)
                        (max-width : Nat)
                        PrintConfig)))

;; Default configuration
(define default-print-config
  (print-config two true true eighty))

;; ============================================================================
;; VALUE PRETTY PRINTING
;; ============================================================================

(define print-ir-value
  (fn (val config indent)
    (match val
      ;; Simple values
      (case (ir-nat n)
        (nat-to-string n))
      
      (case (ir-bool b)
        (if b "true" "false"))
      
      (case (ir-string s)
        (string-append "\"" (string-append s "\"")))
      
      (case ir-unit
        "unit")
      
      ;; Variables
      (case (ir-var name index)
        (string-append name (string-append "@" (nat-to-string index))))
      
      ;; Constructors
      (case (ir-constructor name args evidence)
        (let ((args-str (print-list args config indent)))
          (if (show-evidence config)
              (string-append "(" (string-append name 
                (string-append " " (string-append args-str
                  (string-append " #evidence=" (print-evidence evidence))))))
              (string-append "(" (string-append name 
                (string-append " " (string-append args-str ")")))))))
      
      ;; Types
      (case (ir-type level)
        (string-append "Type" (nat-to-string level)))
      
      (case (ir-arrow from to)
        (string-append "(" (string-append (print-ir-value from config indent)
          (string-append " -> " (string-append (print-ir-value to config indent) ")")))))
      
      ;; Closures
      (case (ir-closure params body env evidence)
        (let ((params-str (string-join params " ")))
          (string-append "(λ " (string-append params-str
            (string-append " . " (print-ir-computation body config (succ indent))))))))))

;; ============================================================================
;; COMPUTATION PRETTY PRINTING
;; ============================================================================

(define print-ir-computation
  (fn (comp config indent)
    (let ((indent-str (make-indent indent config)))
      (match comp
        ;; Return
        (case (ir-return val)
          (string-append indent-str (print-ir-value val config indent)))
        
        ;; Let binding
        (case (ir-let name value body space)
          (string-append indent-str
            (string-append "let " (string-append name
              (string-append " = " (string-append 
                (print-ir-computation value config indent)
                (string-append "\n" (print-ir-computation body config indent))))))))
        
        ;; Application
        (case (ir-app func arg evidence)
          (string-append indent-str
            (string-append "(" (string-append 
              (print-ir-value func config indent)
              (string-append " " (string-append 
                (print-ir-value arg config indent)
                (if (show-evidence config)
                    (string-append " #" (print-comp-evidence evidence))
                    ")")))))))
        
        ;; Nat elimination
        (case (ir-nat-elim motive base step target evidence)
          (string-append indent-str
            (string-append "nat-elim(" (string-append
              (print-ir-value motive config indent) (string-append ", "
                (print-ir-value base config indent) (string-append ", "
                  (print-ir-value step config indent) (string-append ", "
                    (print-ir-value target config indent) ")")))))))
        
        ;; Bool elimination
        (case (ir-bool-elim motive false-case true-case target evidence)
          (string-append indent-str
            (string-append "bool-elim(" (string-append
              (print-ir-value motive config indent) (string-append ", "
                (print-ir-value false-case config indent) (string-append ", "
                  (print-ir-value true-case config indent) (string-append ", "
                    (print-ir-value target config indent) ")")))))))
        
        ;; Sequence
        (case (ir-sequence first second)
          (string-append 
            (print-ir-computation first config indent)
            (string-append "\n" 
              (print-ir-computation second config indent))))
        
        ;; Conditional
        (case (ir-if condition then-branch else-branch evidence)
          (string-append indent-str
            (string-append "if " (string-append
              (print-ir-value condition config indent) (string-append "\n"
                (string-append (make-indent (succ indent) config) (string-append "then "
                  (print-ir-computation then-branch config (succ indent)) (string-append "\n"
                    (string-append (make-indent (succ indent) config) (string-append "else "
                      (print-ir-computation else-branch config (succ indent))))))))))))))))

;; ============================================================================
;; MODULE PRETTY PRINTING
;; ============================================================================

(define print-ir-module
  (fn (module config)
    (match module
      (case (ir-module name imports defs exports evidence)
        (string-append ";; Module: " (string-append name "\n"
          (string-append (print-imports imports) "\n"
            (string-append (print-definitions defs config) "\n"
              (string-append (print-exports exports) "\n"
                (if (show-evidence config)
                    (string-append ";; Module Evidence: " 
                      (print-module-evidence evidence))
                    ""))))))))))

;; Print imports
(define print-imports
  (fn (imports)
    (if (null? imports)
        ""
        (string-append ";; Imports:\n" 
          (string-join (map print-import imports) "\n")))))

;; Print single import
(define print-import
  (fn (imp)
    ";;   (import ...)"))

;; Print definitions
(define print-definitions
  (fn (defs config)
    (string-join (map (fn (def) (print-ir-definition def config)) defs) "\n\n")))

;; Print single definition
(define print-ir-definition
  (fn (def config)
    (match def
      (case (ir-def-value name type value evidence)
        (string-append "(define " (string-append name "\n"
          (string-append "  : " (string-append (print-ir-value type config zero) "\n"
            (string-append "  " (print-ir-computation value config one)))))))
      
      (case (ir-def-type name params type)
        (string-append "(type " (string-append name 
          (string-append " " (string-append (string-join params " ")
            (string-append "\n  " (print-ir-value type config one)))))))
      
      (case (ir-def-handler name effect-type handler)
        (string-append "(handler " (string-append name
          (string-append " : " (string-append (print-effect-type effect-type)
            (string-append "\n  " (print-ir-computation handler config one))))))))))

;; Print exports
(define print-exports
  (fn (exports)
    (if (null? exports)
        ""
        (string-append ";; Exports: " (string-join exports ", ")))))

;; ============================================================================
;; EVIDENCE PRETTY PRINTING
;; ============================================================================

(define print-evidence
  (fn (evidence)
    (match evidence
      (case (immediate-termination steps)
        (string-append "term(" (string-append (nat-to-string steps) ")")))
      (case _
        "evidence"))))

(define print-comp-evidence
  (fn (evidence)
    (match evidence
      (case (comp-evidence term complex space)
        "comp-evidence")
      (case _
        "evidence"))))

(define print-module-evidence
  (fn (evidence)
    "module-evidence"))

(define print-effect-type
  (fn (effect-type)
    (match effect-type
      (case print-effect-type "Print")
      (case file-effect-type "File")
      (case io-effect-type "IO")
      (case computation-effect-type "Computation"))))

;; ============================================================================
;; HELPER FUNCTIONS
;; ============================================================================

;; Create indentation string
(define make-indent
  (fn (level config)
    (match config
      (case (print-config width _ _ _)
        (string-repeat " " (mult level width))))))

;; Print list of values
(define print-list
  (fn (vals config indent)
    (string-join (map (fn (v) (print-ir-value v config indent)) vals) " ")))

;; String operations (stubs)
(define string-join
  (fn (strs sep)
    (match strs
      (case nil "")
      (case (cons s nil) s)
      (case (cons s rest)
        (string-append s (string-append sep (string-join rest sep)))))))

(define string-repeat
  (fn (s n)
    (nat-elim (fn (_) String)
              ""
              (fn (_ acc) (string-append s acc))
              n)))

(define nat-to-string
  (fn (n) "n"))

(define mult
  (fn (x y)
    (nat-elim (fn (_) Nat)
              zero
              (fn (_ acc) (add x acc))
              y)))

(define add
  (fn (x y)
    (nat-elim (fn (_) Nat)
              x
              (fn (_ acc) (succ acc))
              y)))

(define null?
  (fn (lst)
    (match lst
      (case nil true)
      (case _ false))))

(define map
  (fn (f lst)
    (match lst
      (case nil nil)
      (case (cons x xs) (cons (f x) (map f xs))))))

(define eighty (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ zero)))))))))))
(define two (succ (succ zero)))

;; ============================================================================
;; PUBLIC API
;; ============================================================================

;; Pretty print a value with default config
(define pretty-print-value
  (fn (val)
    (print-ir-value val default-print-config zero)))

;; Pretty print a computation with default config
(define pretty-print-computation
  (fn (comp)
    (print-ir-computation comp default-print-config zero)))

;; Pretty print a module with default config
(define pretty-print-module
  (fn (module)
    (print-ir-module module default-print-config)))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export print-ir-value)
(export print-ir-computation)
(export print-ir-module)
(export pretty-print-value)
(export pretty-print-computation)
(export pretty-print-module)
(export PrintConfig)
(export default-print-config)