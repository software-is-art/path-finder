;; ============================================================================
;; PATHFINDER PARSER - MAIN ENTRY POINT
;; ============================================================================
;; Combines lexer, parser, and AST converter for PathFinder self-hosting

(import lexer lexer)
(import parser parser) 
(import parser sexp-to-ast)
(import core ast)
(import effects effects)

;; ============================================================================
;; PARSE RESULT TYPE
;; ============================================================================

(data ParseResult U0
  (case parse-ok (-> HoTT-AST ParseResult))
  (case parse-fail (-> String ParseResult)))

;; ============================================================================
;; MAIN PARSE FUNCTIONS
;; ============================================================================

;; Parse a string into AST
(define parse-pathfinder-string
  (fn (input)
    (let ((tokens (tokenize input)))
      (match (parse-string input)
        (case (ok sexpr)
          (match (sexp-to-ast sexpr)
            (case (conv-ok ast) (parse-ok ast))
            (case (conv-error msg) (parse-fail msg))))
        (case (error msg)
          (parse-fail msg))))))

;; Parse a file (string) into list of ASTs
(define parse-pathfinder-file
  (fn (input)
    (let ((tokens (tokenize input)))
      (match (parse-file tokens)
        (case (ok sexprs)
          (parse-ok (convert-file sexprs)))
        (case (error msg)
          (parse-fail msg))))))

;; ============================================================================
;; EFFECT-BASED PARSING
;; ============================================================================

;; Read and parse a file
(define parse-file-effect
  (fn (filename)
    (perform
      (bind (read-file filename)
            (fn (contents)
              (match (parse-pathfinder-file contents)
                (case (parse-ok ast) (return ast))
                (case (parse-fail msg) 
                  (fail (string-append "Parse error in " 
                                      (string-append filename ": " msg))))))))))

;; ============================================================================
;; COMPATIBILITY LAYER
;; ============================================================================

;; For compatibility with existing code expecting the old parser interface
(define parse-program
  (fn (input)
    (match (parse-pathfinder-file input)
      (case (parse-ok asts)
        (pure (ok (module-node "main" asts))))
      (case (parse-fail msg)
        (pure (error msg))))))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export parse-pathfinder-string)
(export parse-pathfinder-file)
(export parse-file-effect)
(export parse-program)
(export ParseResult)
(export parse-ok)
(export parse-fail)