;; ============================================================================
;; MODULE SYNTAX PARSER EXTENSION
;; ============================================================================
;; Parses module-related syntax: imports, exports, module declarations

(import parser parser)
(import lexer lexer)
(import effects effects)

;; ============================================================================
;; IMPORT SYNTAX PARSING
;; ============================================================================

;; Parse import statement
;; Syntax: 
;;   (import module-name)
;;   (import module-name :as alias)
;;   (import module-name :only (name1 name2))
(type parse-import-statement (-> (Parser AST)))
(define parse-import-statement
  (>>= (expect-token (symbol-token "("))
       (fn (_)
         (>>= (expect-token (identifier-token "import"))
              (fn (_)
                (>>= parse-module-path
                     (fn (path)
                       (>>= parse-import-qualifier
                            (fn (qualifier)
                              (>>= (expect-token (symbol-token ")"))
                                   (fn (_)
                                     (pure (make-import-node path qualifier)))))))))))))

;; Parse module path (can be dotted: core.types)
(type parse-module-path (-> (Parser String)))
(define parse-module-path
  (>>= parse-identifier
       (fn (first)
         (>>= parse-module-path-rest
              (fn (rest)
                (pure (string-join "." (cons first rest))))))))

;; Parse rest of module path
(type parse-module-path-rest (-> (Parser (List String))))
(define parse-module-path-rest
  (choice
    (>>= (expect-token (symbol-token "."))
         (fn (_)
           (>>= parse-identifier
                (fn (next)
                  (>>= parse-module-path-rest
                       (fn (rest)
                         (pure (cons next rest))))))))
    (pure nil)))

;; Parse import qualifier (:as alias or :only (...))
(type parse-import-qualifier (-> (Parser ImportQualifier)))
(define parse-import-qualifier
  (choice
    ;; :as alias
    (>>= (expect-token (keyword-token ":as"))
         (fn (_)
           (>>= parse-identifier
                (fn (alias)
                  (pure (as-qualifier alias))))))
    ;; :only (...)
    (>>= (expect-token (keyword-token ":only"))
         (fn (_)
           (>>= parse-name-list
                (fn (names)
                  (pure (only-qualifier names))))))
    ;; No qualifier
    (pure no-qualifier)))

;; Import qualifier types
(data ImportQualifier U0
  (case no-qualifier ImportQualifier)
  (case as-qualifier (-> String ImportQualifier))
  (case only-qualifier (-> (List String) ImportQualifier)))

;; Parse list of names in parens
(type parse-name-list (-> (Parser (List String))))
(define parse-name-list
  (>>= (expect-token (symbol-token "("))
       (fn (_)
         (>>= parse-identifier-list
              (fn (names)
                (>>= (expect-token (symbol-token ")"))
                     (fn (_)
                       (pure names))))))))

;; Parse list of identifiers
(type parse-identifier-list (-> (Parser (List String))))
(define parse-identifier-list
  (>>= parse-identifier
       (fn (first)
         (>>= parse-identifier-list-rest
              (fn (rest)
                (pure (cons first rest)))))))

;; ============================================================================
;; MODULE DECLARATION PARSING
;; ============================================================================

;; Parse module declaration
;; Syntax: (module name exports...)
(type parse-module-declaration (-> (Parser AST)))
(define parse-module-declaration
  (>>= (expect-token (symbol-token "("))
       (fn (_)
         (>>= (expect-token (identifier-token "module"))
              (fn (_)
                (>>= parse-identifier
                     (fn (name)
                       (>>= parse-export-list
                            (fn (exports)
                              (>>= (expect-token (symbol-token ")"))
                                   (fn (_)
                                     (pure (make-module-node name exports)))))))))))))

;; Parse export list
(type parse-export-list (-> (Parser (List Export))))
(define parse-export-list
  (many parse-export))

;; Parse single export
(type parse-export (-> (Parser Export)))
(define parse-export
  (choice
    ;; :export name
    (>>= (expect-token (keyword-token ":export"))
         (fn (_)
           (>>= parse-identifier
                (fn (name)
                  (pure (export-name name))))))
    ;; :export-all
    (>>= (expect-token (keyword-token ":export-all"))
         (fn (_)
           (pure export-all)))))

;; Export types
(data Export U0
  (case export-name (-> String Export))
  (case export-all Export))

;; ============================================================================
;; AST CONSTRUCTION
;; ============================================================================

;; Make import AST node
(type make-import-node (-> String ImportQualifier AST))
(define make-import-node
  (fn (path qualifier)
    (match qualifier
      (case no-qualifier
        (import-node path))
      (case (as-qualifier alias)
        (import-qualified-node path alias))
      (case (only-qualifier names)
        (import-selective-node path names)))))

;; Make module AST node
(type make-module-node (-> String (List Export) AST))
(define make-module-node
  (fn (name exports)
    (module-node name (export-list-to-ast exports))))

;; Convert export list to AST
(type export-list-to-ast (-> (List Export) (List AST)))
(define export-list-to-ast
  (fn (exports)
    (List-elim exports
      nil
      (fn (export rest rec)
        (cons (export-to-ast export) rec)))))

;; Convert single export to AST
(type export-to-ast (-> Export AST))
(define export-to-ast
  (fn (export)
    (match export
      (case (export-name name)
        (export-node name))
      (case export-all
        (export-all-node)))))

;; ============================================================================
;; TOP-LEVEL MODULE FILE PARSER
;; ============================================================================

;; Parse complete module file
(type parse-module-file (-> String (Effect AST)))
(define parse-module-file
  (fn (content)
    (>>= (tokenize content)
         (fn (tokens)
           (run-parser parse-module-contents tokens)))))

;; Parse module contents (imports followed by definitions)
(type parse-module-contents (-> (Parser AST)))
(define parse-module-contents
  (>>= (many parse-import-statement)
       (fn (imports)
         (>>= (many parse-top-level-form)
              (fn (definitions)
                (pure (module-contents-node imports definitions)))))))

;; Parse top-level form (type, define, data, etc.)
(type parse-top-level-form (-> (Parser AST)))
(define parse-top-level-form
  (choice
    parse-type-declaration
    parse-define-declaration
    parse-data-declaration
    parse-import-statement))  ;; Allow imports anywhere

;; String join helper
(type string-join (-> String (List String) String))
(define string-join
  (fn (sep strings)
    (List-elim strings
      ""
      (fn (first rest rec)
        (List-elim rest
          first  ;; Single element
          (fn (_ _ _)
            (string-append first
              (string-append sep
                (string-join sep rest)))))))))

;; This provides complete parsing support for:
;; 1. Import statements with qualifiers
;; 2. Module declarations with exports
;; 3. Module file structure
;; 4. Integration with existing parser infrastructure