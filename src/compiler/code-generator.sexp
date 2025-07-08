;; ============================================================================
;; RUST CODE GENERATOR
;; ============================================================================
;; Converts RustAST to Rust source code strings
;; This is the core of PathFinder's self-compilation capability

(import types types)
(import compiler rust-ast)
(import core foundations)

;; ============================================================================
;; STRING UTILITIES
;; ============================================================================

;; Join strings with a separator
(type string-join (-> String (List String) String))
(define string-join
  (fn (sep strs)
    (match strs
      (case nil "")
      (case (cons s nil) s)
      (case (cons s rest)
        (string-append s 
                      (string-append sep 
                                    (string-join sep rest)))))))

;; Indent a string
(type indent (-> String String))
(define indent
  (fn (s)
    (string-append "    " s)))

;; Indent all lines in a list
(type indent-lines (-> (List String) (List String)))
(define indent-lines
  (fn (lines)
    (list-map indent lines)))

;; ============================================================================
;; TYPE GENERATION
;; ============================================================================

(type gen-rust-type (-> RustType String))
(define gen-rust-type
  (fn (ty)
    (match ty
      (case rust-unit "()")
      (case rust-bool "bool")
      (case rust-usize "usize")
      (case rust-string "String")
      (case (rust-ref t) 
        (string-append "&" (gen-rust-type t)))
      (case (rust-mut-ref t)
        (string-append "&mut " (gen-rust-type t)))
      (case (rust-box t)
        (string-append "Box<" (gen-rust-type t) ">"))
      (case (rust-vec t)
        (string-append "Vec<" (gen-rust-type t) ">"))
      (case (rust-option t)
        (string-append "Option<" (gen-rust-type t) ">"))
      (case (rust-result ok err)
        (string-append "Result<" 
                      (gen-rust-type ok) ", " 
                      (gen-rust-type err) ">"))
      (case (rust-named name) name)
      (case (rust-fn params ret)
        (string-append "fn(" 
                      (string-join ", " (list-map gen-rust-type params))
                      ") -> "
                      (gen-rust-type ret))))))

;; ============================================================================
;; PATTERN GENERATION
;; ============================================================================

(type gen-rust-pattern (-> RustPattern String))
(define gen-rust-pattern
  (fn (pat)
    (match pat
      (case (rust-var-pat name) name)
      (case rust-wildcard "_")
      (case (rust-struct-pat name fields)
        (string-append name " { "
                      (string-join ", " 
                        (list-map (fn (field)
                                   (string-append (fst field) ": " 
                                                 (gen-rust-pattern (snd field))))
                                 fields))
                      " }"))
      (case (rust-enum-pat name pats)
        (string-append name "("
                      (string-join ", " (list-map gen-rust-pattern pats))
                      ")"))
      (case (rust-tuple-pat pats)
        (string-append "("
                      (string-join ", " (list-map gen-rust-pattern pats))
                      ")")))))

;; ============================================================================
;; EXPRESSION GENERATION
;; ============================================================================

(type gen-rust-expr (-> RustExpr String))
(define gen-rust-expr
  (fn (expr)
    (match expr
      ;; Literals
      (case rust-unit-lit "()")
      (case (rust-bool-lit b)
        (bool-elim (fn (_) String) "false" "true" b))
      (case (rust-usize-lit n)
        (nat-to-string n))
      (case (rust-string-lit s)
        (string-append "\"" s "\""))
      
      ;; Variables
      (case (rust-var name) name)
      
      ;; Function calls
      (case (rust-call f args)
        (string-append (gen-rust-expr f)
                      "("
                      (string-join ", " (list-map gen-rust-expr args))
                      ")"))
      
      ;; Method calls
      (case (rust-method-call obj method args)
        (string-append (gen-rust-expr obj)
                      "."
                      method
                      "("
                      (string-join ", " (list-map gen-rust-expr args))
                      ")"))
      
      ;; Struct construction
      (case (rust-struct name fields)
        (string-append name " { "
                      (string-join ", "
                        (list-map (fn (field)
                                   (string-append (fst field) ": "
                                                 (gen-rust-expr (snd field))))
                                 fields))
                      " }"))
      
      ;; Enum construction
      (case (rust-enum ty variant args)
        (string-append ty "::" variant
                      (if (nil? args)
                          ""
                          (string-append "("
                                       (string-join ", " (list-map gen-rust-expr args))
                                       ")"))))
      
      ;; Control flow
      (case (rust-if cond then else)
        (string-append "if " (gen-rust-expr cond) " {\n"
                      (indent (gen-rust-expr then)) "\n"
                      "} else {\n"
                      (indent (gen-rust-expr else)) "\n"
                      "}"))
      
      ;; Match expression
      (case (rust-match expr arms)
        (string-append "match " (gen-rust-expr expr) " {\n"
                      (string-join ",\n" 
                        (list-map (fn (arm)
                                   (indent (gen-rust-match-arm arm)))
                                 arms))
                      "\n}"))
      
      ;; Block
      (case (rust-block stmts)
        (string-append "{\n"
                      (string-join "\n" (indent-lines (list-map gen-rust-stmt stmts)))
                      "\n}"))
      
      ;; References
      (case (rust-ref e)
        (string-append "&" (gen-rust-expr e)))
      (case (rust-deref e)
        (string-append "*" (gen-rust-expr e)))
      
      ;; Boxing
      (case (rust-box-new e)
        (string-append "Box::new(" (gen-rust-expr e) ")"))
      
      ;; Clone
      (case (rust-clone e)
        (string-append (gen-rust-expr e) ".clone()"))
      
      ;; Binary operators
      (case (rust-binop op left right)
        (string-append "(" (gen-rust-expr left) 
                      " " op " " 
                      (gen-rust-expr right) ")")))))

;; Generate match arm
(type gen-rust-match-arm (-> RustMatchArm String))
(define gen-rust-match-arm
  (fn (arm)
    (match arm
      (case (rust-match-arm pat expr)
        (string-append (gen-rust-pattern pat) " => " (gen-rust-expr expr))))))

;; ============================================================================
;; STATEMENT GENERATION
;; ============================================================================

(type gen-rust-stmt (-> RustStmt String))
(define gen-rust-stmt
  (fn (stmt)
    (match stmt
      (case (rust-let pat ty-opt expr)
        (string-append "let " (gen-rust-pattern pat)
                      (match ty-opt
                        (case (some ty) (string-append ": " (gen-rust-type ty)))
                        (case none ""))
                      " = " (gen-rust-expr expr) ";"))
      (case (rust-expr-stmt expr)
        (string-append (gen-rust-expr expr) ";"))
      (case (rust-return expr)
        (string-append "return " (gen-rust-expr expr) ";")))))

;; ============================================================================
;; ITEM GENERATION
;; ============================================================================

(type gen-rust-item (-> RustItem String))
(define gen-rust-item
  (fn (item)
    (match item
      ;; Function definition
      (case (rust-fn-def name params ret-ty stmts)
        (string-append "pub fn " name "("
                      (string-join ", "
                        (list-map (fn (param)
                                   (string-append (fst param) ": " 
                                                 (gen-rust-type (snd param))))
                                 params))
                      ") -> " (gen-rust-type ret-ty) " {\n"
                      (string-join "\n" (indent-lines (list-map gen-rust-stmt stmts)))
                      "\n}"))
      
      ;; Struct definition
      (case (rust-struct-def name fields)
        (string-append "pub struct " name " {\n"
                      (string-join ",\n"
                        (indent-lines
                          (list-map (fn (field)
                                     (string-append "pub " (fst field) ": "
                                                   (gen-rust-type (snd field))))
                                   fields)))
                      "\n}"))
      
      ;; Enum definition
      (case (rust-enum-def name variants)
        (string-append "pub enum " name " {\n"
                      (string-join ",\n"
                        (indent-lines (list-map gen-rust-variant variants)))
                      "\n}"))
      
      ;; Other items (simplified for now)
      (case _ "// TODO: Other item types"))))

;; Generate enum variant
(type gen-rust-variant (-> RustVariant String))
(define gen-rust-variant
  (fn (var)
    (match var
      (case (rust-variant name types)
        (if (nil? types)
            name
            (string-append name "(" 
                          (string-join ", " (list-map gen-rust-type types))
                          ")"))))))

;; ============================================================================
;; MODULE GENERATION
;; ============================================================================

(type gen-rust-module (-> RustModule String))
(define gen-rust-module
  (fn (mod)
    (match mod
      (case (rust-module name uses items)
        (string-append "// Generated by PathFinder\n"
                      "// Module: " name "\n\n"
                      (string-join "\n" (list-map gen-rust-use uses))
                      "\n\n"
                      (string-join "\n\n" (list-map gen-rust-item items)))))))

;; Generate use statement
(type gen-rust-use (-> RustUse String))
(define gen-rust-use
  (fn (use)
    (match use
      (case (rust-use-item path)
        (string-append "use " path ";")))))

;; ============================================================================
;; HELPER FUNCTIONS
;; ============================================================================

;; Convert natural number to string (simplified)
(type nat-to-string (-> Nat String))
(define nat-to-string
  (fn (n)
    (nat-elim (fn (_) String)
              "0"
              (fn (m rec)
                ;; This is simplified - real implementation would be more complex
                (string-append "succ(" rec ")"))
              n)))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export gen-rust-type)
(export gen-rust-pattern)
(export gen-rust-expr)
(export gen-rust-stmt)
(export gen-rust-item)
(export gen-rust-module)
(export gen-rust-match-arm)
(export gen-rust-variant)
(export gen-rust-use)