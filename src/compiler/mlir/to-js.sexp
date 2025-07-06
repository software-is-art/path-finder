;; ============================================================================
;; MLIR TO JAVASCRIPT LOWERING
;; ============================================================================
;; Converts PathFinder MLIR dialect to JavaScript for web/node execution

(import compiler.mlir dialect)
(import types types)

;; ============================================================================
;; JAVASCRIPT AST
;; ============================================================================

(data JSExpr U0
  ;; Literals
  (case js-number (-> Nat JSExpr))
  (case js-bool (-> Bool JSExpr))
  (case js-string (-> String JSExpr))
  (case js-null JSExpr)
  (case js-undefined JSExpr)
  
  ;; Variables and properties
  (case js-var (-> String JSExpr))
  (case js-prop (-> JSExpr String JSExpr))
  
  ;; Operations
  (case js-binop (-> String JSExpr JSExpr JSExpr))
  (case js-unop (-> String JSExpr JSExpr))
  (case js-ternary (-> JSExpr JSExpr JSExpr JSExpr))
  
  ;; Functions
  (case js-arrow (-> (params : List String) (body : JSExpr) JSExpr))
  (case js-call (-> JSExpr (args : List JSExpr) JSExpr))
  
  ;; Objects and arrays
  (case js-object (-> (fields : List (Pair String JSExpr)) JSExpr))
  (case js-array (-> (elems : List JSExpr) JSExpr))
  
  ;; Control flow
  (case js-return (-> JSExpr JSExpr))
  (case js-if (-> JSExpr JSExpr (else : Option JSExpr) JSExpr))
  (case js-block (-> (stmts : List JSExpr) JSExpr)))

(data JSStmt U0
  ;; Declarations
  (case js-const (-> String JSExpr JSStmt))
  (case js-let (-> String JSExpr JSStmt))
  
  ;; Statements
  (case js-expr-stmt (-> JSExpr JSStmt))
  (case js-return-stmt (-> JSExpr JSStmt))
  
  ;; Module
  (case js-export (-> String JSExpr JSStmt)))

;; ============================================================================
;; RUNTIME REPRESENTATION
;; ============================================================================

;; PathFinder values in JavaScript
(define js-runtime-prelude
  "// PathFinder JavaScript Runtime
const PF = {
  // Natural numbers - use BigInt for arbitrary precision
  nat: (n) => ({ tag: 'nat', value: BigInt(n) }),
  zero: { tag: 'nat', value: 0n },
  succ: (n) => ({ tag: 'nat', value: n.value + 1n }),
  
  // Booleans
  bool: (b) => ({ tag: 'bool', value: b }),
  true: { tag: 'bool', value: true },
  false: { tag: 'bool', value: false },
  
  // Constructors
  constructor: (name, args) => ({ tag: 'constructor', name, args }),
  
  // Closures
  closure: (f, env) => ({ tag: 'closure', fn: f, env }),
  
  // Natural elimination
  natElim: (motive, base, step, n) => {
    if (n.value === 0n) return base;
    let acc = base;
    for (let i = 0n; i < n.value; i++) {
      acc = step({ tag: 'nat', value: i })(acc);
    }
    return acc;
  },
  
  // Boolean elimination  
  boolElim: (motive, falseBranch, trueBranch, b) => {
    return b.value ? trueBranch : falseBranch;
  },
  
  // Effects
  perform: async (effect) => {
    switch (effect.name) {
      case 'print':
        console.log(PF.toString(effect.args[0]));
        return PF.constructor('unit', []);
      case 'read':
        const fs = require('fs');
        return PF.string(fs.readFileSync(PF.toString(effect.args[0]), 'utf8'));
      case 'write':
        const fs = require('fs');
        fs.writeFileSync(PF.toString(effect.args[0]), PF.toString(effect.args[1]));
        return PF.constructor('unit', []);
      default:
        throw new Error(`Unknown effect: ${effect.name}`);
    }
  },
  
  // String conversion
  toString: (val) => {
    switch (val.tag) {
      case 'nat': return val.value.toString();
      case 'bool': return val.value.toString();
      case 'string': return val.value;
      case 'constructor':
        if (val.name === 'nil') return '[]';
        if (val.name === 'cons') return `[${PF.toString(val.args[0])}, ...]`;
        return `${val.name}(${val.args.map(PF.toString).join(', ')})`;
      default: return JSON.stringify(val);
    }
  },
  
  // String creation
  string: (s) => ({ tag: 'string', value: s })
};
")

;; ============================================================================
;; LOWERING CONTEXT
;; ============================================================================

(data JSLoweringContext U0
  (case js-context
    (-> (var-map : List (Pair MLIRValue String))
        (func-map : List (Pair String JSExpr))
        (next-var : Nat)
        (statements : List JSStmt)
        JSLoweringContext)))

;; ============================================================================
;; MODULE LOWERING
;; ============================================================================

(define lower-module-to-js
  (fn (mlir-module)
    (match mlir-module
      (case (mlir-module name attrs functions globals)
        (let ((ctx (make-js-context)))
          (let ((lowered-funcs (lower-functions-to-js functions ctx)))
            (generate-js-module name lowered-funcs)))))))

(define lower-functions-to-js
  (fn (funcs ctx)
    (match funcs
      (case nil nil)
      (case (cons func rest)
        (let ((js-func (lower-function-to-js func ctx)))
          (cons js-func (lower-functions-to-js rest ctx)))))))

(define lower-function-to-js
  (fn (func ctx)
    (match func
      (case (mlir-function name type attrs body)
        (let ((params (extract-param-names type)))
          (let ((body-expr (lower-region-to-js body params ctx)))
            (js-const name (js-arrow params body-expr))))))))

;; ============================================================================
;; REGION AND BLOCK LOWERING
;; ============================================================================

(define lower-region-to-js
  (fn (region params ctx)
    (match region
      (case (mlir-region blocks)
        (match blocks
          (case (cons block nil)
            ;; Single block - inline it
            (lower-block-to-js block ctx))
          (case _
            ;; Multiple blocks need more complex lowering
            (lower-blocks-with-control-flow blocks ctx)))))))

(define lower-block-to-js
  (fn (block ctx)
    (match block
      (case (mlir-block label args ops)
        (let ((js-ops (lower-ops-to-js ops ctx)))
          (js-block js-ops))))))

;; ============================================================================
;; OPERATION LOWERING
;; ============================================================================

(define lower-ops-to-js
  (fn (ops ctx)
    (match ops
      (case nil nil)
      (case (cons op rest)
        (let ((js-exprs (lower-op-to-js op ctx)))
          (append js-exprs (lower-ops-to-js rest ctx)))))))

(define lower-op-to-js
  (fn (op ctx)
    (match op
      ;; Constants
      (case (mlir-const-nat n result)
        (let ((var-name (fresh-js-var ctx)))
          (list (js-const var-name (js-call (js-prop (js-var "PF") "nat") 
                                           (list (js-number n)))))))
      
      (case (mlir-const-bool b result)
        (let ((var-name (fresh-js-var ctx)))
          (list (js-const var-name (js-prop (js-var "PF") (if b "true" "false"))))))
      
      (case (mlir-const-string s result)
        (let ((var-name (fresh-js-var ctx)))
          (list (js-const var-name (js-call (js-prop (js-var "PF") "string")
                                           (list (js-string s)))))))
      
      ;; Constructor
      (case (mlir-constructor name args result evidence)
        (let ((var-name (fresh-js-var ctx)))
          (let ((js-args (map (fn (arg) (lookup-js-var arg ctx)) args)))
            (list (js-const var-name 
                           (js-call (js-prop (js-var "PF") "constructor")
                                   (list (js-string name) (js-array js-args))))))))
      
      ;; Nat elimination
      (case (mlir-nat-elim motive base step target result evidence)
        (lower-nat-elim-to-js motive base step target result ctx))
      
      ;; Bool elimination
      (case (mlir-bool-elim motive false-case true-case target result evidence)
        (lower-bool-elim-to-js motive false-case true-case target result ctx))
      
      ;; Function application
      (case (mlir-apply func arg result evidence)
        (lower-apply-to-js func arg result ctx))
      
      ;; Effects
      (case (mlir-perform effect result evidence)
        (lower-perform-to-js effect result ctx))
      
      ;; Return
      (case (mlir-return values)
        (match values
          (case nil (list (js-return-stmt js-undefined)))
          (case (cons val nil) 
            (list (js-return-stmt (lookup-js-var val ctx))))
          (case _ 
            (list (js-return-stmt (js-array (map (fn (v) (lookup-js-var v ctx)) values)))))))
      
      ;; Default
      (case _ nil))))

;; ============================================================================
;; NAT ELIMINATION TO JS
;; ============================================================================

(define lower-nat-elim-to-js
  (fn (motive base step target result ctx)
    (let ((var-name (fresh-js-var ctx)))
      (let ((motive-var (lookup-js-var motive ctx)))
        (let ((base-var (lookup-js-var base ctx)))
          (let ((step-var (lookup-js-var step ctx)))
            (let ((target-var (lookup-js-var target ctx)))
              (list (js-const var-name
                             (js-call (js-prop (js-var "PF") "natElim")
                                     (list motive-var base-var step-var target-var)))))))))))

;; ============================================================================
;; BOOL ELIMINATION TO JS
;; ============================================================================

(define lower-bool-elim-to-js
  (fn (motive false-case true-case target result ctx)
    (let ((var-name (fresh-js-var ctx)))
      (let ((motive-var (lookup-js-var motive ctx)))
        (let ((false-var (lookup-js-var false-case ctx)))
          (let ((true-var (lookup-js-var true-case ctx)))
            (let ((target-var (lookup-js-var target ctx)))
              (list (js-const var-name
                             (js-call (js-prop (js-var "PF") "boolElim")
                                     (list motive-var false-var true-var target-var)))))))))))

;; ============================================================================
;; APPLICATION TO JS
;; ============================================================================

(define lower-apply-to-js
  (fn (func arg result ctx)
    (let ((var-name (fresh-js-var ctx)))
      (let ((func-var (lookup-js-var func ctx)))
        (let ((arg-var (lookup-js-var arg ctx)))
          ;; Check if it's a closure
          (list (js-const var-name
                         (js-ternary 
                           (js-binop "===" 
                                    (js-prop func-var "tag")
                                    (js-string "closure"))
                           ;; If closure, apply with environment
                           (js-call (js-prop func-var "fn")
                                   (list (js-prop func-var "env") arg-var))
                           ;; Otherwise, direct call
                           (js-call func-var (list arg-var)))))))))

;; ============================================================================
;; EFFECT TO JS
;; ============================================================================

(define lower-perform-to-js
  (fn (effect result ctx)
    (match effect
      (case (mlir-print-effect msg)
        (let ((var-name (fresh-js-var ctx)))
          (let ((msg-var (lookup-js-var msg ctx)))
            (list (js-const var-name
                           (js-call (js-prop (js-var "PF") "perform")
                                   (list (js-object (list 
                                           (pair "name" (js-string "print"))
                                           (pair "args" (js-array (list msg-var))))))))))))
      
      (case _ nil))))

;; ============================================================================
;; CODE GENERATION
;; ============================================================================

(define generate-js-module
  (fn (name funcs)
    (string-append js-runtime-prelude "\n\n"
      (string-append "// PathFinder Module: " (string-append name "\n\n"
        (string-append (generate-functions funcs) "\n\n"
          (generate-exports funcs)))))))

(define generate-functions
  (fn (funcs)
    (string-join "\n\n" (map js-stmt-to-string funcs))))

(define generate-exports
  (fn (funcs)
    (string-append "// Exports\nmodule.exports = {\n"
      (string-append (string-join ",\n" (map generate-export funcs))
        "\n};"))))

(define generate-export
  (fn (func)
    (match func
      (case (js-const name _)
        (string-append "  " (string-append name ": " name)))
      (case _ ""))))

;; ============================================================================
;; JS AST TO STRING
;; ============================================================================

(define js-expr-to-string
  (fn (expr)
    (match expr
      (case (js-number n) (nat-to-string n))
      (case (js-bool b) (if b "true" "false"))
      (case (js-string s) (string-append "\"" (string-append s "\"")))
      (case js-null "null")
      (case js-undefined "undefined")
      (case (js-var name) name)
      (case (js-prop obj prop)
        (string-append (js-expr-to-string obj) (string-append "." prop)))
      (case (js-binop op left right)
        (string-append "(" (string-append (js-expr-to-string left)
          (string-append " " (string-append op 
            (string-append " " (string-append (js-expr-to-string right) ")")))))))
      (case (js-call func args)
        (string-append (js-expr-to-string func)
          (string-append "(" 
            (string-append (string-join ", " (map js-expr-to-string args)) ")"))))
      (case (js-arrow params body)
        (string-append "(" (string-append (string-join ", " params)
          (string-append ") => " (js-expr-to-string body)))))
      (case (js-array elems)
        (string-append "[" (string-append (string-join ", " (map js-expr-to-string elems)) "]")))
      (case (js-object fields)
        (string-append "{ " (string-append (string-join ", " (map js-field-to-string fields)) " }")))
      (case (js-return expr)
        (string-append "return " (js-expr-to-string expr)))
      (case (js-block stmts)
        (string-append "{\n" (string-append (string-join "\n" (map js-expr-to-string stmts)) "\n}")))
      (case _ "/* unknown expr */"))))

(define js-stmt-to-string
  (fn (stmt)
    (match stmt
      (case (js-const name expr)
        (string-append "const " (string-append name 
          (string-append " = " (string-append (js-expr-to-string expr) ";")))))
      (case (js-let name expr)
        (string-append "let " (string-append name
          (string-append " = " (string-append (js-expr-to-string expr) ";")))))
      (case _ "/* unknown stmt */"))))

(define js-field-to-string
  (fn (field)
    (match field
      (case (pair name expr)
        (string-append name ": " (js-expr-to-string expr))))))

;; ============================================================================
;; HELPERS
;; ============================================================================

(define make-js-context
  (fn ()
    (js-context nil nil zero nil)))

(define fresh-js-var
  (fn (ctx)
    (match ctx
      (case (js-context _ _ n _)
        (string-append "_v" (nat-to-string n))))))

(define lookup-js-var
  (fn (mlir-val ctx)
    (js-var (string-append "_v" (nat-to-string zero)))))

(define extract-param-names
  (fn (type)
    (list "x")))  ;; Simplified

(define lower-blocks-with-control-flow
  (fn (blocks ctx)
    (js-block nil)))  ;; Simplified

;; Stubs
(define string-join (fn (sep lst) "joined"))
(define string-append (fn (s1 s2) s1))
(define nat-to-string (fn (n) "0"))
(define map (fn (f lst) nil))
(define append (fn (l1 l2) l1))
(define list (fn (x) (cons x nil)))
(define pair (fn (x y) (cons x y)))

(data Option U0
  (case none Option)
  (case some (-> JSExpr Option)))

;; ============================================================================
;; PUBLIC API
;; ============================================================================

(define compile-to-javascript
  (fn (mlir-module)
    (generate-js-module "PathFinder" (lower-module-to-js mlir-module))))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export compile-to-javascript)
(export lower-module-to-js)
(export js-runtime-prelude)