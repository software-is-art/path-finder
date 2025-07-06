;; ============================================================================
;; MLIR TO LLVM LOWERING
;; ============================================================================
;; Converts PathFinder MLIR dialect to LLVM IR for native code generation

(import compiler.mlir dialect)
(import types types)

;; ============================================================================
;; LLVM TYPE MAPPING
;; ============================================================================

(data LLVMType U0
  ;; Basic types
  (case llvm-i1 LLVMType)              ;; Boolean
  (case llvm-i8 LLVMType)              ;; Byte
  (case llvm-i64 LLVMType)             ;; Natural/Integer
  (case llvm-ptr LLVMType)             ;; Generic pointer (opaque)
  (case llvm-void LLVMType)            ;; Void
  
  ;; Aggregate types
  (case llvm-struct (-> (fields : List LLVMType) LLVMType))
  (case llvm-array (-> (size : Nat) (elem : LLVMType) LLVMType))
  
  ;; Function types
  (case llvm-func (-> (params : List LLVMType) (ret : LLVMType) LLVMType)))

;; Map PathFinder types to LLVM types
(define pathfinder-type-to-llvm
  (fn (pf-type)
    (match pf-type
      (case mlir-nat llvm-i64)        ;; Nat as 64-bit int
      (case mlir-bool llvm-i1)        ;; Bool as i1
      (case mlir-string llvm-ptr)     ;; String as pointer
      (case mlir-unit llvm-i8)        ;; Unit as i8 (0)
      (case (mlir-closure _ _) 
        ;; Closure as struct { fn_ptr, env_ptr }
        (llvm-struct (list llvm-ptr llvm-ptr)))
      (case _
        llvm-ptr))))                  ;; Everything else as pointer

;; ============================================================================
;; RUNTIME REPRESENTATION
;; ============================================================================

;; PathFinder values in LLVM
(data PFValueRep U0
  ;; Tagged union representation
  (case pf-value-struct
    (-> (tag : LLVMValue)           ;; 0=nat, 1=bool, 2=constructor, 3=closure
        (data : LLVMValue)          ;; Actual data (union)
        PFValueRep)))

;; Runtime functions we need
(define runtime-functions
  (list
    ;; Memory allocation
    (pair "pf_alloc" (llvm-func (list llvm-i64) llvm-ptr))
    
    ;; Constructor creation
    (pair "pf_make_constructor" 
      (llvm-func (list llvm-ptr llvm-i64 llvm-ptr) llvm-ptr))
    
    ;; Closure creation
    (pair "pf_make_closure"
      (llvm-func (list llvm-ptr llvm-ptr llvm-i64) llvm-ptr))
    
    ;; Effect handlers
    (pair "pf_print" (llvm-func (list llvm-ptr) llvm-void))
    (pair "pf_read_file" (llvm-func (list llvm-ptr) llvm-ptr))
    (pair "pf_write_file" (llvm-func (list llvm-ptr llvm-ptr) llvm-void))))

;; ============================================================================
;; LOWERING CONTEXT
;; ============================================================================

(data LLVMLoweringContext U0
  (case llvm-context
    (-> (value-map : List (Pair MLIRValue LLVMValue))
        (current-func : LLVMFunction)
        (runtime-funcs : List (Pair String LLVMValue))
        (next-reg : Nat)
        LLVMLoweringContext)))

;; ============================================================================
;; MODULE LOWERING
;; ============================================================================

(define lower-module-to-llvm
  (fn (mlir-module)
    (match mlir-module
      (case (mlir-module name attrs functions globals)
        (let ((ctx (make-llvm-context)))
          (let ((runtime-decls (declare-runtime-functions ctx)))
            (let ((func-decls (lower-function-declarations functions ctx)))
              (let ((func-defs (lower-function-definitions functions ctx)))
                (make-llvm-module runtime-decls func-decls func-defs)))))))))

;; ============================================================================
;; FUNCTION LOWERING
;; ============================================================================

(define lower-function-to-llvm
  (fn (mlir-func ctx)
    (match mlir-func
      (case (mlir-function name type attrs body)
        (let ((llvm-type (function-type-to-llvm type)))
          (let ((llvm-func (create-llvm-function name llvm-type)))
            (let ((body-ctx (enter-llvm-function llvm-func ctx)))
              (let ((lowered-body (lower-region-to-llvm body body-ctx)))
                llvm-func))))))))

;; ============================================================================
;; OPERATION LOWERING
;; ============================================================================

(define lower-op-to-llvm
  (fn (op ctx)
    (match op
      ;; Constants
      (case (mlir-const-nat n result)
        (let ((llvm-val (emit-llvm-const-i64 n ctx)))
          (bind-llvm-value result llvm-val ctx)))
      
      (case (mlir-const-bool b result)
        (let ((llvm-val (emit-llvm-const-i1 b ctx)))
          (bind-llvm-value result llvm-val ctx)))
      
      (case (mlir-const-string s result)
        (let ((llvm-val (emit-llvm-string-const s ctx)))
          (bind-llvm-value result llvm-val ctx)))
      
      ;; Constructor
      (case (mlir-constructor name args result evidence)
        (lower-constructor-to-llvm name args result ctx))
      
      ;; Nat elimination
      (case (mlir-nat-elim motive base step target result evidence)
        (lower-nat-elim-to-llvm motive base step target result ctx))
      
      ;; Bool elimination
      (case (mlir-bool-elim motive false-case true-case target result evidence)
        (lower-bool-elim-to-llvm motive false-case true-case target result ctx))
      
      ;; Function application
      (case (mlir-apply func arg result evidence)
        (lower-apply-to-llvm func arg result ctx))
      
      ;; Effects
      (case (mlir-perform effect result evidence)
        (lower-effect-to-llvm effect result ctx))
      
      ;; Control flow
      (case (mlir-branch target)
        (emit-llvm-branch target ctx))
      
      (case (mlir-cond-branch cond true-block false-block)
        (lower-cond-branch-to-llvm cond true-block false-block ctx))
      
      (case (mlir-return values)
        (lower-return-to-llvm values ctx)))))

;; ============================================================================
;; NAT ELIMINATION LOWERING
;; ============================================================================

(define lower-nat-elim-to-llvm
  (fn (motive base step target result ctx)
    ;; Generate LLVM loop for nat-elim
    (let ((target-val (lookup-llvm-value target ctx)))
      (let ((base-val (lookup-llvm-value base ctx)))
        (let ((step-val (lookup-llvm-value step ctx)))
          ;; Create loop blocks
          (let ((loop-header (create-llvm-block "nat_elim_header")))
            (let ((loop-body (create-llvm-block "nat_elim_body")))
              (let ((loop-exit (create-llvm-block "nat_elim_exit")))
                ;; Set up phi nodes and loop
                (begin
                  ;; Branch to loop header
                  (emit-llvm-branch loop-header ctx)
                  
                  ;; Loop header: check if n > 0
                  (set-current-block loop-header ctx)
                  (let ((n-phi (emit-llvm-phi llvm-i64 
                                  (list (pair target-val (get-current-block ctx)))))
                        (acc-phi (emit-llvm-phi llvm-ptr
                                   (list (pair base-val (get-current-block ctx))))))
                    (let ((is-zero (emit-llvm-icmp "eq" n-phi (llvm-const-i64 0))))
                      (emit-llvm-cond-br is-zero loop-exit loop-body))
                    
                    ;; Loop body: apply step function
                    (set-current-block loop-body ctx)
                    (let ((n-minus-1 (emit-llvm-sub n-phi (llvm-const-i64 1))))
                      (let ((new-acc (apply-step-function step-val n-minus-1 acc-phi ctx)))
                        ;; Add phi incoming values
                        (add-phi-incoming n-phi n-minus-1 loop-body)
                        (add-phi-incoming acc-phi new-acc loop-body)
                        (emit-llvm-branch loop-header)))
                    
                    ;; Loop exit: return accumulator
                    (set-current-block loop-exit ctx)
                    (bind-llvm-value result acc-phi ctx)))))))))))

;; Apply step function in LLVM
(define apply-step-function
  (fn (step-closure n-val acc-val ctx)
    ;; Extract function pointer from closure
    (let ((fn-ptr (emit-llvm-extractvalue step-closure 0)))
      (let ((env-ptr (emit-llvm-extractvalue step-closure 1)))
        ;; Call step function: step(env, n, acc)
        (emit-llvm-call fn-ptr (list env-ptr n-val acc-val) ctx)))))

;; ============================================================================
;; BOOL ELIMINATION LOWERING
;; ============================================================================

(define lower-bool-elim-to-llvm
  (fn (motive false-case true-case target result ctx)
    (let ((target-val (lookup-llvm-value target ctx)))
      (let ((true-block (create-llvm-block "bool_true")))
        (let ((false-block (create-llvm-block "bool_false")))
          (let ((merge-block (create-llvm-block "bool_merge")))
            ;; Branch on boolean
            (emit-llvm-cond-br target-val true-block false-block)
            
            ;; True block
            (set-current-block true-block ctx)
            (let ((true-val (lookup-llvm-value true-case ctx)))
              (emit-llvm-branch merge-block)
              
              ;; False block  
              (set-current-block false-block ctx)
              (let ((false-val (lookup-llvm-value false-case ctx)))
                (emit-llvm-branch merge-block)
                
                ;; Merge block with phi
                (set-current-block merge-block ctx)
                (let ((phi-val (emit-llvm-phi llvm-ptr
                                 (list (pair true-val true-block)
                                       (pair false-val false-block)))))
                  (bind-llvm-value result phi-val ctx))))))))))

;; ============================================================================
;; CONSTRUCTOR LOWERING
;; ============================================================================

(define lower-constructor-to-llvm
  (fn (name args result ctx)
    ;; Allocate constructor struct
    (let ((num-args (length args)))
      (let ((args-array (allocate-array llvm-ptr num-args ctx)))
        ;; Store arguments
        (store-constructor-args args args-array 0 ctx)
        ;; Call runtime constructor function
        (let ((name-str (emit-llvm-string-const name ctx)))
          (let ((ctor-val (emit-llvm-call "pf_make_constructor"
                            (list name-str (llvm-const-i64 num-args) args-array)
                            ctx)))
            (bind-llvm-value result ctor-val ctx)))))))

;; ============================================================================
;; EFFECT LOWERING
;; ============================================================================

(define lower-effect-to-llvm
  (fn (effect result ctx)
    (match effect
      (case (mlir-print-effect msg)
        (let ((msg-val (lookup-llvm-value msg ctx)))
          (emit-llvm-call "pf_print" (list msg-val) ctx)
          (bind-llvm-value result (llvm-const-i8 0) ctx)))
      
      (case (mlir-read-effect path)
        (let ((path-val (lookup-llvm-value path ctx)))
          (let ((content (emit-llvm-call "pf_read_file" (list path-val) ctx)))
            (bind-llvm-value result content ctx))))
      
      (case (mlir-write-effect path content)
        (let ((path-val (lookup-llvm-value path ctx)))
          (let ((content-val (lookup-llvm-value content ctx)))
            (emit-llvm-call "pf_write_file" (list path-val content-val) ctx)
            (bind-llvm-value result (llvm-const-i8 0) ctx)))))))

;; ============================================================================
;; LLVM IR GENERATION
;; ============================================================================

(define generate-llvm-text
  (fn (llvm-module)
    ;; Generate textual LLVM IR
    (string-append 
      "; PathFinder generated LLVM IR\n"
      "target triple = \"x86_64-unknown-linux-gnu\"\n\n"
      (generate-runtime-declarations)
      "\n"
      (generate-function-definitions llvm-module))))

(define generate-runtime-declarations
  (fn ()
    (string-join "\n"
      (list
        "declare i8* @pf_alloc(i64)"
        "declare i8* @pf_make_constructor(i8*, i64, i8*)"
        "declare i8* @pf_make_closure(i8*, i8*, i64)"
        "declare void @pf_print(i8*)"
        "declare i8* @pf_read_file(i8*)"
        "declare void @pf_write_file(i8*, i8*)"))))

;; ============================================================================
;; HELPERS
;; ============================================================================

(define make-llvm-context
  (fn ()
    (llvm-context nil null-func nil zero)))

(define create-llvm-function
  (fn (name type)
    ;; Stub
    null-func))

(define emit-llvm-const-i64
  (fn (n ctx)
    (llvm-const-int llvm-i64 n)))

(define emit-llvm-const-i1
  (fn (b ctx)
    (llvm-const-int llvm-i1 (if b 1 0))))

(define llvm-const-int
  (fn (type val)
    ;; Stub for LLVM constant
    null-val))

(define bind-llvm-value
  (fn (mlir-val llvm-val ctx)
    ;; Update context with binding
    ctx))

(define lookup-llvm-value
  (fn (mlir-val ctx)
    ;; Lookup in context
    null-val))

;; Stubs
(define null-func "null-func")
(define null-val "null-val")
(define string-join (fn (sep lst) "joined"))
(define length (fn (lst) zero))
(define llvm-const-i64 (fn (n) null-val))
(define llvm-const-i8 (fn (n) null-val))
(define pair (fn (x y) (cons x y)))
(define list (fn (x) (cons x nil)))

(data LLVMValue U0
  (case llvm-const-int (-> LLVMType Nat LLVMValue)))

(data LLVMFunction U0
  (case llvm-function (-> String LLVMFunction)))

;; ============================================================================
;; PUBLIC API
;; ============================================================================

(define compile-to-llvm
  (fn (mlir-module)
    (let ((llvm-module (lower-module-to-llvm mlir-module)))
      (generate-llvm-text llvm-module))))

;; ============================================================================
;; EXPORTS
;; ============================================================================

(export compile-to-llvm)
(export lower-module-to-llvm)
(export pathfinder-type-to-llvm)
(export LLVMType)