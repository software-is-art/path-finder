#lang racket/base

(require racket/contract
         racket/match
         racket/string)

(provide (all-defined-out))

;; PathFinder LISP Type System
;; Basic type representations for HoTT-based type system

;; Base type structure
(struct type () #:transparent)

;; Primitive/Atomic types (e.g., Nat, Bool, String)
(struct type-atom type (name) #:transparent)

;; Function types (param-types -> return-type)
(struct function-type type (param-types return-type) #:transparent)

;; Type contracts
(define type/c 
  (or/c type-atom? function-type?))

;; Predefined primitive type instances
(define Nat (type-atom "Nat"))
(define Bool (type-atom "Bool"))
(define String (type-atom "String"))

;; Type constructor helpers
(define/contract (make-type-atom name)
  (-> string? type-atom?)
  (type-atom name))

(define/contract (make-function-type param-types return-type)
  (-> (listof type/c) type/c function-type?)
  (function-type param-types return-type))

;; Type equality comparison
(define/contract (type-equal? t1 t2)
  (-> type/c type/c boolean?)
  (match* (t1 t2)
    ;; Atomic types are equal if their names match
    [((type-atom name1) (type-atom name2))
     (string=? name1 name2)]
    
    ;; Function types are equal if param types and return type are equal
    [((function-type params1 return1) (function-type params2 return2))
     (and (= (length params1) (length params2))
          (andmap type-equal? params1 params2)
          (type-equal? return1 return2))]
    
    ;; Different type constructors are never equal
    [(_ _) #f]))

;; Pretty printing for types
(define/contract (type->string t)
  (-> type/c string?)
  (match t
    [(type-atom name) name]
    [(function-type params return)
     (string-append "("
                   (if (null? params)
                       ""
                       (string-append (string-join (map type->string params) " × ") " "))
                   "→ "
                   (type->string return)
                   ")")]))

;; Check if a type is a primitive type
(define/contract (primitive-type? t)
  (-> type/c boolean?)
  (and (type-atom? t)
       (member (type-atom-name t) '("Nat" "Bool" "String"))
       #t))

;; Helper to create function type with single parameter
(define/contract (make-unary-function-type param-type return-type)
  (-> type/c type/c function-type?)
  (make-function-type (list param-type) return-type))

;; Helper to create function type with two parameters
(define/contract (make-binary-function-type param1-type param2-type return-type)
  (-> type/c type/c type/c function-type?)
  (make-function-type (list param1-type param2-type) return-type))

;; Type validation
(define/contract (valid-type? t)
  (-> any/c boolean?)
  (or (type-atom? t) (function-type? t)))