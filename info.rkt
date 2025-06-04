#lang info
(define collection "pathfinder")
(define deps '("base" "rackunit" "typed-racket"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("docs/manual.scrbl" ())))
(define pkg-desc "PathFinder LISP: HoTT-based functional language with algebraic effects")
(define version "0.1.0")
(define pkg-authors '("PathFinder Development Team"))
(define license '(Apache-2.0 OR MIT))