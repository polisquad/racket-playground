#lang racket

#|
	Mutability example
|#
(define (f1)
	(let ((x (vector 1 2 3)))
		x
	)
)
(define v1 (f1))
(vector-set! v1 0 10)
(display v1)(newline)

#|
	Non mutable examples (literal instead of vector constructor)
|#
(define (f2)
	(let ((x #(1 2 3)))
		x
	)
)
(define v2 (f2))
;(vector-set! v2 0 10) ;-- Contract violation (immutable)
(display v2)(newline)

#|
	Pass-by-value proof
|#
(define (ref obj)
	(set! obj "local")
	(display obj)
	(newline)
)
(define obj "global")
(ref obj)
(display obj)(newline)

#|
	If we pass-by-value reference of a mutable object there can be side effects
|#
(define (mutateMe v)
	(vector-set! v 0 "lol")
)
(define v3 (vector 1 2 3)) ;-- Must be mutable
(mutateMe v3)
(display v3)(newline)