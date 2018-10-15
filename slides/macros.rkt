#lang racket

#|
	Define a while syntax
|#
(define-syntax while
	(syntax-rules () (
		(_ condition body ...)	;-- Syntax pattern
		(let loop ()			;-- Expansion
			(when condition
				(begin
					body ...
					(loop)
				)	
			)
		)
	))
)
(define a 0)
(while (< a 10) ;-- 10 hello world
	(display "hello world")
	(newline)
	(set! a (+ a 1))
)

#|
	My custom recursive let
|#
(define-syntax rec-let*
	(syntax-rules ()

		;-- First syntax
		((_ ((var val)) istr ...)
			((lambda (var) istr ...) val))
		
		;-- Second syntax
		((_ ((var val) . others) istr ...)
			((lambda (var) (rec-let* others istr ...)) val))
	)
)
(rec-let*
	((x 1)(y 2))
	(rec-let*
		((y x)(x y))
		(display x)(newline)
		(display y)(newline)
	)
)

#|
	Let implementation
|#
(define-syntax slet
	(syntax-rules ()
		(
			(_ ((var val) ...) istr ...)
			((lambda (var ...) istr ...) val ...)
		)
	)
)
(slet
	((x 1)(y 2))
	(slet
		((x y)(y x))
		(display x)(newline)
		(display y)(newline)
	)
	
)