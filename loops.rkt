#lang racket

#|
	Non-idiomatic example with `let`
|#
(let
	((x 0))
	
	;-- A.k.a. the terrific GOTO
	(let label ()

		;-- If x < 10 print and increment
		(when (< x 10)
			(display x)
			(newline)
			(set! x (+ x 1))
			
			;-- Goto loop start
			(label)
		)
	)
)

#| 
	Idiomatic way
|#
(let label
	((x 0))
	
	;-- If x < 10 print and increment
	(when (< x 10)
		(display x)
		(newline)
		
		;-- Goto loop start
		(label (+ x 1))
	)
)

#| 
	Factorial without tail-recursion
|#
(define (factorial n)
	(if (= n 0)
		1
		(* n (factorial (- n 1)))
	)
)
(factorial 5)

#| 
	Factorial with tail-recursion
|#
(define (factorial2 n)
	(define (factTail n accum)
		(if (= n 0)
			accum
			(factTail (- n 1) (* n accum))
		)
	)

	;-- Bootstrap, with initial values
	(factTail n 1)
)