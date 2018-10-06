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
(factorial2 4)

#| 
	For each loops
|#
(define (printList l)
	(for-each (lambda (elem)
		(display elem)
		(newline)
	) l)
)
(printList '(1 2 3 4 5))

#|
	For each with vectors
|#
#|(define (printVec v)
	(vector-for-each (lambda (comp)
		(display comp)
		(newline)
	) v)
)
(printVec #(1 2 3 4 5))|# ;-- Doesn't seem to be defined