#lang racket

#|
	Simple example of a closure
|#
(define (genAdder base)
	(lambda (x)
		(+ base x)
	)
)
(define add5 (genAdder 5))
(define sub5 (genAdder -5))
(display "same? ")(= (add5 5) (sub5 15))(newline)

#|
	A more complex example
	Closure used as an iterator
|#
(define (iterVec v)
	(let
		((curr 0) (top (vector-length v)))

		;-- This procedure returns a lambda (clousure)
		;-- This lambda mantains its value for curr
		(lambda ()
			(cond
				((= curr top)
					'<<end>> ;-- WFT??
				)
				(else
					(let ((elem (vector-ref v curr)))
						(set! curr (+ curr 1))
						elem
					)
				)
			)
		)
	)
)
(define itr (iterVec #(1 2 3 4))) ;-- Define the iterator
(itr)(itr)(itr)(itr)(itr)(itr)

#|
	Map, filter, accum examples
|#
(map
	(lambda (x)
		(+ 1 x)
	)
	'(1 2 3)
)
(filter
	(lambda (x)
		(< x 3)
	)
	'(2 3 4)
)
(foldl ;-- From right to left
	string-append				;-- Accum function
	""							;-- Initial accumulator
	'("y" "p" "p" "e" "n" "s")	;-- Arguments
)
(foldr ;-- From left to right
	string-append				;-- Accum function
	""							;-- Initial accumulator
	'("s" "n" "e" "p" "p" "y")	;-- Arguments
)
(foldl ;-- Invert a list
	cons
	'()
	'(1 2 3)
)

#|
	Define a factorial using foldr
|#
(define (fact n)
	;-- List generator
	(define (genList n out)
		(if (= n 0)
			out
			(genList (- n 1) (append out (list n)))
		)
	)

	;-- Factorial
	(let
		((nums (genList n '())))
		(foldr
			*
			1
			nums
		)
	)
)
(fact 7)