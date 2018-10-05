#lang racket

#|
	Compute the minimum value of a list of values
|#
(define (minimum list)
	(let
		;-- Define current element and pointer to the next one
		((head (car list)) (ptr (cdr list)))

		(if (null? ptr)

			;-- End of recursion, this is the minimum
			head

			;-- Take minimum between this and next element
			;-- and recursively compare it with the rest of the list
			(minimum
				(cons

					;-- Take minimum
					(if (< head (car ptr))
						head
						(car ptr)
					)

					;-- Attach rest of the list
					(cdr ptr)
				)
			)
		)
	)
)

#|
	Variant with a variable number of arguments
|#
(define (minimum2 elem . rest)
	(if (null? rest)
		;-- Return current element
		;-- End of recursion
		elem

		;-- Recursive call with the current minimum
		(apply minimum2
			;-- Compute partial list
			(cons
				;-- Current minimum
				(if (< elem (car rest))
					elem
					(car rest)
				)

				;-- Rest of the list
				(cdr rest)
			)	
		)
	)
)

;-- Test functions
(minimum `(6 0 2 1 12 29 3))
(minimum2 6 0 2 1 12 29 3)