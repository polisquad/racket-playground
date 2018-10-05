#lang racket

#| 
	... Hello world ...
|#
(define (helloWorld)
	(display "hello world!")
	(newline)
)
(helloWorld)

#| 
	Reverse list
|#
(define (tsil l)
	(cond
		((null? l) l)
		(else (append
			(tsil (cdr l))
			(tsil (list (car l)))
		))
	)
)

#| 
	Generate a list
|#
(define (genList n)
	(define (genListHelper n)
		(cond
			((< n 1) (list 0))
			(else (append (genListHelper (- n 1)) (list n)))	
		)
	)
	(genListHelper (- n 1))
)
(genList 10)

#| 
	Generate a list with tail-recursion
|#
(define (genList2 n)
	(define (genList2Helper x accum)
		(if (< x n)
			(genList2Helper (+ x 1) (append accum (list x)))
			accum
		)
	)
	(genList2Helper 0 `())
)
(genList2 10)

#| 
	Another list generator which is tail-recursive
|#
(define (genList3 n)
	(if (= n 1)
		(list 0)
		(let
			((p (- n 1)))
			(append (genList3 p) (list p))
		)
	)
)
(genList3 10)

#| 
	Flatten list
|#
(define (flat ls)
	(define (flatHelper l accum)
		(cond
			((null? l) accum)
			((list? (car l)) (flatHelper (cdr l) (append accum (flat (car l)))))
			(else (flatHelper (cdr l) (append accum (list (car l)))))
		)
	)
	(flatHelper ls `())
)
(flat `(1 (2 3 (4 5 (6 7))) 8 (9 10)))

#| 
	Cartesian product
|#
(define (cartesian v1 v2)
	v1
)