#lang racket

#|
	A first example of call/cc (current continuation)
|#
(+ 3 (call/cc
	(lambda (exit)
		(for-each
			(lambda (x)
				(when (negative? x) (exit x))
			)
			'(54 0 37 -3 245 19)
		)
		10
	)
))
;-- 0 is returned even if we expected 10

#|
	Another example
|#
(define saved-cont #f) ;-- Place to save k
(define (test-cont)
	(let
		((x 0))
		(call/cc
			(lambda (k)
				(set! saved-cont k)
				(k)
			)
		)
		(set! x (+ x 1))
		(display x)(newline)
	)
)
(test-cont)		;-- 1
(test-cont)		;-- 1
(test-cont)		;-- 1
(saved-cont)	;-- 2
(define other-cont saved-cont)
(define super-cont other-cont)
(test-cont)		;-- 1
(test-cont)		;-- 1
(test-cont)		;-- 1
(other-cont)	;-- 3
(super-cont)	;-- 4
(saved-cont)	;-- 2

#|
	Called them from within a procedure
|#
(define (cont-caller)
	(test-cont)
	(saved-cont)
	(other-cont)
	(super-cont)
)
;(cont-caller) ;-- Infinite loop LOL

#|
	Why not implementing it ourselves (suuure)
	First approach: with garbage collector! (stackless)
	Second approach: with stack
|#
;-- Just joking, not gonna do it if you don't tell me how!

#|
	But we can create a break statement inside a for loop
|#
(define-syntax for-with-break ;-- Good old macros
	(syntax-rules (from to do) ;-- What the fuck is this?? This are the "reserved words of the syntax" which have no "meaning attached"

		((_ var from min to max break do body ...)

			(let* ;-- Why let* and not simply let? because inc uses min1 and max1
				(
					(min1 min)
					(max1 max)
					(inc (if (< min1 max1) + -)) ;-- Machine learning technique to recognize direction LOL
					;(inc (if (< min max) + -)) ;-- This works with let
				)
				(call/cc
					(lambda (break)
						(let loop
							((var min1))
							body ...
							(unless (= var max1) (loop (inc var 1)))
						)
					)
				)
			)
		)
	)
)
(for-with-break i from 1 to 10 break do
	(display i)
	(when (= i 5) (break)) ;-- So this break it's like a goto to the start of the loop, but without restarting the loop?
)

; EXCEPTIONS ------------------------------------

#|
	Exceptions with continuations ... interesting
	First, a stack oh handlers (nested exceptions to watch)
|#
(define __handlers__ (list)) ;-- A list of handlers (i.e. procedures)

#|
	Add an handler to the list
|#
(define (push-handler proc)
	(set! __handlers__ (cons proc __handlers__))
)

#|
	Remove an handler from the list
|#
(define (pop-handler)
	(let
		((head (car __handlers__))) ;-- Save it to return it
		(set! __handlers__ (cdr __handlers__)) ;-- Move LL to next
		head ;-- Return saved handler
	)
)

#|
	Pop and call an handler (i.e. throw an exception)
|#
(define (throw x)
	(if (pair? __handlers__)
		((pop-handler) x)
		(apply error x)
	)
)

#|
	Finally, try-catch syntax
|#
(define-syntax try
	(syntax-rules (catch)
		((_ body ... (catch exception recover ...))
			(call/cc
				(lambda (reset)
					;-- Register the handler
					(push-handler (lambda (x)
						(if (equal? x exception)
							(exit
								;-- Eval recover
								(begin recover ...)
							)
							(throw x)
						)
					))

					(let
						;-- Eval body
						((result (begin body ...))) ;-- So what if something happens here??

						;-- All ok, discard handler
						(pop-handler)

						;-- Return result
						result
					)
				)
			)
		)
	)
)

#|
	Define a stupid foo that throws a bar exception
|#
(define (foo x)
	(display x)(newline)
	(throw "Bar")
)

;-- Try to foo (to foo or no to foo?)
(try
	(foo 2)
	(display "all ok")

	(catch "Bar"
		(display "don't be a fool")(newline))
)