#lang racket

#|
	In racket there's no such concept as classes
	However, we can implement an object-oriented approach using closures as objects
|#
(define (Dog)
	(let
		(
			;-- Initial values
			(name "A common dog")
			(age 8)
		)

		;-- Methods

		;-- Object constructor
		(define (create _name _age)
			(set! name _name)
			(set! age _age)
		)

		;-- Say hello to the dog
		(define (sayHello)
			(display "Hello ")(display name)
		)

		;-- Dispatcher
		(lambda (method . args)
			(apply (case method
				((create)	create)
				((sayHello)	sayHello)
				(else (error "Unkown method"))
			) args)
		)
	)
)
(define lucky (Dog))
(lucky 'create "Lucky" 12)
(lucky 'sayHello) ;-- Hello Lucky!
(newline)

#|
	We can even inherit, using delegation
|#
(define (Pet)
	(let
		(
			;-- Parent object
			(parent (Dog))
			(nickname "Doge")
		)

		;-- Constructor
		(define (create _name _age _nickname)
			(parent 'create _name _age)
			(set! nickname _nickname)
		)

		(define (sayHello)
			(parent 'sayHello)
			(display ", a.k.a ")(display nickname)
		)

		;-- Dispatcher
		(lambda (method . args)
			(apply (case method
				((create)	create)
				((sayHello)	sayHello)
				(else (apply parent (cons method args)))
			) args)
		)
	)
)
(define rudi (Pet))
(rudi 'create "Rudolph" 6 "Rudi")
(rudi 'sayHello)
(newline)
;(rudi 'jump) ;-- Undefined method

#|
	Implement a javascript-like prototype-base object-oriented programming approach (bla bla bla)
	We are using hash tables
|#
;-- Just some typedef essentially
(define _new make-hash)
(define _clone hash-copy)

;-- Setter syntax
(define-syntax !!
	(syntax-rules ()
		((_ obj var val)
			(hash-set! obj 'var val)
		)
	)
)

;-- Getter syntax
(define-syntax ??
	(syntax-rules ()
		((_ obj var)
			(hash-ref obj 'var)
		)
	)
)

;-- Method invocation
(define-syntax ->
	(syntax-rules ()
		((_ obj method args ...)
			((hash-ref obj 'method) obj args ...)
		)
	)
)

;-- Define an object
(define car (_new))
;-- Object properties
(!! car 'model "BMW")
(!! car 'year 1998)
(!! car 'hp 150)
;-- Object methods
(!! car 'getInfo (lambda (self)
	(display "model: ")(display (?? self 'model))(newline)
	(display "year: ")(display (?? self 'year))(newline)
	(display "HP: ")(display (?? self 'hp))(newline)
))
;-- Invoke method
(-> car 'getInfo)

;-- Clone object and change its values
(define car2 (_clone car))
(!! car2 'year 2005)
(!! car2 'hp 280)
(-> car2 'getInfo)

#|
	Again, inheritance by delegation
	But i'm tired, go watch the slides
|#