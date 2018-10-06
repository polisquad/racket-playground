#lang racket

#|
	Struct definition
|#
(struct Person (
	name			;-- This is immutable
	(age #:mutable)	;-- This is mutable
))
;-- Predefined procedures
(define foo (Person "foo" 21))	;-- Constructor
(Person? foo)					;-- Is? predicate

#|
	Display info about a person
|#
(define (printPerson p)
	(cond
		((Person? p)
			(display "name: ")(display (Person-name p))(newline)
			(display "age:  ")(display (Person-age p))(newline)
		)
		(else
			(error "not a person!")(newline)
		)
	)
)
(printPerson foo)

#|
	Make him salute!
|#
(define (salute p1 p2)
	(cond
		((and (Person? p1) (Person? p2))
			(display "hi ")(display (Person-name p2))(display " I'm ")(display (Person-name p1))(display ", nice to meet you!")(newline)
		)
		(else
			(display "someone's not a person here!")(newline)
		)
	)
)
(define bar (Person "bar" 22))
(salute bar foo)

#|
	Mutable fields can be mutated (you don't say??)
|#
(set-Person-age! foo 22)
(set-Person-age! bar 23)
(display "everybody is getting older!")(newline)
(printPerson foo)(newline)
(printPerson bar)(newline)

#|
	Struct inheritance
|#
(struct Student Person (
	(cfus #:mutable)
))
(define (passExam p cfu)
	(if (and (Student? p) (number? cfu))
		(set-Student-cfus! p (+ (Student-cfus p) cfu))
		;-- else
		(begin
			(display "go away non-student!")(newline)
		)
	)
)
(define sneppy (Student "andrea" 22 50))
(passExam sneppy 5)
(passExam sneppy 5)
(display "new cfus: ")(display (Student-cfus sneppy))(newline)
(passExam foo 5)

