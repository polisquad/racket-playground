#lang racket

; hygenic macros: means that we can define any symbol we want and scheme will substitute them properly for us

;++ macro

(define-syntax ++
  (syntax-rules ()  ; keywords
    ((_ i)
     (set! i (+ 1 i)))))

; try to implement ** and +=
(define (pow x y)
  (define (help x y acc)
    (if (zero? y)
        acc
        (help x (- y 1) (* x acc))))
  (help x y 1))

(define-syntax **
  (syntax-rules ()
    ((_ x 0) 1)
    ((_ x y) (pow x y))))

(define-syntax +=
  (syntax-rules ()
    ((_ x y) (set! x (+ x y)))))

(** 2 0)
(** 6 2)
(define x 2)
(define y 1)
(+= x y)
(display x)
; thunking. wrapping function calls into lambdas such that evaluation is just in time instead of eager

; ... == 'expansion', repeat last element any amount of time you want