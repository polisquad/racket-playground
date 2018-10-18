#lang racket

(define (mul35 n)
  (define (help n acc)
    (if (< n 3)
        acc
        (if (or (zero? (modulo n 3)) (zero? (modulo n 5)))
            (help (- n 1) (+ acc n))
            (help (- n 1) acc))))
  (help (- n 1) 0))

(mul35 1000)  ;-- 233168