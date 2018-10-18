#lang racket

(define (primes n)
  (define (help curr xs)
    (cond [(> curr n) xs]
          [(empty? (filter (lambda (x) (zero? (modulo curr x))) xs)) (help (+ curr 1) (append xs (list curr)))]
          [else (help (+ curr 1) xs)]))
  (help 2 '()))

(foldl + (primes 2000000))