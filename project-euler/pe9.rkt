#lang racket
; https://projecteuler.net/problem=9
; https://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple
(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (modulo a b))))

(define (coprimes? a b)
  (= (gcd a b) 1))

(define (triplet k m n)
  (let ((a (* k (- (* m m) (* n n))))  ; a = k*(m^2 - n^2)
        (b (* k 2 m n))
        (c (* k (+ (* m m) (* n n))))) ; c = k*(m^2 + n^2)
    (list a b c)))

(define (solve target)
  (define (help k m n)
    (let ((t (triplet k m n)))
      (apply (lambda (a b c)
               (cond [(> a target) (list a b c)]
                     [(> (+ a b c) target) (help 1 (+ m 2) n)]
                     [(= (+ a b c) target) (* a b c)]
                     [else (help (+ k 1) m n)])
               )
             t)))
  (help 1 2 1))

(solve 1000)