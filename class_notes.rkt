#lang racket

; Finds the minimum value in a list of atleast one element
(define (minimum l)
  (define (loop curr min_v)
    (if (null? curr)
      min_v
      (if (< (car curr) min_v)
        (loop (cdr curr) (car curr))
        (loop (cdr curr) min_v))))
  (loop (cdr l) (car l)))

; A variant using a varadic function
(define (minimum_ head . tail)
  (if (null? tail)
    head
    (apply minimum_
      (cons
        (if (< head (car tail))
          head
          (car tail))
        (cdr tail)))))

(minimum (list 1 -20 3))
(minimum_ 1 -20 3)

; Translation of tail recursion
(define (fact x)
  (define (loop x accum)
    (if (= x 0) accum
      (loop (- x 1) (* x accum))))
  (loop x 1))

(define (fact-low-level n)
  (define x n)
  (define accum 1)
  (let loop ()
    (if (= x 0)
      accum
      (begin
        (set! accum (* x accum))
        (set! x (- x 1))
        (loop))))) ; goto loop

(= (fact 10) (fact-low-level 10)) ; #t

; closure as an iterator
(define (iter-vector vec)
  (let ((cur 0)
        (top (vector-length vec)))
    (lambda ()
      (if (= cur top)
        'exhausted
        (let ((v (vector-ref vec cur)))
          (set! cur (+ cur 1))
          v)))))

(define next (iter-vector #(1 2)))
(next) ; 1
(next) ; 2
(next) ; 'exhausted<
