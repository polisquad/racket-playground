#lang racket

; Reverses a list
(define (reverse_ l)
  (define (loop curr acc)
  (if (null? curr)
    acc
    (loop (cdr curr) (cons (car curr) acc))
    ))
  (loop l '()))


; Return a list of elements from 0 to n excluded if n > 0 otherwise empty list
(define (till n)
  (define (loop x l)
    (if (< x n)
      (loop (+ x 1) (append l (list x)))
      l
     ))
  (loop 0 '()))


; Flattens a list
(define (flatten_ l)
  (define (loop curr acc)
    ; To see the evaluation
    ; (display curr)
    ; (display acc)
    (if (null? curr)
      acc
    (if (list? (car curr))
      (loop (cdr curr) (append acc (flatten_ (car curr))))
      (loop (cdr curr) (append acc (list (car curr)))))))
  (loop l '()))


(reverse_ (list 1 2 3 4 5))
(till 10)
(flatten_ (list 1 2 3 (list 2 (list (list 7 8) 4 5) (list 6 7))))
