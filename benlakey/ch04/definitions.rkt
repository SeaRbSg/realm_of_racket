#lang racket
(define WIDTH 100)
(define HEIGHT 200)

(define X-CENTER (quotient WIDTH 2))
(define Y-CENTER (quotient HEIGHT 2))

(unless (> HEIGHT 0)
  (error 'guess-my-number "HEIGHT may not be negative"))

(define (outer-func arg)
  (define is-even (even? arg))
  is-even)

(outer-func 2)

;;my-sorter is only in scope inside munge
(define (munge lst)
  (define (my-sorter lst)
    (if (empty? lst)
        lst
        '(4 5 6)))
  (my-sorter lst))

(munge '(1 2 3))
(munge '())

