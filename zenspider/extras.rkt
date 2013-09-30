#lang racket

(provide check-output-equal?)

(require rackunit)

(define (check-output-equal? x str)
  (check-equal? (with-output-to-string (lambda () (print x)))
                str))
