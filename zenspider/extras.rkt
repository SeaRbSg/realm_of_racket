#lang racket

(provide check-output-equal?)

(require rackunit)

(define (check-output-equal? str x)
  (check-equal? str (with-output-to-string (lambda () (print x)))))
