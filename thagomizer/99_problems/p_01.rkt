#lang racket
;; Find the last box of a list.
;; Example:
;; * (my-last '(a b c d))
;; (D)

(define (my-last l)
  (cond [(empty? l) l]
        [(empty? (cdr l)) l]
        [else (my-last (cdr l))]))

(module+ test 

  (require rackunit rackunit/text-ui)
  
  (check-equal? (my-last '(a b c d)) '(d))
  (check-equal? (my-last '(a)) '(a))
  (check-equal? (my-last '()) '())
  
  "all tests pass")