#lang racket
;; Find the last box of a list.
;; Example:
;; * (my-last '(a b c d))
;; (D)

(define (my-last l)
  (cond [(empty? (cdr l)) l]
        [else
         (my-last (cdr l))]))

(module+ test 

  (require rackunit rackunit/text-ui)
  
  (check-equal? '(d) (my-last '(a b c d)))
  (check-equal? '(a) (my-last '(a)))
  
  "all tests pass")