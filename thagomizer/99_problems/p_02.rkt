#lang racket
;; Find the last but one box of a list. 
;; Example:
;; * (last-but-one '(a b c d))
;; c

(define (last-but-one l) 
  (let ([len (length l)])
    (cond [(< len 2) '()]
          [(= len 2) (car l)]
          [else (last-but-one (cdr l))])))

(module+ test 

  (require rackunit rackunit/text-ui)

  (check-equal? (last-but-one '(a b c d)) 'c)
  (check-equal? (last-but-one '(a b)) 'a)
  (check-equal? (last-but-one '(a)) '())
  (check-equal? (last-but-one '()) '())
  
  "all tests pass")
