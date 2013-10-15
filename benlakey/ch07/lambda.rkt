#lang racket

(define (my-map func lst)
    (cond [(empty? lst) empty]
          [else (cons (func (first lst))
                      (my-map func (rest lst)))]))

(my-map add1 '(1 2 3 4))

;; and now with a lambda for no good reason
(define (add-them-by-1 lst)
  (my-map (lambda (thing)
            (add1 thing))
          lst))

(add-them-by-1 '(1 2 3 4))

;; basic lambda
((lambda (n) (+ n 1)) 5)

;; filtration
(define (my-filter predicate lst)
  (cond [(empty? lst) empty]
        [(predicate (first lst)) (cons(first lst) (my-filter predicate (rest lst)))]
        [else (my-filter predicate (rest lst))]))

(my-filter (lambda (n) (equal? 1 n)) '(1 2 1 3 1 4))

;; They call this 'my-ormap' in the book but that seems silly. I called it 'any'.
(define (any predicate lst)
  (cond [(empty? lst) #f]
        [else (or (predicate (first lst))
                  (any predicate (rest lst)))]))

(any (lambda (n) (equal? n 2)) '(1 2 3))
(any (lambda (n) (equal? n 4)) '(1 2 3))

;; Tests! I hadn't written any in the chapters prior because I'm bad.
(module+ test
  (require rackunit)
  
  (define test-predicate (lambda (n) (equal? n 2)))
  (check-equal? #f (any test-predicate '(1 3 4)))
  (check-equal? #t (any test-predicate '(1 2 4))))
