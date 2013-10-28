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

;; They call these 'my-ormap' and 'my-andmap' in the book but that seems silly. 
;; I called them 'any' and 'all'.
(define (any predicate lst)
  (cond [(empty? lst) #f]
        [else (or (predicate (first lst))
                  (any predicate (rest lst)))]))

(define (all predicate lst)
  (cond [(empty? lst) #f]
        [else (and (predicate (first lst))
                   (all predicate (rest lst)))]))

;; 'my-folder' seemed silly too so I called this 'accumulate'. 
(define (accumulate func base lst)
  (cond [(empty? lst) base]
        [else (func (first lst) (accumulate func base (rest lst)))]))

(define (my-build-list start func)
  (define (builder n)
    (cond [(= start n) empty]
          [else (cons (func n) (builder (add1 n)))]))
  (builder 0))

;; Tests! I hadn't written any in the chapters prior because I'm bad.
(module+ test
  (require rackunit)
  
  (define test-predicate (lambda (n) (equal? n 2)))
  (define test-accumulator (lambda (x y) (+ x y)))
  
  (check-equal? (any test-predicate '(1 3 4)) #f)
  (check-equal? (any test-predicate '(1 2 4)) #t)
  (check-equal? (all test-predicate '(2 2 1)) #f)
  (check-equal? (any test-predicate '(2 2 2)) #t)
  (check-equal? (accumulate test-accumulator 2 '(1 2 3)) 8)
  (check-equal? (my-build-list 4 (lambda (i) (* i 2))) '(0 2 4 6))
  (check-equal? (apply + '(1 2 3 4 5)) 15))
