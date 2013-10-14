#lang racket
(define (my-filter pred lst)
  (cond [(empty? lst)empty]
        [(pred (first lst))
         (cons (first lst)(my-filter pred (rest lst)))]
        [else (my-filter pred (rest lst))]))

(define (my-map func lst)
  (cond [(empty? lst) empty]
        [else (cons (func(first lst))
                    (my-map func (rest lst)))]))

(define (my-foldl func base lst)
  (cond [(empty? lst) base]
        [else (my-foldl func (func (first lst) base) (rest lst))]))

(define (my-foldr func base lst)
  (cond [(empty? lst) base]
        [else (func (first lst) (my-foldr func base (rest lst)))]))

(define (my-build-list n func)
  (define (builder k)
    (cond [(= n k) empty]
          [else (cons (func k) (builder (add1 k)))]))
  (builder 0))

#|
  -Derivative function-
  Takes a a function.
  Returns another function that approximates the
  derivative of the original function
|#
(define (d/dx func)
  (define delta (/ 1 100000))
  (lambda (x)
    (/ (- (func (+ x delta)) (func (- x delta))) 2 delta)))