#lang racket

;;Chapter 7 examples

(define (my-map func lst)
  (cond [(empty? lst) empty]
        [else (cons (func (first lst))
                    (my-map func (rest lst)))]))

(struct goo (loc expire))
(define (rot goos)
  (my-map (lambda (f)
            (goo (goo-loc f) (sub1 (goo-expire f))))
          goos))

(define (my-filter pred lst)
  (cond [(empty? lst) empty]
        [(pred (first lst))
         (cons (first lst) (my-filter (rest lst)))]
        [else (my-filter (rest lst))]))