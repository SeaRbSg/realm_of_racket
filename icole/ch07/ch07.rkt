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

(define (my-ormap pred lst)
  (cond [(empty? lst) #f]
        [else (or (pred (first lst))
                  (my-ormap pred (rest lst)))]))

(define (my-andmap pred lst)
  (cond [(empty? lst) #t]
        [else (and (pred (first lst))
                   (my-andmap pred (rest lst)))]))

(define (my-foldl f base lst)
  (cond [(empty? lst) base]
        [else (my-foldl f (f (first lst) base) (rest lst))]))

(define (d/dx fun)
  (define ∂ (/ 1 100000))
  (lambda (x)
    (/ (- (fun (+ x ∂)) (fun (- x ∂))) 2 ∂)))
