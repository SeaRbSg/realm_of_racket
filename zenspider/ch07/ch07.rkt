#lang racket

(require rackunit)
(require (file "../extras.rkt"))

;; 7.1

(check-output-equal? add1
                     "#<procedure:add1>")

(define (my-map f xs)
  (cond [(empty? xs) empty]
        [else (cons (f (first xs))
                    (my-map f (rest xs)))]))

(check-equal? (my-map add1 '(1 2 3 4))
              '(2 3 4 5))

;; (define (rot goos)
;;   (my-map decay goos))

;; 7.2

;; (define (rot goos)
;;   (my-map (lambda (f) (goo (goo-loc f) (sub1 (goo-expire f)))) goos))

(define (sub2 n)
  (+ n 2))

;; same as: (lambda (n) (- n 2))

(check-equal? ((lambda (n) (- n 2)) 5)
              3)

;; 7.3

(define (my-filter pred lst)
  (cond [(empty? lst) empty]
        [(pred (first lst)) (cons (first lst) (my-filter (rest lst)))]
        [else (my-filter (rest lst))]))

;; (define (renew goos)
;;   (my-filter (lambda (f) (not (rotten? f))) goos))

(define (my-ormap pred lst)
  (cond [(empty? lst) false]
        [else (or (pred (first lst))
                  (my-ormap pred (rest lst)))]))

(define (my-andmap pred lst)
  (cond [(empty? lst) true]
        [else (and (pred (first lst))
                   (my-andmap pred (rest lst)))]))

(check-true  (my-andmap number? empty))
(check-false (my-ormap  number? empty))
(check-false (my-andmap number? '(1 2 3 "a")))
(check-true  (my-ormap  number? '(1 2 3 "a")))

;; (define (can-eat sn goos)
;;   (define head (snake-head sn))
;;   (my-ormap (lambda (g) (and (close? head g) g)) goos))

(define (my-foldr f base lst)
  (cond [(empty? lst) base]
        [else (f (first lst) (my-foldr f base (rest lst)))]))

(check-equal? (my-foldr + 0 '(1 2 3))
              6)

;; (my-foldr beside empty-image (list <images...>))

;; (define (img-list+scene posns img scene)
;;   (my-foldr (lambda (p s) (img+scene p img s)) scene posns))

;; 7.4

(define (my-foldl f base lst)
  (cond [(empty? lst) base]
        [else (my-foldl f (f (first lst) base) (rest lst))]))

(check-equal? (my-foldl cons empty '(a b c))
              '(c b a))

(check-equal? (my-foldr cons empty '(a b c))
              '(a b c))

(define (my-build-list n f)
  (define (builder k)
    (cond [(= n k) empty]
          [else (cons (f k) (builder (add1 k)))]))
  (builder 0))

(check-equal? (my-build-list 5 add1)
              '(1 2 3 4 5))
(check-equal? (my-build-list 10 (lambda (n) (* n 2)))
              '(0 2 4 6 8 10 12 14 16 18))

;; 7.5

(define (d/dx f)
  (define ∂ (/ 1 100000))
  (lambda (x)
    (/ (- (f (+ x ∂)) (f (- x ∂))) 2 ∂)))

(define two (d/dx (lambda (x) (* 2 x))))
(check-equal? (two 17)
              2)

(check-equal? (map two '(2 -1 0 1 24))
              '(2 2 2 2 2))

(define newcos (d/dx sin))

(check-= (newcos 0)
         0.9999
         0.001)

(define answers (map newcos (list (/ pi 2) pi)))

(check-= (first answers) 0.0    0.001)
(check-= (last answers) -0.9999 0.001)

;; 7.6

(define (sum lon) (apply + lon))
(check-equal? (sum '(1 2 3 4 5 6))
              21)

(define (highest lon) (apply max lon))
(check-equal? (highest '(58 64 77 77 22 94 93 78))
              94)

;; (define (row lop) (apply (beside (map frame lop))))
