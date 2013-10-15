#lang racket

;; 7.1 Functions as Values
(define (my-map func lst)
  (cond [(empty? lst) empty]
        [else (cons (func (first lst))
                    (my-map func (rest lst)))]))

;; From Snake game in Ch. 6
#|
(define (get-posns-from-goo goos)
  (cond [(empty? goos) empty]
        [else (cons (goo-loc (first goos))
                    (get-posns-from-goo (rest goos)))]))
|#

;; Could be simplified to read:
#|
(define (get-posns-from-goo goos)
  (my-map goo-loc goos))
|#

;(define (rot goos)
;  (my-map decay goos))

;; 7.2 Lambda

#|
(define (rot goos)
  (my-map (lambda (f)
            (goo (goo-loc f) (sub1 (goo-expire f))))
          goos))
|#

; Defining a sub2 function
(define (sub2 num)
  (- num 2))

; Using a lambda
((lambda (num)(- num 2)) 5)

;; 7.3 Higher-Order Fun

; Applied to a predicate and a list
; Keeps the value if true
(define (my-filter pred lst)
  (cond [(empty? lst) empty]
        [(pred (first lst))
         (cons (first lst) (my-filter pred (rest lst)))]
        [else (my-filter (rest lst))]))

;; Snake

#|
(define (renew goos)
  (my-filter (lambda (f) (not (rotten? f))) goos))
|#

; If any item in the list returns true, pred applied
(define (my-ormap pred lst)
  (cond [(empty? lst) #f]
        [else (or (pred (first lst))
                  (my-ormap pred (rest lst)))]))

; Opposite of ormap
(define (my-andmap pred lst)
  (cond [(empty? lst) #t]
        [else (and (pred (first lst))
                   (my-andmap pred (rest lst)))]))

(my-andmap number? empty)
; #t
(my-ormap number? empty)
; #f
(my-andmap number? '(1 2 3 "a"))
; #f
(my-ormap number? '(1 2 3 "a"))
; #t

; Snake example
#|
(define (can-eat snake goos)
  (define head (snake-head snake))
  (my-ormap (lambda (g) (and (close? head g) g)) goos))
|#

; Takes a function, a base value and a list
(define (my-foldr f base lst)
  (cond [(empty? lst) base]
        [else (f (first lst) (my-foldr f base (rest lst)))]))

; Use of my-foldr in snake
#|
(define (img-list+scene posns img scene)
  (my-foldr (lambda (p s) (img+scene p img s))
            schene posns))
|#

;; 7.4 Two More Higher-Order Functions

(define (my-foldl f base lst)
  (cond [(empty? lst) base]
        [else (my-foldl f (f (first lst) base) (rest lst))]))

(my-foldl cons empty '(a b c))
; '(c b a)

; Takes in a number and a function

(define (my-build-list n f)
  (define (builder k)
    (cond [(= n k) empty]
          [else (cons (f k) (builder (add1 k)))]))
  (builder 0))

;; 7.5 Derive This!

; ∂ stands for a partial derivative
(define (d/dx fun)
  (define ∂ (/ 1 100000))
  (lambda (x)
    (/ (- (fun (+ x ∂)) (fun (- x ∂))) 2 ∂)))


