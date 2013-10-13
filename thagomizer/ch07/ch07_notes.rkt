#lang racket
(require rackunit rackunit/text-ui)

;; 
;; 7.1 Functions as Values
;;

(define (my-map func lst)
  (cond [(empty? lst) empty]
        [else (cons (func (first lst))
                    (my-map func (rest lst)))]))

;; (define (get-posns-from-goo goos)
;;  (cond [(empty? goos) empty]
;;        [else (cons (goo-loc (first goos))
;;                    (get-posns-from-goo (rest goos)))]))

;; VERSUS

;; (define (get-posns-from-goo goos)
;;   (my-map goo-loc goos))

;; (define (rot goos)
;;   (my-map decay goos))

;;
;; 7.2 Lambda
;;

;; (define (rot goos)
;;   (my-map (lambda (f)
;;             (goo (goo-loc f) (sub1 (goo-expire f))))
;;           goos))

(define (sub2 num)
  (- num 2))

(check-equal? (sub2 5) 3)

(check-equal? ((lambda (num) (- num 2)) 5) 3)

;;
;; 7.3 Higher-Order Fun
;;

(define (my-filter pred lst)
  (cond [(empty? lst) empty]
        [(pred (first lst))
         (cons (first lst) (my-filter (rest lst)))]
        [else (my-filter (rest lst))]))
;; equivalent to ruby Enumerable#select

;; (define (renew-goos)
;;   (my-filter (lambda (f) (not (rotten? f))) goos))

(define (my-ormap pred lst)
  (cond [(empty? lst) #f]
        [else (or (pred (first lst))
                  (my-ormap pred (rest lst)))]))

(define (my-andmap pred lst)
  (cond [(empty? lst) #t]
        [else (and (pred (first lst))
                   (my-andmap pred (rest lst)))]))

(check-equal? (my-andmap number? empty) #t)

(check-equal? (my-ormap number? empty) #f)

(check-equal? (my-andmap number? '(1 2 3 "a")) #f)

(check-equal? (my-ormap number? '(1 2 3 "a")) #t)

;; (define (can-eat snake goos)
;;   (define head (snake-head snake))
;;   (my-ormap (lambda (g) (and (close? head g) g)) goos))

;; r as in right
(define (my-foldr f base lst)
  (cond [(empty? lst) base]
        [else (f (first lst) (my-foldr f base (rest lst)))]))

(check-equal? (my-foldr + 0 '(1 2 3)) 6)

;; (define (img-list+scene posns img scene)
;;   (my-foldr (lambda (p s) (img+scene p img s)) scene posns))

;;
;; 7.4 Two More Higher-Order Functions
;;

(define (my-foldl f base lst)
  (cond [(empty? lst) base]
        [else (my-foldl f (f (first lst) base) (rest lst))]))

(check-equal? (my-foldl cons empty '(a b c)) '(c b a))

(check-equal? (my-foldr cons empty '(a b c)) '(a b c))

(check-equal? (my-foldl + 0 '(1 2 3)) 6)

(define (my-build-list n f)
  (define (builder k)
    (cond [(= n k) empty]
          [else (cons (f k) (builder (add1 k)))]))
  (builder 0))

(check-equal? (my-build-list 5 add1) '(1 2 3 4 5))

(check-equal? (my-build-list 10 (lambda (n) (* n 2))) '(0 2 4 6 8 10 12 14 16 18))

;;
;; 7.5 Derive This!
;;

(define (d/dx fun)
  (define ∂ (/ 1 100000))
  (lambda (x)
    (/ (- (fun (+ x ∂)) (fun (- x ∂))) 2 ∂)))

(define two (d/dx (lambda (x) (* 2 x))))

(check-equal? (two 17) 2)

(check-equal? (map two '(2 -1 0 1 24)) '(2 2 2 2 2))

(define newcos (d/dx sin))
(newcos 0)

(map newcos (list (/ pi 2) pi))

;;
;; 7.6 apply
;; 

(define (sum lon) (apply + lon))

(check-equal? (sum '(1 2 3 4 5 6)) 21)

(define (highest lon) (apply max lon))
(check-equal? (highest '(58 64 77 77 22 94 93 78)) 94)

;; (define (row lop) (apply beside (map frame lop)))