#lang racket

(require rackunit)
(require (file "../extras.rkt"))

;; 9.1

;; (for ([i '(1 2 3 4 5)])
;;      (print i))
;; (newline)

;; (for-each display '(1 2 3 4 5))

(check-equal?
 (for/list ([i '(1 2 3 4 5)])
           (/ 1 i))
 '(1 1/2 1/3 1/4 1/5))

(check-equal?
 (map (lambda (x) (/ 1 x)) '(1 2 3 4 5))
 '(1 1/2 1/3 1/4 1/5))

(check-equal?
 (for/fold ([sqrs 0])
           ([i '(1 2 3 4 5 6 7 8 9 10)])
           (+ (sqr i) sqrs))
 (+ 1 4 9 16 25 36 49 64 81 100))

(check-equal?
 (foldl (lambda (i sqrs) (+ (sqr i) sqrs))
        0
        '(1 2 3 4 5 6 7 8 9 10))
 (+ 1 4 9 16 25 36 49 64 81 100))

(define (check-values-equal? thunk xs)  ; RAWR!
  (check-equal? (call-with-values thunk list) xs))

(check-values-equal? (lambda ()
                       (for/fold ([sqrs 0]
                                  [count 0])
                                 ([i '(1 2 3 4 5 6 7 8 9 10)])
                                 (values (+ (sqr i) sqrs)
                                         (if (> (sqr i) 50)
                                             (add1 count)
                                             count))))
                     '(385 3))

;; 9.2:

(check-values-equal? (lambda () (values 42))
                     '(42))

(check-values-equal? (lambda () (values 'this 'and-this 'and-that))
                     '(this and-this and-that))

(define-values (one two three)
  (values 'three 'two 'one))

(check-equal? one   'three)
(check-equal? two   'two)
(check-equal? three 'one)

;; (define-values (x y)
;;   (if (string=? (today) "tuesday")
;;       (values 10 20)
;;       (values 42 55)))

;; 9.3:

;; 9.4:

(check-equal?
 (for/list ([i '(1 2 3 4 5)]
            #:when (odd? i))
           i)
 '(1 3 5))

(check-equal?
 (for/fold ([sum 0])
           ([i '(1 2 3 4 5)]
            #:when (even? i))
           (+ sum i))
 6)

(check-equal?
 (for/list ([i '(1 2 3 4 5)]
            [j '(1 2 3 4)]
            [k '(5 4 3 2 1)])
           (list i j k))
 '((1 1 5) (2 2 4) (3 3 3) (4 4 2)))

(check-equal?
 (for/list ([i '(1 2 3 4 5)]
            [s '("a" "b" "c" "d" "e")]
            #:when (and (even? i) (string=? s "d")))
           i)
 '(4))

;; (for* ([i '(1 2 3)]
;;        [j '(1 2 3)]
;;        [k '(1 2 3)])
;;       (list i j k))

(check-equal?
 (for*/list ([i '(1 2 3)]
             [j '(4 5 6)])
            (+ i j))
 '(5 6 7 6 7 8 7 8 9))

(check-equal?
 (for/list ([i '(1 2 3)])
           (for/list ([j '(4 5 6)])
                     (+ i j)))
 '((5 6 7) (6 7 8) (7 8 9)))

(check-equal?
 (for*/list ([k '((1 2) (3 4) (5 6) (7 8))]
             [n k])
            n)
 '(1 2 3 4 5 6 7 8))

(check-equal?
 (for/list ([i (in-range 10)]) i)
 '(0 1 2 3 4 5 6 7 8 9))

(check-equal?
 (for/list ([i (in-range 5 10)]) i)
 '(5 6 7 8 9))

(check-equal?
 (for/list ([i (in-range 0 10 2)]) i)
 '(0 2 4 6 8))
