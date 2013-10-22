#lang racket
(require rackunit rackunit/text-ui)

;; 9.1 FOR Loops

(for ([i '(1 2 3 4 5)])
  (display i))
(display "\n")

(for-each display '(1 2 3 4 5))
(display "\n")


(check-equal? (for/list ([i '(1 2 3 4 5)])
                (/ 1 i))
              (list 1 (/ 1 2) (/ 1 3) (/ 1 4) (/ 1 5)))

;; above is equivalent to

(check-equal? (map (lambda (x) (/ 1 x)) '(1 2 3 4 5))
              (list 1 (/ 1 2) (/ 1 3) (/ 1 4) (/ 1 5)))



(check-equal? (for/fold ([sqrs 0])
                ([i '(1 2 3 4 5 6 7 8 9 10)])
                (+ (sqr i) sqrs))
              385)

;; above is equivalent to

(check-equal? (foldl (lambda (i sqrs) (+ (sqr i) sqrs))
                     0
                     '(1 2 3 4 5 6 7 8 9 10))
              385)

;; keep track of number of squares over 50

;; 9.2 Multiple Values

(define-values 
  (sum count) 
  (for/fold ([sqrs 0]
             [count 0])
    ([i '(1 2 3 4 5 6 7 8 9 10)])
    (values (+ (sqr i) sqrs)
            (if (> (sqr i) 50)
                (add1 count)
                count))))
(check-equal? sum 385)
(check-equal? count 3)


;; 9.3 Back to FOR/FOLD
(for/fold ([sqrs 0]
           [count 0])
  ([i '(1 2 3 4 5 6 7 8 9 10)])
  (values (+ (sqr i) sqrs)
          (if (> (sqr i) 50)
              (add1 count)
              count)))

;; 9.4 More on Loops

(check-equal? (for/list ([i '(1 2 3 4 5)]
                         #:when (odd? i))
                i)
              '(1 3 5))

(check-equal? (for/fold ([sum 0])
                ([i '(1 2 3 4 5)]
                 #:when (even? i))
                (+ sum i))
              6)

(check-equal? (for/list ([i '(1 2 3 4 5)]
                         [j '(1 2 3 4)]
                         [k '(5 4 3 2 1)])
                (list i j k))
              '((1 1 5) (2 2 4) (3 3 3) (4 4 2)))

(check-equal? (for/list ([i '(1 2 3 4 5)]
                         [s '("a" "b" "c" "d" "e")]
                         #:when (and (even? i)
                                     (string=? s "d")))
                i)
              '(4))

(for ([i '(1 2 3)])
  (for ([j '(1 2 3)])
    (for ([k '(1 2 3)])
      (displayln (list i j k)))))
              
(for* ([i '(1 2 3)]
       [j '(1 2 3)]
       [k '(1 2 3)])
  (displayln (list i j k)))

(check-equal? (for*/list ([i '(1 2 3)]
                          [j '(4 5 6)])
                (+ i j))
              '(5 6 7 6 7 8 7 8 9))

(check-equal? (for/list ([i '(1 2 3)])
                (for/list ([j '(4 5 6)])
                  (+ i j)))
              '((5 6 7) (6 7 8) (7 8 9)))

(check-equal? (for*/list ([k '((1 2) (3 4) (5 6) (7 8))]
                          [n k])
                n)
              '(1 2 3 4 5 6 7 8))

(check-equal? (for/list ([i (in-range 10)])
                i)
              '(0 1 2 3 4 5 6 7 8 9))

(check-equal? (for/list ([i (in-range 5 10)])
                i)
              '(5 6 7 8 9))

(check-equal? (for/list ([i (in-range 0 10 2)])
                i)
              '(0 2 4 6 8))

(in-range 10)


