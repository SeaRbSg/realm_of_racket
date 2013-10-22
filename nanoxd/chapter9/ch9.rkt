#lang racket

(for/list ([i '(1 2 3 4 5)])
    (/ 1 i))

(map (lambda (x) (/ 1 x)) '(1 2 3 4 5))

(for/fold ([sqrs 0])
  ([i '(1 2 3 4 5 6 7 8 9 10)])
  (+ (sqr i) sqrs))

(foldl (lambda (i sqrs) (+ (sqr i) sqrs))
       0
       '(1 2 3 4 5 6 7 8 9 10))

(for/fold ([sqrs 0]
           [count 0])
  ([i '(1 2 3 4 5 6 7 8 9 10)])
  (values (+ (sqr i) sqrs)
          (if (> (sqr i) 50)
              (add1 count)
              count)))

(for/list ([i '(1 2 3 4 5)]
             [j '(1 2 3 4)]
             [k '(5 4 3 2 1)])
    (list i j k))

(for/list ([i '(1 2 3 4 5)]
           [s '("a" "b" "c" "d" "e")]
           #:when (and (even? i) (string=? s "d")))
  i)

(for* ([i '(1 2 3)]
       [j '(1 2 3)]
       [k '(1 2 3)])
       (displayln (list i j k)))

(for*/list ([i '(1 2 3)]
              [j '(4 5 6)])
              (+ i j))

(for*/list ([k '((1 2) (3 4) (5 6) (7 8))]
            [n k])
  n)

(for/list ([i (in-range 10)])
  i)

(for/list ([i (in-range 5 10)])
  i)

(for/list ([i (in-range 0 10 2)])
  i)
