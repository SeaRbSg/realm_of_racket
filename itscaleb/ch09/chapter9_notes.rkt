#lang racket
;; Chapter 9 - Loops

; basic for loop
(for ([i '(1 2 3 4 5)])
  (display i))

; can use #:when to filter
(for ([i '(1 2 3 4 5)] #:when (odd? i))
  (display i))

; for loops can even run over multiple lists
(for ([i '(1 2 3)]
      [j '(4 5 6)])
  (display (list i j))) ; (1 4)(2 5)(3 6) 

; for/list takes the last value of each iteration and
; collects them into a list

(for/list ([i '(1 2 3 4 5)])
  (/ 1 i))

; for/fold accumulates a value, much like foldl

(for/fold ([sum 0]) ; default value of sum
  ([i '(1 2 3 4 5)])
  (+ sum i)) ; loop returns 15

; we can use 'values' to return multiple values

(for/fold ([sum 0][num_over_10 0]) ; default values for multiple items
  ([i '(2 4 6 8 10 12 14 16 18 20)])
  (values (+ sum i)
          (if (> i 10) 
              (add1 num_over_10)
          num_over_10))) ; loop returns the sum of all items, and the number of items over 10

; the 'for*' variety of loops lets us compactly do nested for loops

(for* ([i '(1 2 3)]
       [j '(1 2 3)])
  (display (list i j))) ; (1 1)(1 2)(1 3)(2 1)(2 2)(2 3)(3 1)(3 2)(3 3)

; we can use the for* loops to flatten multiple lists

(for* ([i '((1 2)(3 4)(5 6))]
       [j i])
  (display j)) ;123456

; in-range returns a 'stream' of sequences

(for ([i (in-range 10)])
  (display i)) ; 0123456789

; it can also take arguments for start, range and skip