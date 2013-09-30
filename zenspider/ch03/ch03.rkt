#lang racket

(require rackunit)

;; 3.2

(define (square n)
  (* n n))

;; 3.3

(check-false (zero? 1))
(check-true  (zero? (sub1 1)))
(check-false (symbol=? 'foo 'FoO))

(check-equal? (expt 53 53)
              24356848165022712132477606520104725518533453128685640844505130879576720609150223301256150373)

(check-equal? (sqrt -1)
              0+1i)

(check-equal? (* (sqrt -1) (sqrt -1))
             -1)

(check-equal? (/ 4 6)
              2/3)

(check-= 0.66666 (/ 4.0 6) 0.001)

(check-equal? "tutti frutti" "tutti frutti")

(check-equal? (string-append "tutti" "frutti")
              "tuttifrutti")

(check-equal? (string-append "tutti" " " "frutti")
              "tutti frutti")

;; 3.4

(check-equal? (cons 1 2)
              '(1 . 2))

(define cell (cons 'a 'b))

(check-equal? (car cell)
              'a)

(check-equal? (cdr cell)
              'b)

(check-equal? (cons 'chicken empty)
              '(chicken))

(check-equal? (cons 'chicken '())
              '(chicken))

(check-equal? (cons 'pork '(beef chicken))
              '(pork beef chicken))

(check-equal? (cons 'beef (cons 'chicken '()))
              '(beef chicken))

(check-equal? (cons 'pork (cons 'beef (cons 'chicken '())))
              '(pork beef chicken))

(check-equal? (list 'pork 'beef 'chicken)
              '(pork beef chicken))

(check-equal? (first (cons 'pork (cons 'beef (cons 'chicken empty))))
              'pork)

(check-equal? (rest (list 'pork 'beef 'chicken))
              '(beef chicken))

;;; blah blah blah

;; 3.5

(struct student (name id# dorm))

(define freshman1 (student 'Joe 1234 'NewHall))

(check-equal? (student-name freshman1)
              'Joe)

(check-equal? (student-id# freshman1)
              1234)

(define mimi   (student 'Mimi   1234 'NewHall))
(define nicole (student 'Nicole 5678 'NewHall))
(define rose   (student 'Rose   8765 'NewHall))
(define eric   (student 'Eric   4321 'NewHall))
(define in-class (list mimi nicole rose eric))

(check-equal? (student-id# (third in-class))
              8765)

(struct student-body (freshmen sophomores juniors seniors))
(define all-students (student-body (list freshman1 (student 'Mary 0101 'OldHall))
                                   (list (student 'Jeff 5678 'OldHall))
                                   (list (student 'Bob  4321 'Apartment))
                                   empty))

(check-equal? (student-name (first  (student-body-freshmen all-students)))
              'Joe)
(check-equal? (student-name (second (student-body-freshmen all-students)))
              'Mary)
(check-equal? (student-name (first  (student-body-juniors  all-students)))
              'Bob)

(struct example (x y z))
(define ex1 (example 1 2 3))

(check-output-equal? ex1
                     "#<example>")

(struct example2 (p q r) #:transparent)
(define ex2 (example2 9 8 7))

;; ex2

(check-output-equal? ex2
                     "(example2 9 8 7)")
