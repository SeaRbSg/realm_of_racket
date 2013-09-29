#lang racket

(require rackunit)

;; 3.2

(define (square n)
  (* n n))

;; 3.3

(check-false (zero? 1))
(check-true  (zero? (sub1 1)))
(check-false (symbol=? 'foo 'FoO))

(check-equal? 24356848165022712132477606520104725518533453128685640844505130879576720609150223301256150373
              (expt 53 53))

(check-equal? 0+1i
              (sqrt -1))

(check-equal? -1
             (* (sqrt -1) (sqrt -1)))

(check-equal? 2/3
              (/ 4 6))

(check-= 0.66666 (/ 4.0 6) 0.001)

(check-equal? "tutti frutti" "tutti frutti")

(check-equal? "tuttifrutti"  (string-append "tutti" "frutti"))

(check-equal? "tutti frutti" (string-append "tutti" " " "frutti"))

;; 3.4

(check-equal? '(1 . 2) (cons 1 2))

(define cell (cons 'a 'b))

(check-equal? 'a (car cell))

(check-equal? 'b (cdr cell))

(check-equal? '(chicken)
              (cons 'chicken empty))

(check-equal? '(chicken)
              (cons 'chicken '()))

(check-equal? '(pork beef chicken)
              (cons 'pork '(beef chicken)))

(check-equal? '(beef chicken)
              (cons 'beef (cons 'chicken '())))

(check-equal? '(pork beef chicken)
              (cons 'pork (cons 'beef (cons 'chicken '()))))

(check-equal? '(pork beef chicken)
              (list 'pork 'beef 'chicken))

(check-equal? 'pork
              (first (cons 'pork (cons 'beef (cons 'chicken empty)))))

(check-equal? '(beef chicken)
              (rest (list 'pork 'beef 'chicken)))

;;; blah blah blah

;; 3.5

(struct student (name id# dorm))

(define freshman1 (student 'Joe 1234 'NewHall))

(check-equal? 'Joe (student-name freshman1))

(check-equal? 1234 (student-id# freshman1))

(define mimi   (student 'Mimi   1234 'NewHall))
(define nicole (student 'Nicole 5678 'NewHall))
(define rose   (student 'Rose   8765 'NewHall))
(define eric   (student 'Eric   4321 'NewHall))
(define in-class (list mimi nicole rose eric))

(check-equal? 8765 (student-id# (third in-class)))

(struct student-body (freshmen sophomores juniors seniors))
(define all-students (student-body (list freshman1 (student 'Mary 0101 'OldHall))
                                   (list (student 'Jeff 5678 'OldHall))
                                   (list (student 'Bob  4321 'Apartment))
                                   empty))

(check-equal? 'Joe  (student-name (first  (student-body-freshmen all-students))))
(check-equal? 'Mary (student-name (second (student-body-freshmen all-students))))
(check-equal? 'Bob  (student-name (first  (student-body-juniors  all-students))))

(struct example (x y z))
(define ex1 (example 1 2 3))

(check-output-equal? "#<example>" ex1)

(struct example2 (p q r) #:transparent)
(define ex2 (example2 9 8 7))

;; ex2

(check-output-equal? "(example2 9 8 7)" ex2)
